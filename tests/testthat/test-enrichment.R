# Tests for the enrichment analysis that need no network access.

# A small hand-made library used by several tests.
demo_gene_sets <- function() {
  new_togoid_gene_sets(
    sets = list(
      "T:1" = c("CD3D", "CD3E", "CD3G", "LCK", "ZAP70"),
      "T:2" = c("CD3D", "CD3E", "LAT"),
      "B:1" = c("MS4A1", "CD79A", "CD79B", "CD19", "BLNK"),
      "BIG" = paste0("G", seq_len(600)),
      "TINY" = "CD3D"
    ),
    labels = c("T:1" = "TCR signalling", "B:1" = "BCR signalling", "BIG" = "Huge set"),
    id_map = character(0),
    unmapped = character(0),
    route = c("ncbigene", "uniprot", "demo"),
    target_dataset = "demo"
  )
}

test_that("the hypergeometric p-value is exact", {
  # P(X >= 1) with N=4, M=2, n=2 is 1 - C(2,0)C(2,2)/C(4,2) = 1 - 1/6 = 5/6.
  expect_equal(togoid_hypergeometric_pvalue(1, 4, 2, 2), 5 / 6)

  expect_equal(togoid_hypergeometric_pvalue(0, 100, 10, 10), 1)
  expect_equal(togoid_hypergeometric_pvalue(-3, 100, 10, 10), 1)
  expect_equal(togoid_hypergeometric_pvalue(11, 100, 10, 10), 0)

  # It must agree with a one-sided Fisher's exact test on the same table.
  k <- 7; background <- 300; term <- 25; query <- 30
  table <- matrix(
    c(k, term - k, query - k, background - term - query + k),
    nrow = 2
  )
  expect_equal(
    togoid_hypergeometric_pvalue(k, background, term, query),
    stats::fisher.test(table, alternative = "greater")$p.value,
    tolerance = 1e-12
  )

  # Vectorised, and monotonically decreasing in the overlap.
  values <- togoid_hypergeometric_pvalue(1:20, 2000, 100, 200)
  expect_length(values, 20)
  expect_true(all(diff(values) <= 1e-15))
  expect_true(all(values >= 0 & values <= 1))
})

test_that("the FDR correction behaves", {
  expect_length(togoid_fdr(numeric(0)), 0)
  expect_equal(togoid_fdr(0.03), 0.03)

  pvalues <- c(0.01, 0.04, 0.03, 0.005, 0.2, 0.9, 1e-8)
  adjusted <- togoid_fdr(pvalues)
  expect_length(adjusted, length(pvalues))
  expect_true(all(adjusted >= pvalues - 1e-15))
  expect_true(all(adjusted >= 0 & adjusted <= 1))

  # Monotone in the raw p-value.
  ordered <- adjusted[order(pvalues)]
  expect_true(all(diff(ordered) >= -1e-15))
})

test_that("fold enrichment is observed over expected", {
  # Expected overlap = 100 * 100 / 1000 = 10, so 20 observed is 2-fold.
  expect_equal(togoid_fold_enrichment(20, 1000, 100, 100), 2)
  expect_equal(togoid_fold_enrichment(5, 100, 0, 10), 0)
})

test_that("local_id strips CURIE prefixes and leaves everything else alone", {
  expect_equal(local_id("GO:0005634"), "0005634")
  expect_equal(local_id("ENSG00000121410"), "ENSG00000121410")
  expect_equal(local_id("http://example.org/a:b"), "http://example.org/a:b")
  expect_equal(local_id(c("GO:1", "R-HSA-2")), c("1", "R-HSA-2"))
  expect_equal(local_id(42), 42)
})

test_that("the gene-set container works", {
  sets <- demo_gene_sets()

  expect_s3_class(sets, "togoid_gene_sets")
  expect_length(sets, 5)
  expect_true("CD3D" %in% togoid_gene_set_genes(sets))
  expect_equal(togoid_term_labels(sets, "T:1"), "TCR signalling")
  # Unknown labels fall back to the term ID.
  expect_equal(togoid_term_labels(sets, "T:2"), "T:2")

  filtered <- togoid_filter_gene_sets(sets, min_size = 3, max_size = 500)
  expect_setequal(names(filtered$sets), c("T:1", "T:2", "B:1"))
  expect_length(sets, 5)  # the original is untouched
  expect_false("TINY" %in% names(filtered$labels))

  frame <- as.data.frame(sets)
  expect_equal(frame$term_id[1], "BIG")   # sorted by set size
  expect_equal(frame$n_genes[1], 600)
})

test_that("a gene-set library round-trips through JSON", {
  sets <- demo_gene_sets()
  path <- file.path(tempdir(), "nested", "library.json")

  togoid_save_gene_sets(sets, path)
  expect_true(file.exists(path))

  restored <- togoid_load_gene_sets(path)
  expect_equal(restored$sets[order(names(restored$sets))],
               lapply(sets$sets, sort)[order(names(sets$sets))])
  expect_equal(restored$labels[order(names(restored$labels))],
               sets$labels[order(names(sets$labels))])
  expect_equal(restored$route, sets$route)
  expect_equal(restored$target_dataset, sets$target_dataset)

  unlink(path)
})

test_that("togoid_enrich finds the right terms", {
  sets <- demo_gene_sets()
  query <- c("CD3D", "CD3E", "CD3G", "LCK", "ZAP70")

  result <- togoid_enrich(query, sets, min_set_size = 3, max_set_size = 500)

  expect_true("T:1" %in% result$term_id)
  expect_false("BIG" %in% result$term_id)    # above max_set_size
  expect_false("TINY" %in% result$term_id)   # below min_set_size

  top <- result[1, ]
  expect_equal(top$term_id, "T:1")
  expect_equal(top$overlap_count, 5L)
  expect_equal(top$term_size, 5L)
  expect_equal(top$term_label, "TCR signalling")
  expect_gt(top$fold_enrichment, 1)
  expect_equal(top$genes, paste(sort(query), collapse = ","))

  # Sorted by FDR.
  expect_true(all(diff(result$fdr) >= -1e-15))

  # Columns are the documented ones, in the documented order.
  expect_equal(names(result),
               setdiff(togoid_enrichment_columns(), "cluster"))
})

test_that("the background controls the p-values", {
  sets <- demo_gene_sets()
  query <- c("CD3D", "CD3E", "CD3G", "LCK", "ZAP70")

  default <- togoid_enrich(query, sets, min_set_size = 3)
  wider <- togoid_enrich(
    query, sets,
    background = c(togoid_gene_set_genes(sets), "X1", "X2"),
    min_set_size = 3
  )

  expect_equal(wider$background_size[1], default$background_size[1] + 2L)
  # The same overlap in a larger universe is more surprising.
  expect_lt(wider$pvalue[1], default$pvalue[1])

  # Genes outside the background must not inflate the query size.
  padded <- togoid_enrich(c(query, "NOT_A_GENE"), sets, min_set_size = 3)
  expect_equal(padded$query_size[1], default$query_size[1])
})

test_that("empty inputs give empty results with the right columns", {
  sets <- demo_gene_sets()
  empty_sets <- new_togoid_gene_sets(list(), character(0), character(0),
                                     character(0), c("a", "b"), "b")

  expect_equal(nrow(togoid_enrich(character(0), sets)), 0)
  expect_equal(nrow(togoid_enrich(c("CD3D"), empty_sets)), 0)
  expect_equal(names(togoid_enrich(character(0), sets)),
               setdiff(togoid_enrichment_columns(), "cluster"))
})

test_that("togoid_enrich_clusters shares one background", {
  sets <- demo_gene_sets()
  clusters <- list(
    "1" = c("MS4A1", "CD79A", "CD79B", "CD19", "BLNK"),
    "0" = c("CD3D", "CD3E", "CD3G", "LCK", "ZAP70")
  )

  results <- togoid_enrich_clusters(clusters, sets, min_set_size = 3)

  expect_true("cluster" %in% names(results))
  expect_equal(unique(results$cluster), c("0", "1"))   # numerically ordered
  expect_equal(results$term_id[results$cluster == "0"][1], "T:1")
  expect_equal(results$term_id[results$cluster == "1"][1], "B:1")
  # One shared background means the FDRs are comparable across clusters.
  expect_length(unique(results$background_size), 1)
})

test_that("cluster labels are ordered numerically, then alphabetically", {
  # Regression test: cluster_sort_key() returns an ordering vector, so applying
  # order() to it again would scramble the clusters.
  expect_equal(c("T", "B", "Myeloid")[cluster_sort_key(c("T", "B", "Myeloid"))],
               c("B", "Myeloid", "T"))

  numeric_labels <- as.character(c(0, 1, 2, 10, 11, 12, 3))
  expect_equal(numeric_labels[cluster_sort_key(numeric_labels)],
               as.character(c(0, 1, 2, 3, 10, 11, 12)))

  sets <- demo_gene_sets()
  clusters <- list(
    T = c("CD3D", "CD3E", "CD3G"),
    B = c("MS4A1", "CD79A", "CD79B"),
    Myeloid = c("CD3D", "MS4A1")
  )
  results <- togoid_enrich_clusters(clusters, sets, min_set_size = 3)
  expect_equal(unique(results$cluster), c("B", "Myeloid", "T"))
})

test_that("togoid_enrich_clusters accepts a data frame", {
  sets <- demo_gene_sets()
  frame <- data.frame(
    cluster = c(rep("0", 5), rep("1", 5)),
    gene = c("CD3D", "CD3E", "CD3G", "LCK", "ZAP70",
             "MS4A1", "CD79A", "CD79B", "CD19", "BLNK"),
    stringsAsFactors = FALSE
  )

  from_frame <- togoid_enrich_clusters(frame, sets, min_set_size = 3)
  from_list <- togoid_enrich_clusters(
    split(frame$gene, frame$cluster), sets, min_set_size = 3
  )
  expect_equal(from_frame, from_list)

  expect_error(
    togoid_enrich_clusters(data.frame(a = 1), sets),
    "missing column"
  )
})

test_that("filtering and summarising work", {
  sets <- demo_gene_sets()
  clusters <- list(
    "0" = c("CD3D", "CD3E", "CD3G", "LCK", "ZAP70"),
    "1" = c("MS4A1", "CD79A", "CD79B", "CD19", "BLNK")
  )
  results <- togoid_enrich_clusters(clusters, sets, min_set_size = 3)

  expect_true(all(togoid_significant(results, alpha = 0.5)$fdr < 0.5))
  expect_equal(nrow(togoid_significant(results, alpha = 1e-300)), 0)

  top <- togoid_top_terms(results, n = 1)
  expect_equal(nrow(top), length(unique(results$cluster)))

  summary <- togoid_enrichment_summary(results)
  expect_true(grepl("Cluster 0:", summary, fixed = TRUE))
  expect_true(grepl("Cluster 1:", summary, fixed = TRUE))
  expect_true(grepl("terms tested", summary, fixed = TRUE))

  expect_true(grepl("No terms tested",
                    togoid_enrichment_summary(empty_enrichment(TRUE)),
                    fixed = TRUE))
})

test_that("presets are well formed", {
  routes <- togoid_enrichment_routes()
  expect_equal(routes$reactome[length(routes$reactome)], "reactome_pathway")
  expect_equal(routes$go[length(routes$go)], "go")
  expect_equal(routes$mondo[length(routes$mondo)], "mondo")
  expect_true(all(vapply(routes, function(r) r[1] == "ncbigene", logical(1))))

  expect_error(togoid_gene_sets_from_preset("nonexistent", "CD3D"), "Unknown preset")
  expect_error(togoid_go_gene_sets("CD3D", aspect = "not_an_aspect"), "aspect must be one of")
  expect_error(togoid_gene_sets("CD3D", route = "ncbigene"), "at least a source")
})

test_that("centroids and the spiral behave", {
  embedding <- data.frame(
    umap_1 = c(0, 2, 10, 12),
    umap_2 = c(0, 2, 10, 12),
    cluster = c("0", "0", "1", "1"),
    stringsAsFactors = FALSE
  )

  centroids <- togoid_cluster_centroids(embedding)
  expect_equal(centroids$x[centroids$cluster == "0"], 1)
  expect_equal(centroids$y[centroids$cluster == "0"], 1)
  expect_equal(centroids$n_cells[centroids$cluster == "0"], 2L)
  expect_equal(centroids$x[centroids$cluster == "1"], 11)

  expect_error(
    togoid_cluster_centroids(data.frame(umap_1 = 1, umap_2 = 1)),
    "missing column"
  )
  expect_error(
    togoid_cluster_centroids(
      data.frame(umap_1 = numeric(0), umap_2 = numeric(0), cluster = character(0))
    ),
    "no cells"
  )

  positions <- togoid_spiral_positions(0, 0, 16, radius_start = 1, radius_step = 1)
  expect_equal(nrow(positions), 16)
  radii <- sqrt(positions$x^2 + positions$y^2)
  expect_equal(radii[1], 1)
  expect_gt(radii[16], radii[1])
  expect_equal(nrow(togoid_spiral_positions(0, 0, 0, 1, 1)), 0)
})

test_that("the box helpers are correct", {
  expect_true(boxes_overlap(c(0, 0, 1, 1), c(0.5, 0.5, 1.5, 1.5)))
  expect_false(boxes_overlap(c(0, 0, 1, 1), c(2, 2, 3, 3)))
  expect_true(boxes_overlap(c(0, 0, 1, 1), c(1.05, 0, 2, 1), padding = 0.1))
  expect_false(boxes_overlap(c(0, 0, 1, 1), c(1.05, 0, 2, 1), padding = 0))

  expect_true(box_contains(c(0, 0, 10, 10), c(1, 1, 2, 2)))
  expect_false(box_contains(c(0, 0, 10, 10), c(1, 1, 20, 2)))
  expect_true(box_contains(NULL, c(1, 1, 2, 2)))
})

test_that("term selection applies every filter", {
  enrichment <- data.frame(
    cluster = c("0", "0", "0", "1"),
    term_id = c("A", "B", "C", "D"),
    term_label = c("alpha", "beta", "gamma", strrep("d", 60)),
    pvalue = c(1e-6, 1e-3, 0.4, 1e-4),
    fdr = c(1e-5, 2e-3, 0.5, 1e-3),
    stringsAsFactors = FALSE
  )

  selected <- togoid_select_terms(enrichment, top_n = NULL, fdr_cutoff = 0.05)
  expect_setequal(unique(selected$cluster), c("0", "1"))
  expect_equal(sum(selected$cluster == "0"), 2)   # gamma is above the cut-off
  expect_equal(selected$term_id[1], "A")          # most significant first

  expect_equal(
    sum(togoid_select_terms(enrichment, top_n = 1, fdr_cutoff = 0.05)$cluster == "0"),
    1
  )
  expect_equal(
    nrow(togoid_select_terms(enrichment, top_n = NULL, fdr_cutoff = NULL,
                             pval_cutoff = 1e-5)),
    1
  )
  expect_equal(
    unique(togoid_select_terms(enrichment, top_n = NULL, fdr_cutoff = 0.05,
                               clusters = "1")$cluster),
    "1"
  )

  truncated <- togoid_select_terms(enrichment, top_n = NULL, fdr_cutoff = 0.05,
                                   max_label_chars = 20)
  long <- truncated$label[truncated$term_id == "D"]
  expect_equal(nchar(long), 20)
  expect_true(endsWith(long, "..."))

  # A zero p-value must not become Inf, which would break the font size.
  zero <- togoid_select_terms(
    data.frame(cluster = "0", term_id = "Z", term_label = "z",
               pvalue = 0, fdr = 0, stringsAsFactors = FALSE),
    top_n = NULL
  )
  expect_true(is.finite(zero$weight))
})

test_that("the label layout produces no overlaps", {
  skip_if_not_installed("ggplot2")

  set.seed(1)
  embedding <- do.call(rbind, lapply(seq_along(c(0, 10, 5)), function(i) {
    centre <- list(c(0, 0), c(10, 0), c(5, 9))[[i]]
    data.frame(
      umap_1 = centre[1] + stats::rnorm(80, sd = 0.8),
      umap_2 = centre[2] + stats::rnorm(80, sd = 0.8),
      cluster = as.character(i - 1),
      stringsAsFactors = FALSE
    )
  }))

  enrichment <- do.call(rbind, lapply(c("0", "1", "2"), function(cluster) {
    data.frame(
      cluster = cluster,
      term_id = paste0(cluster, ":", seq_len(4)),
      term_label = paste("term", cluster, seq_len(4)),
      pvalue = 10^-(6:3),
      fdr = 10^-(5:2),
      stringsAsFactors = FALSE
    )
  }))

  selected <- togoid_select_terms(enrichment, top_n = 4, fdr_cutoff = 0.05)
  centroids <- togoid_cluster_centroids(embedding)
  bounds <- c(range(embedding$umap_1), range(embedding$umap_2))[c(1, 3, 2, 4)]

  layout <- layout_labels(selected, centroids, bounds,
                          panel_width_in = 8, panel_height_in = 7)

  expect_gt(nrow(layout), 0)
  expect_lte(nrow(layout), nrow(selected))

  # No two placed labels may overlap.
  collisions <- 0
  for (i in seq_len(nrow(layout))) {
    for (j in seq_len(nrow(layout))) {
      if (j <= i) next
      a <- c(layout$xmin[i], layout$ymin[i], layout$xmax[i], layout$ymax[i])
      b <- c(layout$xmin[j], layout$ymin[j], layout$xmax[j], layout$ymax[j])
      if (boxes_overlap(a, b)) collisions <- collisions + 1
    }
  }
  expect_equal(collisions, 0)

  # More significant terms get larger type.
  expect_true(all(diff(layout$fontsize[order(-selected$weight[
    match(layout$label, selected$label)
  ])]) <= 1e-9))
})

test_that("the figure builds", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  set.seed(2)
  embedding <- do.call(rbind, lapply(c("0", "1"), function(cluster) {
    offset <- if (cluster == "0") 0 else 10
    data.frame(
      umap_1 = offset + stats::rnorm(60),
      umap_2 = stats::rnorm(60),
      cluster = cluster,
      stringsAsFactors = FALSE
    )
  }))
  enrichment <- data.frame(
    cluster = c("0", "0", "1"),
    term_id = c("A", "B", "C"),
    term_label = c("alpha", "beta", "gamma"),
    pvalue = c(1e-6, 1e-4, 1e-5),
    fdr = c(1e-5, 1e-3, 1e-4),
    stringsAsFactors = FALSE
  )

  figure <- togoid_plot_umap_enrichment(embedding, enrichment, top_n = 2)
  expect_s3_class(figure, "patchwork")

  path <- file.path(tempdir(), "figure.pdf")
  ggplot2::ggsave(path, figure, width = 20, height = 8)
  expect_gt(file.size(path), 0)
  unlink(path)

  # Nothing significant is a normal outcome, not an error.
  expect_s3_class(
    togoid_plot_umap_enrichment(embedding, enrichment, fdr_cutoff = 1e-300),
    "patchwork"
  )

  expect_s3_class(togoid_plot_umap_centroids(embedding), "ggplot")
})

test_that("the Seurat marker adapter filters and orders", {
  markers <- data.frame(
    cluster = c("0", "0", "0", "1", "1"),
    gene = c("CD3D", "CD3E", "WEAK", "MS4A1", "NS"),
    p_val_adj = c(1e-10, 1e-5, 0.9, 1e-8, 0.5),
    avg_log2FC = c(2.0, 1.5, 1.0, 2.5, 3.0),
    stringsAsFactors = FALSE
  )

  result <- togoid_markers_from_seurat(markers)
  expect_equal(result[["0"]], c("CD3D", "CD3E"))   # WEAK fails the p-value cut
  expect_equal(result[["1"]], "MS4A1")             # NS fails the p-value cut

  # The log fold change filter bites independently.
  strict <- togoid_markers_from_seurat(markers, logfc_min = 2.2)
  expect_equal(strict[["1"]], "MS4A1")
  expect_null(strict[["0"]])

  expect_equal(togoid_markers_from_seurat(markers, top_n = 1)[["0"]], "CD3D")
  expect_error(togoid_markers_from_seurat(data.frame(a = 1)), "missing column")
  expect_equal(togoid_markers_from_seurat(markers, pval_cutoff = 1e-300), list())
})

test_that("the CSV adapters round-trip", {
  directory <- tempdir()

  umap_path <- file.path(directory, "umap.csv")
  utils::write.csv(
    data.frame(umap_1 = c(1, 2), umap_2 = c(3, 4), cluster = c("0", "1")),
    umap_path, row.names = FALSE
  )
  embedding <- togoid_umap_from_csv(umap_path)
  expect_equal(nrow(embedding), 2)
  expect_equal(embedding$cluster, c("0", "1"))

  markers_path <- file.path(directory, "markers.csv")
  utils::write.csv(
    data.frame(cluster = c("0", "0", "1"), gene = c("A", "B", "C")),
    markers_path, row.names = FALSE
  )
  markers <- togoid_markers_from_csv(markers_path, pval_cutoff = NULL, logfc_min = NULL)
  expect_equal(markers[["0"]], c("A", "B"))

  lists_dir <- file.path(directory, "gene_lists")
  paths <- togoid_write_marker_lists(markers, lists_dir)
  expect_true(all(file.exists(paths)))
  expect_equal(readLines(paths[["0"]]), c("A", "B"))

  unlink(c(umap_path, markers_path), force = TRUE)
  unlink(lists_dir, recursive = TRUE)
})
