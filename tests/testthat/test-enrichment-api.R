# Tests for the enrichment analysis that call the TogoID API.
#
# These check that the documented routes still work and that the results stay
# biologically sensible: classic T cell, B cell and myeloid marker genes must
# come out enriched for the matching pathways, GO terms and diseases.

# Well-known PBMC lineage markers, used as the query throughout.
marker_clusters <- function() {
  list(
    T = c("CD3D", "CD3E", "CD3G", "IL7R", "LCK", "ZAP70", "CD2", "CD28", "LAT", "TRAC"),
    B = c("MS4A1", "CD79A", "CD79B", "CD19", "BLNK",
          "BANK1", "TNFRSF13C", "IGHM", "CR2", "PAX5"),
    Myeloid = c("LYZ", "CD14", "FCGR3A", "CSF1R", "ITGAM",
                "TLR2", "TLR4", "FCN1", "S100A8", "S100A9")
  )
}

marker_genes <- function() {
  sort(unique(unlist(marker_clusters(), use.names = FALSE)))
}

# Lower-cased labels of a cluster's most significant terms, as one string.
top_labels <- function(enrichment, cluster, n = 10) {
  part <- enrichment[enrichment$cluster == cluster, , drop = FALSE]
  part <- part[order(part$fdr, part$pvalue), , drop = FALSE]
  paste(tolower(utils::head(part$term_label, n)), collapse = " | ")
}

test_that("gene symbols resolve to NCBI Gene IDs", {
  skip_on_cran()
  skip_if_offline()

  resolved <- togoid_map_labels(c("CD3D", "MS4A1", "LYZ"), verbose = FALSE)
  expect_length(resolved, 3)
  expect_equal(unname(resolved["CD3D"]), "915")
  expect_equal(unname(resolved["MS4A1"]), "931")

  mixed <- togoid_map_labels(c("CD3D", "NOT_A_REAL_GENE_XYZ"), verbose = FALSE)
  expect_true("CD3D" %in% names(mixed))
  expect_false("NOT_A_REAL_GENE_XYZ" %in% names(mixed))
})

test_that("the Reactome route recovers TCR, BCR and neutrophil biology", {
  skip_on_cran()
  skip_if_offline()

  clusters <- marker_clusters()
  sets <- togoid_reactome_gene_sets(marker_genes(), verbose = FALSE)

  expect_s3_class(sets, "togoid_gene_sets")
  expect_gt(length(sets), 20)
  expect_equal(sets$target_dataset, "reactome_pathway")
  expect_equal(sets$route, togoid_enrichment_routes()$reactome)
  expect_length(sets$unmapped, 0)
  expect_true(all(grepl("^R-", utils::head(names(sets$sets), 10))))

  results <- togoid_enrich_clusters(clusters, sets, min_set_size = 3)
  expect_gt(nrow(results), 0)

  expect_true(grepl("tcr|t cell", top_labels(results, "T")))
  expect_true(grepl("b cell|bcr", top_labels(results, "B")))
  expect_true(grepl("neutrophil|toll|myd88", top_labels(results, "Myeloid")))

  expect_gt(nrow(togoid_significant(results)), 0)

  # Internal consistency of every row.
  expect_true(all(results$fdr >= results$pvalue - 1e-15))
  expect_true(all(results$overlap_count <= results$term_size))
  expect_true(all(results$overlap_count <= results$query_size))
  expect_true(all(results$pvalue >= 0 & results$pvalue <= 1))
})

test_that("the GO route works and the aspect filter partitions terms", {
  skip_on_cran()
  skip_if_offline()

  clusters <- marker_clusters()
  biological <- togoid_go_gene_sets(marker_genes(),
                                    aspect = "biological_process", verbose = FALSE)
  expect_gt(length(biological), 50)

  results <- togoid_enrich_clusters(clusters, biological, min_set_size = 3)
  expect_true(grepl("t cell", top_labels(results, "T")))
  expect_true(grepl("b cell", top_labels(results, "B")))

  cellular <- togoid_go_gene_sets(marker_genes(),
                                  aspect = "cellular_component", verbose = FALSE)
  expect_gt(length(cellular), 0)
  # A term belongs to exactly one aspect, so the two libraries must be disjoint.
  expect_length(intersect(names(biological$sets), names(cellular$sets)), 0)

  unfiltered <- togoid_go_gene_sets(marker_genes(), aspect = NULL, verbose = FALSE)
  expect_gt(length(unfiltered), length(biological))
})

test_that("the MONDO route returns disease terms", {
  skip_on_cran()
  skip_if_offline()

  sets <- togoid_mondo_gene_sets(marker_genes(), verbose = FALSE)
  skip_if(length(sets) == 0, "MONDO route returned no gene sets for these genes")

  expect_equal(sets$route, togoid_enrichment_routes()$mondo)
  expect_gt(length(sets$labels), 0)

  # Disease annotation is sparse, so only check that the analysis runs.
  results <- togoid_enrich_clusters(marker_clusters(), sets, min_set_size = 2)
  expect_true(is.data.frame(results))
})

test_that("an explicit route matches the equivalent preset", {
  skip_on_cran()
  skip_if_offline()

  explicit <- togoid_gene_sets(
    marker_genes(),
    route = c("ncbigene", "uniprot", "reactome_pathway"),
    verbose = FALSE
  )
  preset <- togoid_reactome_gene_sets(marker_genes(), verbose = FALSE)
  expect_setequal(names(explicit$sets), names(preset$sets))
})

test_that("a broken route fails loudly instead of returning nothing", {
  skip_on_cran()
  skip_if_offline()

  # An empty library here would look exactly like a gene list with no
  # annotations, so the error must surface.
  expect_error(
    suppressWarnings(
      togoid_gene_sets("CD3D", route = c("ncbigene", "hp_phenotype"), verbose = FALSE)
    ),
    "failed for all"
  )
})

test_that("identifiers can be passed instead of symbols", {
  skip_on_cran()
  skip_if_offline()

  sets <- togoid_gene_sets(
    c("915", "931", "3320"),   # CD3D, MS4A1, HSP90AA1
    route = togoid_enrichment_routes()$reactome,
    id_source = NULL,
    verbose = FALSE
  )
  expect_gt(length(sets), 0)
  expect_length(sets$unmapped, 0)
  expect_true("915" %in% togoid_gene_set_genes(sets))
})

test_that("a cached library reproduces the analysis offline", {
  skip_on_cran()
  skip_if_offline()

  clusters <- marker_clusters()
  sets <- togoid_reactome_gene_sets(marker_genes(), verbose = FALSE)
  live <- togoid_enrich_clusters(clusters, sets, min_set_size = 3)

  path <- file.path(tempdir(), "reactome_cache.json")
  togoid_save_gene_sets(sets, path)
  cached <- togoid_enrich_clusters(
    clusters, togoid_load_gene_sets(path), min_set_size = 3
  )
  unlink(path)

  expect_equal(nrow(cached), nrow(live))
  expect_equal(cached$term_id, live$term_id)
  expect_equal(cached$pvalue, live$pvalue)
})

test_that("the figure builds from real results", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  clusters <- marker_clusters()
  sets <- togoid_reactome_gene_sets(marker_genes(), verbose = FALSE)
  results <- togoid_enrich_clusters(clusters, sets, min_set_size = 3)

  # A stand-in embedding: three well-separated blobs, one per cluster.
  set.seed(3)
  embedding <- do.call(rbind, Map(function(cluster, centre) {
    data.frame(
      umap_1 = centre[1] + stats::rnorm(50, sd = 0.6),
      umap_2 = centre[2] + stats::rnorm(50, sd = 0.6),
      cluster = cluster,
      stringsAsFactors = FALSE
    )
  }, names(clusters), list(c(0, 0), c(10, 2), c(5, -9))))

  figure <- togoid_plot_umap_enrichment(embedding, results, top_n = 3)
  expect_s3_class(figure, "patchwork")
})
