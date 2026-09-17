#' Word-cloud visualisation of enrichment results on a UMAP embedding
#'
#' The layout places each cluster's enriched terms around that cluster's
#' centroid, sized by significance, rejecting candidate positions that would
#' overlap text already placed. Nothing here is specific to Seurat: the
#' embedding is passed in as plain coordinates plus cluster labels.
#'
#' @name enrichment-plot
NULL

#' Check that ggplot2 and patchwork are installed
#'
#' @return Invisibly `TRUE`, or an error with an actionable message.
#' @keywords internal
require_plot_packages <- function() {
  missing <- c("ggplot2", "patchwork")[
    !vapply(c("ggplot2", "patchwork"), requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(sprintf(
      "Plotting requires %s. Install with: install.packages(c(%s))",
      paste(missing, collapse = " and "),
      paste(sprintf('"%s"', missing), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate an embedding data frame
#'
#' @param embedding Data frame with the embedding coordinates.
#' @param x_column Column holding the first dimension.
#' @param y_column Column holding the second dimension.
#' @param cluster_column Column holding the cluster label.
#'
#' @return The embedding, with the three columns coerced and renamed to
#'   `umap_1`, `umap_2` and `cluster`.
#' @keywords internal
normalise_embedding <- function(embedding, x_column, y_column, cluster_column) {
  missing <- setdiff(c(x_column, y_column, cluster_column), names(embedding))
  if (length(missing) > 0) {
    stop(sprintf(
      "embedding is missing column(s): %s. Found: %s",
      paste(missing, collapse = ", "), paste(names(embedding), collapse = ", ")
    ), call. = FALSE)
  }
  if (nrow(embedding) == 0) {
    stop("embedding contains no cells", call. = FALSE)
  }

  data.frame(
    umap_1 = as.numeric(embedding[[x_column]]),
    umap_2 = as.numeric(embedding[[y_column]]),
    cluster = as.character(embedding[[cluster_column]]),
    stringsAsFactors = FALSE
  )
}

#' Compute cluster centroids in an embedding
#'
#' @param embedding Data frame with embedding coordinates.
#' @param x_column Column holding the first dimension.
#' @param y_column Column holding the second dimension.
#' @param cluster_column Column holding the cluster label.
#'
#' @return A data frame with `cluster`, `x`, `y` and `n_cells`.
#' @export
#'
#' @examples
#' embedding <- data.frame(
#'   umap_1 = c(0, 2, 10, 12),
#'   umap_2 = c(0, 2, 10, 12),
#'   cluster = c("0", "0", "1", "1")
#' )
#' togoid_cluster_centroids(embedding)
togoid_cluster_centroids <- function(embedding,
                                     x_column = "umap_1",
                                     y_column = "umap_2",
                                     cluster_column = "cluster") {
  embedding <- normalise_embedding(embedding, x_column, y_column, cluster_column)
  parts <- split(embedding, embedding$cluster)

  result <- do.call(rbind, lapply(names(parts), function(cluster) {
    part <- parts[[cluster]]
    data.frame(
      cluster = cluster,
      x = mean(part$umap_1),
      y = mean(part$umap_2),
      n_cells = nrow(part),
      stringsAsFactors = FALSE
    )
  }))
  result <- result[cluster_sort_key(result$cluster), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Generate candidate label positions spiralling out from a centre
#'
#' @param center_x Centre x coordinate.
#' @param center_y Centre y coordinate.
#' @param n Number of candidate positions.
#' @param radius_start Radius of the first candidate.
#' @param radius_step Radius added per full turn.
#' @param angle_step Angle between consecutive candidates, in radians.
#'
#' @return A data frame with `x` and `y`, closest to the centre first.
#' @export
#'
#' @examples
#' togoid_spiral_positions(0, 0, 8, radius_start = 1, radius_step = 1)
togoid_spiral_positions <- function(center_x, center_y, n,
                                    radius_start, radius_step,
                                    angle_step = pi / 4) {
  if (n <= 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  index <- seq_len(n) - 1
  angle <- index * angle_step
  radius <- radius_start + index * (radius_step / max(1, (2 * pi / angle_step)))
  data.frame(
    x = center_x + radius * cos(angle),
    y = center_y + radius * sin(angle)
  )
}

#' Test whether two bounding boxes overlap
#'
#' @param a Numeric vector `c(x_min, y_min, x_max, y_max)`.
#' @param b Numeric vector in the same form.
#' @param padding Gap kept around `a`.
#'
#' @return `TRUE` when the boxes intersect.
#' @keywords internal
boxes_overlap <- function(a, b, padding = 0) {
  !(a[3] + padding < b[1] || a[1] - padding > b[3] ||
      a[4] + padding < b[2] || a[2] - padding > b[4])
}

#' Test whether a box lies entirely within another
#'
#' @param outer Numeric vector `c(x_min, y_min, x_max, y_max)`, or `NULL`.
#' @param inner Numeric vector in the same form.
#'
#' @return `TRUE` when `inner` is inside `outer`.
#' @keywords internal
box_contains <- function(outer, inner) {
  if (is.null(outer)) {
    return(TRUE)
  }
  inner[1] >= outer[1] && inner[2] >= outer[2] &&
    inner[3] <= outer[3] && inner[4] <= outer[4]
}

#' Measure a text label in inches
#'
#' Uses grid's font metrics, so the size reflects the font that will actually be
#' used rather than a character-count approximation. A null device is opened if
#' none is active, which keeps the function usable in scripts and in `R CMD
#' check`.
#'
#' @param label Character vector of labels.
#' @param fontsize Numeric vector of font sizes in points, recycled to `label`.
#'
#' @return A data frame with `width` and `height` in inches.
#' @keywords internal
measure_text_inches <- function(label, fontsize) {
  # A device must be open for grid to resolve font metrics. Only close the one
  # we opened, so an interactive session's device survives.
  if (grDevices::dev.cur() == 1L) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  fontsize <- rep_len(fontsize, length(label))
  width <- numeric(length(label))
  height <- numeric(length(label))

  for (i in seq_along(label)) {
    grob <- grid::textGrob(label[i], gp = grid::gpar(fontsize = fontsize[i]))
    width[i] <- grid::convertWidth(grid::grobWidth(grob), "inches", valueOnly = TRUE)
    height[i] <- grid::convertHeight(grid::grobHeight(grob), "inches", valueOnly = TRUE)
  }

  data.frame(width = width, height = height)
}

#' Select the terms to draw for each cluster
#'
#' @param enrichment An enrichment data frame.
#' @param top_n Keep at most this many terms per cluster; `NULL` keeps all.
#' @param pval_cutoff Drop terms with a p-value at or above this value.
#' @param fdr_cutoff Drop terms with an FDR at or above this value; `NULL`
#'   disables the filter.
#' @param clusters Restrict to these clusters; `NULL` keeps all.
#' @param max_label_chars Truncate labels longer than this.
#'
#' @return A data frame of the selected terms, with added `label` and `weight`
#'   (`-log10(pvalue)`) columns, most significant first.
#' @export
togoid_select_terms <- function(enrichment,
                                top_n = 3,
                                pval_cutoff = NULL,
                                fdr_cutoff = 0.05,
                                clusters = NULL,
                                max_label_chars = 40) {
  if (nrow(enrichment) == 0) {
    return(enrichment)
  }

  selected <- enrichment
  if (!("cluster" %in% names(selected))) {
    selected$cluster <- "query"
  }
  selected$cluster <- as.character(selected$cluster)

  if (!is.null(clusters)) {
    selected <- selected[selected$cluster %in% as.character(clusters), , drop = FALSE]
  }
  if (!is.null(pval_cutoff)) {
    selected <- selected[selected$pvalue < pval_cutoff, , drop = FALSE]
  }
  if (!is.null(fdr_cutoff)) {
    selected <- selected[selected$fdr < fdr_cutoff, , drop = FALSE]
  }
  if (nrow(selected) == 0) {
    return(selected)
  }

  selected$label <- ifelse(
    is.na(selected$term_label) | !nzchar(selected$term_label),
    selected$term_id,
    selected$term_label
  )
  if (!is.null(max_label_chars)) {
    too_long <- nchar(selected$label) > max_label_chars
    selected$label[too_long] <- paste0(
      substr(selected$label[too_long], 1, max_label_chars - 3), "..."
    )
  }

  # Guard against p == 0 from extreme enrichment, which would be Inf.
  selected$weight <- ifelse(selected$pvalue > 0, -log10(selected$pvalue), 300)

  selected <- selected[order(selected$cluster, selected$fdr, selected$pvalue), , drop = FALSE]
  if (!is.null(top_n)) {
    parts <- lapply(split(selected, selected$cluster), utils::head, top_n)
    selected <- do.call(rbind, parts)
  }
  rownames(selected) <- NULL
  selected
}

#' Lay out term labels around cluster centroids
#'
#' Each label is measured once and tested against candidate positions spiralling
#' outwards from its cluster's centroid. Positions that keep the label inside
#' the panel are tried first, so labels only leave the visible area when there is
#' genuinely nowhere else to put them. Labels that cannot be placed at all are
#' dropped rather than drawn on top of each other.
#'
#' @param selected Output of [togoid_select_terms()].
#' @param centroids Output of [togoid_cluster_centroids()].
#' @param bounds Numeric vector `c(x_min, y_min, x_max, y_max)` of the data range.
#' @param panel_width_in Panel width in inches, used to convert font metrics into
#'   data units.
#' @param panel_height_in Panel height in inches.
#' @param fontsize_range Smallest and largest font size in points.
#' @param weight_scale Points of font size added per unit of `-log10(p)`.
#' @param candidates_per_term Spiral positions tried per term.
#' @param padding_fraction Gap kept between labels, as a fraction of the span.
#' @param reserve_centroids Reserve space around each centroid marker.
#' @param centroid_size Centroid marker size, used to scale the reserved space.
#'
#' @return A data frame with `cluster`, `label`, `x`, `y`, `fontsize` and the
#'   label's bounding box, one row per placed label.
#' @keywords internal
layout_labels <- function(selected,
                          centroids,
                          bounds,
                          panel_width_in,
                          panel_height_in,
                          fontsize_range = c(6, 14),
                          weight_scale = 0.8,
                          candidates_per_term = 40,
                          padding_fraction = 0.004,
                          reserve_centroids = TRUE,
                          centroid_size = 2) {
  x_span <- bounds[3] - bounds[1]
  y_span <- bounds[4] - bounds[2]
  span <- max(x_span, y_span)
  padding <- span * padding_fraction

  # Font metrics come back in inches; convert them into data units using the
  # panel size, so the collision test works in the same space as the points.
  x_per_inch <- x_span / max(panel_width_in, 1e-6)
  y_per_inch <- y_span / max(panel_height_in, 1e-6)

  selected$fontsize <- pmin(
    fontsize_range[2],
    fontsize_range[1] + selected$weight * weight_scale
  )
  sizes <- measure_text_inches(selected$label, selected$fontsize)
  selected$half_w <- (sizes$width * x_per_inch) / 2
  selected$half_h <- (sizes$height * y_per_inch) / 2

  obstacles <- list()
  if (reserve_centroids) {
    # Reserve a box a little larger than the marker itself, scaled with the
    # requested size so labels keep clear of it.
    marker <- span * 0.010 * max(1, sqrt(centroid_size / 2))
    for (i in seq_len(nrow(centroids))) {
      if (centroids$cluster[i] %in% selected$cluster) {
        obstacles[[length(obstacles) + 1]] <- c(
          centroids$x[i] - marker, centroids$y[i] - marker,
          centroids$x[i] + marker, centroids$y[i] + marker
        )
      }
    }
  }

  placed <- list()
  placed_boxes <- list()

  # Draw the most significant terms first so they win the space nearest their
  # centroid; later terms settle further out or are dropped.
  cluster_order <- names(sort(
    vapply(split(selected$weight, selected$cluster), max, numeric(1)),
    decreasing = TRUE
  ))

  for (cluster in cluster_order) {
    entries <- selected[selected$cluster == cluster, , drop = FALSE]
    centroid <- centroids[centroids$cluster == cluster, , drop = FALSE]
    if (nrow(centroid) == 0) {
      next
    }

    candidates <- togoid_spiral_positions(
      centroid$x[1], centroid$y[1],
      n = max(1, nrow(entries)) * candidates_per_term,
      radius_start = span * 0.02,
      radius_step = span * 0.06
    )

    for (i in seq_len(nrow(entries))) {
      entry <- entries[i, ]
      chosen <- NULL

      # Pass 1 keeps the label inside the panel; pass 2 drops that requirement
      # so an edge cluster still gets labelled.
      for (require_inside in c(TRUE, FALSE)) {
        for (j in seq_len(nrow(candidates))) {
          box <- c(
            candidates$x[j] - entry$half_w, candidates$y[j] - entry$half_h,
            candidates$x[j] + entry$half_w, candidates$y[j] + entry$half_h
          )
          if (require_inside && !box_contains(bounds, box)) {
            next
          }
          collides <- FALSE
          for (other in c(obstacles, placed_boxes)) {
            if (boxes_overlap(box, other, padding)) {
              collides <- TRUE
              break
            }
          }
          if (!collides) {
            chosen <- list(x = candidates$x[j], y = candidates$y[j], box = box)
            break
          }
        }
        if (!is.null(chosen)) break
      }

      if (is.null(chosen)) {
        next
      }

      placed_boxes[[length(placed_boxes) + 1]] <- chosen$box
      placed[[length(placed) + 1]] <- data.frame(
        cluster = cluster,
        label = entry$label,
        x = chosen$x,
        y = chosen$y,
        fontsize = entry$fontsize,
        xmin = chosen$box[1], ymin = chosen$box[2],
        xmax = chosen$box[3], ymax = chosen$box[4],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(placed) == 0) {
    return(data.frame(
      cluster = character(0), label = character(0),
      x = numeric(0), y = numeric(0), fontsize = numeric(0),
      xmin = numeric(0), ymin = numeric(0), xmax = numeric(0), ymax = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, placed)
  rownames(result) <- NULL
  result
}

#' Assign a stable colour to every cluster
#'
#' @param clusters Character vector of cluster labels.
#'
#' @return A named character vector of hex colours.
#' @keywords internal
cluster_palette <- function(clusters) {
  ordered <- unique(clusters)
  ordered <- ordered[cluster_sort_key(ordered)]
  # hue_pal-style evenly spaced hues, so the palette scales to any cluster count
  # without adding a dependency.
  colours <- grDevices::hcl(
    h = seq(15, 375, length.out = length(ordered) + 1)[seq_along(ordered)],
    c = 100, l = 62
  )
  stats::setNames(colours, ordered)
}

#' Plot the cluster UMAP
#'
#' @param embedding Normalised embedding data frame.
#' @param colours Named colour vector.
#' @param title Panel title.
#' @param point_size Point size.
#' @param point_alpha Point alpha.
#' @param legend Show the cluster legend.
#'
#' @return A ggplot object.
#' @keywords internal
plot_cluster_panel <- function(embedding, colours, title,
                               point_size = 0.5, point_alpha = 0.6,
                               legend = TRUE) {
  # Order the legend numerically where the labels are numbers; the default
  # factor ordering would put cluster 10 between 1 and 2.
  levels <- unique(embedding$cluster)
  embedding$cluster <- factor(embedding$cluster,
                              levels = levels[cluster_sort_key(levels)])

  plot <- ggplot2::ggplot(
    embedding,
    ggplot2::aes(x = .data$umap_1, y = .data$umap_2, colour = .data$cluster)
  ) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::scale_colour_manual(values = colours, name = "Cluster") +
    ggplot2::labs(x = "UMAP 1", y = "UMAP 2", title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      panel.grid = ggplot2::element_blank()
    )

  if (legend) {
    plot <- plot + ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(size = 3, alpha = 1))
    )
  } else {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }
  plot
}

#' Draw enrichment results on a UMAP embedding
#'
#' Produces a two-panel figure: the usual cluster UMAP on the left, and the same
#' embedding on the right with each cluster's enriched terms written around its
#' centroid, sized by `-log10(p)`.
#'
#' @param embedding Data frame with the embedding coordinates and cluster labels
#'   of every cell.
#' @param enrichment An enrichment data frame from [togoid_enrich_clusters()].
#' @param top_n Terms drawn per cluster; `NULL` draws all that pass the cut-offs.
#' @param pval_cutoff Optional p-value cut-off.
#' @param fdr_cutoff FDR cut-off; `NULL` disables it.
#' @param clusters Restrict the labels to these clusters.
#' @param max_label_chars Truncate term labels longer than this.
#' @param x_column Embedding column for the first dimension.
#' @param y_column Embedding column for the second dimension.
#' @param cluster_column Embedding column holding cluster labels.
#' @param width Figure width in inches, used for the text-size calculation and
#'   as the width to save at.
#' @param height Figure height in inches.
#' @param point_size Point size in the left panel; the right panel uses half.
#' @param fontsize_range Smallest and largest term font size, in points.
#' @param weight_scale Points of font size added per unit of `-log10(p)`.
#' @param candidates_per_term Spiral positions tried per term before giving up.
#' @param title_left Title of the left panel.
#' @param title_right Title of the right panel; `NULL` generates one.
#' @param show_centroids Mark cluster centroids on the right panel.
#' @param centroid_shape ggplot2 point shape for the centroids; 16 (a filled
#'   circle) by default.
#' @param centroid_size Centroid marker size.
#' @param centroid_colour Centroid marker colour.
#' @param verbose Report how many labels were placed.
#'
#' @return A patchwork object combining the two panels. Save it with
#'   [ggplot2::ggsave()], passing the same `width` and `height`.
#' @export
#'
#' @examples
#' \dontrun{
#' figure <- togoid_plot_umap_enrichment(embedding, results, top_n = 3)
#' ggplot2::ggsave("umap_enrichment.pdf", figure, width = 20, height = 8)
#' }
togoid_plot_umap_enrichment <- function(embedding,
                                        enrichment,
                                        top_n = 3,
                                        pval_cutoff = NULL,
                                        fdr_cutoff = 0.05,
                                        clusters = NULL,
                                        max_label_chars = 40,
                                        x_column = "umap_1",
                                        y_column = "umap_2",
                                        cluster_column = "cluster",
                                        width = 20,
                                        height = 8,
                                        point_size = 0.5,
                                        fontsize_range = c(6, 14),
                                        weight_scale = 0.8,
                                        candidates_per_term = 40,
                                        title_left = "UMAP clustering",
                                        title_right = NULL,
                                        show_centroids = TRUE,
                                        centroid_shape = 16,
                                        centroid_size = 2,
                                        centroid_colour = "black",
                                        verbose = FALSE) {
  require_plot_packages()

  embedding <- normalise_embedding(embedding, x_column, y_column, cluster_column)
  colours <- cluster_palette(embedding$cluster)
  centroids <- togoid_cluster_centroids(embedding)

  if (is.null(title_right)) {
    title_right <- if (is.null(top_n)) {
      "Enriched terms"
    } else {
      sprintf("Enriched terms (top %d per cluster)", top_n)
    }
  }

  left <- plot_cluster_panel(embedding, colours, title_left,
                             point_size = point_size, legend = TRUE)

  selected <- togoid_select_terms(
    enrichment,
    top_n = top_n,
    pval_cutoff = pval_cutoff,
    fdr_cutoff = fdr_cutoff,
    clusters = clusters,
    max_label_chars = max_label_chars
  )

  right <- plot_cluster_panel(embedding, colours, title_right,
                              point_size = point_size / 2, point_alpha = 0.2,
                              legend = FALSE)

  if (nrow(selected) == 0) {
    right <- right + ggplot2::annotate(
      "text",
      x = mean(range(embedding$umap_1)), y = mean(range(embedding$umap_2)),
      label = "No enrichment results match the selected criteria",
      size = 5, colour = "grey40"
    )
    if (verbose) {
      message("  no terms matched the selected criteria")
    }
    return(patchwork::wrap_plots(left, right, ncol = 2))
  }

  bounds <- c(range(embedding$umap_1), range(embedding$umap_2))[c(1, 3, 2, 4)]

  # Each panel gets about half the figure; the rest goes to axes and the legend.
  layout <- layout_labels(
    selected,
    centroids,
    bounds = bounds,
    panel_width_in = width * 0.40,
    panel_height_in = height * 0.85,
    fontsize_range = fontsize_range,
    weight_scale = weight_scale,
    candidates_per_term = candidates_per_term,
    reserve_centroids = show_centroids,
    centroid_size = centroid_size
  )

  if (verbose) {
    message(sprintf("  placed %d term labels, skipped %d (no free space)",
                    nrow(layout), nrow(selected) - nrow(layout)))
  }

  if (show_centroids) {
    marked <- centroids[centroids$cluster %in% selected$cluster, , drop = FALSE]
    if (nrow(marked) > 0) {
      right <- right + ggplot2::geom_point(
        data = marked,
        mapping = ggplot2::aes(x = .data$x, y = .data$y),
        inherit.aes = FALSE, shape = centroid_shape, size = centroid_size,
        stroke = 0.8, colour = centroid_colour, alpha = 0.8
      )
    }
  }

  if (nrow(layout) > 0) {
    right <- right + ggplot2::geom_text(
      data = layout,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                             colour = .data$cluster),
      inherit.aes = FALSE,
      # ggplot2 sizes text in millimetres; .pt converts from points.
      size = layout$fontsize / ggplot2::.pt,
      show.legend = FALSE, alpha = 0.9
    )

    # A label that had to go outside the data range would be cut off, so widen
    # the panel to cover everything placed.
    margin <- max(bounds[3] - bounds[1], bounds[4] - bounds[2]) * 0.02
    right <- right + ggplot2::coord_cartesian(
      xlim = c(min(bounds[1], min(layout$xmin)) - margin,
               max(bounds[3], max(layout$xmax)) + margin),
      ylim = c(min(bounds[2], min(layout$ymin)) - margin,
               max(bounds[4], max(layout$ymax)) + margin)
    )
  }

  patchwork::wrap_plots(left, right, ncol = 2)
}

#' Draw the embedding with cluster centroids marked
#'
#' A reference figure showing where the labels of
#' [togoid_plot_umap_enrichment()] are anchored.
#'
#' @param embedding Data frame with coordinates and cluster labels.
#' @param x_column Embedding column for the first dimension.
#' @param y_column Embedding column for the second dimension.
#' @param cluster_column Embedding column holding cluster labels.
#' @param point_size Point size.
#' @param title Figure title.
#' @param centroid_shape ggplot2 point shape for the centroids; 16 (a filled
#'   circle) by default.
#' @param centroid_size Centroid marker size.
#' @param centroid_colour Centroid marker colour.
#' @param show_labels Write the cluster label beside each centroid.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' togoid_plot_umap_centroids(embedding)
#' }
togoid_plot_umap_centroids <- function(embedding,
                                       x_column = "umap_1",
                                       y_column = "umap_2",
                                       cluster_column = "cluster",
                                       point_size = 0.5,
                                       title = "UMAP with cluster centroids",
                                       centroid_shape = 16,
                                       centroid_size = 3,
                                       centroid_colour = "black",
                                       show_labels = TRUE) {
  require_plot_packages()

  embedding <- normalise_embedding(embedding, x_column, y_column, cluster_column)
  colours <- cluster_palette(embedding$cluster)
  centroids <- togoid_cluster_centroids(embedding)

  plot <- plot_cluster_panel(embedding, colours, title, point_size = point_size,
                             point_alpha = 0.5, legend = FALSE) +
    ggplot2::geom_point(
      data = centroids,
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      inherit.aes = FALSE, shape = centroid_shape, size = centroid_size,
      stroke = 1.2, colour = centroid_colour
    )

  if (isTRUE(show_labels)) {
    plot <- plot + ggplot2::geom_text(
      data = centroids,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$cluster),
      inherit.aes = FALSE, hjust = -0.6, fontface = "bold", size = 4
    )
  }

  plot
}
