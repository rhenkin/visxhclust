utils::globalVariables(c("where"))

#' List of colors used in the Shiny app for clusters
#' @export
cluster_colors <-
  c(
    "#4c78a8",
    "#f58518",
    "#e45756",
    "#72b7b2",
    "#54a24b",
    "#eeca3b",
    "#b07aa1",
    "#FF9DA7",
    "#9c755f",
    "#bab0ab",
    "#4c78a8",
    "#f58518",
    "#e45756",
    "#4c78a8",
    "#f58518",
    "#e45756",
    "#72b7b2",
    "#54a24b",
    "#eeca3b",
    "#b07aa1",
    "#FF9DA7",
    "#9c755f",
    "#bab0ab",
    "#4c78a8",
    "#f58518",
    "#e45756",
    "#4c78a8",
    "#f58518",
    "#e45756",
    "#72b7b2",
    "#54a24b",
    "#eeca3b",
    "#b07aa1",
    "#FF9DA7",
    "#9c755f",
    "#bab0ab",
    "#4c78a8",
    "#f58518",
    "#e45756")

#' Plot heatmap with cluster results and dendrogram
#'
#' @param scaled_selected_data scaled matrix or data frame with variables used for clustering
#' @param clusters hierarchical cluster results produced by [fastcluster::hclust()]
#' @param k targeted number of clusters
#' @param cluster_colors list of cluster colors to match with boxplots
#' @param scaled_unselected_data (optional) scaled matrix or data frame with variables not used for clustering
#' @param annotation (optional) [ComplexHeatmap::columnAnnotation] object
#'
#' @return a [ComplexHeatmap::Heatmap]
#' @export
#'
#' @examples
#' dmat <- compute_dmat(iris, "euclidean", TRUE, c("Petal.Length", "Sepal.Length"))
#' clusters <- compute_clusters(dmat, "complete")
#' species_annotation <- create_annotations(iris, "Species")
#' cluster_heatmaps(scale(iris[c("Petal.Length", "Sepal.Length")]),
#'                  clusters,
#'                  3,
#'                  visxhclust::cluster_colors,
#'                  annotation = species_annotation)
cluster_heatmaps <- function(scaled_selected_data,
                             clusters,
                             k,
                             cluster_colors,
                             scaled_unselected_data = NULL,
                             annotation = NULL) {

  top_matrix <- t(scaled_selected_data)
  if (!is.null(scaled_unselected_data)) {
    bottom_matrix <- t(scaled_unselected_data)
  }
  else {
    bottom_matrix <- NULL
  }
  heatmap_clusters <- reorder_dendrograms(clusters, k, cluster_colors)
  plot_cluster_heatmaps(top_matrix,
                        bottom_matrix,
                        heatmap_clusters$dendrogram,
                        heatmap_clusters$ids,
                        annotation)
}

#' Draw two heatmaps
#'
#' @param top_matrix matrix with selected variables
#' @param bottom_matrix matrix with unselected variables
#' @param dendrograms to draw above top matrix
#' @param clusters_set list of cluster indices
#' @param annotation (optional) any kind of annotation object to draw as top_annotation
#' @param scaled (optional) boolean to modify colour scale if data has already been scaled
#' @param distance_method (optional) if "Binary", use discrete colors for heatmap
#' @param cluster_features (optional) If FALSE, row order does not change
#' @param show_col_names (optional) If FALSE, does not show column names at base of heatmap
#'
#' @return two concatenated heatmaps drawn with ComplexHeatmap::draw
#' @keywords internal
#'
#' @importFrom ComplexHeatmap Heatmap %v% draw
#' @importFrom RColorBrewer brewer.pal
#'
plot_cluster_heatmaps <-
  function(top_matrix,
           bottom_matrix,
           dendrograms,
           clusters_set,
           annotation = NULL,
           scaled = FALSE,
           distance_method = NULL,
           cluster_features = TRUE,
           show_col_names = TRUE) {

  if (scaled) {
    if (!is.null(bottom_matrix)) {
      min_both <- min(min(top_matrix), min(bottom_matrix))
      max_both <- max(max(top_matrix), max(bottom_matrix))
      scale_values <- c(min_both, (max_both + min_both)/2 , max_both)
    } else {
      scale_values <- c(min(top_matrix),
                        (max(top_matrix)+min(top_matrix))/2,
                        max(top_matrix))
    }
    col_fun <-
      grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu")))(256)

  } else {
    if (!is.null(bottom_matrix)) {
      abs_min_both <- abs(min(min(top_matrix), min(bottom_matrix)))
      max_both <- max(max(top_matrix), max(bottom_matrix))
      scale_end <- max(max_both, abs_min_both)
    } else {
      scale_end <- max(abs(min(top_matrix)), max(top_matrix))
    }

    scale_values <-
      seq(as.integer(-scale_end), as.integer(scale_end), length.out = 11)
    col_fun <-
      circlize::colorRamp2(
        scale_values,
        rev(RColorBrewer::brewer.pal(length(scale_values), "RdBu"))
    )
  }
  if (!is.null(distance_method))
    if (distance_method == "Binary") {
      col_fun <- structure(c("#97c354", "#935fd1"), names = 0:1)
    }
  col_split <- ifelse(length(clusters_set) == 1, NULL, length(clusters_set))
  htmp <- ComplexHeatmap::Heatmap(top_matrix,
            name="Selected variables",
            col = col_fun,
            cluster_columns = dendrograms,
            cluster_rows = cluster_features,
            column_split = col_split,
            column_title = as.character(clusters_set),
            show_column_names = show_col_names,
            row_names_gp = grid::gpar(fontsize = 8),
            column_names_side = "bottom",
            column_names_gp = grid::gpar(fontsize = 8),
            cluster_column_slices = FALSE,
            column_dend_height = grid::unit(3, "cm"),
            top_annotation = if (is.null(annotation)) NULL else annotation
  )
  if (!is.null(bottom_matrix)) {
    htmp2 <- ComplexHeatmap::Heatmap(bottom_matrix,
                     name = "Not used in \n clustering",
                     col = col_fun,
                     cluster_columns = dendrograms,
                     column_split = col_split,
                     show_column_names = show_col_names,
                     row_names_gp = grid::gpar(fontsize = 8),
                     column_names_gp = grid::gpar(fontsize = 8),
                     cluster_column_slices = FALSE,
                     show_column_dend = FALSE,
                     cluster_rows = FALSE,
                     show_heatmap_legend = FALSE)

    ComplexHeatmap::draw(htmp %v% htmp2,
                       annotation_legend_side = "bottom")
  } else {
    ComplexHeatmap::draw(htmp,
                         annotation_legend_side = "bottom")
  }

}

reorder_dendrograms <- function(clustered_data, k, color_list) {
  dendro <- stats::as.dendrogram(clustered_data)
  cut_data <- dendextend::cutree(clustered_data, k = k)
  cut_data <- cut_data[stats::order.dendrogram(dendro)]
  cluster_ids <- unique(cut_data)
  subset_colors <- color_list[1:k]
  subset_colors <- subset_colors[cluster_ids]
  list(dendrogram = dendro %>%
                    dendextend::color_branches(k = k,
                                   groupLabels = cluster_ids,
                                   col = subset_colors),
    ids = cluster_ids
    )
}

#' Create heatmap annotations from selected variables
#'
#' This function will create a [ComplexHeatmap::columnAnnotation] object with rows
#' for each variable passed as argument. Character columns will be coerced into factors.
#' For factors, the ColorBrewer palette `Set3` will be used. For non-negative numeric, the
#' `PuBu` palette will be used, and for columns with negative values, the reversed `RdBu` will be used.
#'
#' @param df a data frame. It can be an original unscaled data, or a scaled one
#' @param selected_variables list of columns in the data frame to create annotations for
#'
#' @return a [ComplexHeatmap::columnAnnotation] object
#'
#' @export
#' @importFrom ComplexHeatmap columnAnnotation
#' @importFrom RColorBrewer brewer.pal
#' @importFrom circlize colorRamp2
create_annotations <- function(df, selected_variables) {
  scaled_annotation <- df %>%
    dplyr::select(dplyr::all_of(selected_variables)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), scale))
  cat_annot_df <- as.data.frame(scaled_annotation)
  cat_annot_colors <- lapply(names(cat_annot_df), function(x) {
    values <- as.vector(cat_annot_df[[x]])
    if (!is.numeric(values)) {
      values <- ifelse(is.na(values), 99, values)
      max_values <- length(unique(values))
      pal_n <- min(max(3, max_values + 1), 12)
      anno_colors <-
        brewer.pal(pal_n, "Set3")[1:max_values]
      names(anno_colors) <- levels(as.factor(values))
    } else {
      if (all(values >= 0)) {
        breaks <- pretty(values, 4)
        anno_colors <- colorRamp2(breaks,
                        rev(brewer.pal(length(breaks), "PuBu")))
      } else {
        anno_colors <- colorRamp2(c(-4, -2, -1, 0, 1, 2, 4),
                        rev(brewer.pal(7, "RdBu")))
      }
    }
    anno_colors
  })
  names(cat_annot_colors) <- names(cat_annot_df)
  columnAnnotation(df = cat_annot_df, col = cat_annot_colors)
}

#' Draw a minimal heatmap without labels or dendrograms
#'
#' @param df a data frame
#'
#' @return ComplexHeatmap heatmap
#' @noRd
#'
#'
dmat_heatmap <- function(df) {
  ComplexHeatmap::Heatmap(as.matrix(df),
          name = "Distance matrix",
          col = RColorBrewer::brewer.pal(9, "RdGy"),
          show_column_names = FALSE,
          show_row_names = FALSE,
          show_row_dend = FALSE,
          show_column_dend = FALSE
  )
}

#' Plot a correlation heatmap
#'
#' Computes pairwise Pearson correlation; if there are fewer than 15 columns, prints
#' the value of the correlation coefficient inside each tile.
#'
#' @param df numeric data frame to compute correlations
#'
#' @return a [ComplexHeatmap::Heatmap]
#'
#' @export
correlation_heatmap <- function(df) {
  corr_df <- stats::cor(df, method = "pearson")
  if (length(colnames(corr_df)) < 15) {
    ComplexHeatmap::Heatmap(
      corr_df,
      name = "Pearson's r",
      col = circlize::colorRamp2(c(-1, -0.5, 0, 0.5, 1),
                       rev(RColorBrewer::brewer.pal(5, "RdBu"))),
      cell_fun = function(j, i, x, y, width, height, fill) {
        if (i != j)
          grid::grid.text(sprintf("%.2f", corr_df[i, j]),
                    x, y, gp = grid::gpar(fontsize = 10))
      }
    )
  } else {
    ComplexHeatmap::Heatmap(corr_df,
            name = "Pearson's r",
            col = circlize::colorRamp2(c(-1, -0.5, 0, 0.5, 1),
                             rev(RColorBrewer::brewer.pal(5, "RdBu"))))
  }
}
