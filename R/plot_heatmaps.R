utils::globalVariables(c("where"))
#' Draw two heatmaps
#'
#' @param top_matrix matrix with selected variables
#' @param bottom_matrix matrix with unselected variables
#' @param dendrograms to draw above top matrix
#' @param clusters_set list of cluster indices
#' @param annotation any kind of annotation object to draw as top_annotation
#'
#' @return two concatenated heatmaps drawn with ComplexHeatmap::draw
#' @keywords internal
#'
#' @importFrom ComplexHeatmap Heatmap %v% draw
#' @importFrom RColorBrewer brewer.pal
#'
cluster_heatmaps <- function(top_matrix, bottom_matrix, dendrograms,
                                      clusters_set, annotation = NULL) {

  col_fun <- circlize::colorRamp2(c(-4,-2,-1,0,1,2,4),
                                  rev(RColorBrewer::brewer.pal(7, "RdBu")))

  col_split <- if (length(clusters_set) == 1) {
    NULL
  } else {
    length(clusters_set)
  }

  htmp <- ComplexHeatmap::Heatmap(top_matrix,
            name="Selected variables",
            col = col_fun,
            cluster_columns = dendrograms,
            column_split = col_split,
            column_title = as.character(clusters_set),
            cluster_column_slices = FALSE,
            column_dend_height = unit(5, "cm"),
            clustering_distance_rows = "pearson",
            top_annotation = if (is.null(annotation)) NULL else annotation
  )
  if (!is.null(bottom_matrix)) {
    htmp2 <- ComplexHeatmap::Heatmap(bottom_matrix, name = "Not used in \n clustering",
                     col = col_fun,
                     cluster_columns = dendrograms,
                     column_split = col_split,
                     cluster_column_slices = FALSE,
                     show_column_dend = FALSE,
                     cluster_rows = FALSE,
                     clustering_distance_rows = "pearson")

    ComplexHeatmap::draw(htmp %v% htmp2, padding=unit(c(0,0,0,0),"mm"), annotation_legend_side = "bottom")
  } else {
    ComplexHeatmap::draw(htmp, padding=unit(c(0,0,0,0),"mm"), annotation_legend_side = "bottom")
  }

}

#' @importFrom stats order.dendrogram as.dendrogram
#' @importFrom dendextend cutree color_branches
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

#' @importFrom ComplexHeatmap columnAnnotation
#' @importFrom RColorBrewer brewer.pal
create_annotations <- function(df, selected_variables) {


  scaled_annotation <- df[, selected_variables] %>%
                        mutate(across(where(is.numeric), scale))
  cat_annot_df <- as.data.frame(scaled_annotation)
  cat_annot_colors <- lapply(names(cat_annot_df), function(x) {
    values <- as.vector(cat_annot_df[[x]])
    if (!is.numeric(values)) {
      values <- ifelse(is.na(values), 99, values)
      max_values <- length(unique(values))
      anno_colors <-
        RColorBrewer::brewer.pal(max(3, max_values + 1), "Set3")[1:max_values]
      names(anno_colors) <- levels(as.factor(values))
    } else {
      anno_colors <- circlize::colorRamp2(c(-4,-2,-1,0,1,2,4),
                      rev(RColorBrewer::brewer.pal(7, "RdBu")))
    }
    anno_colors
  })
  names(cat_annot_colors) <- names(cat_annot_df)
  ComplexHeatmap::columnAnnotation(df = cat_annot_df, col = cat_annot_colors)
}

#' Draw a minimal heatmap without labels or dendrograms
#'
#' @param df a data frame
#'
#' @return ComplexHeatmap heatmap
#' @noRd
#'
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom RColorBrewer brewer.pal
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

#' @importFrom ComplexHeatmap Heatmap
#' @importFrom circlize colorRamp2
#' @importFrom RColorBrewer brewer.pal
correlation_heatmap <- function(df) {
  corr_df <- cor(df, method = "pearson")
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
    Heatmap(corr_df,
            name = "Pearson's r",
            col = colorRamp2(c(-1, -0.5, 0, 0.5, 1),
                             rev(brewer.pal(5, "RdBu"))))
  }
}
