#' Faceted boxplots with points or violin plots
#'
#' @param df a data frame containing all the variables matching the remaining arguments
#' @param x categorical variable
#' @param y continuous variable
#' @param facet_var optional variable to facet data
#' @param boxplot_colors list of colors to use as fill for boxplots
#' @param shape either "boxplot" or "violin"
#' @param plot_points boolean variable to overlay jittered points or not. Default is `TRUE`
#'
#' @return a [ggplot2::ggplot] object
#' @export
#'
#' @examples
#' facet_boxplot(iris, x = "Species", y = "Sepal.Length", facet_var = "Species")
facet_boxplot <- function(df, x, y, facet_var = NULL,
                          boxplot_colors = NULL, shape = c("boxplot", "violin"),
                          plot_points = TRUE) {

  shape <- match.arg(shape)
  if (!x %in% colnames(df)) stop("x variable not found in data frame")
  if (!y %in% colnames(df)) stop("y variable not found in data frame")
  if (!is.null(facet_var) && (!facet_var %in% colnames(df)))
    stop("facet_var variable not found in data frame")
  p <- ggplot(df, aes(x = .data[[x]], fill = .data[[x]])) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line())

  if (shape == "boxplot") {
    p <- p +
      geom_boxplot(aes(y = .data[[y]]),
                          alpha = 0.6,
                          outlier.shape = NA)
    # Plot points only works with violin plot
    if (plot_points) {
      p <- p +
        geom_point(aes(y = .data[[y]]),
                   size = 0.2,
                   position = "jitter")
    }
  } else {
    p <- p +
      geom_violin(aes(y = .data[[y]]))
  }

  if (!is.null(facet_var)) {
    facet_formula <- stats::as.formula(paste("~", facet_var))
    p <- p +
      facet_wrap(facet_formula, ncol = 4, scales = "free")
  }
  if (!is.null(boxplot_colors)) {
    p <- p +
      scale_fill_manual(values = boxplot_colors)
  }
  p

}

#' Plot boxplots with clusters
#'
#' This is a convenience wrapper function for `facet_boxplot()`.
#'  Combined with `annotate_clusters()`, it
#' doesn't require specifying axes in `facet_boxplot()`.
#'
#' @param annotated_data data frame returned by `annotate_clusters()`
#' @param ... arguments passed to `facet_boxplot()`
#'
#' @return boxplots faceted by clusters
#' @export
#'
#' @examples
#' dmat <- compute_dmat(iris, "euclidean", TRUE, c("Petal.Length", "Sepal.Length"))
#' clusters <- compute_clusters(dmat, "complete")
#' cluster_labels <- cut_clusters(clusters, 2)
#' annotated_data <- annotate_clusters(iris[, c("Petal.Length", "Sepal.Length")], cluster_labels)
#' cluster_boxplots(annotated_data, boxplot_colors = visxhclust::cluster_colors)
cluster_boxplots <- function(annotated_data, ...)  {
  facet_boxplot(annotated_data, "Cluster", "Value", "Measurement", ...)
}


#' A custom line plot with optional vertical line
#'
#' @param df data source
#' @param x variable for horizontal axis
#' @param y variable for vertical axis
#' @param xintercept optional value in horizontal axis to highlight
#'
#' @return a [ggplot2::ggplot] object
#'
#' @export
#'
line_plot <- function(df, x, y, xintercept = NULL) {
  if (!x %in% colnames(df)) stop("x variable not found in data frame")
  if (!y %in% colnames(df)) stop("y variable not found in data frame")
  p <- ggplot(df) +
    geom_line(aes(x = .data[[x]], y = .data[[y]], group = 1)) +
    theme_bw()
  if (!is.null(xintercept)) {
    if (!is.numeric(xintercept)) stop("xintercept must be numeric")
    p <- p + geom_vline(xintercept = xintercept, linetype = "dashed")
  }
  p
}

#' Plot the first two components from the results of PCA
#'
#' @param pcres results of prcomp
#' @param cluster_labels a list of cluster labels for each point in pcres
#' @param cluster_colors a list of colors to match the cluster labels
#'
#' @noRd
pca_scatterplot <-
  function(pcres, cluster_labels, cluster_colors, xdim = "PC1", ydim ="PC2") {
  pc_df <- as.data.frame(pcres$x)
  var_explained <- round(pcres$sdev / sum(pcres$sdev) * 100, 2)
  pc_labels <- stats::setNames(paste0(colnames(pc_df),
                      " (", as.character(var_explained), "%)"), colnames(pc_df))
  pc_df$Cluster <- as.factor(cluster_labels)

  ggplot(pc_df, aes(.data[[xdim]], .data[[ydim]])) +
    geom_point(aes(color = .data$Cluster)) +
    scale_colour_manual(values = cluster_colors) +
    theme_bw() +
    labs(title = "PCA") +
    xlab(pc_labels[xdim]) +
    ylab(pc_labels[ydim]) +
    theme(legend.position = "bottom")
}

#' Plot a drivers plot
#'
#' @param df a long formatted data frame with columns `PC`, `Variable`, `p`
#' or `q`, and `Significant`
#' @param adjusted boolean to use either `p` or `q` when plotting
#'
#' @noRd
pca_driversplot <- function(df, adjusted = TRUE) {

  p_value_var <- if (adjusted) "q" else "p"
  ylimits <- rev(levels(df$Variable))
  ggplot(df,
                aes(
                  x = .data$PC,
                  y= .data$Variable,
                  fill = .data$Association,
                  color = .data$Significant)) +
    geom_tile(size = 1L, width = 0.9, height = 0.9) +
    scale_color_manual(values = c("grey90", "black")) +
    scale_fill_gradientn(colors = c("white", "pink", "orange",
                                    "red", "darkred"),
                         limits = c(0, 1)) +
    # To consider again: name = bquote(~-log(italic(.(p_value_var))))) +
    scale_x_discrete(labels = as.character(df$PC),
                     expand = expansion(add = .5)) +
    scale_y_discrete(limits = ylimits) +
    theme(panel.grid = element_blank(),
          plot.margin = margin(1, 0, 0, 0, "cm"),) +
    labs(title = "Drivers plot", y = NULL, x= NULL)
}

#' Plot a 2D MDS projection of a distance matrix
#'
#' @param dmat distance matrix
#' @param point_colors optional list of labels to color points (will be coerced to factor)
#' @param point_palette optional palette used with [ggplot2::scale_colour_manual()]
#'
#' @return a ggplot object
#'
#' @importFrom stats cmdscale
#'
#' @export
#' @examples
#' dmat <- dist(iris[, c("Sepal.Width", "Sepal.Length")])
#' dmat_projection(dmat)
dmat_projection <- function(dmat, point_colors = NULL, point_palette = NULL) {
  proj <- as.data.frame(cmdscale(dmat, k = 2))
  point_aes <- NULL
  if (!is.null(point_colors)) {
    proj$color <- as.factor(point_colors)
    point_aes <- aes(color = .data$color)
  }
  p <- ggplot(proj, aes(.data$V1, .data$V2)) +
    theme_bw() +
    labs(x = "D1", y = "D2") +
    theme(legend.position = "bottom") +
    geom_point(point_aes)
  if (!is.null(point_palette))
    p <- p + scale_colour_manual(values = point_palette)
  p
}

#' Plot distribution of annotation data across clusters
#'
#' @param annotations_df data frame with variables not used in clustering
#' @param cluster_labels output from [visxhclust::cut_clusters()]
#' @param selected_clusters optional vector of cluster labels to include in plots
#'
#' @return a `patchwork` object
#' @export
#'
#'
#' @examples
#' dmat <- compute_dmat(iris, "euclidean", TRUE, c("Petal.Length", "Sepal.Length"))
#' clusters <- compute_clusters(dmat, "complete")
#' cluster_labels <- cut_clusters(clusters, 2)
#' plot_annotation_dist(iris["Species"], cluster_labels)
plot_annotation_dist <- function(annotations_df,
                                 cluster_labels,
                                 selected_clusters = NULL) {
  if (!all(selected_clusters %in% cluster_labels)) {
    return(NULL)
  }

  annot <-
    annotate_clusters(annotations_df, cluster_labels, FALSE, selected_clusters)
  # If annotation variable is numeric, plot a histogram and facet by cluster
  # If annotation variable is categorical, plot bar charts and facet by cluster
  plots <- lapply(colnames(annotations_df), function(var_name) {
    if (is.numeric(annot[[var_name]])) {
      ggplot(stats::na.omit(annot), aes(x = .data[[var_name]])) +
        geom_histogram(bins = 20) +
        facet_wrap(~ Cluster, nrow = 1) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(),
              aspect.ratio = 1,
              axis.title.x = element_text(face = "bold", size = 12))
    } else
      ggplot(annot, aes(x = .data[[var_name]], fill = .data[[var_name]])) +
      geom_bar(stat = "count") +
      facet_wrap(~ Cluster, nrow = 1) +
      scale_fill_brewer(palette = "Set3")  + theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            aspect.ratio = 1,
            axis.title.x = element_text(face = "bold", size = 12))
  })
  do.call(patchwork::wrap_plots,
          list(plots, ncol = 1))

}
