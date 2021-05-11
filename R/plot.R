#' Faceted boxplots with points or violin plots
#'
#' @param df a data frame containing all the variables matching the remaining arguments
#' @param x categorical variable
#' @param y continuous variable
#' @param facet_var optional variable to facet data
#' @param boxplot_colors list of colors to use as fill for boxplots
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
  } else {
    p <- p +
      geom_violin(aes(y = .data[[y]]))
  }

  if (shape == "boxplot")
  if (plot_points) {
    p <- p +
      geom_point(aes(y = .data[[y]]),
                          size = 0.2,
                          position = "jitter")
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
#' This is a convenience wrapper function for `facet_boxplot()`. Combined with `annotate_clusters()`, it
#' doesn't require specifying axes in `facet_boxplot()`.
#'
#' @param annotated_data data frame returned by `annotate_clusters()`
#' @param ... arguments passed to `facet_boxplot()`
#'
#' @return boxplots faceted by clusters
#' @export
#'
#' @examples
#' dmat <- compute_dmat(iris, "euclidean", "z-scores", c("Petal.Length", "Sepal.Length"))
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
  p <- ggplot(df) +
    geom_line(aes(x = .data[[x]], y = .data[[y]], group = 1)) +
    theme_bw()
  if (!is.null(xintercept)) {
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
pca_scatterplot <- function(pcres, cluster_labels, cluster_colors) {
  pc_df <- as.data.frame(pcres$x)
  var_explained <- round(pcres$sdev / sum(pcres$sdev) * 100, 2)
  pc_labels <- paste0(colnames(pc_df),
                      "(", as.character(var_explained), "%)")
  pc_df$Cluster <- as.factor(cluster_labels)

  ggplot(pc_df, aes(.data$PC1, .data$PC2)) +
    geom_point(aes(color = .data$Cluster)) +
    scale_colour_manual(values = cluster_colors) +
    theme_bw() +
    labs(title = "PCA") +
    xlab(pc_labels[1]) +
    ylab(pc_labels[2]) +
    theme(legend.position = "bottom")
}

#' Plot a drivers plot
#'
#' @param df a long formatted data frame with columns `PC`, `Variable`, `p`,
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
    scale_color_manual(values = c('grey90', 'black')) +
    scale_fill_gradientn(colors = c('white', 'pink', 'orange',
                                    'red', 'darkred'),
                         limits = c(0, 1)) +
    # To consider again: name = bquote(~-log(italic(.(p_value_var))))) +
    scale_x_discrete(labels = as.character(df$PC),
                     expand = expansion(add = .5)) +
    scale_y_discrete(limits = ylimits) +
    theme(panel.grid = element_blank()) +
    labs(title = "Drivers plot", y = NULL, x= NULL)
}

#' @noRd
silhouette_plot <- function(points, clusters, sil_widths, cluster_colors) {

  points$cluster <- as.factor(clusters)
  points$sil_width <- sil_widths
  points[, "id"] <- reorder_within(factor(1:nrow(points)),
                                   .data$sil_width,
                                   .data$cluster)
  ggplot(points,
                  aes_string(y = "id", x = "sil_width", fill = "cluster")) +
    geom_col(orientation = "y") +
    theme_bw() +
    facet_wrap(~ cluster, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = cluster_colors) +
    scale_y_reordered() +
    scale_x_continuous(expand = c(0,0))+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggtitle("Silhouette widths per cluster")
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

plot_compare <- function(compare_df) {
  ggplot(
    compare_df,
    aes_string(
      x = "Config",
      alluvium = "Subject",
      stratum = "Cluster",
      fill = "Cluster",
      label = "Cluster"
    )
  ) +
    geom_flow() +
    geom_stratum() +
    geom_text(stat = "stratum", size = 4) +
    theme_bw()
}
