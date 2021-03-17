#' Faceted boxplots with points
#'
#' @param df a data frame containing all the variables matching the remaining arguments
#' @param x categorical variable
#' @param y continuous variable
#' @param facet_var optional variable to facet data
#' @param boxplot_colors list of colors to use as fill for boxplots
#' @param plot_points boolean variable to overlay jittered points or not. Default is `TRUE`
#'
#' @return a ggplot object
#' @export
#'
#' @importFrom stats as.formula
#'
#' @examples
#' facet_boxplot(iris, x = "Species", y = "Sepal.Length", facet_var = "Species")
facet_boxplot <- function(df, x, y, facet_var = NULL,
                          boxplot_colors = NULL, plot_points = TRUE) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], fill = .data[[x]])) +
    ggplot2::geom_boxplot(aes(y = .data[[y]]),
                          alpha = 0.6,
                          outlier.shape = NA) +
    ggplot2::theme_bw()

  if (plot_points) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(y = .data[[y]]),
                          size = 0.2,
                          position = "jitter")
  }

  if (!is.null(facet_var)) {
    facet_formula <- as.formula(paste("~", facet_var))
    p <- p +
      ggplot2::facet_wrap(facet_formula, ncol = 4, scales = "free")
  }
  if (!is.null(boxplot_colors)) {
    p <- p +
      ggplot2::scale_fill_manual(values = boxplot_colors)
  }
  p

}

#' @noRd
line_plot <- function(df, x, y, xintercept = NULL) {
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = .data[[x]], y = .data[[y]], group = 1)) +
    ggplot2::theme_bw()
  if (!is.null(xintercept)) {
    p <- p + ggplot2::geom_vline(xintercept = xintercept, linetype = "dashed")
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

  ggplot2::ggplot(pc_df, ggplot2::aes(.data$PC1, .data$PC2)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Cluster)) +
    ggplot2::scale_colour_manual(values = cluster_colors) +
    ggplot2::theme_bw() +
    ggplot2:: labs(title = "PCA") +
    ggplot2::xlab(pc_labels[1]) +
    ggplot2::ylab(pc_labels[2]) +
    ggplot2::theme(legend.position = "bottom")
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
  ggplot2::ggplot(df,
                ggplot2::aes(
                  x = .data$PC,
                  y= .data$Variable,
                  fill = .data$Association,
                  color = .data$Significant)) +
    ggplot2::geom_tile(size = 1L, width = 0.9, height = 0.9) +
    ggplot2::scale_color_manual(values = c('grey90', 'black')) +
    ggplot2::scale_fill_gradientn(colors = c('white', 'pink', 'orange',
                                    'red', 'darkred'),
                         limits = c(0, 1)) +
    # To consider again: name = bquote(~-log(italic(.(p_value_var))))) +
    ggplot2::scale_x_discrete(labels = as.character(df$PC),
                     expand = ggplot2::expansion(add = .5)) +
    ggplot2::scale_y_discrete(limits = ylimits) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "Drivers plot", y = NULL, x= NULL)
}

#' @noRd
silhouette_plot <- function(points, clusters, sil_widths, cluster_colors) {

  points$cluster <- as.factor(clusters)
  points$sil_width <- sil_widths
  points[, "id"] <- reorder_within(factor(1:nrow(points)),
                                   .data$sil_width,
                                   .data$cluster)
  ggplot2::ggplot(points,
                  ggplot2::aes_string(y = "id", x = "sil_width", fill = "cluster")) +
    ggplot2::geom_col(orientation = "y") +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~ cluster, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(values = cluster_colors) +
    ggplot2::scale_y_reordered() +
    ggplot2::scale_x_continuous(expand = c(0,0))+
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::ggtitle("Silhouette widths per cluster")
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
    point_aes <- ggplot2::aes(color = .data$color)
  }
  p <- ggplot2::ggplot(proj, ggplot2::aes(.data$V1, .data$V2)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "D1", y = "D2") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_point(point_aes)
  if (!is.null(point_palette))
    p <- p + ggplot2::scale_colour_manual(values = point_palette)
  p
}
