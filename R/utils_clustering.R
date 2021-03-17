#' @noRd
#' @importFrom stats cov dist
cholMaha <- function(df) {
  dec <- chol( cov(df) )
  tmp <- forwardsolve(t(dec), t(df) )
  dist(t(tmp))
}

#' Compute a distance matrix from scaled data
#'
#' @description This function applies scaling to the columns of a data frame and
#' computes and returns a distance matrix from a chosen distance measure.
#'
#' @param x a numeric data frame or matrix
#' @param scaling_method method to scale data frame columns. Valid values are `z-scores` and `robust`, any other will return unscaled data.
#' @param dist_method a distance measure to apply to the scaled data. Must be those supported by [stats::dist()], plus 'mahalanobis' and 'cosine'.
#' @param subset_cols a list of columns to subset the data
#'
#' @return an object of class "dist" (see [stats::dist()])
#'
#' @export
#'
#' @importFrom stats dist as.dist
compute_dmat <- function(x,
                         dist_method = "euclidean",
                         scaling_method = NULL,
                         subset_cols = NULL) {
  methods <- c("euclidean", "cosine", "mahalanobis", "manhattan", "maximum",
               "canberra", "minkowski")
  dist_method <- match.arg(tolower(dist_method), methods)
  if (dist_method == -1)
    stop("Unsupported distance method")
  if (!is.null(subset_cols))
    x <- x[, subset_cols]
  if (dist_method == "cosine") {
    # https://stats.stackexchange.com/a/367216
    numeric_mat <- as.matrix(x)
    sim <- numeric_mat / sqrt(rowSums(numeric_mat * numeric_mat))
    sim <- sim %*% t(sim)
    as.dist(1 - sim)
  } else if (dist_method == "mahalanobis") {
    cholMaha(x)
  } else if (dist_method %in% c("euclidean", "maximum", "manhattan",
                                "canberra", "minkowski")) {
    if (missing(scaling_method)) {
      dist(x, method = dist_method)
    } else {
      dist(scale_data(x, scaling_method), method = dist_method)
    }
  }
}

#' Compute clusters hierarchically from distance matrix
#'
#' @param dmat a distance matrix
#' @param linkage_method a linkage method supported by [fastcluster::hclust()]
#'
#' @return clusters computed by [fastcluster::hclust()]
#' @export
#'
#' @examples
#' dmat <- compute_dmat(iris, "euclidean", "z-scores", c("Petal.Length", "Sepal.Length"))
#' res <- compute_clusters(dmat, "complete")
compute_clusters <- function(dmat, linkage_method) {
  fastcluster::hclust(dmat, method = linkage_method)
}

#' Cut a hierarchical tree targeting k clusters
#'
#' @param clusters cluster results, produced by e.g. [fastcluster::hclust()]
#' @param k target number of clusters
#'
#' @return cluster labels
#' @export
#'
#' @examples
#' dmat <- compute_dmat(iris, "euclidean", "z-scores", c("Petal.Length", "Sepal.Length"))
#' clusters <- compute_clusters(dmat, "complete")
#' cluster_labels <- cut_clusters(clusters, 2)
#' head(cluster_labels)
cut_clusters <- function(clusters, k) {
  dendextend::cutree(clusters,
                     k = k,
                     order_clusters_as_data = TRUE)
}

#' @noRd
#' @importFrom stats aggregate median
relabel_clusters <- function(list_of_labels, df, rank_variable) {
  lapply(list_of_labels, function(x) {
    df[,"Cluster"] <- x[[2]]
    medians <- aggregate(. ~ Cluster, df, median)
    medians <- medians[order(medians[rank_variable]),]
    factor(x[[2]], levels = medians$Cluster)
  })
}

#' Compute an internal evaluation metric for clustered data
#'
#' @param df data frame used to compute clusters
#' @param clusters output of [compute_clusters()] or [fastcluster::hclust()]
#' @param metric_name valid metric name from [clusterCrit::getCriteriaNames()] (with TRUE argument)
#' @param max_k maximum number of clusters to cut using [dendextend::cutree()]
#'
#' @return a data frame with columns `k` and `score`
#' @export
#'
#' @examples
#' data_to_cluster <- iris[, c("Petal.Length", "Sepal.Length")]
#' dmat <- compute_dmat(data_to_cluster, "euclidean", "z-scores")
#' clusters <- compute_clusters(dmat, "complete")
#' compute_metric(scale_data(data_to_cluster), clusters, "Dunn")
compute_metric <- function(df, clusters, metric_name, max_k = 14) {
  if (!metric_name %in% clusterCrit::getCriteriaNames(TRUE)) {
    stop("Invalid metric name. Please check
         clusterCrit::getCriteriaNames(TRUE) for a valid argument")
  }
  calc_measure <- as.vector(unlist(lapply(2:max_k, function(k)  {
    clusterCrit::intCriteria(
      as.matrix(df),
      dendextend::cutree(
        clusters,
        k = k,
        order_clusters_as_data = TRUE
      ),
      metric_name
    )
  })))
  data.frame(k = factor(2:max_k), score = calc_measure)
}

#' Compute Gap statistic for clustered data
#'
#' @param df the data used to compute clusters
#' @param clusters output of [compute_clusters()] or [fastcluster::hclust()]
#' @param gap_B number of bootstrap samples for [clustGap()] function
#' @param max_k maximum number of clusters to compute the statistic
#'
#' @return a data frame with the Tab component of [clustGap()] results
#' @export
#'
#' @importFrom cluster clusGap
#' @importFrom dendextend cutree
#'
#' @examples
#' data_to_cluster <- iris[, c("Petal.Length", "Sepal.Length")]
#' dmat <- compute_dmat(data_to_cluster, "euclidean", "z-scores")
#' clusters <- compute_clusters(dmat, "complete")
#' gap_results <- compute_gapstat(data_to_cluster, clusters)
#' head(gap_results)
compute_gapstat <- function(df, clusters, gap_B = 50, max_k = 14) {
  FUN_gap <- function(clusters, k) {
    list(cluster = dendextend::cutree(clusters,
                                      k = k,
                                      order_clusters_as_data = TRUE))
  }
  res <- clusGap(df,
                 function(x, k, clusters) FUN_gap(clusters, k),
                 B = gap_B,
                 K.max = max_k,
                 clusters = clusters)
  gap_table <- res$Tab
  gap_table <- as.data.frame(gap_table)
  gap_table$k <- as.factor(1:max_k)
  gap_table
}

#' Find minimum or maximum score in a vector
#'
#' @description This function is meant to be used with compute_metric. For Gap statistic,
#' use [cluster::maxSE()].
#'
#' @param x a numeric vector
#' @param method one of "firstmax", "globalmax", "firstmin" or "globalmin"
#'
#' @return the index (not k) of the identified maximum or minimum score
#' @export
#'
#' @examples
#' data_to_cluster <- iris[, c("Petal.Length", "Sepal.Length")]
#' dmat <- compute_dmat(data_to_cluster, "euclidean", "z-scores")
#' clusters <- compute_clusters(dmat, "complete")
#' res <- compute_metric(scale_data(data_to_cluster), clusters, "Dunn")
#' optimal_score(res$score, method = "firstmax")
optimal_score <- function(x,
                           method = c(
                             "firstmax",
                             "globalmax",
                             "firstmin",
                             "globalmin"
                           )
)
{
  method <- match.arg(method)
  stopifnot((K <- length(x)) >= 1)
  switch(method,
         "firstmin" = { ## the first local minimum
           decr <- diff(x) <= 0 # length K-1
           if(any(decr)) which.min(decr) else K # the first TRUE, or K
         },
         "globalmin" = {
           which.min(x)
         },
         "firstmax" = { ## the first local maximum
           decr <- diff(x) < 0 # length K-1
           if(any(decr)) which.max(decr) else K # the first TRUE, or K
         },
         "globalmax" = {
           which.max(x)
         })
}

#' Annotate data frame with clusters
#'
#' @param df a data frame
#' @param clusters list of cluster labels, automatically converted to factor.
#' @param long if TRUE, returned data frame will be in long format. See details for spec.
#' @param selected_clusters optional labels to filter
#'
#' @details Long data frame will have columns: `Cluster`, `Measurement` and `Value`.
#'
#' @return a wide or long data frame
#' @export
#'
#' @examples
#' res <- compute_clusters(dist(iris[, c("Petal.Length", "Sepal.Length")]), "complete")
#' cluster_labels <- cut_clusters(res, 2)
#' annotated_data <- annotate_clusters(iris[, c("Petal.Length", "Sepal.Length")], cluster_labels)
#' head(annotated_data)
annotate_clusters <- function(df, clusters, long = TRUE,
                             selected_clusters = NULL) {
  df$Cluster <- as.factor(clusters)
  if (!is.null(selected_clusters)) {
    df <- df[df$Cluster %in% selected_clusters, ]
  }
  if (long) {
    pivot_longer(df,
                c(-.data$Cluster),
                names_to = "Measurement",
                values_to = "Value")
  }
  else {
    df
  }
}
