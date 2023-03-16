#' @noRd
cholMaha <- function(df) {
  dec <- chol(stats::cov(df))
  tmp <- forwardsolve(t(dec), t(df) )
  stats::dist(t(tmp))
}

#' Compute a distance matrix from scaled data
#'
#' @description This function applies scaling to the columns of a data frame and
#' computes and returns a distance matrix from a chosen distance measure.
#'
#' @param x a numeric data frame or matrix
#' @param dist_method a distance measure to apply to the scaled data. Must be those supported by [stats::dist()], plus `"mahalanobis"` and `"cosine"`. Default is `"euclidean"`.
#' @param apply_scaling use TRUE to apply [base::scale()]. By default does not scale data.
#' @param subset_cols (optional) a list of columns to subset the data
#'
#' @return an object of class "dist" (see [stats::dist()])
#'
#' @export
#'
#' @examples
#' dmat <- compute_dmat(iris, "euclidean", TRUE, c("Petal.Length", "Sepal.Length"))
#' print(class(dmat))
compute_dmat <- function(x,
                         dist_method = "euclidean",
                         apply_scaling = FALSE,
                         subset_cols = NULL) {
  methods <- c("euclidean", "cosine", "mahalanobis", "manhattan", "maximum",
               "canberra", "minkowski", "binary")
  dist_method <- match.arg(tolower(dist_method), methods)

  if (!is.null(subset_cols))
    x <- x[, subset_cols]

  validate_dataset(x)

  if (dist_method == "cosine") {
    # https://stats.stackexchange.com/a/367216
    numeric_mat <- as.matrix(x)
    sim <- numeric_mat / sqrt(rowSums(numeric_mat * numeric_mat))
    sim <- sim %*% t(sim)
    stats::as.dist(1 - sim)
  } else if (dist_method == "mahalanobis") {
    cholMaha(x)
  } else if (dist_method == "binary") {
    stats::dist(x, method = "binary")
  } else if (dist_method %in% c("euclidean", "maximum", "manhattan",
                                "canberra", "minkowski")) {
    stats::dist(scale_data(x, apply_scaling), method = dist_method)
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
#' dmat <- compute_dmat(iris, "euclidean", TRUE, c("Petal.Length", "Sepal.Length"))
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
#' dmat <- compute_dmat(iris, "euclidean", TRUE, c("Petal.Length", "Sepal.Length"))
#' clusters <- compute_clusters(dmat, "complete")
#' cluster_labels <- cut_clusters(clusters, 2)
#' head(cluster_labels)
cut_clusters <- function(clusters, k) {
  dendextend::cutree(clusters,
                     k = k,
                     order_clusters_as_data = TRUE)
}


# nocov start
#' @noRd
relabel_clusters <- function(list_of_labels, df, rank_variable) {
  lapply(list_of_labels, function(x) {
    df[,"Cluster"] <- x[[2]]
    medians <- stats::aggregate(. ~ Cluster, df, stats::median)
    medians <- medians[order(medians[rank_variable]),]
    factor(x[[2]], levels = medians$Cluster)
  })
}
# nocov end

#' Compute an internal evaluation metric for clustered data
#'
#' @description Metric will be computed from 2 to max_k clusters. Note that the row number in results will be different from k.
#'
#' @param dmat distance matrix output of [compute_dmat()] or [stats::dist()]
#' @param clusters output of [compute_clusters()] or [fastcluster::hclust()]
#' @param metric_name "silhouette" or "dunn"
#' @param max_k maximum number of clusters to cut using [dendextend::cutree()]. Default is 14.
#'
#' @return a data frame with columns `k` and `score`
#' @export
#'
#' @examples
#' data_to_cluster <- iris[c("Petal.Length", "Sepal.Length")]
#' dmat <- compute_dmat(data_to_cluster, "euclidean", TRUE)
#' clusters <- compute_clusters(dmat, "complete")
#' compute_metric(dmat, clusters, "dunn")
compute_metric <- function(dmat, clusters, metric_name, max_k = 14) {
  if (!metric_name %in% c("silhouette", "dunn")) {
    stop("Invalid metric name. metric_name should be
         one of 'silhouette' or 'dunn'")
  }
  stopifnot(class(dmat) == "dist", max_k > 2)
  calc_measure <- as.vector(unlist(lapply(2:max_k, function(k)  {
    if (metric_name == "silhouette") {
      sil <- cluster::silhouette(
        x = dendextend::cutree(
          clusters,
          k = k,
          order_clusters_as_data = TRUE
        ),
        dist = dmat
      )
      summary_sil <- summary(sil)
      summary_sil$avg.width
    } else {
      clValid::dunn(
        distance = dmat,
        clusters = dendextend::cutree(
          clusters,
          k = k,
          order_clusters_as_data = TRUE
        )
      )
    }
  })))
  data.frame(k = factor(2:max_k), score = calc_measure)
}

#' Compute Gap statistic for clustered data
#'
#' @param df the data used to compute clusters
#' @param clusters output of [compute_clusters()] or [fastcluster::hclust()]
#' @param gap_B number of bootstrap samples for [cluster::clusGap()] function. Default is 50.
#' @param max_k maximum number of clusters to compute the statistic. Default is 14.
#'
#' @return a data frame with the Tab component of [cluster::clusGap()] results
#' @export
#'
#' @examples
#' data_to_cluster <- iris[c("Petal.Length", "Sepal.Length")]
#' dmat <- compute_dmat(data_to_cluster, "euclidean", TRUE)
#' clusters <- compute_clusters(dmat, "complete")
#' gap_results <- compute_gapstat(scale(data_to_cluster), clusters)
#' head(gap_results)
compute_gapstat <- function(df, clusters, gap_B = 50, max_k = 14) {
  FUN_gap <- function(clusters, k) {
    list(cluster = dendextend::cutree(clusters,
                                      k = k,
                                      order_clusters_as_data = TRUE))
  }
  if (max_k <= 2)
    stop("max_k must be greater than 2")
  res <- cluster::clusGap(df,
                 function(x, k, clusters) FUN_gap(clusters, k),
                 B = gap_B,
                 K.max = max_k,
                 clusters = clusters,
                 verbose = FALSE)
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
#' data_to_cluster <- iris[c("Petal.Length", "Sepal.Length")]
#' dmat <- compute_dmat(data_to_cluster, "euclidean", TRUE)
#' clusters <- compute_clusters(dmat, "complete")
#' res <- compute_metric(dmat, clusters, "dunn")
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
           if (!all(decr) & any(decr)) which.min(decr) else K # the first TRUE, or K
         },
         "globalmin" = {
           which.min(x)
         },
         "firstmax" = { ## the first local maximum
           decr <- diff(x) < 0 # length K-1
           if (!all(decr) & any(decr)) which.max(decr) else 1 # the first TRUE, or K
         },
         "globalmax" = {
           which.max(x)
         })
}

#' Annotate data frame with clusters
#'
#' @param df a data frame
#' @param cluster_labels list of cluster labels, automatically converted to factor.
#' @param long if `TRUE`, returned data frame will be in long format. See details for spec. Default is `TRUE`.
#' @param selected_clusters optional cluster labels to filter
#'
#' @details Long data frame will have columns: `Cluster`, `Measurement` and `Value`.
#'
#' @return a wide or long data frame
#' @export
#'
#' @examples
#' dmat <- compute_dmat(iris, "euclidean", TRUE, c("Petal.Length", "Sepal.Length"))
#' res <- compute_clusters(dmat, "complete")
#' cluster_labels <- cut_clusters(res, 2)
#' annotated_data <- annotate_clusters(iris[, c("Petal.Length", "Sepal.Length")], cluster_labels)
#' head(annotated_data)
annotate_clusters <- function(df, cluster_labels, long = TRUE,
                             selected_clusters = NULL) {
  stopifnot(is.data.frame(df),
            "Selected clusters are not valid" =
              all(selected_clusters %in% cluster_labels))
  df$Cluster <- as.factor(cluster_labels)
  if (!is.null(selected_clusters)) {
    df <- df[df$Cluster %in% selected_clusters, ]
  }
  if (long) {
    tidyr::pivot_longer(df,
                c(-"Cluster"),
                names_to = "Measurement",
                values_to = "Value")
  }
  else {
    df
  }
}
