#' Loads a file
#'
#' @param filepath a file path provided by the user or a Shiny app
#'
#' @return a data frame
#' @noRd
#'
#'
load_data <- function(filepath) {
  ext <- tools::file_ext(filepath)
  loaded_data <- switch(ext,
         rds = readRDS(filepath),
         csv = readr::read_csv(filepath, col_types = readr::cols()),
         tsv = readr::read_delim(filepath, delim = "\t", col_types = readr::cols()),
         txt = readr::read_delim(filepath, delim = "\t", col_types = readr::cols())
  )
  if (!is.data.frame(loaded_data)) {
    loaded_data <- as.data.frame(loaded_data)
  }
  colnames(loaded_data) <- make.names(colnames(loaded_data))
  loaded_data
}

validate_dataset <- function(x) {
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns in data must be numeric.")
  }
}

#' Conditional scaling
#'
#' @param x a numeric data frame or matrix
#' @param flag scale or not? Default is TRUE
#'
#' @details Robust scaling is done via median absolute deviation
#'
#' @return a scaled data frame
#'
#' @noRd
#' @examples
#' scaled_data <- scale_data(iris[, c("Petal.Length", "Sepal.Length")])
#' head(scaled_data)
scale_data <- function(x, flag = TRUE) {
  validate_dataset(x)
  if (flag) {
    scale(x)
  } else {
    x
  }
}
#' Get list of correlated variables
#'
#' @param df data frame
#' @param threshold correlation threshold
#'
#' @noRd
get_correlated_variables <- function(df, threshold) {

  corr_df <- as.data.frame(stats::cor(df, method="spearman"))
  corr_df$Var1 <- rownames(corr_df)
  corr_df <- tidyr::pivot_longer(corr_df,
                            -.data$Var1, names_to="Var2", values_to="Corr")
  corr_df <- corr_df[corr_df$Var1 != corr_df$Var2, ]

  # Rather than having a null hypothesis of rho (correlation) = 0 and an
  # alternative of two-sided rho != 0, we change our test to be a null
  # hypothesis H0: rho = 0.9, and an alternative hypothesis of H1: rho < 0.9
  # (one-sided). The interpretation will be that we will reject H0 for those
  # pairs of variables with a correlation significantly lower than a
  # threshold, let's say the usual 0.05. So we drop variables with correlation
  # above 0.9, as well as those that are not significantly lower than 0.9; with
  # this we might catch correlations close enough to 0.9, even if not exactly
  # 0.9

  max_corr <- threshold
  dof <- nrow(df) - 2
  t0 <- sqrt(dof) * max_corr / (sqrt(1 - (max_corr**2)))
  t1 <- sqrt(dof) * corr_df$Corr / sqrt(1 - (corr_df$Corr ** 2))

  #Besides calculating the p-value (and adjusted), the code below also look
  #for duplicate rows of combinations of variables and remove them
  corr_df <- corr_df %>%
    dplyr::mutate(pval = stats::pt(t1, df = dof, ncp = t0),
           padj = stats::p.adjust(.data$pval, method="fdr")) %>%
    dplyr::group_by(grp = paste(pmax(.data$Var1, .data$Var2),
                         pmin(.data$Var1, .data$Var2), sep="_")) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$grp)
  #Look for those that are not significantly lower
  corr_df[corr_df$padj > 0.05, ]

}

#From: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
#' @importFrom stats reorder
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

#' Remove selected rows by cluster
#'
#' @param df data frame with Cluster column
#' @param selected_cluster number of the selected cluster
#' @param row_numbers row numbers within the selected cluster subset
#'
#' @return subset of data frame with the selected rows
#' @noRd
remove_selected_rows <- function(df, clusters, selected_cluster, row_numbers) {
  to_remove <- df %>%
    dplyr::mutate(Cluster = as.factor(clusters)) %>%
    dplyr::filter(.data$Cluster == selected_cluster) %>%
    dplyr::filter(dplyr::row_number() %in% row_numbers)
  df[!(df$ID %in% to_remove$ID) ,]
}

#' Keep selected rows by cluster
#'
#' @param df data frame with Cluster column
#' @param selected_cluster number of the selected cluster
#' @param row_numbers row numbers within the selected cluster subset
#'
#' @return subset of data frame with the selected rows
#' @noRd
keep_selected_rows <- function(df, clusters, selected_cluster) {
  df %>%
    dplyr::mutate(Cluster = as.factor(clusters)) %>%
    dplyr::filter(.data$Cluster == selected_cluster)

}

#' Vectorized computation of p values
#'
#' @param x a data frame
#' @param y an optional second data frame
#' @param ... additional arguments to cor.test
#'
#' @noRd
compute_pvalues <- function(x, y = NULL, ...) {
  FUN <- function(x, y, ...) stats::cor.test(x, y, ...)[["p.value"]]
  if (missing(y)) {
    y <- t(x)
  }
  z <- outer(
    colnames(x),
    colnames(y),
    Vectorize(function(i, j, ...) {
      FUN(x[[i]], y[[j]], ...)
    },
    vectorize.args = c("i", "j")),
    ...
  )
  dimnames(z) <- list(colnames(x), colnames(y))
  z
}

#' Vectorized computation of correlation
#'
#' @param x a data frame
#' @param y an optional second data frame
#' @param ... additional arguments to cor.test
#'
#' @noRd
compute_corr <- function(x, y = NULL, ...) {
  FUN <- function(x, y, ...) stats::cor.test(x, y, ...)[["estimate"]]
  if (missing(y)) {
    y <- t(x)
  }
  z <- outer(
    colnames(x),
    colnames(y),
    Vectorize(function(i, j, ...) {
      FUN(x[[i]], y[[j]], ...)
    },
    vectorize.args = c("i", "j")),
    ...
  )
  dimnames(z) <- list(colnames(x), colnames(y))
  z
}

#' Create data frame for a drivers plot
#'
#' @param df main data frame
#' @param pcres raw PCA results
#' @param max_pc maximum number of PCs to keep. Default value is 8
#' @param adjust adjust p-values? Default `TRUE` includes column `q` in results
#'
#' @noRd
#'
pca_drivers_df <- function(df, pcres, max_pc = 8, adjust = TRUE) {
  pc_df <- as.data.frame(pcres$x)

  var_explained <- round(pcres$sdev / sum(pcres$sdev) * 100, 2)
  # Fast computation of p values for columns of different dataframes
  corr <- as.data.frame(compute_corr(df, pc_df))
  colnames(corr) <- paste0(colnames(pc_df),
                           " \n(", as.character(var_explained), "%)")

  pvalues <- as.data.frame(compute_pvalues(df, pc_df))
  colnames(pvalues) <- paste0(colnames(pc_df),
                              " \n(", as.character(var_explained), "%)")
  # Select first max_pc components
  corr <- corr[, 1:min(max_pc, ncol(pc_df))]
  corr$Variable <- as.factor(rownames(corr))

  pvalues <- pvalues[, 1:min(max_pc, ncol(pc_df))]
  pvalues$Variable <- as.factor(rownames(pvalues))

  corr_long <- tidyr::pivot_longer(corr, -.data$Variable,
                            names_to = "PC", values_to = "Association")
  pvalues_long <- tidyr::pivot_longer(pvalues, -.data$Variable,
                            names_to = "PC")
  pvalues_long$Association <- corr_long$Association ^ 2
  pvalues_long$Significant <-
    ifelse(pvalues_long$value <= 0.05, TRUE, FALSE )
  pvalues_long$p <- -log10(pvalues_long$value)
  if (adjust)
    pvalues_long$q <- -log10(stats::p.adjust(pvalues_long$value, method = "fdr"))
  pvalues_long
}

