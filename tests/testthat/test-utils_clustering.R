test_that("compute dmat tests", {
  # Including all columns shouldn't work
  expect_error(compute_dmat(iris))
  # Either way of subsetting should work
  expect_error(compute_dmat(iris[, "Sepal.Length"]), NA)
  expect_error(compute_dmat(iris, subset_cols = c("Sepal.Length")), NA)
  # Invalid column shouldn't work
  expect_error(compute_dmat(iris, subset_cols = "petal"))
  # Partial matching works
  expect_error(compute_dmat(iris_numeric, "euclid"), NA)
  expect_error(compute_dmat(iris_numeric, "Euclidean"), NA)
  expect_error(compute_dmat(iris_numeric, "cosine"), NA)
  expect_error(compute_dmat(iris_numeric, "mahalanobis"), NA)
  expect_error(compute_dmat(iris_numeric, "binary"), NA)
  # Invalid metric shouldn't work
  expect_error(compute_dmat(iris_numeric, "gibberish"))
  # Test scaling (any string works)
  expect_error(compute_dmat(iris_numeric, apply_scaling = TRUE), NA)
})

test_that("compute clusters tests", {

  # Expected functionality
  expect_error(compute_clusters(dmat, "complete"), NA)
  # Invalid linkage method
  expect_error(compute_clusters(dmat, "none"))
  # Wrong object
  expect_error(compute_clusters(iris_numeric, "complete"))
})

test_that("cut clusters test", {
  # Missing k
  expect_error(cut_clusters(clusters))
  # K = 0
  expect_error(cut_clusters(clusters, 0))
  #  k = 2 should work
  expect_error(cut_clusters(clusters, 2), NA)
  # Wrong object
  expect_error(cut_clusters(dmat, 2))
})

test_that("compute metric tests", {
  # Missing metric name
  expect_error(compute_metric(dmat, clusters))
  # Invalid metric name
  expect_error(compute_metric(dmat, clusters, "runn"))
  # Invalid max_k
  expect_error(
    compute_metric(dmat, clusters, "dunn", max_k = 0))
  expect_error(
    compute_metric(dmat, clusters, "dunn", max_k = 1))
  expect_error(
    compute_metric(dmat, clusters, "dunn", max_k = 2))
  # This should work
  expect_error(
    compute_metric(dmat, clusters, "dunn", max_k = 3), NA)
  # This shouldn't work - invalid input
  expect_error(compute_metric(scale_data(iris_numeric), clusters, "dunn", max_k = 3))
})

test_that("compute gap stat tests", {
  # This should work
  expect_error(compute_gapstat(scale_data(iris_numeric), clusters), NA)
  # Invalid max_k
  expect_error(compute_gapstat(scale_data(iris_numeric), clusters, max_k = 1))
  # Invalid B
  expect_error(compute_gapstat(scale_data(iris_numeric), clusters, 0))
  # Invalid input
  expect_error(compute_gapstat(dmat, clusters))
})

test_that("optimal score", {
  expect_error(optimal_score(dunn_res$score, method = "gibberish"))
  expect_equal(optimal_score(dunn_res$score, method = "firstmax"), 3)
  expect_equal(optimal_score(dunn_res$score, method = "firstmin"), 1)
  expect_equal(optimal_score(dunn_res$score, method = "globalmin"), 1)
  expect_equal(optimal_score(dunn_res$score, method = "globalmax"), 13)
  expect_equal(optimal_score(ch_res$score, method = "firstmax"), 1)
  expect_equal(optimal_score(ch_res$score, method = "globalmax"), 1)
  expect_equal(optimal_score(ch_res$score, method = "firstmin"), 5)
  expect_equal(optimal_score(ch_res$score, method = "globalmin"), 10)
})

test_that("cluster annotation tests", {
  # This should work
  expect_error(annotate_clusters(iris_numeric, cluster_labels), NA)
  # Invalid inputs
  expect_error(annotate_clusters(scaled_df, cluster_labels))
  expect_error(annotate_clusters(iris_numeric, clusters))
  # Valid clusters
  expect_error(annotate_clusters(iris_numeric, cluster_labels,
                                 selected_clusters = c(1,2)), NA)
  expect_error(annotate_clusters(iris_numeric, cluster_labels,
                                 selected_clusters = 2), NA)
  expect_error(annotate_clusters(iris_numeric, cluster_labels, FALSE,
                                 selected_clusters = 2), NA)
  # Invalid clusters
  expect_error(annotate_clusters(iris_numeric, cluster_labels,
                                 selected_clusters = 3))
})


