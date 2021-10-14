test_that("cluster heatmaps", {
  expect_error(cluster_heatmaps(scaled_df, clusters, 2, c("red", "blue")), NA)
  expect_error(cluster_heatmaps(scaled_df, clusters, 2, c("red", "blue"),
                                scaled_unselected_df), NA)
  # Should work with clusters, not cluster labels
  expect_error(cluster_heatmaps(scaled_df, cluster_labels, 2, c("red", "blue"),
                                scaled_unselected_df))
  # Won't be pretty but it will work if number of clusters
  # does not match number of colors
  expect_error(cluster_heatmaps(scaled_df, clusters, 3, c("red", "blue")), NA)
})

test_that("correlation heatmap", {
  # Shouldn't work with mixed data
  expect_error(correlation_heatmap(iris))
  # Should work only with numeric data
  expect_error(correlation_heatmap(iris_numeric), NA)
})
