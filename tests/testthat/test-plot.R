test_that("check if boxplots fail", {
  # Basic usage should work
  expect_error(facet_boxplot(longdata_with_labels, "Cluster", "Value",
                             facet_var = "Measurement"), NA)
  expect_error(facet_boxplot(longdata_with_labels, "Cluster", "Value",
                             facet_var = "Measurement", shape = "violin"), NA)
  # Wrong shape should fail
  expect_error(facet_boxplot(longdata_with_labels, "Cluster", "Value",
                             facet_var = "Measurement", shape = "other"))
  expect_error(facet_boxplot(longdata_with_labels, "Cluster", "Value",
                             plot_points = TRUE), NA)
  expect_error(facet_boxplot(longdata_with_labels, "Cluster", "Value",
                             boxplot_colors = c("red", "blue")), NA)
  # Shouldn't work with original column names if data was annotated as long
  expect_error(facet_boxplot(longdata_with_labels, "Cluster", "Sepal.Length"))
  # But should work if it's wide format
  expect_error(
    facet_boxplot(widedata_with_labels, "Cluster", "Sepal.Length"), NA)
  # Should fail with wrong variable name
  expect_error(facet_boxplot(widedata_with_labels, "cluster", "Sepal.Length"))
  expect_error(facet_boxplot(widedata_with_labels, "Cluster", "Sepal.Length",
                             facet_var = "measurement"))

})

test_that("test cluster_boxplots", {
  # Should work
  expect_error(cluster_boxplots(longdata_with_labels), NA)
  # Shouldn't work
  expect_error(cluster_boxplots(widedata_with_labels))
})


test_that("line plot", {
  expect_error(line_plot(dunn_res, "k", "score"), NA)
  expect_error(line_plot(dunn_res, "k", "aa"))
  expect_error(line_plot(dunn_res, "kk", "score"))
  expect_error(line_plot(dunn_res, "k", "score", 10), NA)
  expect_error(line_plot(dunn_res, "k", "score", "5"))
})

test_that("dmat projection", {
  expect_error(dmat_projection(dmat), NA)
  expect_error(dmat_projection(dmat_all), NA)
  expect_error(dmat_projection(dmat,
                               point_colors = rep("red", nrow(iris))), NA)
  expect_error(dmat_projection(dmat,
                               point_palette = grDevices::heat.colors(2)), NA)
  # Wrong usage
  expect_error(dmat_projection(iris_numeric))

})

test_that("plot annotations dist", {
  expect_error(plot_annotation_dist(iris, cluster_labels), NA)
  # Shouldn't work if number of rows doesn't match
  expect_error(plot_annotation_dist(iris[1:10,], cluster_labels))
  # Shouldn't work if asking for clusters that are not present
  expect_error(plot_annotation_dist(iris, cluster_labels, c(3,4)), NA)
})
