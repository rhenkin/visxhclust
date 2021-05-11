test_that("scale data", {
  expect_error(scale_data(iris))
  expect_error(scale_data(iris_numeric), NA)
  expect_error(scale_data(iris_numeric, "z-scores"), NA)
  expect_error(scale_data(iris_numeric, "robust"), NA)
})
