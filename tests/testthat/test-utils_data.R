test_that("scale data", {
  expect_error(scale_data(iris))
  expect_error(scale_data(iris_numeric), NA)
  expect_error(scale_data(iris_numeric, TRUE), NA)
  expect_error(scale_data(iris_numeric, FALSE), NA)
})
