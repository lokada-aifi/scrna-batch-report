context("running get_median_range")

test_that("get median range output for numeric values with options", {
  expect_equal(object = get_median_range(1:10/3, digits_round = 2, verbose = F),"1.83 (0.33-3.33)")
  expect_equal(object = get_median_range(1001:1010, digits_round = 0, comma_separate = T, verbose = F),"1,006 (1,001-1,010)")
})

test_that("get median range output for missing values", {
  expect_equal(object = get_median_range(c(1:10,NA,NA), digits_round = 1, add_missing = F, verbose = F), "5.5 (1.0-10.0)")
  expect_equal(object = get_median_range(c(1:10,NA,NA), digits_round = 1, add_missing = T, verbose = F), "5.5 (1.0-10.0) [2]")
})

test_that("get median range output for non-finite values", {
  expect_equal(object = get_median_range(c(1:10,Inf), digits_round = 1, add_missing = F, verbose = F), "6.0 (1.0-Inf)")
  expect_equal(object = get_median_range(c(1:10,-Inf,NA), digits_round = 1, add_missing = T, verbose = F), "5.0 (-Inf-10.0) [1]")
})



