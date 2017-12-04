context("test-bonferroni.R")

test_that("bonferroni works", {
  result <- bonferroni(c(0.1,0.2,0.3,0.4))
  expect_equal(result$adjPValues, c(0.4, 0.8, 1.0, 1.0))
})
