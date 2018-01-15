

testthat::test_that("we can use emld validator", {
  f <- system.file("extdata", "example.xml", package = "emld")
  testthat::expect_true(eml_validate(f))
})
