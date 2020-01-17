testthat::context("version")

test_that("we can get the namespace for different EML versions", {
  expect_equal(eml_ns("eml-2.0.0"), "eml://eml.ecoinformatics.org/eml-2.0.0")
  expect_equal(eml_ns("eml-2.1.1"), "eml://eml.ecoinformatics.org/eml-2.1.1")
  expect_equal(eml_ns("eml-2.2.0"), "https://eml.ecoinformatics.org/eml-2.2.0")
  expect_equal(eml_ns("eml-2.2.1"), "https://eml.ecoinformatics.org/eml-2.2.1")
})
