

testthat::test_that("we can use emld validator", {
  f <- system.file("extdata", "example.xml", package = "emld")
  testthat::expect_true(eml_validate(f))
  testthat::expect_true(eml_validate(xml2::read_xml(f)))
  testthat::expect_error(eml_validate(f, schema = "notafile"))
})
