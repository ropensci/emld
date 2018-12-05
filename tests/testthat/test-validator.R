

testthat::test_that("we can use emld validator", {
  f <- system.file("extdata", "example.xml", package = "emld")
  testthat::expect_true(eml_validate(f))
  testthat::expect_true(eml_validate(xml2::read_xml(f)))
  my_eml <- as_emld(f)
  testthat::expect_true(eml_validate(my_eml))

  testthat::expect_error(eml_validate(f, schema = "notafile"))


})
