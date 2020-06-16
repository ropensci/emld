

testthat::test_that("we can use emld validator", {
  f <- system.file("extdata", "example.xml", package = "emld")
  testthat::expect_true(eml_validate(f))
  testthat::expect_true(eml_validate(xml2::read_xml(f)))
  my_eml <- as_emld(f)
  testthat::expect_true(eml_validate(my_eml))

  testthat::expect_error(eml_validate(f, schema = "notafile"))


})


testthat::test_that("validation uses the right unitDictionary", {
  f <- system.file("tests", "eml-2.1.1", "invalid", "eml-2.1.1-invalidunit.xml", package = "emld")
  testthat::expect_warning(eml_validate(f), "not recognized")

  f <- system.file("tests", "eml-2.2.0", "eml-2.2.0-milligramPerLiter.xml", package = "emld")
  testthat::expect_true(eml_validate(f))
})
