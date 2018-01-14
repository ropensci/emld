testthat::context("textType")



me <- list(individualName = list(givenName = "Carl", surName = "Boettiger"))
eml <- list(dataset = list(
  title = "dataset title",
  contact = me,
  creator = me),
  "#system" = "doi",
  "#packageId" = "10.xxx")

testthat::test_that("We have created a minimal, valid EML file", {
  as_xml(eml, "ex.xml")
  testthat::expect_true( EML::eml_validate("ex.xml") )
  unlink("ex.xml")
})


testthat::test_that("We can add abstract as text string", {
  eml$dataset$abstract <- "This is a short abstract for this dataset."
  as_xml(eml, "ex.xml")
  testthat::expect_true( EML::eml_validate("ex.xml") )
  unlink("ex.xml")
})

testthat::test_that("We can add abstract with multiple paragraphs", {
  eml$dataset$abstract <-
    list(para = list(
      "This is a short abstract for this dataset.",
      "This is the second paragraph"))
  as_xml(eml, "ex.xml")
  testthat::expect_true( EML::eml_validate("ex.xml") )
  unlink("ex.xml")
})


testthat::test_that("We can round-trip text test file", {
  f <- system.file("tests/eml-text.xml", package="emld")
  text <- as_emld(f)
  as_xml(text, "text.xml", "text", "txt") # Note custom root & ns
  testthat::expect_true(EML::eml_validate("text.xml") )

  unlink("text.xml")

  as_json(text, "text.json")
  unlink("text.json")
})
