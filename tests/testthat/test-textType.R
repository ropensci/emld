testthat::context("textType")

library(xml2)

me <- list(individualName = list(givenName = "Carl", surName = "Boettiger"))
eml <- list(dataset = list(
  title = "dataset title",
  contact = me,
  creator = me),
  system = "doi",
  packageId = "10.xxx")

ex.xml <- tempfile("ex", fileext = ".xml")
text.xml <- tempfile("text", fileext = ".xml")
text.json <- tempfile("text", fileext = ".json")


testthat::test_that("We have created a minimal, valid EML file", {
  as_xml(eml, ex.xml)
  testthat::expect_true( eml_validate(ex.xml) )
  unlink(ex.xml)
})


testthat::test_that("We can add abstract as text string", {
  eml$dataset$abstract <- "This is a short abstract for this dataset."
  as_xml(eml, ex.xml)
  testthat::expect_true( eml_validate(ex.xml) )
})

testthat::test_that("We can add abstract with multiple paragraphs", {
  eml$dataset$abstract <-
    list(para = list(
      "This is a short abstract for this dataset.",
      "This is the second paragraph"))
  as_xml(eml, ex.xml)
  testthat::expect_true( eml_validate(ex.xml) )
})


testthat::test_that("We can round-trip text test file", {
  f <- system.file(file.path("tests", eml_version()),
                   "eml-text.xml", package = "emld")
  text <- as_emld(f)

  as_xml(text, text.xml, "text", "txt") # Note custom root & ns
  testthat::expect_true(eml_validate(text.xml) )

  ## same number of XML elements before and after
  start <- length(unlist(as_list(read_xml(f)), recursive = TRUE))
  end <- length(unlist(as_list(read_xml(text.xml)), recursive = TRUE))
  testthat::expect_equal(start, end)


  as_json(text, text.json)
})


## FIXME TextType needs to be able to handle the crazyiness that is `eml-literature.xml`:

testthat::test_that("we can handle crazy mixed text types", {
  xml <- "
  <abstract>this is the abstract without TextType elements.
  <para>This is the abstract's first paragraph.</para>
  <para>This is the abstract's second paragraph. With
  <emphasis>emphasis</emphasis>.</para>
  </abstract>
  "
  x <- xml2::read_xml(xml)
  emld <- as_emld(x)
  y <- as_xml(emld, root = "abstract", ns = "", schemaLocation = NULL)
  testthat::expect_identical(xml_text(xml_find_first(xml_root(y), "//emphasis")), "emphasis")
})

testthat::test_that("internationalized text values", {

  xml <- '
  <dataset>
  <title xml:lang="es">
    Histórico Cocinera base de datos para el quelpo gigante (Macrocystis pyrifera) de la biomasa en California y México.
    <value xml:lang="en">Historical Kelp Database for giant kelp (Macrocystis pyrifera) biomass in California and Mexico.</value>
  </title>
  </dataset>
  '
  x <- xml2::read_xml(xml)
  emld <- as_emld(x)
  y <- as_xml(emld, root = "dataset", ns = "", schemaLocation = NULL)
  testthat::expect_identical(xml_attrs(xml_find_first(xml_root(y), "//title")), c(lang="es"))
  testthat::expect_identical(xml_attrs(xml_find_first(xml_root(y), "//value")), c(lang="en"))

})
