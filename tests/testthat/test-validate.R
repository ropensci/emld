testthat::context("Test round trip validation")

library(xml2)

guess_ns <- function(file){
  x <- xml2::xml_root(xml2::read_xml(file))
  root <- xml2::xml_name(x)
  ns <- xml2::xml_ns(x)
  i <- grep(strsplit(xml2::xml_attr(x, "schemaLocation"), "\\s+")[[1]][1], ns)
  ns <- names(ns[i])[[1]]
  list(root=root, ns=ns)
}


test_roundtrip <- function(f, schema = NULL, check_lengths = TRUE){
  testthat::test_that(paste(
    "testing that", basename(f), "can roundtrip & validate"),
  {

  ## guess root and ns for sub-modules
  ns <- guess_ns(f)

  out <- basename(f)
  emld <- as_emld(f)
  as_xml(emld, out, ns$root, ns$ns)

  ## Make sure output xml is still valid
  testthat::expect_true( eml_validate(out, schema = schema) )

  ## Make sure we have the same number & names of elements as we started with
  if(check_lengths){
    elements_at_end <- sort(names(unlist(as_emld(out), recursive = TRUE)))
    elements_at_start <- sort(names(unlist(emld, recursive = TRUE)))
    testthat::expect_equal(elements_at_start, elements_at_end)
  }
  unlink(out)
  })
}

options("emld_db" = "eml-2.1.1")

## Test all citation-* examples:
suite <- list.files(system.file(
  file.path("tests", getOption("emld_db", "eml-2.2.0")), package="emld"),
                    pattern="citation", full.names = TRUE)
out <- lapply(suite, purrr::safely(test_roundtrip))
failed <- purrr::map_lgl(purrr::map(out, "result"), is.null)
bib <- unlist(purrr::map(out, "error"))

suite <- list.files(system.file(
  file.path("tests", getOption("emld_db", "eml-2.2.0")), package="emld"),
                    pattern="eml-", full.names = TRUE)
out <- lapply(suite, purrr::safely(test_roundtrip))
other <- unlist(purrr::map(out, "error"))






testthat::test_that("unitDictionary", {
  f <- system.file(file.path("tests", getOption("emld_db", "2.2.0"),
                             "eml-unitDictionary.xml"), package="emld")
  schema <- system.file("xsd/eml-2.1.1/stmml.xsd", package = "emld")
  out <- basename(f)
  emld <- as_emld(f)
  elements_at_start <- names(unlist(emld, recursive = TRUE))

  ## Applies JSON-LD framing.  Because vocab is stmml, framing drops all elements!
  ## So do this manually
  # as_xml(emld, out, "unitList", "stmml")

  context <- emld[["@context"]]
  emld[["@type"]] <- NULL
  emld[["@context"]] <- NULL
  xml <- emld:::as_eml_document(emld, "unitList", "stmml")
  xml <- emld:::context_namespaces(context, xml)
  root <- xml_root(xml)
  #xml_set_name(root, "stmml:unitList", ns = xml_ns(xml))
  xml2::xml_set_attr(root, "xmlns", gsub("/$", "", "http://www.xml-cml.org/schema/stmml-1.1"))

  write_xml(xml, out)

  eml_validate(out, schema = schema)

  testthat::expect_true(eml_validate(out, schema = schema))

  elements_at_end <- names(unlist(as_emld(out), recursive = TRUE))
  testthat::expect_equal(elements_at_start, elements_at_end)

  unlink(out)
})

