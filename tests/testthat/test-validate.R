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


test_roundtrip <- function(f, schema = NULL, check_lengths=TRUE){
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

## Test all citation-* examples:
suite <- list.files(system.file("tests", package="emld"),
                    pattern="citation", full.names = TRUE)
lapply(suite, test_roundtrip)


## Special or imperfect cases ##



## Modular subsets
test_roundtrip(system.file("tests/eml.xml", package="emld"))
test_roundtrip(system.file("tests/eml-access.xml", package="emld"))
test_roundtrip(system.file("tests/eml-attribute.xml", package="emld"))
test_roundtrip(system.file("tests/eml-citationWithContact.xml", package="emld"))
test_roundtrip(system.file("tests/eml-citationWithContactReference.xml", package="emld"))
test_roundtrip(system.file("tests/eml-dataset.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetMultipleDistribution.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWhitespacePatterns.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccess.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccessOverride.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccessUnitsLiteralLayout.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAttributelevelMethods.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithCitation.xml", package="emld"))
## Framing decides several of the quote chars are literally identical, e.g. " and \"
test_roundtrip(system.file("tests/eml-datasetWithNonwordCharacters.xml", package="emld"),
               check_lengths = FALSE)
test_roundtrip(system.file("tests/eml-datasetWithUnits.xml", package="emld"))
test_roundtrip(system.file("tests/eml-dataTable.xml", package="emld"))
test_roundtrip(system.file("tests/eml-entity.xml", package="emld"))
## Short one element
test_roundtrip(system.file("tests/eml-i18n.xml", package="emld"),
               check_lengths = FALSE)
test_roundtrip(system.file("tests/eml-inline.xml", package="emld"))
## lenghts differ only due to whitespace nodes(?)
test_roundtrip(system.file("tests/eml-literature.xml", package="emld"),
               check_lengths = FALSE)
test_roundtrip(system.file("tests/eml-literatureInPress.xml", package="emld"),
               check_lengths = FALSE)
test_roundtrip(system.file("tests/eml-method.xml", package="emld"))
test_roundtrip(system.file("tests/eml-offline.xml", package="emld"))
test_roundtrip(system.file("tests/eml-party.xml", package="emld"))
test_roundtrip(system.file("tests/eml-physical-inline-cdatasection.xml", package="emld"))
test_roundtrip(system.file("tests/eml-physical-inline.xml", package="emld"))
test_roundtrip(system.file("tests/eml-physical.xml", package="emld"))
test_roundtrip(system.file("tests/eml-project.xml", package="emld"))
test_roundtrip(system.file("tests/eml-protocol.xml", package="emld"))
test_roundtrip(system.file("tests/eml-sample.xml", package="emld"))
test_roundtrip(system.file("tests/eml-software.xml", package="emld"))
test_roundtrip(system.file("tests/eml-softwareWithAcessDistribution.xml", package="emld"))
test_roundtrip(system.file("tests/eml-spatialVector.xml", package="emld"))
test_roundtrip(system.file("tests/eml-storedProcedure.xml", package="emld"))
test_roundtrip(system.file("tests/eml-text.xml", package="emld"))
test_roundtrip(system.file("tests/eml-view.xml", package="emld"))

## Define test without framing for a few special cases.


testthat::test_that("unitDictionary", {
  f <- system.file("tests/eml-unitDictionary.xml", package="emld")
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
  xml <- as_eml_document(emld, "unitList", "stmml")
  xml <- context_namespaces(context, xml)
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

