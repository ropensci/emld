testthat::context("Test round trip validation")

guess_ns <- function(file){
  x <- xml_root(read_xml(file))
  root <- xml_name(x)
  ns <- xml_ns(x)
  i <- grep(strsplit(xml_attr(x, "schemaLocation"), "\\s+")[[1]][1], ns)
  ns <- names(ns[i])
  list(root=root, ns=ns)
}


test_roundtrip <- function(f){
  testthat::test_that(paste(
    "testing that", basename(f), "can roundtrip & validate"),
  {

  ## guess root and ns for sub-modules
  ns <- guess_ns(f)

  out <- basename(f)
  emld <- as_emld(f)
  elements_at_start <- length(unlist(emld, recursive = TRUE))
  as_xml(emld, out, ns$root, ns$ns)

  #print(EML::eml_validate(out))

  ## Make sure output xml is still valid
  testthat::expect_true(EML::eml_validate(out))

  ## Make sure we have the same number of elements as we started with
  elements_at_end <- length(unlist(as_emld(out), recursive = TRUE))
  testthat::expect_equal(elements_at_start, elements_at_end)
  unlink(out)
  })
}
## Test everything in test suite:
suite <- list.files(system.file("tests", package="emld"), full.names = TRUE)
#lapply(suite, test_roundtrip)


## Test all citation-* examples:
suite <- list.files(system.file("tests", package="emld"),
                    pattern="citation", full.names = TRUE)
lapply(suite, test_roundtrip)



## These even fail to parse with as_emld
## All error with: 'names' attribute [1] must be the same length as the vector [0]
#test_roundtrip("inst/tests/eml-datasetWithAccessUnitsLiteralLayout.xml")
#test_roundtrip("inst/tests/eml-datasetWithCitation.xml")
#test_roundtrip("inst/tests/eml-datasetWithUnits.xml")             ## 'names' must be same length!

#test_roundtrip("inst/tests/eml-datasetWithNonwordCharacters.xml") ## loses elements


## Modular subsets
test_roundtrip(system.file("tests/eml.xml", package="emld"))
test_roundtrip(system.file("tests/eml-access.xml", package="emld"))
test_roundtrip(system.file("tests/eml-attribute.xml", package="emld"))
test_roundtrip(system.file("tests/eml-citationWithContact.xml", package="emld"))
test_roundtrip(system.file("tests/eml-citationWithContactReference.xml", package="emld"))
test_roundtrip(system.file("tests/eml-dataset.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetMultipleDistribution.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWhitespacePatterns.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAttributelevelMethods.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccess.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccessOverride.xml", package="emld"))
test_roundtrip(system.file("tests/eml-dataTable.xml", package="emld"))
test_roundtrip(system.file("tests/eml-entity.xml", package="emld"))
#test_roundtrip(system.file("tests/eml-i18n.xml", package="emld"))
#test_roundtrip(system.file("tests/eml-inline.xml", package="emld"))
#test_roundtrip(system.file("tests/eml-literature.xml", package="emld"))
#test_roundtrip(system.file("tests/eml-literatureInPress.xml", package="emld"))
test_roundtrip(system.file("tests/eml-method.xml", package="emld"))
test_roundtrip(system.file("tests/eml-offline.xml", package="emld"))
test_roundtrip(system.file("tests/eml-party.xml", package="emld"))
test_roundtrip(system.file("tests/eml-physical-inline-cdatasection.xml", package="emld"))
test_roundtrip(system.file("tests/eml-physical-inline.xml", package="emld"))
#test_roundtrip(system.file("tests/eml-physical.xml", package="emld"))
test_roundtrip(system.file("tests/eml-project.xml", package="emld"))
test_roundtrip(system.file("tests/eml-protocol.xml", package="emld"))
#test_roundtrip(system.file("tests/eml-sample.xml", package="emld"))
test_roundtrip(system.file("tests/eml-software.xml", package="emld"))
test_roundtrip(system.file("tests/eml-softwareWithAcessDistribution.xml", package="emld"))
test_roundtrip(system.file("tests/eml-spatialVector.xml", package="emld"))
test_roundtrip(system.file("tests/eml-storedProcedure.xml", package="emld"))
test_roundtrip(system.file("tests/eml-text.xml", package="emld"))
#test_roundtrip(system.file("tests/eml-unitDictionary.xml", package="emld"))
test_roundtrip(system.file("tests/eml-view.xml", package="emld"))



