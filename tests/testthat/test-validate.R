testthat::context("Test round trip validation")

## Slows test suite by about a minute

test_roundtrip <- function(f){
  testthat::test_that(paste(
    "testing that", basename(f), "can roundtrip & validate"),
  {
  out <- basename(f)
  emld <- as_emld(f)
  elements_at_start <- length(unlist(emld, recursive = TRUE))
  as_xml(emld, out)

  ## Make sure output xml is still valid
  testthat::expect_true(EML::eml_validate(out))

  ## Make sure we have the same number of elements as we started with
  elements_at_end <- length(unlist(as_emld(out), recursive = TRUE))
  testthat::expect_equal(elements_at_start, elements_at_end)
  unlink(out)
  })
}
suite <- list.files(system.file("tests", package="emld"), full.names = TRUE)
#lapply(suite, test_roundtrip)

## Passing subset (most of those of full eml, not fragment type)
test_roundtrip(system.file("tests/eml.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetMultipleDistribution.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWhitespacePatterns.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAttributelevelMethods.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccess.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccessOverride.xml", package="emld"))

## Citation types are all full examples too:
suite <- list.files(system.file("tests", package="emld"), pattern="citation", full.names = TRUE)
lapply(suite, test_roundtrip)


# THESE ARE STILL FAILING:

## These fail validation
test_roundtrip("inst/tests/eml-i18n.xml")

## These even fail to parse with as_emld
## All error with: 'names' attribute [1] must be the same length as the vector [0]
test_roundtrip("inst/tests/eml-datasetWithAccessUnitsLiteralLayout.xml")
test_roundtrip("inst/tests/eml-datasetWithCitation.xml")
test_roundtrip("inst/tests/eml-datasetWithUnits.xml")

## loses elements
test_roundtrip("inst/tests/eml-datasetWithNonwordCharacters.xml")


## Remaining examples simply have wrong namespace since are fragments, not EML objects


