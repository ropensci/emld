testthat::context("Test round trip validation")

## Slows test suite by about a minute

test_roundtrip <- function(f){
  testthat::test_that(paste(
    "testing that", basename(f), "can roundtrip & validate"),
  {
  out <- basename(f)
  json <- xml_to_json(f)
  json_to_xml(json, out)
  testthat::expect_true(EML::eml_validate(out))
  unlink(out)
  })
}
suite <- list.files(system.file("tests", package="emld"), full.names = TRUE)
#lapply(suite, test_roundtrip)

## Passing subset (most of those of full eml, not fragment type)
test_roundtrip(system.file("tests/eml.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccess.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetMultipleDistribution.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWhitespacePatterns.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAccessOverride.xml", package="emld"))
test_roundtrip(system.file("tests/eml-datasetWithAttributelevelMethods.xml", package="emld"))

## Citation types are all full examples too:
suite <- list.files(system.file("tests", package="emld"), pattern="citation", full.names = TRUE)
lapply(suite, test_roundtrip)


# THESE ARE STILL FAILING:
#test_roundtrip("inst/tests/eml-i18n.xml")
#test_roundtrip("inst/tests/eml-datasetWithAccessUnitsLiteralLayout.xml")
#test_roundtrip("inst/tests/eml-datasetWithCitation.xml")
#test_roundtrip("inst/tests/eml-datasetWithNonwordCharacters.xml")
#test_roundtrip("inst/tests/eml-datasetWithUnits.xml")

## Many other examples simply have wrong namespace since are fragments, not EML objects


