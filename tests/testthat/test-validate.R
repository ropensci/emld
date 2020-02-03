testthat::context("Test round trip validation")

library(xml2)

test_roundtrip <- function(f, schema = NULL, check_lengths = TRUE){
  testthat::test_that(paste(
    "testing that", basename(f), "can roundtrip & validate"),
  {

  ## guess root and ns for sub-modules
  doc <- xml2::read_xml(f)
  ns <- find_real_root_name(doc)

  out <- tempfile(basename(f), fileext = ".xml")

  emld <- as_emld(f)
  as_xml(emld, out, ns$name, ns$prefix)

  ## Make sure output xml is still valid unless it's supposed to be invalid
  if (!grepl("invalid", f, ignore.case = TRUE)) {
    testthat::expect_true( eml_validate(out, schema = schema) )

  } else {
    testthat::expect_false( eml_validate(out, schema = schema) )
  }

  ## Make sure we have the same number & names of elements as we started with
  if(check_lengths){
    elements_at_end <- sort(names(unlist(as_emld(out), recursive = TRUE)))
    elements_at_start <- sort(names(unlist(emld, recursive = TRUE)))

    # Filter out schemaLocation since it can be absent at start and added at end
    elements_at_start <- Filter(
      function (e) { e != "schemaLocation"},
      elements_at_start)

    elements_at_end <- Filter(
      function (e) { e != "schemaLocation"},
      elements_at_end)

    testthat::expect_equal(elements_at_start, elements_at_end)
  }
  })
}

## Enforce testing on 2.1.1 for this
options("emld_db" = "eml-2.1.1")

## Test all citation-* examples:
suite <- list.files(system.file(
  file.path("tests", eml_version()), package="emld"),
                    pattern="citation", full.names = TRUE)
lapply(suite, test_roundtrip)

suite <- list.files(system.file(
  file.path("tests", eml_version()), package="emld"),
                    pattern="eml-", full.names = TRUE)
drop <- basename(suite) %in% c("eml-datasetWithNonwordCharacters.xml",
                       "eml-i18n.xml",
                       "eml-literature.xml",
                       "eml-literatureInPress.xml",
                       "eml-unitDictionary.xml",
                       "eml-units.xml")
test_suite <- suite[!drop]
lapply(test_suite, test_roundtrip)

## These four skip the length-check
partial_test <- basename(suite) %in%
  c("eml-datasetWithNonwordCharacters.xml",
    "eml-i18n.xml", "eml-literature.xml", "eml-literatureInPress.xml")
lapply(suite[partial_test], test_roundtrip, check_lengths = FALSE)

## Add testing for 2.2.0 suite separately here.

## Helper methods for debugging
#' out <- lapply(suite, purrr::safely(test_roundtrip))
#' failed <- purrr::map_lgl(purrr::map(out, "result"), is.null)
#' suite[failed]
#' msg <- unlist(purrr::map(out, "error"))


## 35 & 36 have units we check separately since our validator can't automatically find schema



testthat::test_that("unitDictionary", {
  f <- system.file(file.path("tests", getOption("emld_db", "2.2.0"),
                             "eml-unitDictionary.xml"), package="emld")
  schema <- system.file("xsd/eml-2.1.1/stmml.xsd", package = "emld")
  out <- tempfile(basename(f), fileext = ".xml")
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

})











##########

## Enforce testing on 2.2.0 for this
options("emld_db" = "eml-2.2.0")

## Test all citation-* examples:
suite <- list.files(system.file(
  file.path("tests", eml_version()), package="emld"),
  pattern="citation", full.names = TRUE)
drop <- basename(suite) %in% c("citation-sbclter-bibliography.284.xml",
                               "citation-sbclter-bibliography.289.xml",
                               "eml-citationWithContact.xml")
test_suite <- suite[!drop]
lapply(test_suite, test_roundtrip)

suite <- list.files(system.file(
  file.path("tests", eml_version()), package="emld"),
  pattern="eml-", full.names = TRUE)
drop <- basename(suite) %in% c("eml-datasetWithNonwordCharacters.xml",
                               "eml-i18n.xml",
                               "eml-literature.xml",
                               "eml-literatureInPress.xml",
                               "eml-unitDictionary.xml",
                               "eml-units.xml",
                               "eml-citationWithContact.xml")
test_suite <- suite[!drop]
lapply(test_suite, test_roundtrip)

## These ones skip the length-check
partial_test <- basename(suite) %in%
  c("eml-datasetWithNonwordCharacters.xml",
    "eml-i18n.xml",
    "eml-literature.xml",
    "eml-literatureInPress.xml",
    "eml-citationWithContact.xml")
lapply(suite[partial_test], test_roundtrip, check_lengths = FALSE)


######## Test that new schema validation logic that doesn't use schemaLocation
######## still validates
test_that("we can still validate XML docs without schemaLocation set on them", {
  testthat::expect_true(
    eml_validate(
      system.file(
        file.path("tests",
                  "eml-2.2.0",
                  "eml-datasetNoSchemaLocation.xml"),
        package="emld")
    ))

  testthat::expect_false(
    eml_validate(
      system.file(
        file.path("tests",
                  "eml-2.2.0",
                  "eml-datasetNoSchemaLocationInvalid.xml"),
        package="emld")
    ))
})


######### Helper methods
test_that("get_root_ns works for a variety of cases", {
  testthat::expect_equal(
    find_real_root_name(
      xml2::read_xml("<dictionary
                        xmlns=\"http://www.xml-cml.org/schema/stmml-1.1\">
                     </dictionary>")),
    list(prefix = "d1", name = "dictionary"))

  testthat::expect_equal(
    find_real_root_name(
      xml2::read_xml("<eml:eml
                        xmlns:eml=\"https://eml.ecoinformatics.org/eml-2.2.0\">
                     </eml:eml>")),
    list(prefix = "eml", name = "eml"))

  testthat::expect_equal(
    find_real_root_name(
      xml2::read_xml("<cit:citation
                        xmlns:cit=\"https://eml.ecoinformatics.org/literature-2.2.0\">
                     </cit:citation>")),
    list(prefix = "cit", name = "citation"))
})

test_that("get_root_ns works for a variety of cases", {
  testthat::expect_equal(
    guess_root_schema(
      xml2::read_xml("<dictionary
                        xmlns=\"http://www.xml-cml.org/schema/stmml-1.1\">
                     </dictionary>")),
    list(module = "stmml",
         version = "1.1",
         namespace = "http://www.xml-cml.org/schema/stmml-1.1"))

  testthat::expect_equal(
    guess_root_schema(
      xml2::read_xml("<eml:eml
                        xmlns:eml=\"https://eml.ecoinformatics.org/eml-2.2.0\">
                     </eml:eml>")),
    list(module = "eml",
         version = "2.2.0",
         namespace = "https://eml.ecoinformatics.org/eml-2.2.0"))

  testthat::expect_equal(
    guess_root_schema(
      xml2::read_xml("<cit:citation
                        xmlns:cit=\"https://eml.ecoinformatics.org/literature-2.2.0\">
                     </cit:citation>")),
    list(module = "literature",
         version = "2.2.0",
         namespace = "https://eml.ecoinformatics.org/literature-2.2.0"))
})
