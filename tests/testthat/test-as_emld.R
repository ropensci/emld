library(magrittr)
library(xml2)
library(jsonlite)
library(testthat)
testthat::context("as_* methods")

hf205 <- system.file("extdata/hf205.xml", package="emld")
ex <- system.file("extdata/example.xml", package="emld")


test_that("we can convert example.xml into complete JSON-LD", {
  X <- as_emld(ex)
  expect_true("dataset" %in% names(X))
  expect_length(unlist(X), 17)
})

test_that("we can round-trip and validate a simple example", {
  emld <- as_emld(ex)
  as_xml(emld, "test.xml")
  expect_true(eml_validate("test.xml"))


  elements_at_end <- sort(names(unlist(as_emld("test.xml"), recursive = TRUE)))
  elements_at_start <- sort(names(unlist(emld, recursive = TRUE)))
  testthat::expect_equal(elements_at_start, elements_at_end)

  unlink("test.xml")
})



test_that("we can convert hf205.xml into JSON-LD", {
  f <- system.file("extdata/hf205.xml", package="emld")
  as_json(as_emld(f), "ex.json")
  expect_true(file.exists("ex.json"))
  unlink("ex.json")
})

test_that(
  "We can parse an EML <url> element with an attribute into JSON",
  {
    xml <- xml2::read_xml(
'<url name="Ecophysiology">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf168</url>')
    emld <- as_emld(xml)
    expect_is(emld, "emld")
    json <- as_json(emld)
    expect_is(json, "json")
  })


test_that("we can parse repeated name elements", {
  xml <-
    xml2::read_xml(
  '<additionalLinks>
    <url name="Ecophysiology">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf168</url>
    <url name="Effects of Prey">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf109</url>
  </additionalLinks>')

  emld <- as_emld(xml)
  expect_is(emld, "emld")
  json <- as_json(emld)
  expect_is(json, "json")
  })


test_that("raw files can be parsed", {
  emld <- as_emld(ex)

  ex_char <- paste(readLines(ex), collapse = "\n")
  # potentially test for char formats here. currently doesn't work (converts to list of 1 instead of 6)
  # emld_char <- as_emld(ex_char)
  # expect_equal(emld, emld_char)

  ex_raw <- charToRaw(ex_char)
  emld_raw <- as_emld(ex_raw, "xml")
  expect_equal(emld, emld_raw)
  expect_warning(emld_raw <- as_emld(ex_raw), "assuming raw vector is xml")
})
