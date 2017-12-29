library(magrittr)
library(xml2)
library(jsonlite)
library(testthat)

context("xml_to_json")

hf205 <- system.file("extdata/hf205.xml", package="emld")
ex <- system.file("extdata/example.xml", package="emld")


test_that("we can convert example.xml into complete JSON-LD", {
  X <- as_emld(ex)
  expect_true("dataset" %in% names(X))
  expect_length(unlist(X), 15)
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

