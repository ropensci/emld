context("xml_to_json")
library(magrittr)
library(xml2)
library(jsonlite)


hf205 <- system.file("extdata/hf205.xml", package="emld")
ex <- system.file("extdata/example.xml", package="emld")


test_that("we can convert example.xml into complete JSON-LD", {

  json <- xml_to_json(ex)

  X <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  expect_true("dataset" %in% names(X))

  ## count elements
  expect_length(unlist(X), 14)
})




test_that("we can convert hf205.xml into JSON-LD", {

  f <- system.file("extdata/hf205.xml", package="emld")
  xml_to_json(f, "ex.json")
  expect_true(file.exists("ex.json"))
  unlink("ex.json")
})

test_that(
  "We can parse an EML <url> element with an attribute into JSON",
  {
    x <- xml_to_json(
'<url name="Ecophysiology">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf168</url>')

    expect_is(x, "json")
  })


test_that("we can parse repeated name elements", {
  x <- xml_to_json(
  '<additionalLinks>
    <url name="Ecophysiology">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf168</url>
    <url name="Effects of Prey">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf109</url>
  </additionalLinks>')

  expect_is(x, "json")
  })

