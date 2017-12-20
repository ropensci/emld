context("xml_to_json")
library(magrittr)
library(xml2)
library(jsonlite)

devtools::load_all()

hf205 <- system.file("extdata/hf205.xml", package="emld")
ex <- system.file("extdata/example.xml", package="emld")

hf205 %>%
  xml_to_json() %>%
  fromJSON(simplifyVector = FALSE) %>%
  write_json("hf205.json", auto_unbox = TRUE, pretty = TRUE)

hf205 %>%
  xml_to_json() %>%
  json_to_xml("hf205.xml")
EML::eml_validate(hf205)

ex %>%
  xml_to_json() %>%
  json_to_xml("ex.xml")
EML::eml_validate("ex.xml")

f <- system.file("extdata/example.xml", package="emld")
as_jsonlist.xml_node(xml)

## FIXME we're missing elements here!
test_that("we can convert example.xml into complete JSON-LD", {

  f <- system.file("extdata/example.xml", package="emld")
  json <- xml_to_json(f)

  X <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_named(X, c("@context", "eml"))

  expect_true("dataset" %in% names(X[["eml"]]))

  ## count elements
  unlist(X)
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

