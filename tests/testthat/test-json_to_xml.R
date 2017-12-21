context("json_to_xml")

library(magrittr)
hf205 <- system.file("extdata/hf205.xml", package="emld")
ex <- system.file("extdata/example.xml", package="emld")

test_that("we can round trip and validate hf205", {

  hf205 %>%
    xml_to_json() %>%
    json_to_xml("hf205.xml")

  EML::eml_validate("hf205.xml")

  expect_true(EML::eml_validate("hf205.xml"))
  unlink("hf205.xml")

})
test_that("we can round trip and validate ex", {
  ex %>%
    xml_to_json() %>%
    json_to_xml("ex.xml")
  expect_true(EML::eml_validate("ex.xml"))
  unlink("ex.xml")
})




