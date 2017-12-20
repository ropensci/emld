context("xml_to_json")

system.file("extdata/hf205.xml", package="emld") %>%
  xml_to_json("ex.json")

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

