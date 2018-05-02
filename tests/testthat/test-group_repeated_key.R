testthat::context("group repeated key")

test_that("we can group repeated xml keys into json", {
  input <-
    '<node>
  <key>value1</key>
  <key>value2</key>
  <key>value3</key>
  <other>stuff</other>
  </node>'

  output <-
    '{
  "key" = ["value", "value2","value3"],
  "other" = "stuff"
  }'

  ## tests
  in_list <- xml2::as_list(xml2::read_xml(input))
  json <- jsonlite::toJSON(emld:::group_repeated_key(in_list), auto_unbox = TRUE)
  expect_is(json, "json")

})
