testthat::context("base methods")

## These tests show round trips with base jsonld and xml2 methods
## independent of the methods built into this package. The tests show
## that even these require a little care to work successfully

library(jsonld)
library(jsonlite)
library(magrittr)
library(xml2)


test_that("we can roundtrip JSON-LD expansion and compaction", {


  ex <- system.file("extdata/hf205.json", package = "emld")

  x <- jsonlite::read_json(ex)
  json_context <- toJSON(x[["@context"]], auto_unbox = TRUE)
  roundtrip <-
    jsonld_expand(ex) %>%
    jsonld_compact(json_context)

  A <- unlist(read_json(ex))
  B <- unlist(fromJSON(roundtrip,simplifyVector = FALSE))
  testthat::expect_equivalent(length(A),
                              length(B))

})

