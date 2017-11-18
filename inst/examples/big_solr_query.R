library("httr")
library("jsonlite")
library("jsonld")
library("tidyverse")
source(system.file("examples/solr_functions.R", package="emljson"))

## See: https://cn.dataone.org/cn/v2/query/solr

numFound <-
  GET(paste0("https://knb.ecoinformatics.org/knb/d1/mn/v2/query/solr",
             "?q=-obsoletedBy:*&fl=identifier&rows=1")) %>%
  content() %>%
  xml2::xml_find_first("//result") %>%
  xml2::xml_attr("numFound") %>%
  as.integer()
message(paste(numFound, "records found"))
rows <- 1000
n_calls <- numFound %/% rows

query <-
  paste0(
    "https://knb.ecoinformatics.org/knb/d1/mn/v2/query/solr?",
    "q=*:*&fl=identifier,",
    "northBoundCoord,eastBoundCoord,southBoundCoord,westBoundCoord,",
    "namedLocation,geohash_9",
    "scientificName,kingdom,phylum,class,order,family,genus,species,",
    "beginDate,endDate,originator",
    "&wt=json",
    "&-obsoletedBy:*",
    "&start=", rows*(0:n_calls), "&rows=", rows)

df <- map_df(query, solr_df)

