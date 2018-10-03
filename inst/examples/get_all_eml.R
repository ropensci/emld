library("httr")
library("jsonlite")
library("jsonld")
library("tidyverse")
library("xml2")
source(system.file("examples/solr_functions.R", package="emld"))

## See: https://cn.dataone.org/cn/v2/query/solr/

eml_solr_query <-
"http://cn.dataone.org/cn/v2/query/solr/?q=formatId:eml*+AND+-obsoletedBy:*"

numFound <-
  GET(eml_solr_query) %>%
  content() %>%
  xml2::xml_find_first("//result") %>%
  xml2::xml_attr("numFound") %>%
  as.integer()
message(paste(numFound, "records found"))
rows <- 1000
n_calls <- numFound %/% rows



query <-
  paste0(eml_solr_query,
    "&fl=identifier,formatId,dataUrl,fileName",
    "&wt=json",
    "&start=", rows*(0:n_calls), "&rows=", rows)

solr_df <- function(q){
  message(paste(q))
  resp <- GET(q)
  if(!grepl("json", headers(resp)$`content-type`))
    return(tibble())
  json <- jsonlite::fromJSON(content(resp, as = "text"))
  json$response$docs
}

df <- map_dfr(query, solr_df)
readr::write_csv(df, "eml-solrs.csv.bz2")

#eml <- df %>%
#  filter(grepl("eml://", formatId)) %>%
#  mutate(fileName = paste0("emls/", basename(identifier), ".xml"))

#base <- "https://cn.dataone.org/cn/v2/resolve/"
#eml2 <- eml %>% mutate(resolve = gsub(base, "", dataUrl))


safe_down <- function(url, dest){
  if(!file.exists(dest))
    safely(download.file(url, dest))
}

dir.create("emls", FALSE)
map2(df$fileName, df$dataUrl, ~safe_down(.y, .x))
