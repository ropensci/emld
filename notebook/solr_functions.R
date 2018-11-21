
## get JSON!
# json_queries <- paste0(query, "&wt=json")

 q = "https://knb.ecoinformatics.org/knb/d1/mn/v2/query/solr?q=genus:*&fl=identifier,northBoundCoord,eastBoundCoord,southBoundCoord,westBoundCoord,namedLocation,scientificName,kingdom,phylum,class,order,family,genus,species,beginDate,endDate&-obsoletedBy:*&start=0&rows=1000&wt=json"
# solr_df(q)

solr_df <- function(q){
  message(paste(q))
  resp <- GET(q)
  if(!grepl("json", headers(resp)$`content-type`))
    return(tibble())
  json <- content(resp, as = "parsed", type = "application/json")
  docs_to_nquads(json$response$docs, append = TRUE)
}

docs_to_nquads <- function(docs, f = tempfile(), ...){
  list("@context" = list("@vocab"="http://ecoinformatics.org/eml-2.1.1/",
                         "identifier" = "@id"),
       "@graph" = docs) %>%
    toJSON(pretty=TRUE, auto_unbox = TRUE) %>%
    jsonld_to_rdf() %>%
    write_lines(path = f, ...)
  tabularize(f)
}


tabularize <- function(file){

  ## Express type string inside "" so we can parse
  read_lines(file) %>% str_replace("\"\\^\\^(.*)>", "\\^\\^\\1\"") %>% write_lines(file)

  df <- readr::read_delim(file, delim = " ", col_names = FALSE, col_types = "cccc")
  names(df) <- c("id", "property", "value", "about")
  df %>%
    mutate(property = gsub("<http://ecoinformatics.org/eml-2.1.1/(.*)>", "\\1", property)) %>%
    separate(value, c("value", "type"), sep="\\^\\^<", fill="right")

}

spread_nquads <- function(file){

  tabularize(file) %>%
    select(id, property, value) %>%
    group_by(id, property) %>%
    mutate(key = row_number()) %>%
    spread(property, value) %>%
    select(-key) %>%
    fill(-id) %>%  ## Fill in NA based on known properties for id, needs group_by id
    ungroup() %>%
    select(-identifier) %>% View()
}





## Attempt a json & unnest strategy
pure_json <- function(docs){
  toJSON(auto_unbox = TRUE) %>%
    fromJSON() %>% as_tibble() -> df

  ## cannot unnest appropriately across multiple areas
  df$scientificName[[1]] <- ""
  df$species[[1]] <- ""
  unnest(df, scientificName) %>% distinct() %>% View()
}


