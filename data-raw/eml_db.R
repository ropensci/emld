## FIXME: Consider generating a version that returns a data.frame for each class:
## with columns as property, required, type, & description (indicating if it takes a text/data value or another node)
## Would be constructed by parsing the XSD again directly...



library(readr)
library(stringr)
library(magrittr)
library(devtools)



## FIXME: does not get all class names!

#class_defs <- read_lines("../../ropensci/EML/R/classes.R")
#matches <- str_match(class_defs, "setClass\\(\'(\\w+)\'.*") %>% na.omit()
#classes <- matches[,2]
#real_classes <- classes[ - grep("ListOf\\w+",classes)]



library(EML)
real_classes <- readr::read_lines("data-raw/eml_classes.txt")
drop_bad <-  function(tag){
  bad <- c(".Data", "slot_order", "schemaLocation", "lang", "value", "references", "character")
  drop <- as.integer(na.omit(match(bad, order)))
  if(length(drop) > 0){
    order <- order[-drop]
  }
  order <- gsub("^id$", "@id", order)
  attrs <- c("scope","system","authSystem","function",
             "typeSystem","phonetype","keywordType","enforced",
             "exclusive","directory", "packageId")
  for(attr in attrs){
    order <- gsub(paste0("^(", attr, ")$"), "#\\1", order)
  }

  order
}

eml_db <- lapply(real_classes, function(tag){
  order <- methods::slotNames(tag)
  drop_bad(tag)

})
names(eml_db) <- real_classes


## Replace any capital-first (i.e. Element Group) slot with it's slotNames,
## e.g. BoundsGroup -> bounds
eml_db_ <- lapply(real_classes, function(tag){
  order <- eml_db[[tag]]

  i <- grep("^[A-Z].*", order)
  if(length(i) == 0){
    return(order)
  } else if(length(i) > 1){
    message(paste(order[i]))
    return(order)
  } else {
    if(i>1){
      start <- seq.int(1,i-1)
    } else {
      start <- integer(0)
    }
    if(i < length(order)){
      end <- seq.int(i+1, length(order))
    } else {
      end <- integer(0)
    }
    expanded <- eml_db[[order[i]]]
    order <- c(order[start], expanded, order[end])
    order
  }
})
names(eml_db_) <- real_classes

eml_db <- eml_db_



######### Mannually create stmml  #######
stmml_db <-
list(
  'appinfo' = character(0),
  'documentation' = character(0),
  'annotation' = c("appinfo", "documentation"),
  'description' = character(0),
  'dimension' = c("#name", "power"),
  'unitList' = c('#href', 'unit', 'unitType'),
  'unitType' = c('dimension', '@id', '#name'),
  'unit' = c('description', 'annotation', '@id', '#abbreviation', '#name',
             '#parentSI', '#unitType', '#multiplierToSI', '#constantToSI')
)

eml_db$unit <- stmml_db$unit
eml_db <- c(eml_db, stmml_db)


devtools::use_data(eml_db, overwrite = TRUE, internal = TRUE)
