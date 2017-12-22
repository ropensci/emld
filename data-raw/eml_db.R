library(readr)
library(stringr)
library(magrittr)
library(devtools)
library(EML)

class_defs <- read_lines("../../ropensci/EML/R/classes.R")
#stmml_classes <- read_lines("../../ropensci/EML/R/classes-stmml.R")

matches <- str_match(class_defs, "setClass\\(\'(\\w+)\'.*") %>% na.omit()
classes <- matches[,2]
real_classes <- classes[ - grep("ListOf\\w+",classes)]


eml_db <- lapply(real_classes, function(tag){
  order <- methods::slotNames(tag)
  drop <- grep("(.Data|slot_order)", order)
  if(length(drop) > 0){
    order <- order[-drop]
  }
  # attr <- grep("(schemaLocation|lang|id|scope|system|authSystem)", order)
  order
  })
names(eml_db) <- real_classes

devtools::use_data(eml_db, overwrite = TRUE, internal = TRUE)
