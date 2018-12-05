## 2018-11-28
## This is still the most current official script for creating:
##
## - eml-2.1.1.json, eml-2.1.1.tsv
## - eml-2.2.0.json, eml-2.2.0.tsv

library(tidyverse)
library(glue)
library(xml2)
library(jsonlite)


# test case
#xsd <- xsd_files[[4]]

####### Super basic helper functions #######
drop_prefix <- function(x) gsub("^\\w+:", "", x)
xml_select <- function(nodeset, pattern = ".*", fn = xml_name,  ...){
  names <- map_chr(nodeset, fn, ...)
  match <- grepl(pattern, names)
  which(match)
}
# replace the i'th element of list A with list B in flat list
list_insert <- function(A, B, i){
  if(length(i)>1)
    stop("i > 1 case not handled")
  n <- length(A)
  if(n < 2)
    return(B)
  if(i < n)
    return(c( A[seq(1,i) - 1], B, A[seq(i+1, n)]))
  if(i == n)
    return(c( A[seq(1,i) - 1], B))
}

## drop nodes from nodeset that match select pattern
xml_drop <- function(nodeset, pattern, ...){
  i <- xml_select(nodeset, pattern, ...)
  if(length(i) == 0)
    return(nodeset)
  else
    return(nodeset[-i])
}

## Select either attribute A or B robustly.
xml_attr_or <- function(x, A, B){
  a <- xml_attr(x, A)
  b <- xml_attr(x, B)
  out <- as.character(na.omit(c(a,b)))
  if(length(out) > 1)
    out <- out[[1]]
  if(length(out) == 0)
    out <- as.character(NA)
  out
}

#########################



## Primary parsing function: a recursive descent
xsd_recursion <- function(node, grouplist = NULL, typelist=NULL){
  nodeset <- xml_children(node)
  ## drop docs
  #nodeset <- xml_drop(nodeset, "annotation")
  docs <- xml_select(nodeset, "annotation")
  if(length(docs) > 0){
     tmp <- xml_children(xml_child(nodeset[[docs]]))
     tmp2 <- tmp[[ xml_select(tmp, "description") ]]
    nodeset <- list_insert(nodeset, tmp, docs)
  }

  simpleContent <- xml_select(nodeset, "simpleContent")
  if(length(simpleContent) > 0){
    tmp <- xml_children( xml_child(nodeset[[simpleContent]]) )
    nodeset <- list_insert(nodeset, tmp, simpleContent)
  }

  ## Recursively expand choice and sequence into elements=
  choice <- xml_select(nodeset, "choice")
  if(length(choice) > 0){
    tmp <- xsd_recursion(nodeset[[choice]], grouplist)
    nodeset <- list_insert(nodeset, tmp, choice)
  }
  sequence <- xml_select(nodeset, "sequence")
  if(length(sequence) > 0){
    tmp <- xsd_recursion(nodeset[[sequence]], grouplist)
    nodeset <- list_insert(nodeset, tmp, sequence)
  }

  ## Replace group reference with group definition
  group <- xml_select(nodeset, "group")
  if(length(group) > 0){
    ref <- drop_prefix(xml_attr(nodeset[[group]], "ref"))
    nodeset <- list_insert(nodeset, grouplist[[ref]], group)
  }

  ## complexContent extension of a complexType
  complexContent <- xml_select(nodeset, "complexContent")
  if(length(complexContent) > 0){
    extension <- xml_child(nodeset[[complexContent]])  # extension (can't have restriction of a complexContent?)
    ## insert the base type
    type <- drop_prefix(xml_attr(extension, "base"))
    nodeset <- list_insert(nodeset, typelist[[type]], complexContent)
    ## add any children of extension too
    tmp <- xsd_recursion(extension)
    nodeset <- c(nodeset, tmp)
    class(nodeset) <- "xml_nodeset"
  }

  if(!is.null(nodeset))
    class(nodeset) <- "xml_nodeset"
  nodeset
}


## Okay, here we go: Strategy is as follows:

# Start by gathering xml_nodesets containing
# (1) all named groups,
# (2) all named complexTypes
# (3) all named elements

get_group_nodes <- function(xsd_files){
  map(xsd_files, function(xsd){
    xml <- read_xml(xsd)
    groups <- xml_find_all(xml, "//xs:group[@name]")
    def <- map(groups, xsd_recursion)
    who <- map_chr(groups, xml_attr, "name")
    setNames(def, who)
  }) %>% unlist(FALSE)  %>% purrr::compact()
}

## All named complexTypes
get_type_nodes <- function(xsd_files, group_nodes, type_nodes = NULL){
  map(xsd_files, function(xsd){
    xml <- read_xml(xsd)
    groups <- xml_find_all(xml, "//xs:complexType[@name]")
    def <- map(groups, xsd_recursion, group_nodes, type_nodes)
    who <- map_chr(groups, xml_attr, "name")
    setNames(def, who)
  }) %>% unlist(FALSE)  %>% purrr::compact()
}


get_element_nodes <- function(xsd_files, group_nodes, type_nodes){
  map(xsd_files, function(xsd){
    xml <- read_xml(xsd)

    named_elements <- xml_find_all(xml, "//xs:element[@name]")

    ## Element declares a type:
    types <- drop_prefix(map_chr(named_elements, xml_attr, "type"))
    elements <- unname(type_nodes[types])
    missing_type <- map_lgl(elements, is.null)

    ## Element contains a type, which we recurse
    defs <- map(named_elements, xsd_recursion, group_nodes)
    missing_def <- map_lgl(defs, ~length(.x)< 1)
    defs[!missing_def] <-  map(defs[!missing_def],
                               ~xsd_recursion(.x, group_nodes, type_nodes))
    elements[missing_type] <- defs[missing_type]

    who <- map_chr(named_elements, xml_attr, "name")
    setNames(elements, who)
  }) %>%
    unlist(FALSE)  %>%
    purrr::compact()
}



get_docs <- function(node, docbit = "details"){
  kids <- xml_children(node)
  i <- xml_select(kids, "annotation")
  if(length(i) > 0){
    docs <- xml_children(xml_children(kids[[i]]))
    j <- xml_select(docs, docbit)
    if(length(j) > 0 ){
      txt <- xml_text( docs[[ j ]] )
      stringr::str_squish(txt)
    } else {
      ""
    }
  } else {
    ""
  }
}


## convert nodesets to a simple list naming child elements and attributes
node_info <- function(element_nodes){

  map_dfr(element_nodes, function(nodeset){
    names <- map_chr(nodeset, xml_attr_or, "name", "ref")
    drop <- map_lgl(names, is.na)
    names <- names[!drop]
    nodeset <- nodeset[!drop]

    summary <- map_chr(nodeset, get_docs, "summary")
    details <- map_chr(nodeset, get_docs, "details")
    attribute <- map_lgl(nodeset, function(n) xml_name(n) == "attribute")
    tibble(names, summary, details, attribute)

  }, .id = "parent")

}



table_from_schema <- function(xsd_files){
  ## Get all named groups
  group_nodes <- get_group_nodes(xsd_files)
  ## get all named complexTypes
  type_nodes <- get_type_nodes(xsd_files, group_nodes)
    ## That doesn't get the recrursive type definitions, so:
    ## Manual recursion for types based on types based on types
    type_nodes <- get_type_nodes(xsd_files, group_nodes, type_nodes)
    type_nodes <- get_type_nodes(xsd_files, group_nodes, type_nodes)
    type_nodes <- get_type_nodes(xsd_files, group_nodes, type_nodes)

  ## Okay, now we possess node definitions for all types and all groups.
  element_nodes <- get_element_nodes(xsd_files, group_nodes, type_nodes)

  dplyr::distinct( node_info(element_nodes) )

  }


xsd_files <- list.files("inst/xsd/eml-2.2.0", pattern=".xsd", full.names = TRUE)
df <- table_from_schema(xsd_files)

readr::write_tsv(df, "data-raw/eml-2.2.0.tsv")



list.files("inst/xsd/eml-2.1.1", pattern=".xsd", full.names = TRUE) %>%
  table_from_schema() %>%
  readr::write_tsv("data-raw/eml-2.1.1.tsv")

###############



stmml <- "inst/xsd/eml-2.2.0/stmml.xsd" %>% classes_from_schema()

## Ugh, manually define stmml type
stmml <-
  list(
        "dimension"= c("#name", "#power"),
        "unitList"= c("#href", "unitType", "unit"),
        "unitType"= c("dimension", "@id", "#name"),
        "unit"= c("standardUnit", "customUnit", "description", "annotation",
                  "@id", "#abbreviation", "#name", "#parentSI", "#unitType",
                  "#multiplierToSI", "#constantToSI")
)
