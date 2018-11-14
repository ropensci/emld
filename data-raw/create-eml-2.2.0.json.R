library(tidyverse)
library(glue)
library(xml2)
library(jsonlite)
library(purrr)

## Sync inst/xsd/eml-2.2.0 first!



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
  nodeset <- xml_drop(nodeset, "annotation")

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
    # extension (can't have restriction of a complexContent?)
    extension <- xml_child(nodeset[[complexContent]])
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

## convert nodesets to a simple list naming child elements and attributes
nodes_to_listdefs <- function(element_nodes){
  map(element_nodes, function(nodeset){
    names <- map_chr(nodeset, xml_attr_or, "name", "ref")
    drop <- map_lgl(names, is.na)
    names <- names[!drop]
    type <- map_lgl(nodeset[!drop], function(n) xml_name(n) == "attribute")
    names[type] <- map_chr(names[type], ~ paste0("#", .x))
    names
  })
}


## convert nodesets to a simple list naming child elements and attributes
nodes_to_json_defs <- function(element_nodes){
  map(element_nodes, function(nodeset){
    names <- map_chr(nodeset, xml_attr_or, "name", "ref")
    drop <- map_lgl(names, is.na)
    names <- names[!drop]
    type <- map_lgl(nodeset[!drop], function(n) xml_name(n) == "attribute")
    names[type] <- map_chr(names[type], ~ paste0("#", .x))
    names
  })
}

classes_from_schema <-
function(xsd_files){
  ## Get all named groups
  group_nodes <- get_group_nodes(xsd_files)
  ## all named complexTypes
  type_nodes <- get_type_nodes(xsd_files, group_nodes)
    ## Manual recursion for types based on types based on types
    type_nodes <- get_type_nodes(xsd_files, group_nodes, type_nodes)
    type_nodes <- get_type_nodes(xsd_files, group_nodes, type_nodes)
    type_nodes <- get_type_nodes(xsd_files, group_nodes, type_nodes)

  element_nodes <- get_element_nodes(xsd_files, group_nodes, type_nodes)

  ## FIXME Some things are missed:  (These create NULLS in the data)
  ## map(element_nodes, xml_name) %>% unlist() %>% table()

  out <- nodes_to_listdefs(element_nodes)

  ## "#id" -> "@id"
  out <- map(out, ~gsub("^#id$", "@id", .x))
  ## Drop duplicates
  ## FIXME union duplicates, don't drop them!
  who <- unique(names(out))
  out[who]
}



####### HERE WE GO

eml_def <-
list.files("inst/xsd/eml-2.2.0", full.names = TRUE) %>%
  classes_from_schema()


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


## Fix duplicates / namespace collisions
eml_def$unit <- stmml$unit
eml_def$parameter <- c("name", "domainDescription", "required", "repeats")

c(eml_def, stmml) %>%
  write_json("data-raw/eml-2.2.0.json", pretty=TRUE)

eml_db <- list("eml-2.1.1" = jsonlite::read_json("data-raw/eml-2.1.1.json"),
               "eml-2.2.0" = jsonlite::read_json("data-raw/eml-2.2.0.json"))
devtools::use_data(eml_db, overwrite = TRUE, internal = TRUE)


