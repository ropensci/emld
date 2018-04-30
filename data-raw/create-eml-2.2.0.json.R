library(tidyverse)
library(glue)
library(xml2)
library(jsonlite)
xsd_files <- list.files("inst/xsd/eml-2.2.0", full.names = TRUE)


# test case
#xsd <- xsd_files[[4]]

## Basic helper functions #######
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

xml_drop <- function(nodeset, pattern){
  i <- xml_select(nodeset, pattern)
  if(length(i) == 0)
    return(nodeset)
  else
    return(nodeset[-i])
}

xsd_recursion <- function(node, grouplist = NULL){

  nodeset <- xml_children(node)
  ## keep attributes
  ## keep elements

  ## drop docs
  nodeset <- xml_drop(nodeset, "annotation")

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

  if(!is.null(nodeset))
    class(nodeset) <- "xml_nodeset"
  nodeset
}

group_nodes <- map(xsd_files, function(xsd){
   xml <- read_xml(xsd)
   groups <- xml_find_all(xml, "//xs:group[@name]")
   def <- map(groups, xsd_recursion)
   who <- map_chr(groups, xml_attr, "name")
  setNames(def, who)

}) %>% unlist(FALSE)  %>% purrr::compact()

type_nodes <- map(xsd_files, function(xsd){
   xml <- read_xml(xsd)
   groups <- xml_find_all(xml, "//xs:complexType[@name]")
   def <- map(groups, xsd_recursion, group_nodes)
   who <- map_chr(groups, xml_attr, "name")
  setNames(def, who)

}) %>% unlist(FALSE)  %>% purrr::compact()



element_nodes <-
map(xsd_files, function(xsd){
  xml <- read_xml(xsd)
  named_elements <- xml_find_all(xml, "//xs:element[@name]")
  get_nodes(named_elements, xml, "element", type_nodes, group_nodes)
}) %>% unlist(FALSE)  %>% purrr::compact()









type_magic <- function(node){
  type <- xml_attr(node, "type")

  if(is.na(type)){
    type <- xml_child(node, "xs:complexType") %>%
      xml_child("xs:complexContent") %>%
      xml_child("xs:extension") %>% # could be restriction?
      xml_attr("base")
  }

  if(is.na(type)){
    type <- xml_child(node, "xs:simpleType") %>%
      xml_child("xs:restriction") %>%
      xml_attr("base")
  }

  ## drop trivial types (drop all lower-case types)
  type <- switch(type,
                 "xs:string" = character(),
                 "xs:decimal" = numeric(),
                 "xs:float" = numeric(),
                 "xs:integer" = integer(),
                 "xs:int" = integer(),
                 "xs:time" = character(),
                 "xs:boolean" = logical(),
                 "xs:anyURI" = character(),
                 "res:i18nNonEmptyStringType" = character(),
                 "res:NonEmptyStringType" = character(),
                 "res:i18nString" = character(),
                 "i18nNonEmptyStringType" = character(),
                 "NonEmptyStringType" = character(),
                 "i18nString" = character(),
                 type)

  if(length(type)>0){
    if(is.na(type)){
      type <- character(0L)
    }
  }
  type <- drop_prefix(type)
  type
}

## FIXME Some things are missed:  (These create NULLS in the data)
map(element_nodes, xml_name) %>% unlist() %>% table()
## lots of unnamed elements too, (on data-valued nodes)
map(element_nodes, xml_attr, "name") %>% map_int(~ sum(is.na(.x)))



out <-
map(element_nodes, function(nodeset){
  names <- map_chr(nodeset, xml_attr, "name")
  type <- map_lgl(nodeset, function(n) xml_name(n) == "attribute")
  names[type] <- map_chr(names[type], ~ paste0("#", .x))
  as.character(na.omit(names))
})

out %>%
  ## FIXME Drop duplicate keys
  write_json("data-raw/eml-2.2.0.json", pretty=TRUE)


