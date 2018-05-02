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


element_nodes <- map(xsd_files, function(xsd){
   xml <- read_xml(xsd)

   ## fixme get elements with refs instead of name

   groups <- xml_find_all(xml, "//xs:element[@name]")
   ## Element declares a type:
   types <- drop_prefix(map_chr(groups, xml_attr, "type"))
   elements <- unname(type_nodes[types])
   missing_type <- map_lgl(elements, is.null)

   ## Element contains a type, which we recurse
   defs <- map(groups, xsd_recursion, group_nodes)
   missing_def <- map_lgl(defs, ~length(.x)< 1)
   defs[!missing_def] <-  map(defs[!missing_def],
                              ~xsd_recursion(.x, group_nodes))
   elements[missing_type] <- defs[missing_type]

   who <- map_chr(groups, xml_attr, "name")
  setNames(elements, who)
}) %>% unlist(FALSE)  %>% purrr::compact()



## FIXME Some things are missed:  (These create NULLS in the data)
map(element_nodes, xml_name) %>% unlist() %>% table()
## lots of unnamed elements too, (on data-valued nodes)
# map(element_nodes, xml_attr, "name") %>% map_int(~ sum(is.na(.x)))

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


out <-
map(element_nodes, function(nodeset){
    names <- map_chr(nodeset, xml_attr_or, "name", "ref")
    drop <- map_lgl(names, is.na)
    names <- names[!drop]
    type <- map_lgl(nodeset[!drop], function(n) xml_name(n) == "attribute")
    names[type] <- map_chr(names[type], ~ paste0("#", .x))
    names
  })

## "#id" -> "@id"
out <- map(out, ~gsub("^#id$", "@id", .x))


## Drop duplicates
who <- unique(names(out))
out[who] %>%
  ## FIXME Drop duplicate keys
  write_json("data-raw/eml-2.2.0.json", pretty=TRUE)


eml_db <- list("eml-2.1.1" = jsonlite::read_json("data-raw/eml-2.1.1.json"),
               "eml-2.2.0" = jsonlite::read_json("data-raw/eml-2.2.0.json"))
devtools::use_data(eml_db, overwrite = TRUE, internal = TRUE)


