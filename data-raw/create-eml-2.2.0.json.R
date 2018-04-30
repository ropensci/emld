library(tidyverse)
library(glue)
library(xml2)
library(jsonlite)
xsd_files <- list.files("inst/xsd/eml-2.2.0", full.names = TRUE)

# test case
#xsd <- xsd_files[[4]]


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


expand_series <- function(nodeset){
  ## Recursively expand choice and sequence into elements=
  choice <- xml_select(nodeset, "choice")
  if(length(choice) > 0){
    tmp <- expand_series(xml_children(nodeset[[choice]]))
    nodeset <- list_insert(nodeset, tmp, choice)
  }
  sequence <- xml_select(nodeset, "sequence")
  if(length(sequence) > 0){
    tmp <- expand_series(xml_children(nodeset[[sequence]]))
    nodeset <- list_insert(nodeset, tmp, sequence)
  }
  class(nodeset) <- "xml_nodeset"
  nodeset
}


## For each member of a nodeset which contains all xs:elements in an XML XSD file
## Extract all of it's xs:attribute and xs:element members
get_slots <- function(nodeset, xml, nodename = "element", typelist = NULL, grouplist = NULL){
  who <- map_chr(nodeset, xml_attr, "name")

  def <- map(nodeset, function(node){
    #docstring <- xml_children(xml_child(xml_child(node, "xs:annotation"), "xs:appinfo"))

    name <- xml_attr(node, "name")
    type <- type_magic(node)

    ## Attributes of the node
    first <- xml_find_first(xml,
                            glue::glue("//xs:{nodename}[@name='{name}']//xs:attribute"))
    attribute_nodes <- c(list(first), xml_siblings(first))
    class(attribute_nodes) <- "xml_nodeset"
    attribs <- as.character(na.omit(map_chr(attribute_nodes, xml_attr, "name")))
    attribs <- map_chr(attribs, ~ paste0("#", .x)) # has len 0 when attribs has len 0

    ## Get all the xs:elements OR xs:group which are descendants. order matters!
    ## Note! xpath always works on full doc, even if given a node.
    first <- xml_find_first(xml,
      glue::glue("//xs:{nodename}[@name='{name}']//xs:element|//xs:{nodename}[@name='{name}']//xs:group"))
    nodeset<- c(list(first), xml_siblings(first))
    class(nodeset) <- "xml_nodeset"
    element_nodes <- expand_series(nodeset)

    elements <-
      unlist(map(element_nodes, function(n){
      out <- xml_attr(n, "name")
      if(is.na(out)){ ## only xs:group don't have names(?)
        out <- drop_prefix(xml_attr(n, "ref"))
#        if(!is.na(out)){
#          out <- grouplist[[out]]
#        }
      }
      out
      }))
    elements <- as.character(na.omit(elements))

    if(length(type)>0){
      type <- typelist[[type]]
    }

    c(attribs, elements, type)
  })
  setNames(def, who)
}



typelist <- map(xsd_files, function(xsd){
  xml <- read_xml(xsd)
  complex_types <- xml_find_all(xml, "//xs:complexType[@name]")
  get_slots(complex_types, xml, "complexType")
})  %>% unlist(FALSE)



grouplist <- map(xsd_files, function(xsd){
  xml <- read_xml(xsd)
  groups <- xml_find_all(xml, "//xs:group[@name]")
  get_slots(groups, xml, "group", typelist)
})  %>% unlist(FALSE)



map(xsd_files, function(xsd){
  xml <- read_xml(xsd)
  named_elements <- xml_find_all(xml, "//xs:element[@name]")
  get_slots(named_elements, xml, "element", typelist, NULL)
})  %>%
  unlist(FALSE) %>%
  ## FIXME Drop duplicate keys
  write_json("data-raw/eml-2.2.0.json", pretty=TRUE)
