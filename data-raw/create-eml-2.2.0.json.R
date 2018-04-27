library(tidyverse)
library(glue)
library(xml2)
library(jsonlite)
xsd_files <- list.files("inst/xsd/eml-2.2.0", full.names = TRUE)

# test case
#xsd <- xsd_files[[4]]

drop_prefix <- function(x) gsub("^\\w+:", "", x)

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


xml_select <- function(nodeset, pattern = ".*", fn = xml_name,  ...){
  names <- map_chr(nodeset, fn, ...)
  match <- grepl(pattern, names)
  nodeset[match]
}

## FIXME NOT DONE
## get an xs:element and all it's siblings, including xs:group, xs:choice, xs:sequence
## xs:element descendents contained in these latter (and their siblings) should be
## treated as siblings of the first (i.e. they are all properties of the same class)
all_element_siblings <- function(xml, name, nodename="element"){

  first <- xml_find_first(xml,
  glue::glue("//xs:{nodename}[@name='{name}']//xs:element|//xs:{nodename}[@name='{name}']//xs:group"))
  siblings <- xml_siblings(first)

  ## Replace xs:choice with xs:element components
  choice <- xml_select(siblings, "choice")
  sequence <- xml_select(siblings, "sequence")
  element_nodes <- c(list(first), siblings)
  class(element_nodes) <- "xml_nodeset"
  element_nodes
}

get_slots <- function(nodeset, xml, nodename = "element", typelist = NULL){
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
    element_nodes <- c(list(first), xml_siblings(first))
    class(element_nodes) <- "xml_nodeset"

    elements <- map_chr(element_nodes, function(n){
      out <- xml_attr(n, "name")
      if(is.na(out))
        out <- drop_prefix(xml_attr(n, "ref"))
      out
      })
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

map(xsd_files, function(xsd){
  xml <- read_xml(xsd)
  named_elements <- xml_find_all(xml, "//xs:element[@name]")
  get_slots(named_elements, xml, "element", typelist)
})  %>%
  unlist(FALSE) %>%
  ## FIXME Drop duplicate keys
  write_json("data-raw/eml-2.2.0.json", pretty=TRUE)
