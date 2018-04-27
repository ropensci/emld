library(tidyverse)
library(glue)
library(xml2)
library(jsonlite)
xsd_files <- list.files("inst/xsd/eml-2.2.0", full.names = TRUE)


map(xsd_files, function(xsd){

  xml <- read_xml(xsd)
  ## only when complexType appears with names is it being defined, otherwise it's nested
  complex_types <- xml_find_all(xml, "//xs:complexType[@name]")
  who <- map_chr(complex_types, xml_attr, "name")

  ## FIXME: unwrap xs:group.

  ## Consider also parsing: use, default, type for function def.
  def <- map(complex_types, function(node){
    #docstring <- xml_children(xml_child(xml_child(node, "xs:annotation"), "xs:appinfo"))


    ## Attributes of the complexType
    children <- xml_children(node)
    is_attr <- map_chr(children, xml_name) == "attribute"
    attribs <- as.character(na.omit(map_chr(children[is_attr], xml_attr, "name")))
    attribs <- map_chr(attribs, ~ paste0("#", .x)) # has len 0 when attribs has len 0


    ## Get elements of the complexType:
    ## all xs:elements which are descendants of said complexType
    ## Note! xpath always works on full doc, even if given a node.
    name <- xml_attr(node, "name")
    first <- xml_find_first(xml,
                            glue::glue("//xs:complexType[@name='{name}']//xs:element"))
    element_nodes <- c(list(first), xml_siblings(first))
    class(element_nodes) <- "xml_nodeset"


    elements <- map_chr(element_nodes, xml_attr, "name")
    #elements <- purrr::compact(elements)
    elements <- as.character(na.omit(elements))

    c(attribs,elements)
  })

  setNames(def, who)

  ## xs:group[@ref] means children are defined by children of xs:group[@name]

}) %>% unlist(FALSE) %>% write_json("eml-2.2.0.json", pretty=TRUE)
