library(tidyverse)
library(xml2)
library(jsonlite)
xsd_files <- list.files("inst/xsd/eml-2.2.0", full.names = TRUE)

xsd <- xsd_files[[4]]


map(xsd_files[4], function(xsd){


  xml <- read_xml(xsd)
  ## only when complexType appears with names is it being defined, otherwise it's nested
  complex_types <- xml_find_all(xml, "//xs:complexType[@name]")
  who <- map_chr(complex_types, xml_attr, "name")
  ## Get annotation

  ## Get attributes of the complexType

  ## Get elements of the complexType:
  ## all xs:elements which are descendants of said complexType

  def <- map(complex_types, function(node){

    docstring <- xml_child(node, "xs:annotation")

    children <- xml_children(node)
    is_attr <- map_chr(children, xml_name) == "attribute"
    children[is_attr]

    ## parse attribute name, use, default, type for function def.

    name <- xml_name(node)

    ## descend to first xs:element, return it and it's siblings
    ## Note! xpath always works on full doc, even if given a node.
    first <- xml_find_first(xml,
                            glue::glue("//xs:complexType[@name='{name}']//xs:element"))
    out <- c(list(first), xml_siblings(first))
    class(out) <- "xml_nodeset"
    out
  })

  ## xs:group[@ref] means children are defined by children of xs:group[@name]

  out <- map(complex_types, function(node){
      ## find_all isn't the right strategy here, fails to get only nested subtypes
      map_chr(xml_find_all(node, "//xs:element"), xml_attr, "name")
  })
  names(out) <- who
  out



}) %>% toJSON(pretty=TRUE)
