## EXPERIMENTAL ONLY

library(xml2)
library(purrr)

## List all Classes

## class, description, inherits, inherits2, inherits3,

## class, property, type, description, accepted_types
#
## Define properties of every class
## property definitions



### Get all xs:complexType

xsd <- "inst/xsd/eml-2.2.0/eml-attribute.xsd"

xml <- xml2::read_xml(xsd)

## Types.  Just get the docs and stick stuff back together later??
complex <- xml_find_all(xml, "//xs:complexType[@name]") %>% xml_attr("name")
simple <- xml_find_all(xml, "//xs:simpleType[@name]")  %>% xml_attr("name")
groups <- xml_find_all(xml, "//xs:group[@name]") %>% xml_attr("name")


xml_find_all(xml, "//xs:complexType[@name]/xs:annotation") %>% xml_children() %>% xml_children() %>% xml_text(TRUE)


xml_find_all(xml, "//xs:complexType[@name='NonNumericDomainType']//xs:element[@name]") %>% xml_attr("name")
xml_find_all(xml, "//xs:complexType[@name='NonNumericDomainType']/child::*/xs:element[@name]") %>% xml_attr("name")
xml_find_all(xml, "//xs:complexType[@name='NonNumericDomainType']/child::*/child::*/xs:element[@name]") %>% xml_attr("name")

xml_find_all(xml, "//xs:complexType[@name='NonNumericDomainType']/child::*/(xs:choice | xs:sequence)")


xml_find_all(xml, "//xs:complexType[@name='NonNumericDomainType']") %>% xml_children()  %>% xml_children()

xml_find_all(xml, "//xs:complexType[@name='NonNumericDomainType']/child::*/child::*/child::*")


named_elements <- xml_find_all(xml, "//xs:element[@name]") %>% xml_attr("name")

