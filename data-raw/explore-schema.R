```{r}
library(tidyverse)
library(glue)
library(xml2)
library(jsonlite)
xsd_files <- list.files("../inst/xsd/eml-2.2.0", full.names = TRUE)
```

Root contains: 
 - `attribute`, `attributeGroup`, `complexType`, `element`, `group`, `simpleType`
 - `annotation`, `import` 
 
`element`: contains nothing (but has a type declaration), or has no type declaration and 
contains one of:
  - simpleType
  - complexType
 (plus every element has an annotation child element)

`complexType`: contains
  - `annotation`
  - `attribute`
  - `group`
  - `choice`
  - `sequence`
  - `complexContent`
  - `simpleContent`

`choice`, `sequence`, `group` all contain any number of:
- `choice`, `sequence`, `group`, `element`  

(note that `sequence` also appears in `restriction` & `extension`)
(groups only contain sequences)

- `attribute` occassionally uses `simpleType`.  `attributeGroup` has no child elements(?)
- Both `simpleContent` and `complexContent` contains one of either `xs:extension` or `xs:restriction`
- `simpleType` contains one of `restriction`, `union`, or `list`

```{r}
map(xsd_files, function(xsd){
  read_xml(xsd) %>% xml_find_all("//xs:element") %>% map(xml_children) %>% map(xml_name) })
map(xsd_files, function(xsd){
  read_xml(xsd) %>% xml_find_all("//xs:extension") %>% map(xml_children) %>% map(xml_name) }) %>% unlist() %>% table()
map(xsd_files, function(xsd){
  read_xml(xsd) %>% xml_find_all("//xs:sequence/xs:any") }) %>% compact() -> a


map(xsd_files, function(xsd){
    read_xml(xsd) %>% xml_root() %>% xml_children() %>% map_chr(xml_name)
}) %>% unlist() %>% table()

terms <-
  map(xsd_files, function(xsd){
    read_xml(xsd) %>% xml_find_all("//xs:element") %>% xml_children() %>% map_chr(xml_name)
  }) %>% unlist()
types <- terms[!terms=="annotation"]
table(types)


map(xsd_files, function(xsd){
  read_xml(xsd) %>% xml_find_all("//xs:attribute") %>% xml_children() %>% map_chr(xml_name)
}) %>% unlist() %>% table()

map(xsd_files, function(xsd){
  read_xml(xsd) %>% xml_find_all("//xs:complexType") %>% xml_children() %>% map_chr(xml_name)
}) %>% unlist() %>% table()
map(xsd_files, function(xsd){
  read_xml(xsd) %>% xml_find_all("//xs:simpleType") %>% xml_children() %>% map_chr(xml_name)
}) %>% unlist() %>% table()
map(xsd_files, function(xsd){
  read_xml(xsd) %>% xml_find_all("//xs:choice") %>% xml_children() %>% map_chr(xml_name)
}) %>% unlist() %>% table()
map(xsd_files, function(xsd){
  read_xml(xsd) %>% xml_find_all("//xs:sequence") %>% xml_children() %>% map_chr(xml_name)
}) %>% unlist() %>% table()
```
