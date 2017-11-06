library(magrittr)
library(xml2)
library(jsonlite)
library(emljson)
system.file("extdata/hf205.xml", package="emljson") %>%
  xml_to_json("ex.json")

xml_to_json(
'<url name="Ecophysiology">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf168</url>'
)

emljson::parse_eml(
  '<url name="Ecophysiology">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf168</url>'
)


'<additionalLinks>
  <url name="Ecophysiology">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf168</url>
  <url name="Effects of Prey">http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf109</url>
</additionalLinks>' %>% xml_to_json()

