

# Show slots for the "creator" object
template("creator")
## Recursive list
template("creator", recursive = TRUE)

## template knows about internal classes too
template("ResponsibleParty")

template_file("creator", "creator.yml", recursive = TRUE)
unlink("creator.yml")
## test serializing to XML fragment doc
#f <- "tests/testthat/creator.yml"
#creator <- yaml::read_yaml(f)
#doc <- xml2::xml_new_document()
#add_node(creator, doc, "creator")

## Write element into complete doc
#eml <- parse_eml(system.file("inst/extdata/example.xml", package="emld"))
#eml$eml$dataset$creator <- creator
#doc <- as_eml_document.list(eml)



# this is a terribly bad idea:
#template_file("eml", "eml.yaml", "yaml", recursive = TRUE)
