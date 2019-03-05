# Validate that every `standardUnit` is a standardUnit
# Validate that any `customUnit` is defined in the customUnits
validate_units <- function(eml,
                         encoding = "UTF-8") {

  doc <- generalized_parser(eml, encoding = encoding)
  ns <- xml_ns(doc)
  standard_units <- unique(xml2::xml_text(
    xml2::xml_find_all(doc, "//standardUnit", ns = ns),
    trim = TRUE))
  custom_units <-  unique(xml2::xml_text(
    xml2::xml_find_all(doc, "//customUnit", ns = ns),
    trim = TRUE))

  standard <- xml2::read_xml(system.file("tests",
    getOption("emld_db"),
    "eml-unitDictionary.xml",
    package = "emld"
  ))

  standard_defs <- xml_attr(
    xml2::xml_find_all(standard,
                       "//*[local-name()='unitList']/*[local-name()='unit']"),
    "id"
  )

  custom_defs <- xml2::xml_attr(xml2::xml_find_all(
    doc,
    "//*[local-name()='unitList']/*[local-name()='unit']"
  ), "id")

  error_log <- character()
  if(!all(standard_units %in% standard_defs))
    error_log <- c(error_log, "some standardUnit elements are not recognized.")

  if(!all(custom_units %in% custom_defs))
    error_log <- c(error_log, "not all 'custom units are defined.")

  # If no validity errors are found above or by the parser, then the document is valid
  if(length(error_log) == 0)
    result <- TRUE
  else {
    warning(paste("Document is invalid. Found the following errors:\n",
                  paste(error_log, collapse = "\n")))
    result <- FALSE
  }
  attr(result, "errors") <- error_log
  result
}



