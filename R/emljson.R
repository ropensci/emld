#' xml_to_json
#'
#' Convert an EML file into a json file/string
#'
#' @param x path to an EML file
#' @param out output filename; optional, if ommitted will return JSON string
#' @return json string or json output file
#' @export
#' @importFrom jsonlite toJSON write_json
#' @examples
#'
#' ex <- system.file("extdata/example.xml", package = "emljson")
#' xml_to_json(ex)
#'
xml_to_json <- function(x, out = NULL){
  json <- parse_eml(x)
  if(is.null(out)){
    jsonlite::toJSON(json, pretty = TRUE, auto_unbox = TRUE)
  } else {
    jsonlite::write_json(json, out, pretty = TRUE, auto_unbox = TRUE)
  }
}

#' parse_eml
#'
#' Parse an EML file into an R list object
#' @inheritParams xml_to_json
#' @importFrom xml2 read_xml xml_find_all xml_remove as_list
#'
#' @export
parse_eml <- function(x){
  xml <- xml2::read_xml(x)

  ## Drop comment nodes.
  xml2::xml_remove(xml2::xml_find_all(xml, "//comment()"))

  ## Main transform, map XML to list using a modification of the xml2::as_list convention
  ## See as_list.R
  json <- as_list(xml)

  ## Set up the JSON-LD context
  json <- c(list("@context" = list("@vocab" = "http://ecoinformatics.org/")), json)
  xmlns <- grepl("^xmlns", names(json))
  json <- json[!xmlns]   # just drop namespaces for now, should be appended to context

  list(nexml = json)
}
