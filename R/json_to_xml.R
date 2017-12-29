#' json_to_xml
#'
#' @param x JSON-LD representation of eml, as json, character, or list object.
#' @param file output filename. If NULL (default), will return xml_document
#' @param ... additional arguments to xml2::write_xml
#' @export
json_to_xml <- function(x, file = NULL, ...){ UseMethod("json_to_xml")}

#' @export
json_to_xml.character <- function(x, file = NULL, ...){
  if(file.exists(x)){  ## Read from file
    x <- jsonlite::read_json(x)
  } else { ## Read from string
    x <- jsonlite::fromJSON(x, simplifyVector = FALSE)
  }
  json_to_xml(x, file, ...)
}

#' @export
json_to_xml.json <- function(x, file = NULL, ...){
  x <- jsonlite::fromJSON(x, simplifyVector = FALSE)
  json_to_xml(x, file, ...)
}


#' @export
#' @importFrom methods is
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom xml2 xml_root xml_set_attr write_xml
json_to_xml.list <- function(x, file = NULL, ...){
  # eml_list <- frame.list(x)
  y <- x
  y[["@context"]] <- NULL
  xml <- as_eml_document(y)
  xml <- context_namespaces(x[["@context"]], xml)
  xml2::xml_set_namespace(xml2::xml_find_first(xml2::xml_root(xml), "."), "eml")
  xml <- xml2::as_xml_document(xml)

  if(!is.null(file))
    xml2::write_xml(xml, file, ...)
  else
    xml
}
