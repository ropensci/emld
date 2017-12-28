
#' @importFrom yaml as.yaml
#' @importFrom jsonlite toJSON
#' @export
print.emld <- function(x, ...){
  style <- getOption("emld_print", "yaml")
  #cat("EML as an emld object:\n\n\n")
  switch(style,
         "yaml" = cat(yaml::as.yaml(x)),
         "json" = cat(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE))
  )
}

#' as_xml
#'
#' @param x an emld object
#' @param file optional path to write out to file.
#'   Otherwise, defaults to NULL and will return an xml_document object.
#'
#' @export
as_xml <- function(x, file=NULL){ UseMethod("as_xml") }

#' @export
as_xml.emld <- function(x, file=NULL){
  json_to_xml(x, file = file)
}


#' as_json
#'
#' @param x an emld object
#' @param file optional path to write out to file.
#'   Otherwise, defaults to NULL and will return a json object.
#' @export
as_json <- function(x, file=NULL){ UseMethod("as_json") }

#' @export
as_json.emld <- function(x, file=NULL){
  if(is.null(file)){
    jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)
  } else {
    jsonlite::write_json(x, file, pretty = TRUE, auto_unbox = TRUE)
  }
}
