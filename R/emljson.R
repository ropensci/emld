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
#' ex <- system.file("extdata/example.xml", package = "emld")
#' xml_to_json(ex)
#'
xml_to_json <- function(x, out = NULL){
  json <- parse_eml(x, add_context = TRUE)
  class(json) <- "list"
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
#' @param add_context should we add a default context element?
#' @importFrom xml2 read_xml xml_find_all xml_remove
#'
#' @export
parse_eml <- function(x, add_context = FALSE){
  xml <- xml2::read_xml(x)

  ## Drop comment nodes.
  xml2::xml_remove(xml2::xml_find_all(xml, "//comment()"))

  ## Main transform, map XML to list using a modification of the xml2::as_list convention
  ## See as_list.R
  json <- as_jsonlist(xml)
  #json <- setNames(list(json), xml2::xml_name(xml))
  json <- c("@type" = "eml", json)

  ## Set up the JSON-LD context
  if(add_context){
    json$`#xmlns` <- "http://ecoinformatics.org/"
    json <- add_context(json)
  }

  class(json) <- c("emld", "list")
  json
}



add_context <- function(json){
  ## Set up the JSON-LD context
  con <- list()
  if ("base" %in% names(json)) {
    con$`@base` <- json$base
    json$base <- NULL
  }
  # closing slash on url only if needed
  if(!is.null(json$`#xmlns`)){
    con$`@vocab` <- gsub("(\\w)$", "\\1/", json$`#xmlns`)
    json$`#xmlns` <- NULL
  }
  nss <- json[grepl("#xmlns\\:", names(json))]
  con <- c(con,
           stats::setNames(gsub("(\\w)$", "\\1/", nss),
                           vapply(names(nss),
                                  function(x)
                                    strsplit(x, split = ":")[[1]][[2]],
                                  character(1))
           )
  )
  xmlns <- grepl("^#xmlns", names(json))
  json <- json[!xmlns]
  json$`@context` <- con

  # order names so @context shows up first
  json <- json[order(names(json))]

  ## Add context defining @id types
  json
}

