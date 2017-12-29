## Consider depricating, easy enough to do these two steps manually!


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
  emld <- as_emld(x)
  as_json(emld, file = out)
}
