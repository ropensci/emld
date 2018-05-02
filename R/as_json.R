#' as_json
#'
#' @param x an emld object
#' @param file optional path to write out to file.
#'   Otherwise, defaults to NULL and will return a json object.
#' @export
#' @details Note: since emld list object maintains a 1:1 correspondence with JSON,
#' following the conventions of jsonlite, this function is basically trivial. The
#' only purpose is to default to auto_unbox = TRUE in serializing lists to JSON.
#' @examples
#' f <- system.file("extdata/example.xml", package = "emld")
#' emld <- as_emld(f)
#' json <- as_json(emld)
#'
as_json <- function(x, file=NULL){ UseMethod("as_json") }

#' @export
as_json.emld <- function(x, file=NULL){
  x <- drop_nulls(x)
  if(is.null(file)){
    jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)
  } else {
    jsonlite::write_json(x, file, pretty = TRUE, auto_unbox = TRUE)
  }
}

#' @export
as_json.list <- function(x, file=NULL){
  class(x) <- c("emld", "list")
  as_json(x)
}
