
#' @importFrom yaml as.yaml
#' @importFrom jsonlite toJSON
#' @export
print.emld <- function(x, ...){
  style <- getOption("emld_print", "yaml")
  #cat("EML as an emld object:\n\n\n")
  switch(style,
         "yaml" = cat(yaml::as.yaml(x)),
         "json" = cat(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE ))
  )
}

compact.emld <- function(x, ...){
  if(is.list(x) && length(x) > 1){
    keep <- vapply(x, length, integer(1)) > 0
    lapply(x[keep], compact.emld)
  } else {
    x
  }
}
