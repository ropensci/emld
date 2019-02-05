
#' @importFrom yaml as.yaml
#' @importFrom jsonlite toJSON
#' @export
print.emld <- function(x, ...){
  x[["@context"]] <- NULL # context just adds clutter to display
  x[["@type"]] <- NULL
  style <- getOption("emld_print", "yaml")
  #cat("EML as an emld object:\n\n\n")
  switch(style,
         "yaml" = cat(yaml::as.yaml(x)),
         "json" = cat(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE))
  )
}
