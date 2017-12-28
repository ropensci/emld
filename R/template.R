
## Warning: do not attempt to request `recursive` on high-level objects


#' template
#'
#' @param object the name of an eml object to create
#' @param attributes logical, default FALSE. Should template include
#'  properties that are represented as XML attributes in EML? (e.g. system, id. etc)
#'
#' @return a list with elements named according to the properties of the object.
#'         This can be coerced into EML, see vignettes. NULL-valued elements (~)
#'         can take a data entry directly, while empty list()-valued elements ([])
#'         indicate properties that take other eml objects as values.
#' @details Note: while this function can be called in recursions, doing so may be a bad idea.
#' @export
#'
#' @examples
#' template("creator")
#'
template <- function(object, attributes = FALSE){
  properties <- eml_db[[object]]

  if(!attributes){
    drop <- grep("^(@|#)\\w+",  properties)
    if(length(drop) > 0 ) properties <- properties[-drop]
  }
  output <- vector("list", length(properties))
  names(output) <- properties


  ##  Recursive call is trouble, just single depth
  names(properties) <- properties
  children <-
    vapply(properties,
           function(x){
             properties <- eml_db[[x]]
             if(!attributes){
               drop <- grep("^(@|#)\\w+",  properties)
               if(length(drop) > 0 )
                 properties <- properties[-drop]
             }
             length(properties)
           },
           integer(1))


  for(n in names(which(children > 0)))
    output[[n]] <- vector("list", 0)
  class(output) <- c("emld", "list")
  output
}


## Fixme consider making template an `emld` S3 class and adding print method as json and optionally as yaml




#' @importFrom jsonlite write_json
template_file <- function(object, file, type = c("guess", "json", "yaml"),
                          attributes = FALSE){
  type <- match.arg(type)
  if(type == "guess")
    type <- switch(sub("^\\w+\\.(\\w+)$", "\\1", basename(file)),
           "yml" = "yaml",
           "yaml" = "yaml",
           "json" = "json",
           "json")

  output <- template(object, attributes = attributes)
  if(type == "json"){
    jsonlite::write_json(output, file, auto_unbox=TRUE, pretty = TRUE)
  } else if(type == "yaml"){
    ## Somehow this is okay without importFrom and only Suggests yaml
    requireNamespace("yaml")
    yaml::write_yaml(output, file)
  }
}

