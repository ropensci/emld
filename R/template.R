
## Warning: do not attempt to request `recursive` on high-level objects


#' Create a template for an EML object
#'
#' @param object the name of an eml object to create
#'
#' @return a list with elements named according to the properties of the object.
#'         This can be coerced into EML, see vignettes. NULL-valued elements (~)
#'         can take a data entry directly, while empty list()-valued elements ({})
#'         indicate properties that take other eml objects as values.
#' @details Note: while this function can be called in recursions, doing so may be a bad idea.
#' @export
#'
#' @examples
#' template("creator")
template <- function(object){
  properties <- gsub("^(@|#)", "", eml_db[[eml_version()]][[object]])

  output <- vector("list", length(properties))
  names(output) <- properties


  ##  Recursive call is trouble, just single depth
  names(properties) <- properties
  children <-
    vapply(properties,
           function(x){
             properties <- eml_db[[eml_version()]][[x]]
             drop <-  grep("^(@|#)\\w+",  properties)
             if(length(drop) > 0 ) properties <- properties[-drop]
             length(properties)
           },
           integer(1))


  for(n in names(which(children > 0)))
    output[[n]] <- setNames(list(), character(0))
  class(output) <- c("emld", "list")
  output
}

