
## Warning: do not attempt to request `recursive` on high-level objects


#' template
#'
#' @param object the name of an eml object to create
#' @param recursive logical, default FALSE.  Fill in sub-templates recursively?
#'   WARNING do not attempt to set to TRUE on high level objects like eml, dataset, dataTable, etc
#' @param attributes logical, default FALSE. Should template include
#'  properties that are represented as XML attributes in EML? (e.g. system, id. etc)
#'
#' @return a list with elements named according to the properties of the object.
#'         This can be coerced into EML, see vignettes.
#' @export
#'
#' @examples
#' template("creator")
#' template("creator", recursive = TRUE)
#'
#'
template <- function(object, recursive = FALSE, attributes = FALSE){
  properties <- eml_db[[object]]

  if(!attributes){
    drop <- grep("^(@|#)\\w+",  properties)
    if(length(drop) > 0 ) properties <- properties[-drop]
  }

  output <- vector("list", length(properties))
  names(output) <- properties

  if(recursive){
    names(properties) <- properties
    output <- lapply(properties, template,
                     recursive = recursive, attributes = attributes)
  }
  ## Ideally drop lowest recursion or do not recurse.
  ## So we get NULL instead of list(), which is a better indication
  ## that the template takes a data value and not further information.


  class(output) <- c("emld", "list")
  output
}


## Fixme consider making template an `emld` S3 class and adding print method as json and optionally as yaml




#' @importFrom jsonlite write_json
template_file <- function(object, file, type = c("guess", "json", "yaml"),
                          recursive = FALSE, attributes = FALSE){
  type <- match.arg(type)
  if(type == "guess")
    type <- switch(sub("^\\w+\\.(\\w+)$", "\\1", basename(file)),
           "yml" = "yaml",
           "yaml" = "yaml",
           "json" = "json",
           "json")

  output <- template(object, recursive = recursive, attributes = attributes)
  if(type == "json"){
    jsonlite::write_json(output, file, auto_unbox=TRUE, pretty = TRUE)
  } else if(type == "yaml"){
    ## Somehow this is okay without importFrom and only Suggests yaml
    requireNamespace("yaml")
    yaml::write_yaml(output, file)
  }
}


# template("creator")
# template("creator", recursive = TRUE)

## template knows about internal classes too
#template("ResponsibleParty")


# template_file("creator", "creator.yml", recursive = TRUE)



# this is a terribly bad idea:
#template_file("eml", "eml.yaml", "yaml", recursive = TRUE)
