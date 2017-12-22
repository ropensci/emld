

template <- function(object, recursive = FALSE){
  properties <- eml_db[[object]]
  output <- vector("list", length(properties))
  names(output) <- properties

  if(recursive){
    names(properties) <- properties
    output <- lapply(properties, template, recursive = recursive)
  }

  output
}


template_file <- function(object, file, type = c("json", "yaml"), recursive = FALSE){
  type <- match.arg(type)
  output <- template(object, recursive = recursive)
  if(type == "json"){
    jsonlite::write_json(output, file, auto_unbox=TRUE, pretty = TRUE)
  } else if(type == "yaml"){
    requireNamespace("yaml")
    yaml::write_yaml(output, file)
  }
}

# this is probably a bad idea
#template_file("eml", "eml.yaml", "yaml", recursive = TRUE)

## template knows about internal classes too
#template("ResponsibleParty")
