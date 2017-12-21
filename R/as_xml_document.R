#' json_to_xml
#'
#' json_to_xml
#' @param x JSON-LD representation of eml, as json, character, or list object.
#' @param file output filename. If NULL (default), will return xml_document
#' @param ... additional arguments to xml2::write_xml
#' @export
#' @importFrom methods is
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom xml2 xml_root xml_set_attr write_xml
json_to_xml <- function(x, file = NULL, ...){ UseMethod("json_to_xml")}

#' @export
json_to_xml.character <- function(x, file = NULL, ...){
  if(file.exists(x)){  ## Read from file
    x <- jsonlite::read_json(x)
  } else { ## Read from string
    x <- jsonlite::fromJSON(x, simplifyVector = FALSE)
  }
  json_to_xml(x, file, ...)
}

#' @export
json_to_xml.json <- function(x, file = NULL, ...){
  x <- jsonlite::fromJSON(x, simplifyVector = FALSE)
  json_to_xml(x, file, ...)
}

#' @export
json_to_xml.list <- function(x, file = NULL, ...){

  ## Frame/compact into original context for a standardized structure
  # eml_list <- frame.list(x)

  ## Sort S3/list elements according to eml ordering requirements
  # eml <- sort_eml(eml_list)

  ## Step 3: Serialize S3/list into XML
  xml <- as_eml_document(x)

  ## Step 4: Add namespaces from the original context as xmlns:prefix=""
  #xml <- context_namespaces(eml[["@context"]], xml)

  ## Serialize to file if desired
  if(!is.null(file))
    xml2::write_xml(xml, file, ...)
  else
    xml
}



rename_root <- function(xml, n){
  root <- xml2::xml_root(xml)
  xml_name(root) <- n
  xml
}

context_namespaces <- function(context, xml){
  ## unpack list-contexts
  context <- unlist(lapply(context, function(y){
    if(is.null(names(y)))
      return(NULL)
    else
      y
  }))
  ## Drop terms that aren't namespaces (don't end in / or #); e.g. drop
  context <- as.list(context[grepl(".*(#$|/$)",context)])
  ## add to the xml
  root <- xml2::xml_root(xml)
  for(ns in names(context)){
    switch(ns,
           "@vocab" = xml2::xml_set_attr(root, "xmlns", context[[ns]]),
           "@base" = xml2::xml_set_attr(root, "xml:base", context[[ns]]),
           xml2::xml_set_attr(root, paste("xmlns", ns, sep=":"), context[[ns]]))
  }
  xml
}



## use this instead
sort_eml <- function(eml_list){
  who <- names(eml_list)
  eml_list
}


## Convert list to XML

# - Frame JSON into predictable format; applying context to achieve de-referencing
# - Import framed JSON as list
# - serialize list to XML: Adjust from xml2::as_xml_document

## inside adaptiation of as_xml_document_method?
# - Unfold list-of-elements, property = [{}, {}] into [property={}, property={}, property={}]
# - Data values as XML attributes

## Do these in JSON, list, or XML stage?
# - Replace any non-eml-namespace node with corresponding meta node.
#   (identify URI properties as ResourceMeta rel/href)
# - Fix node ordering to conform to schema.
#   (Sort based on pre-specified order probably best/easiest;
#    alternately check refs come after defs)

is_URI <- function(x){
  grepl("\\w+://.+", x)
}
#is_URI("http://creativecommons.org/publicdomain/zero/1.0/")
#is_URI("creativecommons.org/publicdomain/zero/1.0/")


as_eml_document <- function(x, ...) {UseMethod("as_eml_document")}

#' @importFrom jsonlite fromJSON
as_eml_document.character <- function(x, ...){
  as_eml_document(jsonlite::fromJSON(x, simplifyVector = FALSE))
}
as_eml_document.json <- function(x, ...){
  as_eml_document(jsonlite::fromJSON(x, simplifyVector = FALSE))
}
#' @importFrom xml2 xml_add_child xml_set_attr xml_new_document
as_eml_document.list <- function(x, ...) {
  if (length(x) > 1) {
    if(!is.null(x$eml))
      x <- x["eml"]
    else
      stop("Root eml node not found", call. = FALSE)
  }
  doc <- xml2::xml_new_document()
  add_node(x, doc)

  root <- xml2::xml_root(doc)

  rename_root(root, "eml:eml")


}
## Identical to as_xml_document methods
#' @importFrom xml2 xml_new_root
as_eml_document.xml_node <- function(x, ...) {
  xml2::xml_new_root(.value = x, ..., .copy = TRUE)
}
as_eml_document.xml_nodeset <- function(x, root, ...) {
  doc <- xml2::xml_new_root(.value = root, ..., .copy = TRUE)
  for (i in seq_along(x)) {
    xml2::xml_add_child(doc, x[[i]], .copy = TRUE)
  }
  doc
}
as_eml_document.xml_document <- function(x, ...) {
  x
}


add_node <- function(x, parent, tag = NULL) {
  if (is.atomic(x)) {
    ## No use of xml_set_text please, we want atomic elements to be XML attribute values
    ##return(xml_set_text(parent, as.character(x)))
    return()
  }

  if (!is.null(tag)) {
    if(!is.null(names(x)) & length(x) > 0)
      parent <- xml2::xml_add_child(parent, tag)

    attr <- x[vapply(x, is.atomic, logical(1))]
    for (i in seq_along(attr)) {
      ## Handle special attributes
      is_attr <- grepl("^(@|#)(\\w+)", names(attr)[[i]])
      key <- gsub("^(@|#)(\\w+)", "\\2", names(attr)[[i]]) # drop json-ld `@`

      key <- gsub("^schemaLocation$", "xsi:schemaLocation", key)
      if(length(key) > 0){
        if(!is_attr){
          if(key == tag) ## special case where we use node name instead of content
            xml2::xml_set_text(parent, attr[[i]])
          else {
            textType <- xml2::xml_add_child(parent, key)
            xml2::xml_set_text(textType, attr[[i]])
          }
        } else {
          xml2::xml_set_attr(parent, key, attr[[i]])
        }
      }
    }
  }
  for (i in seq_along(x)) {
    if(!is.null(names(x))){
      tag <- names(x)[[i]]
    }
    add_node(x[[i]], parent, tag)
  }
}







old_add_node <- function(x, parent, tag = NULL) {
  is_attr <- grep("^(@|#).*", names(x))
  attr <- x[ is_attr ]
  if(length(attr) > 0 )
    child_nodes <- x[ - is_attr ]
  else
    child_nodes <- x

  if(is.atomic(x) & is.null(tag)){
    return(xml2::xml_set_text(parent, as.character(x)))
  }

  if(is.atomic(x) & !is.null(tag)){
    parent <- xml2::xml_add_child(parent, tag)
    return(xml2::xml_set_text(parent, as.character(x)))
  }

  if(!is.null(tag)){
    if(!is.null(names(x)) & length(x) > 0){
      parent <- xml2::xml_add_child(parent, tag)
    }

    for (i in seq_along(attr)) {
      key <- gsub("^(@|#)(\\w+)", "\\2", names(attr)[[i]]) # drop json-ld `@`
      if(length(key) > 0 & key != "type"){
        xml2::xml_set_attr(parent, key, attr[[i]])
      }
    }
  }
  for (i in seq_along(child_nodes)) {
    if(!is.null(names(child_nodes))){
      tag <- names(child_nodes)[[i]]
    } else {
      tag <- NULL #xml_name(parent)
    }
    add_node(child_nodes[[i]], parent, tag)
  }
}
