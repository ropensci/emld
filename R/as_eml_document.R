
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
#' xml_set_namespace xml_root xml_find_first
as_eml_document.list <- function(x, ...) {

  doc <- xml2::xml_new_document()
  #type <- x[["@type"]]
  x[["@type"]] <- NULL
  add_node(x, doc, "eml")

  ## Set namespace of <eml> to <eml:eml>
  xml2::xml_set_namespace(xml2::xml_find_first(xml2::xml_root(doc), "."), "eml")
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


add_node <- function(x, parent, tag) {
    if (is.atomic(x)) {
      return() ## bc we call add_node after already eval on is.atomic
    }
    x <- sort_properties(x, tag)
    if(!is.null(names(x)) & length(x) > 0){
      parent <- xml2::xml_add_child(parent, tag)
    }
    update_tag <- !is.null(names(x))
    for(i in seq_along(x)){
      if(is.atomic(x[[i]])){
        serialize_atomics(x, parent, tag, i)
      }
      if(update_tag){
        tag <- names(x)[[i]]
      }
      add_node(x[[i]], parent, tag)
    }
}

## FIXME should be able to replace x, i to just x[[i]], names(x)[[i]]
serialize_atomics <- function(x, parent, tag, i){

  if(is.null(names(x)[[i]])){
    textType <- xml2::xml_add_child(parent, tag)
    return(xml2::xml_set_text(textType, x[[i]]))
  }

  ## Skip
  if(grepl("^@*id$", names(x)[[i]])){
    ## Skip `@id` element if uses a json-ld local id
    if(grepl("^_:b\\d+", x[[i]])){
      return()
    }
    ## Skip `@id` elements unless explicitly permitted
    if(!any(grepl("^@*id$", eml_db[[tag]]))){
      return()
    }
  }
  ## Identify properties that should become xml attributes instead of text values
  is_attr <- grepl("^(@|#)(\\w+)", names(x)[[i]])
  key <- gsub("^(@|#)(\\w+)", "\\2", names(x)[[i]]) # drop json-ld `@`
  key <- gsub("^schemaLocation$", "xsi:schemaLocation", key)

  if(length(key) > 0){
    ## Text-type atomics ##
    if(!is_attr){
      if(grepl("^#\\w+", tag)){
        ## special case where JSON-LD repeats node name (for grouped nodes with attributes, e.g. url)
        xml2::xml_set_text(parent, x[[i]])
      } else {
        textType <- xml2::xml_add_child(parent, key)
        xml2::xml_set_text(textType, x[[i]])
      }
    ## Attribute atomics ##
    } else {
      xml2::xml_set_attr(parent, key, x[[i]])
    }
  }

}



# List of all classes defined by EML and their slot names, in order compatible with schema validation
# This data is primarily intended for internal use by the sort_properties function
#
# @references \url{https://github.com/ropensci/EML}
#"eml_db"


sort_properties <- function(x, tag){

  n <- names(x)
  order <- eml_db[[tag]]
  drop <- grep("(.Data|schemaLocation|lang|slot_order|id|scope|system|authSystem)", order)
  if(length(drop) > 0)
    order <- order[-drop]
  if(length(order) == 0 | length(n) == 0)
    return(x)

  nodes <- x
  attrs <- grep("^(#|@)\\w",n)
  if(length(attrs) > 0 ){
    n <- n[-attrs]
    nodes <- x[-attrs]
  }
  if(!all(n %in% order))
    return(x)

  fixed <- names(sort(vapply(n,
                             function(i) which(i == order), integer(1))))

  c(x[attrs],nodes[fixed])

}

#ld <- parse_eml(system.file("extdata/hf205.xml", package="emld"))
#names(sort_properties(ld$eml$dataset, "dataset"))
#names(sort_properties(ld$eml$access, "access"))
