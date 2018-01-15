#' @importFrom xml2 xml_add_child xml_set_attr xml_new_document
#' @importFrom xml2 xml_set_namespace xml_root xml_find_first
as_eml_document <- function(x, root = "eml", ns ="eml") {

  # drop JSON-LD @type; not explicit in XML
  x[["@type"]] <- NULL
  if(is.null(x[["#schemaLocation"]]))
    x[["#schemaLocation"]] <- "eml://ecoinformatics.org/eml-2.1.1 eml.xsd"

  doc <- xml2::xml_new_document()
  add_node(x, doc, root)
  xml2::xml_set_namespace(xml2::xml_root(doc), ns)
  doc
}



drop_nulls <- function(x){
  if(is.atomic(x))
    return(x)
  i <- vapply(x, length, integer(1)) > 0
  x <- x[i]
  lapply(x, drop_nulls)
}

add_node <- function(x, parent, tag) {
    if (is.atomic(x)) {
      return() ## bc we call add_node after already eval on is.atomic
    }

    #x <- drop_nulls(x)  ## FIXME: drop any numbered tags

    ## unwrap group_by_key sets
    if(!is.null(names(x)) & length(x) > 0){
      parent <- xml2::xml_add_child(parent, tag)
    }

    ## Handle text-type explicitly, parsing back into XML
    ## Note: length-1 case gets handled as an atomic character string
    if(tag %in% c("para", "section")){
      if(length(x) > 1) {
        if(grepl("^<\\w+>", x[[1]])){ ## starts with a tag, assume XML
          for(i in seq_along(x)){
            n <- read_xml(paste0("<", tag, ">", x[[i]], "</", tag, ">"))
            xml2::xml_add_child(parent, n, .copy = TRUE)
          }
          return()
        }
      }
    }

    x <- sort_properties(x, tag)

    ## Handle all other values (nodes and attributes)
    update_tag <- !is.null(names(x))
    for(i in seq_along(x)){
      if(is.atomic(x[[i]])){
        serialize_atomics(x[[i]], parent, tag, names(x)[[i]])
      }
      if(update_tag){
        tag <- names(x)[[i]]
      }
      add_node(x[[i]], parent, tag)
    }




}

serialize_atomics <- function(x, parent, tag, key){
  if(is.null(key)){
    textType <- xml2::xml_add_child(parent, tag)
    return(xml2::xml_set_text(textType, x))
  }

  ## SPECIAL, handle length-1 text types
  if(key %in% c("para", "section")){
      if(grepl("^<\\w+>", x)){ ## starts with a xml element, assume XML
        n <- read_xml(paste0("<", key, ">", x, "</", key, ">"))
        xml2::xml_add_child(parent, n, .copy = TRUE)
        return()
      }
  }



  if(grepl("^@*id$", key)){
    ## Skip `@id` element if uses a json-ld local id
    if(grepl("^_:b\\d+", x)){
      return()
    }
    ## Skip `@id` elements unless explicitly permitted
    if(!any(grepl("^@*id$", eml_db[[tag]]))){
      return()
    }
  }
  ## Identify properties that should become xml attributes instead of text values
  is_attr <- grepl("^(@|#)(\\w+)", key)
  key <- gsub("^(@|#)(\\w+)", "\\2", key) # drop json-ld `@`
  key <- gsub("^schemaLocation$", "xsi:schemaLocation", key)

  if(length(key) > 0){
    ## Text-type atomics ##
    if(!is_attr){
      if(grepl("^#\\w+", tag)){
        ## special case where JSON-LD repeats node name
        ## (for grouped nodes with attributes, e.g. url)
        xml2::xml_set_text(parent, x)
      } else {
        textType <- xml2::xml_add_child(parent, key)
        xml2::xml_set_text(textType, x)
      }
    ## Attribute atomics ##
    } else {
      xml2::xml_set_attr(parent, key, x)
    }
  }

}





sort_properties <- function(x, tag){

  n <- names(x)
  order <- eml_db[[tag]]

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

  fixed <- names(sort(
    vapply(n,function(i) which(i == order), integer(1))))

  c(x[attrs],nodes[fixed])

}

