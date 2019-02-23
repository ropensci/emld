#' @importFrom xml2 xml_add_child xml_set_attr xml_new_document
#' @importFrom xml2 xml_set_namespace xml_root xml_find_first
as_eml_document <- function(x, root = "eml", ns ="eml") {
  doc <- xml2::xml_new_document()
  add_node(x, doc, root)
  xml2::xml_set_namespace(xml2::xml_root(doc), ns)
  doc
}



add_node <- function(x, parent, tag) {
    if (is.atomic(x)) {
      return() ## bc we call add_node after already eval on is.atomic
    }
    #x <- drop_nulls(x)

    ## Unnamed elements can arise when xml text is interspersed with tags
    if(!is.na(suppressWarnings(as.integer(tag)))){
      return(xml2::xml_set_text(parent, paste(x, collapse="")))
    }

    ## unwrap group_by_key sets
    if(!is.null(names(x)) & length(x) > 0){
      parent <- xml2::xml_add_child(parent, tag)
    }

    ## Handle text-type explicitly, parsing back into XML
    if(tag %in% c("para", "section")){
      if(length(x) >= 1) {
        ## Note: length-1 case gets handled as an atomic character string
        for(i in seq_along(x)){
          if(grepl("<\\w+>", x[[i]])){ ## contains an xml opening tag, assume XML
            n <- read_xml(paste0("<", tag, ">", x[[i]], "</", tag, ">"))
            xml2::xml_add_child(parent, n, .copy = TRUE)
          } else {
            textType <- xml2::xml_add_child(parent, tag)
            xml2::xml_set_text(textType, as.character(x[[i]]))
          }
        }
        return()
      }
    }

    x <- sort_properties(x, tag)

    ## Handle all other values (nodes and attributes)
    update_tag <- !is.null(names(x))
    for(i in seq_along(x)){
      if(is.atomic(x[[i]])){
        serialize_atomics(x[[i]], parent, tag, names(x)[[i]])
      }
      ## Non-atomics with repeated keys
      next_tag <- tag
      if(update_tag){
        next_tag <- names(x)[[i]]
      }
      add_node(x[[i]], parent, next_tag) # does nothing if x[[i]] is atomic
    }
}

serialize_atomics <- function(x, parent, tag, key){

  ## Repeated elements all named by (parent) tag name
  if(is.null(key)){
    textType <- xml2::xml_add_child(parent, tag)
    return(xml2::xml_set_text(textType, as.character(x)))
  }

  ## SPECIAL, handle length-1 text types
  if(key %in% c("para", "section")){
      if(grepl("<\\w+>", x)){ ## contains an xml opening tag, assume XML
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
    if(!any(grepl("^@*id$", eml_db[[eml_version()]][[tag]]))){
      return()
    }
  }
  ## Identify properties that should become xml attributes instead of text values
  ## is_attr <- grepl("^(@|#)(\\w+)", key) ## NOPE -- don't rely on prefixing

  is_attr <- FALSE
  order <- eml_db[[eml_version()]][[tag]]
  if(length(order) > 0 & !is.na(key)){
    is_attr <- any(grepl(paste0("^#", key, "$"), order)) | key == "id"
  }
  key <- gsub("^schemaLocation$", "xsi:schemaLocation", key) # assume namespace
  key <- gsub("^lang$", "xml:lang", key) # assume namespace
  if(grepl("xmlns:\\w+", key)) is_attr <- TRUE
  if(grepl("xml:\\w+", key)) is_attr <- TRUE
  if(grepl("xsi:\\w+", key)) is_attr <- TRUE

  ## No longer needed:
  #key <- gsub("^(@|#)(\\w+)", "\\2", key) # drop json-ld `@` and `#`

  if(length(key) > 0){

    if(!is.na(suppressWarnings(as.integer(key)))){
      return(xml2::xml_set_text(parent, paste(as.character(x), collapse="")))
    }

    ## Text-type atomics ##
    if(!is_attr){
      if(xml_name(parent) == key){
        ## special case where JSON-LD repeats node name
        ## (for grouped nodes with attributes, e.g. url)
        xml2::xml_set_text(parent, as.character(x))
      } else {
        textType <- xml2::xml_add_child(parent, key)
        xml2::xml_set_text(textType, as.character(x))
      }
    ## Attribute atomics ##
    } else {
      xml2::xml_set_attr(parent, key, x)
    }
  }
}




## sorts the elements of list x in order given
# by eml_db[[eml_version()]][[tag]]
sort_properties <- function(x, tag){

  n <- names(x)
  children <- eml_db[[eml_version()]][[tag]]

  if(length(children) == 0 | length(n) == 0)
    return(x)


  ## Possible attributes for this object
  is_attr <- grep("^(#|@)\\w",children)
  possible_attrs <- unique(gsub("^(#|@)", "",
                                c(children[is_attr], "id", "schemaLocation")))

  ## Actual attributes present
  attrs <- which(n %in% possible_attrs)
  order <- unique(c(gsub("^(#|@)", "", children), "id", "schemaLocation"))
  nodes <- x
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

