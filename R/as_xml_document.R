
as_xml_document <- function(x, ...) {
  UseMethod("as_xml_document")
}

as_xml_document.list <- function(x, ...) {
  if (length(x) > 1) {
    stop("Root nodes must be of length 1", call. = FALSE)
  }


  add_node <- function(x, parent, tag = NULL) {
    if (is.atomic(x)) {
      return(xml2::xml_set_text(parent, as.character(x)))
    }
    if (!is.null(tag)) {
      parent <- xml2::xml_add_child(parent, tag)
      attr <- r_attrs_to_xml(attributes(x))
      for (i in seq_along(attr)) {
        xml2::xml_set_attr(parent, names(attr)[[i]], attr[[i]])
      }
    }
    for (i in seq_along(x)) {
      add_node(x[[i]], parent, names(x)[[i]])
    }
  }

  doc <- xml2::xml_new_document()
  add_node(x, doc)
  xml2::xml_root(doc)
}

as_xml_document.xml_node <- function(x, ...) {
  xml2::xml_new_root(.value = x, ..., .copy = TRUE)
}

as_xml_document.xml_nodeset <- function(x, root, ...) {
  doc <- xml2::xml_new_root(.value = root, ..., .copy = TRUE)
  for (i in seq_along(x)) {
    xml2::xml_add_child(doc, x[[i]], .copy = TRUE)
  }
  doc
}

as_xml_document.xml_document <- function(x, ...) {
  x
}
