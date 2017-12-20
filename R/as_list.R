
as_jsonlist <- function(x, ns = character(), ...) {
  UseMethod("as_jsonlist")
}


## override xml2 method
#' @importFrom xml2 xml_contents xml_name xml_attrs xml_type xml_text
as_jsonlist.xml_node <- function(x, ns = character(), embed_attr=TRUE, ...) {
  key <- xml_name(x)
  contents <- xml2::xml_contents(x)
  if (length(contents) == 0) {
    # Base case - contents
    type <- xml2::xml_type(x)
    ## node contents
    if (type %in% c("text", "cdata"))
      return(xml2::xml_text(x))
    if (type != "element" && type != "document")
      return(paste("[", type, "]"))
    out <- list()

   }  else if(length(contents) == 1 & xml_type(contents[[1]]) == "text"){
       out <- as_jsonlist(contents[[1]], ns)
   } else {
      out <- as_jsonlist(contents, ns)
   }


  # Add xml attributes as #attribute keys
  node_attr <- special_jsonld_attrs(xml2::xml_attrs(x, ns = ns), prefix = "#")
  if(length(node_attr) > 0){
    ## If attributes become properties, need a property for node content (can't be a value)
    if(is.null(names(out))) names(out) <- xml2::xml_name(x) #"content"
    out <- c(node_attr, out)
  }
  group_repeated_key(out)
}

## override xml2 method
as_jsonlist.xml_nodeset <- function(x, ns = character(), ...) {
  lapply_nodes(x, as_jsonlist, ns = ns, ...)
}

## apply function f to each node, and name element nodes by node name
lapply_nodes <- function(x, f, ns = character(), ...){

  out <- lapply(seq_along(x), function(i) f(x[[i]], ns = ns, ...))
  ## re-attach names
  nms <- ifelse(xml2::xml_type(x) == "element", xml2::xml_name(x, ns = ns), "")
  if (any(nms != "")) {
    names(out) <- nms
  }
  out
}


ld_attributes <- c("id", "type")
special_jsonld_attrs <- function(x, prefix = "") {
  if (length(x) == 0) {
    return(NULL)
  }
  # escape special JSON-LD names
  special <- names(x) %in% ld_attributes
  names(x)[special] <- paste0("@", names(x)[special])
  # prefix other attributes

  names(x)[!special] <-  paste0(prefix, names(x)[!special])
  r_attrs_to_xml(as.list(x))
}

## Adapted from xml2  -- special r attributes we do not want to conflict with
## (mostly important if attaching xml attributes as R attributes)
special_attributes <- c("class", "comment", "dim", "dimnames", "names", "row.names", "tsp")
r_attrs_to_xml <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }
  # Drop R special attributes
  x <- x[!names(x) %in% special_attributes]
  # Rename any xml attributes needed
  special <- names(x) %in% paste0(".", special_attributes)
  names(x)[special] <- sub("^\\.", "", names(x)[special])
  x
}



