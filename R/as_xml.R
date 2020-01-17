#' Coerce an emld object into XML (EML's standard format)
#'
#' @param x an emld object
#' @param file optional path to write out to file.
#'   Otherwise, defaults to NULL and will return an xml_document object.
#' @param root name for the root node; default to 'eml'
#' @param ns namespace abbreviation on root node, default 'eml'
#' @param schemaLocation default to set schemaLocation if not already defined in emld object
#' @export
#' @details Unlike as_json, this function cannot rely on the existing
#' convention of serializing a list to xml, eg, as defined by xml2::as_xml_document()
#' Instead, this relies on a modified version, as_eml_document.  In addition
#' further steps must be taken when working with JSON-LD to deal with
#' different possible framings and namespaces from the JSON-LD context
#' element. Thus this `as_xml` function is particular to EML and `emld`
#' objects alone.
#' @examples
#' f <- system.file("extdata/example.xml", package = "emld")
#' emld <- as_emld(f)
#' xml <- as_xml(emld)
#'
#' ## can also write directly to a file:
#' xml_file <- tempfile()
#' as_xml(emld, xml_file)
#' @return a xml_document object. Or if a file path is provided, the metadata
#' is written out in XML file and the function returns `NULL` invisibly.
as_xml <- function(x, file=NULL, root = "eml", ns = "eml",
  schemaLocation = paste0(eml_ns(), "/ eml.xsd"))
  {
    UseMethod("as_xml")
  }

#' @export
as_xml.list <- function(x, file=NULL, root = "eml", ns = "eml",
                        schemaLocation = paste0(eml_ns(), "/ eml.xsd")){
  x <- as_emld.list(x)
  as_xml.emld(x, file)
}

#' @export
as_xml.emld <- function(x, file=NULL, root = "eml", ns = "eml",
                        schemaLocation = paste0(eml_ns(), "/ eml.xsd")){
  ## Frame/compact into original context for a standardized structure
  x <- eml_frame(x)

  ## store context
  context <- x[["@context"]]

  ## Drop context and type
  x[["@context"]] <- NULL
  x[["@type"]] <- NULL

  ## add schemaLocation
  if(is.null(x[["schemaLocation"]])){
    if(!is.null(schemaLocation))
    x[["schemaLocation"]] <- schemaLocation
  }
  ## here we go, serialize to XML!
  xml <- as_eml_document(x, root = root, ns = ns)

  ## Add any namespaces defined in the @context
  xml <- context_namespaces(context, xml)

  ## Fix missing namespace on root element
  xml2::xml_set_namespace(xml2::xml_root(xml), ns)
  xml <- xml2::as_xml_document(xml)

  ## Serialize to file if desired
  if(!is.null(file)){
    xml2::write_xml(xml, file)
  } else {
    xml
  }
}

## check if a list is JSON-LD
## (e.g. absense of a @context could mean we are just expanded form)
is_jsonld.list <- function(x){
  json <- jsonlite::toJSON(x, auto_unbox = TRUE)
  expanded <- jsonld::jsonld_expand(json)
  length(jsonlite::fromJSON(expanded)) > 0
}

eml_frame <- function(x){
  x <- drop_nulls(x) ## recursive prune of nulls

  ## choose the context we compact into later
  if(is.null(x[["@context"]])){
    context <- system.file(paste0("context/",
                                  eml_version(),
                                  "/eml-context.json"), package = "emld")
  } else {
    context <- jsonlite::toJSON(x[["@context"]], auto_unbox = TRUE)
  }

  ## set a context for framing if we've gotten just a plain json/list
  if(!is_jsonld.list(x)){
    x[["@context"]] <-
      list("@vocab" = paste0(eml_ns(), "/"))
  }

  ## set a type for framing
  if(is.null(x[["@type"]])){
    x[["@type"]] <- "EML"
  }
  json <- jsonlite::toJSON(x, auto_unbox = TRUE)
  frame <- system.file(paste0("frame/",
                              eml_version(),
                              "/eml-frame.json"), package = "emld")
  framed <- jsonld::jsonld_frame(json, frame)
  compacted <- jsonld::jsonld_compact(framed, context)
  out <- jsonlite::fromJSON(compacted, simplifyVector = FALSE)

  class(out) <- c("emld", "list")
  out
}

context_namespaces <- function(context, xml){
  if(is.null(context)){
    return(xml)
  }

  ## unpack list-contexts
  if(is.null(names(context))){
    context <- unlist(lapply(context, function(y){
      if(is.null(names(y))) return(NULL) else y
    }))}
  ## Drop terms that aren't namespaces (don't end in / or #); e.g. drop
  context <- as.list(context[grepl(".*(#$|/$)",context)])


  ## add to the xml. No base, no default namespace
  root <- xml2::xml_root(xml)
  for(ns in names(context)){
    switch(ns,
           "@vocab" = NULL, #xml2::xml_set_attr(root, "xmlns", context[[ns]]),
           "@base" = NULL, # xml2::xml_set_attr(root, "xml:base", context[[ns]]),
           xml2::xml_set_attr(root, paste("xmlns", ns, sep=":"),
                              gsub("/$", "", context[[ns]])))
  }
  xml2::as_xml_document(xml)
}





drop_nulls <- function(x){
  if(is.atomic(x))
    return(x)
  i <- vapply(x, length, integer(1)) > 0
  x <- x[i]
  lapply(x, drop_nulls)
}
