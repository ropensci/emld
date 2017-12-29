#' as_xml
#'
#' @param x an emld object
#' @param file optional path to write out to file.
#'   Otherwise, defaults to NULL and will return an xml_document object.
#'
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
as_xml <- function(x, file=NULL){ UseMethod("as_xml") }

#' @export
as_xml.emld <- function(x, file=NULL){

  ## Frame/compact into original context for a standardized structure
  # eml_list <- frame.list(x)

  ## Drop context and serialize
  y <- x
  y[["@context"]] <- NULL
  xml <- as_eml_document(y)

  ## Add any namespaces defined in the @context
  xml <- context_namespaces(x[["@context"]], xml)

  ## Fix missing namespace on root element
  xml2::xml_set_namespace(xml2::xml_find_first(xml2::xml_root(xml), "."), "eml")
  xml <- xml2::as_xml_document(xml)

  ## Serialize to file if desired
  if(!is.null(file))
    xml2::write_xml(xml, file)
  else
    xml

  }

context_namespaces <- function(context, xml){
  ## unpack list-contexts
  if(is.null(names(context))){
    context <- unlist(lapply(context, function(y){
      if(is.null(names(y))) return(NULL) else y
    }))}
  ## Drop terms that aren't namespaces (don't end in / or #); e.g. drop
  context <- as.list(context[grepl(".*(#$|/$)",context)])
  ## add to the xml
  root <- xml2::xml_root(xml)
  for(ns in names(context)){
    switch(ns,
           "@vocab" = NULL, #xml2::xml_set_attr(root, "xmlns", context[[ns]]),
           "@base" = xml2::xml_set_attr(root, "xml:base", context[[ns]]),
           xml2::xml_set_attr(root, paste("xmlns", ns, sep=":"),
                              gsub("/$", "", context[[ns]])))
  }
  xml2::as_xml_document(xml)
}
