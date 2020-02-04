

#' eml_validate
#'
#' eml_validate processes an EML document using the XSD schema for the
#' appropriate version of EML and determines if the document is schema-valid
#' as defined by the XSD specification
#' @param eml file path, xml_document,
#' @param encoding optional encoding for files, default UTF-8.
#' @param schema path to schema
#' @return Whether the document is valid (logical)
#'
#' @examples \donttest{
#'
#'  f <- system.file("extdata", "example.xml", package = "emld")
#'
#'  ## validate file directly from disk:
#'  eml_validate(f)
#'
#'  ## validate an eml object:
#'  eml <- as_emld(f)
#'  eml_validate(eml)
#'
#' }
#'
#' @export
#' @importFrom xml2 read_xml xml_validate
#' @importFrom methods is
eml_validate <- function(eml,
                         encoding = "UTF-8",
                         schema = NULL) {

  doc <- generalized_parser(eml, encoding = encoding)


  # Use the EML namespace to find the EML version and the schema location
  if (is.null(schema)) {
    try(schema <- eml_locate_schema(doc))
  }
  schema_doc <- xml2::read_xml(schema)

  ## Only do full validation if root element's namespace is EML and not
  ## merely a subclass
  info <- guess_root_schema(doc)
  full <- info$module == "eml"

  ## defaults
  id_valid <- TRUE
  unit_valid <- TRUE
  if(full){
    id_valid <- eml_additional_validation(doc, encoding = encoding)
    unit_valid <- validate_units(doc, encoding = encoding)
  }

  schema_valid <- tryCatch(
    xml2::xml_validate(doc, schema_doc),
    error = function(e) {
      warning("The document could not be validated.")
      list(status = 1,
           errors = c(NULL),
           warnings = c(e))
    }
  )
  result <- schema_valid & id_valid & unit_valid
  attr(result, "errors") <- c(attr(id_valid, "errors"),
                              attr(unit_valid, "errors"),
                              attr(schema_valid, "errors"))
  result
}


# Return an xml_document representation of EML given any of the following
# eml formats:
#
# 1. A path to file
# 2. a list (emld object)
# 3. an xml_document
generalized_parser <- function(eml, encoding = "UTF-8"){
  if (is.character(eml)) {
    if (file.exists(eml)) {
      doc <- xml2::read_xml(eml, encoding = encoding)
    }
  } else if (is(eml, "xml_document")) {
    doc <- eml
  } else if (is.list(eml)){
    ## Shouldn't have to write to tempfile,
    ## but  `doc <- as_xml(eml)` fails to drop xsi prefix on "schemaLocation"
    f <- tempfile()
    as_xml(eml, f)
    doc <- xml2::read_xml(f)
    unlink(f)
  } else {
    stop(paste("Did not recognize eml object with class", class(eml)))
  }
  doc
}



# Not all EML validation rules can be inforced by the XSD definitions
# here we implement the addtional checks described by:
# https://github.com/NCEAS/eml/blob/BRANCH_EML_2_2/docs/eml-validation-refs.md
eml_additional_validation <- function(eml,
                                      encoding = "UTF-8") {


  error_log <- character()
  # Parse the XML document using an XML Schema-compliant parser
  doc <- generalized_parser(eml, encoding = encoding)

  # If the root element is not eml, then the document is invalid
  if(xml2::xml_name(xml2::xml_root(doc), ns = xml_ns(doc)) == "eml")
    error_log <- c(error_log, ("root element is not named 'eml'"))

  # Elements which contain an annotation child element MUST contain an id attribute,
  # unless the containing annotation element contains a references attribute, or it
  # is within a containing additionalMetadata element that contains a describes element
  annot_without_ref <- xml2::xml_find_all(doc, "//annotation[not(@references) and not(ancestor::additionalMetadata/describes)]/..")
  if(any(lapply(xml_attrs(annot_without_ref, "id"), length) == 0))
    error_log <- c(error_log,
                   paste("parent of any annotation must have id",
                         "unless annotation contains a references attribute",
                         "or has an ancestor additionalMetadata with a describes child"))

  ## If annotation has a references, parent cannot have ID
  id_and_annotation <- xml2::xml_find_all(doc, "//*[@id]/annotation")
  annotation_children <- xml2::xml_name(xml2::xml_children(id_and_annotation))
  if("references" %in% annotation_children)
    error_log <- c(error_log, "Annotation elements with ids cannot contain references elements")

  # ID attributes must be unique
  id <- c(xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id"),
          xml_attr(xml2::xml_find_all(doc, "//*[@packageId]"), "packageId"))
  if(any(duplicated(id)))
    error_log <- c(error_log, "all id attributes must be unique")

  # sys_nodes <- xml2::xml_find_all(doc, "//@id/ancestor-or-self::*[@system][1]")
  # sys <- xml_attr(sys_nodes, "system")

  # If the element containing the id contains a references element as an
  # immediate child then the document is invalid
    id_and_references <- xml2::xml_find_all(doc, "//*[@id]/references")
    if(length(id_and_references) > 0 )
      error_log <- c(error_log,
      "elements with id cannot contain references as immediate child")

  # ids given by describes must be defined in doc
   describes <- xml2::xml_text(xml2::xml_find_all(doc, "//describes"), trim = TRUE)
   if(!all(describes %in% id))
     error_log <- c(error_log, "not all 'describes' values match defined id attributes")

   # ids given by references must be defined in doc
   references <- c(xml2::xml_text(xml2::xml_find_all(doc, "//references"), trim = TRUE),
                                  xml_attr(xml2::xml_find_all(doc, "//*[@references]"), "references"))
   if(!all(references %in% id))
     error_log <- c(error_log, "not all 'references' values match defined id attributes")

  # If no validity errors are found above or by the parser, then the document is valid
  if(length(error_log) == 0) {
    result <- TRUE
  } else {
    warning(paste("Document is invalid. Found the following errors:\n", error_log))
    result <- FALSE
  }
  attr(result, "errors") <- error_log
  result
}



#' Get the real `QName` for the root element, including its prefix
#'
#' Note that if a default namespace is used, the prefix will be `d1`.
#'
#' @param doc An `xml_document`
#'
#' @return A `list` with elements `prefix` and `name`. `prefix` will be `NULL`
#' if the element has no namespace prefix but `name` will always be a
#' `character`.
find_real_root_name <- function(doc) {
  name <- xml2::xml_name(xml2::xml_root(doc), xml2::xml_ns(doc))

  if (grepl(":", name)) {
    tokens <- strsplit(name, ":")[[1]]

    return(list(prefix = tokens[1], name = tokens[2]))
  } else {
    return(list(prefix = NULL, name = name))
  }
}

#' Find the root schema module and version
#'
#' @param doc An `xml_document`
#'
#' @return If found, a list with names 'version', 'module', and `namespace. If
#' not found, throws an error.
guess_root_schema <- function(doc) {
  namespaces <- as.list(xml2::xml_ns(doc))
  root <- find_real_root_name(doc)

  # Handle case like <citation .... xmlns="..."> (no prefix). Not sure if this
  # code ever gets called, see next condition.
  if (is.null(root$prefix) && "xmlns" %in% names(namespaces)) {
    match <- namespaces[["xmlns"]]
  # Handle weird case where `xml2` treats `xmlns=` on the root as being for
  # prefix `d1` which seems dynamically generated.
  } else if (is.null(root$prefix) && "d1" %in% names(namespaces)) {
    match <- namespaces[["d1"]]
  # Handle case where a prefix is used (eml:eml) and the `eml` namespace is
  # defined on the root. This is the common case for EML documents.
  } else if (!is.null(root$prefix) && root$prefix %in% names(namespaces)) {
    match <- namespaces[[root$prefix]]
  } else {
    stop("Unhandled error: Couldn't determine schema to validate with.")
  }

  # Hackily parse the xmlns value to get the final path part in order to get
  # the module and version string
  tokens <- unname(strsplit(match, "/"))[[1]]
  last <- strsplit(tokens[length(tokens)], "-")[[1]]

  retval <- list(last[1], last[2], match)
  names(retval) <- c("module", "version", "namespace")

  retval
}


#' eml_locate_schema
#'
#' eml_locate_schema returns the location of the XSD schema file for a given
#' EML document, as shipped with the EML R package.
#'
#' @details The schema location is based on the last path component from the EML
#' namespace (e.g., eml-2.1.1), which corresponds to the directory containing
#' xsd files that ship with the EML package. Schema files are copies of the
#' schemas from the EML versioned releases. If an appropriate schema is not
#' found, the function returns FALSE.
#'
#' @param eml an xml2::xml_document instance for an EML document
#' @param ns the namespace URI for the top (root) element
#' @return fully qualified path to the XSD schema for the appropriate version of EML
#'
#' @examples \donttest{
#' f <- system.file("extdata", "example.xml", package = "emld")
#' eml <- xml2::read_xml(f)
#' schema <- eml_locate_schema(eml)
#' }
#' @importFrom xml2 xml_ns xml_attr xml_root
#' @noRd
eml_locate_schema <- function(eml, ns = NA) {

  if (!is(eml, 'xml_document')) {
    stop("Argument is not an instance of an
         XML document (xml2::xml_document)")
  }
  namespace <- xml2::xml_ns(eml)
  stopifnot(is(namespace, 'xml_namespace'))

  root_schema <- guess_root_schema(eml)

  # Handle special case where root schema is stmml
  if (root_schema$module == "stmml") {
    schema <-
      system.file(paste0("xsd/stmml/", root_schema$module, "-", root_schema$version, ".xsd"),
                  package = 'emld')
  # Handle sub-module case
  } else if (root_schema$module != "eml") {
    schema <-
      system.file(paste0("xsd/eml-", root_schema$version, "/eml-", root_schema$module, ".xsd"),
                  package = 'emld')
  # Handle EML documents themselves
  } else {
    schema <-
      system.file(paste0("xsd/eml-", root_schema$version, "/", root_schema$module, ".xsd"),
                  package = 'emld')
  }

  if (schema == '') {
    stop(paste("No schema found for namespace: ", ns))
  }
  return(schema)
}
