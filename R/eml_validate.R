

#' eml_validate
#'
#' eml_validate processes an EML document using the XSD schema for the
#' appropriate version of EML and determines if the document is schema-valid
#' as defined by the XSD specification
#' @param eml file path, xml_document,
#' @param encoding optional encoding for files, default UTF-8.
#' @param ... additional arguments to eml_write, such as namespaces
#' @param schema path to schema
#' @param eml_version the version of the EML to validate against, see [eml_version()]
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
                         schema = NULL,
                         version = eml_version()) {

  doc <- generalized_parser(eml, encoding = encoding)


  # Use the EML namespace to find the EML version and the schema location
  if (is.null(schema)) {
    try(schema <- eml_locate_schema(doc, version = version))
  }
  schema_doc <- xml2::read_xml(schema)

  ## Only do full validation if schemaLocation is eml.xsd and not
  ## merely a subclass
  full <- grepl(".*eml.xsd$", schema)

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
  # unless the containing annotation element contains a references attribute
  annot_without_ref <- xml2::xml_find_all(doc, "//annotation[not(references)]/..")
  if(any(lapply(xml_attrs(annot_without_ref, "id"), length) == 0))
    error_log <- c(error_log,
                   paste("parent of any annotation must have id",
                         "unless annotation contains a references attribute"))

  ## If annotation has a references, parent cannot have ID
  id_and_annotation <- xml2::xml_find_all(doc, "//*[@id]/annotation")
  annotation_children <- xml2::xml_name(xml2::xml_children(id_and_annotation))
  if("references" %in% annotation_children)
    error_log <- c(error_log, "Annotation elements with ids cannot contain references elements")

  # ID attributes must be unique
  id <- xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
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
   references <- xml2::xml_text(xml2::xml_find_all(doc, "//references"), trim = TRUE)
   if(!all(references %in% id))
     error_log <- c(error_log, "not all 'references' values match defined id attributes")

  # If no validity errors are found above or by the parser, then the document is valid
  if(length(error_log) == 0)
    result <- TRUE
  else {
    warning(paste("Document is invalid. Found the following errors:\n", error_log))
    result <- FALSE
  }
  attr(result, "errors") <- error_log
  result
}


#' eml_locate_schema
#'
#' eml_locate_schema returns the location of the XSD schema file for a given
#' EML document, as shipped with the EML R package.
#'
#' @details The schema version must be given as an argument, or will be inferred
#' from [eml_version()].
#'
#' @param eml an xml2::xml_document instance for an EML document
#' @param ns the namespace URI for the top (root) element
#' @param version the version of the EML to validate against, see [eml_version()]
#' @return fully qualified path to the XSD schema for the appropriate version of EML
#'
#' @examples \donttest{
#' f <- system.file("extdata", "example.xml", package = "emld")
#' eml <- xml2::read_xml(f)
#' schema <- eml_locate_schema(eml)
#' }
#' @importFrom xml2 xml_ns xml_attr xml_root
#' @noRd
eml_locate_schema <- function(eml, ns = NA, version = eml_version()) {


  if (!is(eml, 'xml_document')) {
    stop("Argument is not an instance of an
         XML document (xml2::xml_document)")
  }
  namespace <- xml2::xml_ns(eml)
  stopifnot(is(namespace, 'xml_namespace'))

  ## If we have a schemaLocation element, we parse it only to see
  ## if we're using the full eml.xsd or a submodule (e.g. dataset.xml)
  ## Otherwise, we default to full `emld.xsd`
  schemaLocation <- xml2::xml_attr(xml2::xml_root(eml),
                 "schemaLocation")

  if(is.na(schemaLocation)){
    schema_file <- "eml.xsd"
  } else {
    schemaLocation <- strsplit(schemaLocation,"\\s+")[[1]]
    schema_file <- basename(schemaLocation[2])
  }
  if(is.na(schema_file))
    schema_file <- "eml.xsd"



  ## We alays validate against the package's local copies of the schema and not
  ## the one that is given in schemaLocation attribute
  schema <-
    system.file(paste0("xsd/", version, "/", schema_file),
                package = 'emld')

  schema
}
