

#' eml_validate
#'
#' eml_validate processes an EML document using the XSD schema for the
#' appropriate version of EML and determines if the document is schema-valid
#' as defined by the XSD specification
#' @param eml file path, xml_document,
#' @param encoding optional encoding for files, default UTF-8.
#' @param ... additional arguments to eml_write, such as namespaces
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
#'  eml <- read_eml(f)
#'  eml_validate(eml)
#'
#' }
#'
#' @export
#' @importFrom xml2 read_xml xml_validate
#' @importFrom methods is
#' @importFrom emld as_xml
eml_validate <- function(eml,
                         encoding = "UTF-8",
                         schema = NULL) {
  if (is.character(eml)) {
    if (file.exists(eml)) {
      doc <- xml2::read_xml(eml, encoding = encoding)
    }
  } else if (is(eml, "xml_document")) {
    doc <- eml
  } else if (is(eml, "list")){
    ##  FIXME shouldn't have to write to tempfile,
    ## but  `doc <- emld::as_xml(eml)` fails to drop xsi prefix on "schemaLocation"
    f <- tempfile()
    x <- emld::as_xml(eml, f)
    doc <- xml2::read_xml(f)
    unlink(f)
  } else {
    stop(paste("Did not recognize eml object with class", class(eml)))
  }

  # Use the EML namespace to find the EML version and the schema location
  if (is.null(schema)) {
    try(schema <- eml_locate_schema(doc))
  }
  schema_doc <- xml2::read_xml(schema)
  result <- tryCatch(
    xml2::xml_validate(doc, schema_doc),
    error = function(e) {
      warning("The document could not be validated.")
      list(status = 1,
           errors = c(NULL),
           warnings = c(e))
    }
  )

  result
}

#' eml_locate_schema
#'
#' eml_locate_schema returns the location of the XSD schema file for a given
#' EML document, as shipped with the EML R package.
#'
#' @details The schema location is based on the last path component from the EML
#' namespace (e.g., eml-2.1.1), which corresponds to the directory containing xsd
#' files that ship with the EML package. Schema files are copies of the schemas
#' from the EML versioned releases. If an appropriate schema is not found,
#' the function returns FALSE.
#'
#' @param eml an xml2::xml_document instance for an EML document
#' @param ns the namespace URI for the top (root) element
#' @return fully qualified path to the XSD schema for the appropriate version of EML
#'
#' @examples \donttest{
#' f <- system.file("examples", "example-eml-2.1.1.xml", package = "EML")
#' eml <- xml2::read_xml(f)
#' schema <- eml_locate_schema(eml)
#' }
#' @importFrom xml2 xml_ns xml_attr xml_root
#' @export
eml_locate_schema <- function(eml, ns = NA) {


  if (!is(eml, 'xml_document')) {
    stop("Argument is not an instance of an
         XML document (xml2::xml_document)")
  }
  namespace <- xml2::xml_ns(eml)
  stopifnot(is(namespace, 'xml_namespace'))

  schemaLocation <- strsplit(
    xml2::xml_attr(xml2::xml_root(eml),
                   "schemaLocation"),
                             "\\s+")[[1]]
  schema_file <- basename(schemaLocation[2])

  ##
  if (is.na(ns)) {
    i <- grep(schemaLocation[1], namespace)
    if (length(i) == 0)
      i <- 1
    ns <- namespace[i]
  }

  eml_version <- strsplit(ns, "-")[[1]][2]
  schema <-
    system.file(paste0("xsd/eml-", eml_version, "/", schema_file),
                package = 'eml2')
  if (schema == '') {
    stop(paste("No schema found for namespace: ", ns))
  }
  return(schema)
  }
