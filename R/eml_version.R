

#' Set or check the EML version default
#'
#' @param version EML version, currently either eml-2.2.0 (current version), or
#' eml-2.1.1
#' @return returns the EML version string. As a side-effect, sets the
#' requested version as the default version by setting the `emld_db`
#' variable in [options()].
#' @examples
#' eml_version()
#'
#' @export
eml_version <- function(version = getOption("emld_db", "eml-2.2.0")){
 options(emld_db = version)
 version
}

#' Get the XML namespace for a version of EML
#'
#' Utility function for use when filling in `xmlns`, `schemaLocation`, or
#' `vocab` in various representations of EML. This is a little more future-proof
#' than keeping a dictionary for each version since this won't break on the next
#'release.
#'
#' @param version EML version, currently either eml-2.2.0 (current version) or
#'eml-2.1.1. Defaults to current version.
#'
#' @return returns the full XML namespace URI for the specified version of the
#' schema
eml_ns <- function(version = eml_version()) {
  prefix <- "https://eml.ecoinformatics.org/"

  parts <- tryCatch({
    as.numeric(strsplit(strsplit(version, "-")[[1]][2], "\\.")[[1]])
  },
  error = function(e) {
    warning("Failed to parse output of eml_version().",
      " Defaulting to https://eml.ecoinformatics.org.")
  })

  # Use eml:// form for schema versions equal to or prior to 2.2
  if (parts[1] <= 2 && parts[2] < 2) {
    prefix <- "eml://eml.ecoinformatics.org/"
  }

  paste0(prefix, version)
}

# Helper string used below to factor out the host from the generation of
# schemaLocation values
eml_host <- "https://eml.ecoinformatics.org"

#' Get an appropriate `schemaLocation` value for a given version of the schema
#'
#' This is a simple helper to make filling in the `schemaLocation` attribute
#' on documents this package creates.
#'
#' @param version Optional. Override the version of the schema.
#' @param url Override the URL portion of the `schemaLocation` value.
#'
#' @return Returns a string suitable as a value for `schemaLocation`.
#' @examples
#' \dontrun{
#' # Get an appropriate schemaLocation value for the current version fo EML
#' eml_schema_location()
#'
#' # Get an appropriate value for EML 2.1.1
#' eml_schema_location("eml-2.1.1")
#'
#' # Set `schemaLocation` to a custom URL of your choosing
#' eml_schema_location(url = "https://example.org/eml.xsd")
#' }
eml_schema_location <- function(version = eml_version(), url = NULL) {
  # Fill in a default value if `url` is not set
  if (is.null(url)) {
    url <- paste(eml_host, version, "eml.xsd", sep = "/")
  }

  paste(eml_ns(version), url)
}
