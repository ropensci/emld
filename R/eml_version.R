

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
  if (missing(version) && interactive()) {
    version <- ask_eml_version()
  }

  # Warn if the user provides a version that doesn't follow the form eml-x.y.z
  if(!grepl("eml\\-[\\d\\.]+", version, perl = TRUE)) {
    warning("Your provided version of '", version, "' does not look like a valid ",
            "version string. Be sure it starts with 'eml-' and ends with the ",
            "schema version. e.g., for EML 2.1.1, use 'eml-2.1.1'.")
  }

  options(emld_db = version)
  version
}


#' Ask the user to choose an EML version
#'
#' @return An EML version string suitable for `options(emld_db)`
ask_eml_version <- function() {
  options <- c("2.1.1", "2.2.0")
  cat("Choose an EML version from the options below:\n")
  choice <- utils::menu(options)

  if (!choice %in% seq_along(options)) {
    stop("Invalid choice. Please choose one of the available options.",
         " Your EML version has not been changed.")
  }

  paste0("eml-", options[choice])
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

# Map of namespace<->schemaLocation values
schemaLocations <- list(
  "eml-2.1.1" = "https://eml.ecoinformatics.org/eml-2.1.1 https://eml.ecoinformatics.org/eml-2.1.1/eml.xsd",
  "eml-2.2.0" = "https://eml.ecoinformatics.org/eml-2.2.0 https://eml.ecoinformatics.org/eml-2.2.0/eml.xsd"
)

#' Guess an appropriate `schemaLocation` value for a given version of the schema
#'
#' This is a simple helper to make filling in the `schemaLocation` attribute
#' on documents this package creates. Supports EML 2.1.1 and newer.
#'
#' @param version Optional. Override the version of the schema. Defaults to the
#' current version returned by `eml_version`. See `eml_version` for information
#' on how to change the current version.
#'
#' @return Returns a string suitable as a value for `schemaLocation` or `NULL`
#' if a value wasn't found.
#' @examples
#' \dontrun{
#' # Get an appropriate schemaLocation value for the current version fo EML
#' guess_schema_location()
#'
#' # Get an appropriate value for EML 2.1.1
#' guess_schema_location("eml-2.1.1")
#' }
guess_schema_location <- function(version = eml_version()) {
  if (!(version %in% names(schemaLocations))) {
    return(NULL)
  }

  schemaLocations[version][[1]]
}
