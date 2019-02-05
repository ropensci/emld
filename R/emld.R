#' emld: Ecological Metadata as Linked Data
#'
#' The goal of emld is to provide a way to work with EML metadata
#' in the JSON-LD format. At it's heart, the package is simply a
#' way to translate an EML XML document into JSON-LD and be able
#' to reverse this so that any semantically equivalent JSON-LD
#' file can be serialized into EML-schema valid XML.
#'
#' The package has only three core functions:
#'
#' - [as_emld()] Convert EML's `xml` files (or the `json` version created
#'   by this package) into a native R object (an S3 class called `emld`,
#'   essentially just a `list`).
#' - [as_xml()] Convert the native R format, `emld`, back into
#'   XML-schema valid EML.
#' - [as_json()] Convert the native R format, `emld`, into `json`(LD).
#'
"_PACKAGE"
