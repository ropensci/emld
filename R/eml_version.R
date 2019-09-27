

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
