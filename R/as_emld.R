#' as_emld
#'
#' Parse an EML file into an emld object.
#' @param x path to an EML file
#' @importFrom xml2 read_xml xml_find_all xml_remove
#' @importFrom methods is
#' @importFrom jsonld jsonld_compact jsonld_frame
#'
#' @export
as_emld <- function(x)
{
  UseMethod("as_emld")
}

#' @export
as_emld.character <- function(x){
  ## Read json or xml files, based on extension
    if(file.exists(x)){
      if(grepl("\\.xml$", x)){
        x <- xml2::read_xml(x)
        as_emld.xml_document(x)
      } else if(grepl("\\.json$", x)){
        ## read_json returns list, not json object
        x <- jsonlite::read_json(x)
        x <- jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)
        as_emld.json(x)
      } else {
        stop(paste("extension for", basename(x), "not recognized"))
      }
    } else {
      ## what other kind of character string examples do we expect other than filenames?

    as_emld.list(as.list(x))
    }
}


### FROM JSON FILES ###
#' @export
as_emld.json <- function(x){
  ## Convert json or xml_document to the S3 emld object

    ## FIXME technically this assumes only our context
    frame <- system.file(paste0("frame/",
                              getOption("emld_db", "eml-2.2.0"),
                              "/eml-frame.json"), package = "emld")
    context <- system.file(paste0("context/",
                           getOption("emld_db", "eml-2.2.0"),
                           "/eml-context.json"), package = "emld")
    framed <- jsonld::jsonld_frame(x, frame)
    compacted <- jsonld::jsonld_compact(framed, context)
    emld <- jsonlite::fromJSON(compacted, simplifyVector = FALSE)
    class(emld) <- c("emld", "list")
    emld
}


### FROM XML FILES ######
#' @export
as_emld.xml_document <- function(x){

    ## Drop comment nodes
    xml2::xml_remove(xml2::xml_find_all(x, "//comment()"))

    ## Main transform, map XML to list via mod of the xml2::as_list
    emld <- as_jsonlist(x)

    ## Set a base type  Do we want to declare `@type`?
    emld <- c("@type" = "EML", emld)

    ## Set up the JSON-LD context
    if(is.null(emld[["xmlns"]])){
      emld[["xmlns"]] <- paste0("eml://ecoinformatics.org/",
                                getOption("emld_db", "eml-2.2.0"),"/")
    }
    emld <- add_context(emld)
    class(emld) <- c("emld", "list")

    emld
  }

## CHECKME xml_document and json are also list!
#' @export
as_emld.list <- function(x){
  # Note that xml_document is.list too
    class(x) <- c("emld", "list")
    return(x)
}



add_context <- function(json){
  ## Set up the JSON-LD context
  context <- system.file(paste0("context/",
                                getOption("emld_db", "eml-2.2.0"),
                                "/eml-context.json"), package = "emld")
  con <- jsonlite::read_json(context)[["@context"]]
  if ("base" %in% names(json)) {
    con$`@base` <- json$base
    json$base <- NULL
  }
  # closing slash on url only if needed
  if(!is.null(json$`#xmlns`)){
    con$`@vocab` <- gsub("(\\w)$", "\\1/", json$`#xmlns`)
    json$`xmlns` <- NULL
  }
  nss <- json[grepl("xmlns\\:", names(json))]
  additional_ns <-
           stats::setNames(gsub("(\\w)$", "\\1/", nss),
                           vapply(names(nss),
                                  function(x)
                                    strsplit(x, split = ":")[[1]][[2]],
                                  character(1))
           )
  taken <- which(names(additional_ns) %in% names(con))
  if(length(taken)>0)
    additional_ns <- additional_ns[-taken]
  con <- c(con, additional_ns)

  xmlns <- grepl("^xmlns", names(json))
  json <- json[!xmlns]
  json$`@context` <- con

  # order names so @context shows up first
  json <- json[order(names(json))]

  json
}

