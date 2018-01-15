#' as_emld
#'
#' Parse an EML file into an emld object.
#' @param x path to an EML file
#' @importFrom xml2 read_xml xml_find_all xml_remove
#' @importFrom methods is
#' @importFrom jsonld jsonld_compact jsonld_frame
#'
#' @export
as_emld <- function(x){
  ## Read json or xml files, based on extension
  if(is.character(x)){
    if(file.exists(x)){
      if(grepl("\\.xml$", x)){
        x <- xml2::read_xml(x)
      } else if(grepl("\\.json$", x)){
        ## read_json returns list, not json object
        x <- jsonlite::read_json(x)
        x <- jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)
      } else {
        stop(paste("extension for", basename(x), "not recognized"))
      }
    }
  }

  ### FROM JSON FILES ###

  ## Convert json or xml_document to the S3 emld object
  if(is(x, "json")){

    ## FIXME technically this assumes our context
    frame <- system.file("frame/eml-frame.json", package = "emld")
    context <- system.file("context/eml-context.json", package = "emld")
    framed <- jsonld::jsonld_frame(x, frame)
    compacted <- jsonld::jsonld_compact(framed, context)
    emld <- jsonlite::fromJSON(compacted, simplifyVector = FALSE)
    class(emld) <- c("emld", "list")
    return(emld)
  }


  ### FROM XML FILES ######
  else if(is(x, "xml_document")){
    ## Drop comment nodes
    xml2::xml_remove(xml2::xml_find_all(x, "//comment()"))

    ## Main transform, map XML to list via mod of the xml2::as_list
    emld <- as_jsonlist(x)

    ## Set a base type  Do we want to declare `@type`?
    emld <- c("@type" = "EML", emld)

    ## Set up the JSON-LD context
    if(is.null(emld[["#xmlns"]])){
      emld[["#xmlns"]] <- "eml://ecoinformatics.org/eml-2.1.1/"
    }
    emld <- add_context(emld)
    class(emld) <- c("emld", "list")

    return(emld)
  } else if(is.list(x)){ # Note that xml_document is.list too
    class(x) <- c("emld", "list")
    return(x)
  }


}



add_context <- function(json){
  ## Set up the JSON-LD context
  con <- list()
  if ("base" %in% names(json)) {
    con$`@base` <- json$base
    json$base <- NULL
  }
  # closing slash on url only if needed
  if(!is.null(json$`#xmlns`)){
    con$`@vocab` <- gsub("(\\w)$", "\\1/", json$`#xmlns`)
    json$`#xmlns` <- NULL
  }
  nss <- json[grepl("#xmlns\\:", names(json))]
  con <- c(con,
           stats::setNames(gsub("(\\w)$", "\\1/", nss),
                           vapply(names(nss),
                                  function(x)
                                    strsplit(x, split = ":")[[1]][[2]],
                                  character(1))
           )
  )
  xmlns <- grepl("^#xmlns", names(json))
  json <- json[!xmlns]
  json$`@context` <- con

  # order names so @context shows up first
  json <- json[order(names(json))]

  ## Add context defining @id types
  json
}

