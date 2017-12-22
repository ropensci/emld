
# List of all classes defined by EML and their slot names, in order compatible with schema validation
# This data is primarily intended for internal use by the sort_properties function
#
# @references \url{https://github.com/ropensci/EML}
#"eml_db"


sort_properties <- function(x, tag){

  n <- names(x)
  order <- eml_db[[tag]]
  drop <- grep("(.Data|schemaLocation|lang|slot_order|id|scope|system|authSystem)", order)
  if(length(drop) > 0)
    order <- order[-drop]
  if(length(order) == 0 | length(n) == 0)
    return(x)

  nodes <- x
  attrs <- grep("^(#|@)\\w",n)
  if(length(attrs) > 0 ){
    n <- n[-attrs]
    nodes <- x[-attrs]
  }
  if(!all(n %in% order))
    return(x)

  fixed <- names(sort(vapply(n,
             function(i) which(i == order), integer(1))))

 c(x[attrs],nodes[fixed])

}

#ld <- parse_eml(system.file("extdata/hf205.xml", package="emld"))
#names(sort_properties(ld$eml$dataset, "dataset"))
#names(sort_properties(ld$eml$access, "access"))
