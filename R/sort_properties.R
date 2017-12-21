
# FIXME create db of all slotNames for all classes, then we can get rid of the hard EML dependency

sort_properties <- function(x, tag){

  n <- names(x)
  requireNamespace("EML")
  order <- methods::slotNames(tag)
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

ld <- parse_eml(system.file("extdata/hf205.xml", package="emld"))
names(sort_properties(ld$eml$dataset, "dataset"))
#names(sort_properties(ld$eml$access, "access"))
