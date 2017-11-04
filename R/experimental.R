## Should we modify the as_list recursion, or work on the JSON?


#' @importFrom stats setNames
group_repeated_key <- function(out){
  ## Note: does not preserve ordering of keys
  property <- names(out)
  duplicate <- duplicated(property)
  if(sum(duplicate) > 0){
    for(p in unique(property[duplicate])){
      orig <- out
      i <- names(out) == p
      out <- out[!i]
      out <- c(out, setNames(list(unname(orig[i])), p))
    }
  }
out
}



ungroup_repeated_key <- function(){}



## make xml attributes into json keys with prefix, and reverse
attribute_to_property <- function(){}
attribute_form_property <- function(){}

## Add and remove Type declarations
## All JSON-LD nodes should be explicitly typed; though this isn't required.
## Should use Types from the Schema itself

## JSON keys should reflect the order of the frame
## Notes: Sometimes this will be unnecessary, order doesn't always matter.
##        Sometimes more tricky: e.g. classes that can have properties A or B, but not both.
order_by_frame <- function(){}


