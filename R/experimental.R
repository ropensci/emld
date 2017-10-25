
library(xml2)
library(jsonlite)

input <-
'<node>
<key>value1</key>
<key>value2</key>
<key>value3</key>
<other>stuff</other>
</node>'

output <-
'{
"key" = ["value", "value2","value3"],
"other" = "stuff"
}'

in_list <- as_list(read_xml(input))

group_repeated_key <- function(){}
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


