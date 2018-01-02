
Mapping between Schema.Org and EML via JQ
=========================================

We will use J-Query as XSLT-like stylesheets. These two examples shipping in this package are currently a work in progress:

``` r
library(jqr)
```

``` r
eml_to_schema <- readr::read_file(system.file("jq/eml_to_schema.jq", package="emld"))
schema_to_eml <-  readr::read_file(system.file("jq/schema_to_eml.jq", package="emld"))
```

Let's map a more complete EML document into schema.org:

``` r
eml <- readr::read_file("https://raw.githubusercontent.com/cboettig/emld/master/inst/extdata/hf205.json")
jq(eml, eml_to_schema)
```

    ## {
    ##     "id": "HF205",
    ##     "type": null,
    ##     "temporalCoverage": "2012-06-01/2013-12-31",
    ##     "spatialCoverage": {
    ##         "description": "Harvard Forest Greenhouse, Tom Swamp Tract (Harvard Forest)",
    ##         "geo": {
    ##             "box": "+42.42 -72.29 +42.55 -72.10"
    ##         }
    ##     }
    ## }

Convert a dataset marked up in <http://schema.org/Dataset> terms into EML

``` r
schema <- readr::read_file("../inst/extdata/schema-org-dataset.json")

jq(schema, schema_to_eml)
```

    ## {
    ##     "@id": null,
    ##     "@type": "Dataset",
    ##     "coverage": {
    ##         "temporalCoverage": {
    ##             "rangeOfDates": {
    ##                 "beginDate": "1950-01-01",
    ##                 "endDate": "2013-12-18"
    ##             }
    ##         },
    ##         "geographicCoverage": {
    ##             "geographicDescription": null,
    ##             "boundingCoordinates": {
    ##                 "westBoundingCoordinate": "-65.0",
    ##                 "eastBoundingCoordinate": "172.0",
    ##                 "northBoundingCoordinate": "72.0",
    ##                 "southBoundingCoordinate": "18.0"
    ##             }
    ##         }
    ##     }
    ## }
