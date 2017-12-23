
[![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/joethorley/stability-badges#experimental) [![Travis-CI Build Status](https://travis-ci.org/cboettig/emld.svg?branch=master)](https://travis-ci.org/cboettig/emld) [![Coverage Status](https://img.shields.io/codecov/c/github/cboettig/emld/master.svg)](https://codecov.io/github/cboettig/emld?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
emld
====

The goal of emld is to provide a way to work with EML metadata in the JSON-LD format.

Installation
------------

You can install emld from github with:

``` r
# install.packages("devtools")
devtools::install_github("cboettig/emld")
```

**Work in progress** The outline below illustrates things we can do or will be able to do with this package. More examples forthcoming soon.

``` r
library(emld)
library(jsonlite)
library(magrittr)
```

Reading EML
-----------

`emld` excels at reading, extracting, and manipulating existing EML files.

### Parse & serialize

We can parse a simple example and manipulate is as a familar list object (S3 object):

``` r
f <- system.file("extdata/example.xml", package="emld")
eml <- parse_eml(f)
eml$dataset$title
#> [1] "Data from Cedar Creek LTER on productivity and species richness\n  for use in a workshop titled \"An Analysis of the Relationship between\n  Productivity and Diversity using Experimental Results from the Long-Term\n  Ecological Research Network\" held at NCEAS in September 1996."
```

``` r
eml$dataset$title <- "A new title"

## fixme replace with a simpler serialize function(?)
toJSON(eml, auto_unbox = TRUE) %>%
json_to_xml("test.xml")
```

Prove this is still valid

``` r
EML::eml_validate("test.xml")
#> [1] TRUE
#> attr(,"errors")
#> character(0)
unlink("test.xml")
```

### Flatten and Compact.

We can flatten it, so we don't have to do quite so much subsetting. When we're done editing, we can compact it back into valid EML.

### Query

We can query it with SPARQL, a rich, semantic way to extract data from one or many EML files.

We can query it with JQ, a simple and powerful query language that gives us a lot of flexibility over the return structure of our results:

Writing EML
-----------

We can also create EML from scratch using lists with help from the `template` function.

Let's create a minimal EML document

``` r
eml <- template("eml")
eml
#> access: ~
#> dataset: ~
#> citation: ~
#> software: ~
#> protocol: ~
#> additionalMetadata: ~
#> packageId: ~
```

This gives us an idea of the possible elements (unfortunately it does not currently indicate which are required, optional, or mutually exclusive). We would also see these listed as tab-completion options if we type `eml$` using the usual list indexing mechanism.

Let's go ahead and get a dataset template as well.

``` r
dataset <- template("dataset")
```

I know what you're thinking: surely this could be done recrusively? Yes indeed, but getting all possible options will get out of control (and maybe out of memory too!) Recursive creation is safest for lower-level objects:

``` r
contact <- template("contact", recursive = TRUE)
contact
#> individualName:
#>   salutation: {}
#>   givenName: {}
#>   surName: {}
#> organizationName: {}
#> positionName: {}
#> address:
#>   deliveryPoint: {}
#>   city: {}
#>   administrativeArea: {}
#>   postalCode: {}
#>   country: {}
#> phone: {}
#> electronicMailAddress: {}
#> onlineUrl: {}
#> userId: {}
```

Let's start filling out some metadata!

``` r
contact$individualName$givenName <- "Carl"
contact$individualName$surName <- "Boettiger"
contact$organiziationName <- "UC Berkeley"
contact$electronicMailAddress <- "cboettig@ropensci.org"
```

``` r
dataset$title <- "example"
dataset$creator <- contact
dataset$contact <- contact
eml$dataset <- dataset

eml
#> access: ~
#> dataset:
#>   alternateIdentifier: ~
#>   shortName: ~
#>   title: example
#>   creator:
#>     individualName:
#>       salutation: {}
#>       givenName: Carl
#>       surName: Boettiger
#>     organizationName: {}
#>     positionName: {}
#>     address:
#>       deliveryPoint: {}
#>       city: {}
#>       administrativeArea: {}
#>       postalCode: {}
#>       country: {}
#>     phone: {}
#>     electronicMailAddress: cboettig@ropensci.org
#>     onlineUrl: {}
#>     userId: {}
#>     organiziationName: UC Berkeley
#>   metadataProvider: ~
#>   associatedParty: ~
#>   pubDate: ~
#>   series: ~
#>   abstract: ~
#>   keywordSet: ~
#>   additionalInfo: ~
#>   intellectualRights: ~
#>   distribution: ~
#>   coverage: ~
#>   purpose: ~
#>   maintenance: ~
#>   contact:
#>     individualName:
#>       salutation: {}
#>       givenName: Carl
#>       surName: Boettiger
#>     organizationName: {}
#>     positionName: {}
#>     address:
#>       deliveryPoint: {}
#>       city: {}
#>       administrativeArea: {}
#>       postalCode: {}
#>       country: {}
#>     phone: {}
#>     electronicMailAddress: cboettig@ropensci.org
#>     onlineUrl: {}
#>     userId: {}
#>     organiziationName: UC Berkeley
#>   publisher: ~
#>   pubPlace: ~
#>   methods: ~
#>   project: ~
#>   dataTable: ~
#>   spatialRaster: ~
#>   spatialVector: ~
#>   storedProcedure: ~
#>   view: ~
#>   otherEntity: ~
#> citation: ~
#> software: ~
#> protocol: ~
#> additionalMetadata: ~
#> packageId: ~
#json_to_xml(eml, "eml.xml")
```
