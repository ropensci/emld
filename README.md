
[![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/joethorley/stability-badges#experimental)
[![Travis-CI Build
Status](https://travis-ci.org/cboettig/emld.svg?branch=master)](https://travis-ci.org/cboettig/emld)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/cboettig/emld?branch=master&svg=true)](https://ci.appveyor.com/project/cboettig/emld)
[![Coverage
Status](https://img.shields.io/codecov/c/github/cboettig/emld/master.svg)](https://codecov.io/github/cboettig/emld?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/emld)](https://cran.r-project.org/package=emld)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# emld

The goal of emld is to provide a way to work with EML metadata in the
JSON-LD format. At it’s heart, the package is simply a way to translate
an EML XML document into JSON-LD and be able to reverse this so that any
semantically equivalent JSON-LD file can be serialized into EML-schema
valid XML. The package has only three core functions:

  - `as_emld()` Convert EML’s `xml` files (or the `json` version created
    by this package) into a native R object (an S3 class called `emld`,
    essentially just a `list`).
  - `as_xml()` Convert the native R format, `emld`, back into XML-schema
    valid EML.
  - `as_json()` Convert the native R format, `emld`, into `json`(LD).

## Installation

You can install emld from github with:

``` r
# install.packages("devtools")
devtools::install_github("cboettig/emld")
```

## Motivation

In contrast to the existing [EML
package](https://ropensci.github.io/EML), this package aims to a very
light-weight implementation that seeks to provide both an intuitive data
format and make maximum use of existing technology to work with that
format. In particular, this package emphasizes tools for working with
linked data through the JSON-LD format. This package is not meant to
replace `EML`, as it does not support the more complex operations found
in that package. Rather, it provides a minimalistic but powerful way of
working with EML documents that can be used by itself or as a backend
for those complex operations. The next release of the EML R package will
use `emld` under the hood.

Note that the JSON-LD format is considerably less rigid than the EML
schema. This means that there are many valid, semantically equivalent
representations on the JSON-LD side that must all map into the same or
nearly the same XML format. At the extreme end, the JSON-LD format can
be serialized into RDF, where everything is flat set of triples
(e.g. essentially a tabular representation), which we can query
directly with semantic tools like SPARQL, and also automatically coerce
back into the rigid nesting and ordering structure required by EML. This
ability to “flatten” EML files can be particularly convenient for
applications consuming and parsing large numbers of EML files. This
package may also make it easier for other developers to build on the
EML, since the S3/list and JSON formats used here have proven more
appealing to many R developers than S4 and XML serializations.

JSON-LD also makes it easier to extend EML with other existing semantic
vocabularies. The standard JSON-LD operations (e.g. framing, compaction)
make it easy for developers to specify desired data structures, filter
unnecessary terms and provide defaults for needed ones, or even define
custom property names, rather than working with the often cumbersome
prefixes or URIs of linked data.

``` r
library(emld)
library(jsonlite)
library(magrittr)
```

## Reading EML

The `EML` package can get particularly cumbersome when it comes to
extracting and manipulating existing metadata in highly nested EML
files. The `emld` approach can leverage a rich array of tools for
reading, extracting, and manipulating existing EML files.

We can parse a simple example and manipulate is as a familiar list
object (S3 object):

``` r
f <- system.file("extdata/example.xml", package="emld")
eml <- as_emld(f)
eml$dataset$title
#> [1] "Data from Cedar Creek LTER on productivity and species richness\n  for use in a workshop titled \"An Analysis of the Relationship between\n  Productivity and Diversity using Experimental Results from the Long-Term\n  Ecological Research Network\" held at NCEAS in September 1996."
```

## Writing EML

Becuase `emld` objects are just nested lists, we can create EML just by
writing lists:

``` r

me <- list(individualName = list(givenName = "Carl", surName = "Boettiger"))

eml <- list(dataset = list(
              title = "dataset title",
              contact = me,
              creator = me),
              system = "doi",
              packageId = "10.xxx")

as_xml(eml, "ex.xml")
testthat::expect_true(eml_validate("ex.xml") )
```

Note that we don’t have to worry about the order of the elements here,
`as_xml` will re-order if necessary to validate. (For instance, in valid
EML the `creator` becomes listed before `contact`.) Of course this is a
very low-level interface that does not help the user know what an EML
looks like. Creating EML from scratch without knowledge of the schema is
a job for the `EML` package and beyond the scope of the lightweight
`emld`.

# Working with EML as JSON-LD

For many applications, it is useful to merely treat EML as a list
object, as seen above, allowing the R user to leverage a standard tools
and intutition in workign with these files. However, `emld` also opens
the door to new possible directions by thinking of EML data in terms of
a JSON-LD serialization rather than an XML serialization. First, owing
to it’s comparative simplicity and native data typing (e.g. of
boolean/string/numeric data), JSON is often easier for many developers
to work with than EML’s native XML format.

## As JSON: Query with JQ

For example, JSON can be queried with with JQ, a [simple and powerful
query language](https://stedolan.github.io/jq/manual/) that also gives
us a lot of flexibility over the return structure of our results. JQ
syntax is both intuitive and well documented, and often easier than the
typical munging of JSON/list data using `purrr`. Here’s an example query
that turns EML to JSON and then extracts the north and south bounding
coordinates:

``` r
library(jqr)
hf205 <- system.file("extdata/hf205.xml", package="emld")

as_emld(hf205) %>% 
  as_json() %>% 
  jq('.dataset.coverage.geographicCoverage.boundingCoordinates | 
       { northLat: .northBoundingCoordinate, 
         southLat: .southBoundingCoordinate }') %>%
  fromJSON()
#> $northLat
#> [1] "+42.55"
#> 
#> $southLat
#> [1] "+42.42"
```

Nice features of JQ include the ability to do recursive descent (common
to XPATH but not possible in `purrr`) and specify the shape of the
return object. Some prototype examples of how we can use this to
translate between EML and <http://schema.org/Dataset> representations of
the same metadata can be found in
<https://github.com/cboettig/emld/blob/master/notebook/jq_maps.md>

## As semantic data: SPARQL queries

Another side-effect of the JSON-LD representation is that we can treat
EML as “semantic” data. This can provide a way to integrate EML records
with other data sources, and means we can query the EML using semantic
SPARQL queries. One nice thing about SPARQL queries is that, in contrast
to XPATH, JQ, or other graph queries, SPARQL always returns a
`data.frame` which is a particularly convenient format. SPARQL queries
look like SQL queries in that we name the columns we want with a
`SELECT` command. Unlike SQL, these names are act as variables. We then
use a WHERE block to define how these variables relate to each other.

``` r
library(rdflib)
```

``` r
f <- system.file("extdata/hf205.xml", package="emld")

as_emld(f) %>%
as_json("hf205.json")

sparql <- 
  'PREFIX eml: <eml://ecoinformatics.org/eml-2.1.1/>

  SELECT ?genus ?species ?northLat ?southLat ?eastLong ?westLong 

  WHERE { 
    ?y eml:taxonRankName "genus" .
    ?y eml:taxonRankValue ?genus .
    ?y eml:taxonomicClassification ?s .
    ?s eml:taxonRankName "species" .
    ?s eml:taxonRankValue ?species .
    ?x eml:northBoundingCoordinate ?northLat .
    ?x eml:southBoundingCoordinate ?southLat .
    ?x eml:eastBoundingCoordinate ?eastLong .
    ?x eml:westBoundingCoordinate ?westLong .
  }
'
  
rdf <- rdf_parse("hf205.json", "jsonld")
df <- rdf_query(rdf, sparql)
df
#> # A tibble: 1 x 6
#>   genus      species  northLat southLat eastLong westLong
#>   <chr>      <chr>       <dbl>    <dbl>    <dbl>    <dbl>
#> 1 Sarracenia purpurea     42.6     42.4    -72.1    -72.3
```

-----

Please note that the `emld` project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.
