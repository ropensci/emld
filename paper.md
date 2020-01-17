---
title: 'Ecological Metadata as Linked Data'
tags:
 - linked data
 - EML
 - JSON-LD
 - RDF
 - Ecology
 
authors:
 - name: Carl Boettiger
   orcid: 0000-0002-1642-628X
   affiliation: 1
affiliations:
 - name: University of California, Berkeley
   index: 1
date: 2019-01-25
bibliography: paper.bib
---

The Ecological Metadata Language, (EML), is a widely used metadata
standard in the ecological and environmental sciences \[@Michener1997;
@Jones2006; @Leinfelder2010\]. Efforts such as the Long Term Ecological
Research (LTER) and the National Center for Ecological Analysis and
Synthesis (NCEAS) have used and driven the development of EML over the
past two decades, and now major efforts such as the NSF-sponsored
National Ecological Observatory Network (NEON), the DataONE Network, are
making an unprecedented amount of ecological data available with rich,
machine-readable metadata descriptions using EML \[@NEON; @Keller2008;
@Overpeck2011\]. EML defines a strict XML schema that ensures metadata
are machine readable and inter-operable through an XML-schema validation
process.

While this provides a predictable but extensible format, it also poses a
significant technical barrier to many researchers seeking to analyze
existing EML documents or create their own. The creation of EML in the
late 1990s also pre-dated the advent of more developer-friendly and
user-friendly technologies such as the popular JSON serialization, as
well as the advent of semantic or linked data model and it’s application
in ecoinformatics \[@Michener2012\]. More recently still, the creation
of the W3C JSON-LD \[@jsonld-w3c\] has brought combined the
developer-friendly JSON format with the powerful semantics of the
Resource Description Format (RDF) popular in informatics \[@rdf-w3c\].
This package seeks to bring both of these advantages to bear on the
rich-but-clunky XML structure used by EML. By automatically mapping EML
into JSON-LD, we are able to realize the benefits easier creation and
manipulation of EML structures as JSON objects or R list objects, while
relying on the power of JSON-LD semantics (in particular, context,
compaction and framing, \[@jsonld\]) to transform that data to and from
EML-schema-compliant XML. This approach opens numerous research
applications, ranging from the simple parsing or creation of EML files
as R list objects to complex queries and semantic reasoning over large
numbers of EML documents with semantic SPARQL queries \[@sparql-w3c\].
The package vignette provides scripted examples of each of these.

``` r
library(emld)
library(jsonlite)
library(magrittr) # for pipes
library(jqr)      # for JQ examples only
library(rdflib)   # for RDf examples only
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

cat(eml$dataset$title)
```

    ## Data from Cedar Creek LTER on productivity and species richness
    ##   for use in a workshop titled "An Analysis of the Relationship between
    ##   Productivity and Diversity using Experimental Results from the Long-Term
    ##   Ecological Research Network" held at NCEAS in September 1996.

## Writing EML

Because `emld` objects are just nested lists, we can create EML just by
writing
lists:

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
EML the `creator` becomes listed before `contact`. Of course this is a
very low-level interface that does not help the user know what an EML
looks like. Creating EML from scratch without knowledge of the schema is
a job for the `EML` package and beyond the scope of the lightweight
`emld`.

# Working with EML as JSON-LD

For many applications, it is useful to merely treat EML as a list
object, as seen above, allowing the R user to leverage a standard tools
and intuition in working with these files. However, `emld` also opens
the door to new possible directions by thinking of EML data in terms of
a JSON-LD serialization rather than an XML serialization. First, owing
to it’s comparative simplicity and native data typing (e.g. of
Boolean/string/numeric data), JSON is often easier for many developers
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
hf205 <- system.file("extdata/hf205.xml", package="emld")

as_emld(hf205) %>% 
  as_json() %>% 
  jq('.dataset.coverage.geographicCoverage.boundingCoordinates | 
       { northLat: .northBoundingCoordinate, 
         southLat: .southBoundingCoordinate }') %>%
  fromJSON()
```

    ## $northLat
    ## [1] "+42.55"
    ## 
    ## $southLat
    ## [1] "+42.42"

Nice features of JQ include the ability to do recursive descent (common
to XPATH but not possible in `purrr`) and specify the shape and names of
the return object (e.g. as a list with elements named `northLat` and
`southLat` in this case.)

## As semantic data: SPARQL queries

Another side-effect of the JSON-LD representation is that we can treat
EML as *semantic* data. This can provide a way to integrate EML records
with other data sources, and means we can query the EML using semantic
SPARQL queries. One nice thing about SPARQL queries is that, in contrast
to XPATH, JQ, or other graph queries, SPARQL always returns a
`data.frame` – a particularly convenient and familiar format for R
users.

First, we render the EML XML document into JSON-LD file:

``` r
f <- system.file("extdata/hf205.xml", package="emld")
as_emld(f) %>%
  as_json("hf205.json")
```

We can now construct a SPARQL query. SPARQL queries look like SQL
queries in that we name the columns we want with a `SELECT` command.
Unlike SQL, SPARQL allows us to walk the graph by treating these names
as variables, indicated by prefixing a `?` to the variable name. We then
use a WHERE block to define how these variables relate to each other. In
this case, we ask for the genus and species name and bounding box found
in the EML file.

``` r
sparql <-
  'PREFIX eml: <https://eml.ecoinformatics.org/eml-2.2.0/>

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
```

We can now use the `rdflib` library to execute this SPARQL query on the
EML document and display the resulting `data.frame`:

``` r
rdf <- rdf_parse("hf205.json", "jsonld")
df <- rdf_query(rdf, sparql)
df
```

    ## # A tibble: 1 x 6
    ##   genus      species  northLat southLat eastLong westLong
    ##   <chr>      <chr>       <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 Sarracenia purpurea     42.6     42.4    -72.1    -72.3

# References
