R Notebook
================

``` r
library(jsonld)
library(jsonlite)
library(tidyverse)
library(printr)
```

Switching Contexts: Expansion & Compaction
------------------------------------------

-   Good: homonyms (same word has different meanings), synonyms (different word has ~ same meaning)
-   Fails: Mapping isn't 1:1

### A simple example

``` r
ex <- 
 '{ 
      "date": "1993-10-02",
      "site": "N654",
      "scientificName": "Picea rubens",
      "area": 2,
      "count": 26
 }'
```

No need to alter your own data files/source data files. Simply define a context that maps those terms into the reference vocabulary. Data can use whatever fields it likes.

``` r
map <- 
'{ 
  "@context": {
    "@vocab": "http://example.com/",
    "scientificName": "http://example.com/species"
  }
}'
target <- '{"@context": {"@vocab": "http://example.com/"}}'
add_context(ex, map) %>% 
  jsonld_compact(target)
```

    ## {
    ##   "@context": {
    ##     "@vocab": "http://example.com/"
    ##   },
    ##   "area": 2,
    ##   "count": 26,
    ##   "date": "1993-10-02",
    ##   "site": "N654",
    ##   "species": "Picea rubens"
    ## }

### Using tabular schema

``` r
df <- 
tribble(
  ~Date,       ~Site, ~Species, ~Area, ~Count,
  "10/1/1993", "N654", "PIRU",   2.0,     26,
  "10/3/1994", "N654", "PIRU",   2.0,     29,
  "10/1/1993", "N654", "BEPA",   1.0,     3
)

map <- 
'{ 
  "@context": {
    "@vocab": "http://example.com/",
    "Species": "http://example.com/scientificName"
  }
}'
target <- '{"@context": {"@vocab": "http://example.com/"}}'

df %>% 
  toJSON() %>% 
  add_context(map) %>% 
  jsonld_compact(target) %>% 
  drop_context() %>%
  fromJSON()
```

|     Area|     Count| Date         | Site    | scientificName      |
|--------:|---------:|:-------------|:--------|:--------------------|
|        2|        26| 10/1/1993    | N654    | PIRU                |
|        2|        29| 10/3/1994    | N654    | PIRU                |
|        1|         3| 10/1/1993    | N654    | BEPA                |
|  Wow doe|  s that s| eem like a r | ound-ab | out way to just do: |

``` r
df %>% rename(scientificName = Species)
```

| Date      | Site | scientificName |  Area|  Count|
|:----------|:-----|:---------------|-----:|------:|
| 10/1/1993 | N654 | PIRU           |     2|     26|
| 10/3/1994 | N654 | PIRU           |     2|     29|
| 10/1/1993 | N654 | BEPA           |     1|      3|

and it is. But a few things are worth noting:

-   Robust to what columns are actually present. Instead of a default vocabulary we can map all terms explicitly.
-   Terms not expanded by the map (that is, not part of our context) will be dropped. (`Count` in the example below)
-   Terms not in our target context remain as external keys, and are not compacted.
-   Typesafe transforms. Terms (keys/column names) with the wrong type will not compact if the types do not match the target transform. This way they are not actually lost, but cannot be confused.

``` r
map <- 
'{ 
  "@context": {
    "ex": "http://example.com/",
    "Species": "ex:scientificName",
    "Site": "ex:Site",
    "Date": "ex:Date",
    "Area": "ex:Area"
  }
}'

target <- '{
"@context": {
  "ex": "http://example.com/",
  "scientificName": "ex:scientificName",
  "Site": "ex:Site",
  "Count": "ex:count",
  "Date": {"@id": "ex:Date", "@type": "ex:Date"}
}}'

df %>% 
  toJSON() %>% 
  add_context(map) %>% 
  jsonld_compact(target) %>% 
  drop_context() %>%
  fromJSON() 
```

|  ex:Area| ex:Date   | Site | scientificName |
|--------:|:----------|:-----|:---------------|
|        2| 10/1/1993 | N654 | PIRU           |
|        2| 10/3/1994 | N654 | PIRU           |
|        1| 10/1/1993 | N654 | BEPA           |

``` r
map <- '{ "@context": { "@vocab": "https://data.berkeley.edu/" }}'

target <- '{
"@context": {
  "ucb": "https://data.berkeley.edu/",
  "hav": "http://data.harvard.edu/",
  "scientificName": "ucb:Species",
  "Site": "ucb:Site",
  "Count": "ucb:Count",
  "Date": "ucb:Date",
  "Area": "ucb:Area",
  "Density": "hav:density"
}}'

df %>% 
  toJSON() %>% 
  add_context(map) %>% 
  jsonld_compact(target) %>% 
  drop_context() %>%
  fromJSON() -> ucsb_df

ucsb_df %>% mutate(density = Count / Area)
```

|  Area|  Count| Date      | Site | scientificName |  density|
|-----:|------:|:----------|:-----|:---------------|--------:|
|     2|     26| 10/1/1993 | N654 | PIRU           |     13.0|
|     2|     29| 10/3/1994 | N654 | PIRU           |     14.5|
|     1|      3| 10/1/1993 | N654 | BEPA           |      3.0|

------------------------------------------------------------------------

Integration: Flattening shares id
---------------------------------

``` r
context <- '{   "@context": { "@vocab": "http://example.com/" } }'

ex <- '{
  "@context": { "@vocab": "http://example.com/" },
  "datasets": [
    {
      "@id": "http://global_unique_id",
      "@type": "dataset",
      "image": "http://image_uri",
      "name": "stuff"
    },
  
    {
      "@type": "dataset",
      "@id": "http://global_unique_id",
      "name": "Species name"
    }
  ]
}'

jsonld_flatten(ex, context)
```

    ## {
    ##   "@context": {
    ##     "@vocab": "http://example.com/"
    ##   },
    ##   "@graph": [
    ##     {
    ##       "@id": "_:b0",
    ##       "datasets": {
    ##         "@id": "http://global_unique_id"
    ##       }
    ##     },
    ##     {
    ##       "@id": "http://global_unique_id",
    ##       "@type": "dataset",
    ##       "image": "http://image_uri",
    ##       "name": [
    ##         "stuff",
    ##         "Species name"
    ##       ]
    ##     }
    ##   ]
    ## }

------------------------------------------------------------------------

hdf5: column data, with units, metadata
