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
date: 2018-05-30
bibliography: paper.bib
---


The Ecological Metadata Language, (EML), is a widely used metadata standard in the ecological and environmental sciences [@Michener1997; @Jones2006; @Leinfelder2010]. Efforts such as the Long Term Ecological Research (LTER) and the National Center for Ecological Analysis and Synthesis (NCEAS) have used and driven the development of EML over the past two decades, and now major efforts such as the NSF-sponsored National Ecological Observatory Network (NEON), the DataONE Network, are making an unprecedented amount of ecological data available with rich, machine-readable metadata descriptions using EML  [@NEON; @Keller2008; @Overpeck2011]. EML defines a strict XML schema that ensures metadata are machine readable and inter-operable through an XML-schema validation process.  

While this provides a predictable but extensible format, it also poses a significant technical barrier to many researchers seeking to analyze existing EML documents or create their own. The creation of EML in the late 1990s also pre-dated the advent of more developer-friendly and user-friendly technologies such as the popular JSON serialization, as well as the advent of semantic or linked data model and it's application in ecoinformatics [@Michener2012].  More recently still, the creation of the W3C JSON-LD [@jsonld-w3c] has brought combined the developer-friendly JSON format with the powerful semantics of the Resource Description Format (RDF) popular in informatics [@rdf-w3c].  This package seeks to bring both of these advantages to bear on the rich-but-clunky XML structure used by EML.  By automatically mapping EML into JSON-LD, we are able to realize the benefits easier creation and manipulation of EML structures as JSON objects or R list objects, while relying on the power of JSON-LD semantics (in particular, context, compaction and framing, [@jsonld]) to transform that data to and from EML-schema-compliant XML.  This approach opens numerous research applications, ranging from the simple parsing or creation of EML files as R list objects to complex queries and semantic reasoning over large numbers of EML documents with semantic SPARQL queries [@sparql-w3c].  The package vignette provides scripted examples of each of these. 


# References
