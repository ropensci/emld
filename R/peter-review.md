

> In the `Motivations` section of `README.md` the purpose and method of "extending EML with other semantic vocabularies"  isn't clear (to me). Consider adding a section such as "Extending EML" with an example, if this is within the scope of the package.

Good point, I've removed this now since it is merely confusing.  What I had in mind here would be contingent on EML 2.2.0's support for arbitrary RDF annotations in EML.  In principle, a user could just add another term, say, `schema:editor` onto an `eml:dataset` object, and `emld` would translate it into the appropriate (and potentially less intuitive to construct) EML `annotation` element.  But this isn't implemented yet (see issue #1), and would probably be more trouble than help.  

> The help text for the `as_xml` `Description` contains only the text `as_xml` and does not describe what the function does. Also, the return type is not specified. The same is true for both `as_emld` and `as_json`, `template`.

fixed. 


> There is no documentation index is available for the package.

not actually sure to what this refers?  complete pdf & pkgdown-based versions of the package manual / docs should now be in place.

> There is no documentation for `?emld'` or `?emld-package`.

Thanks, fixed!

> This may be outside the scope of the package, but no functions, strategies or examples are presented in the documentation for simple editing of EML data, i.e. (reading, simple edit, write out). This would be very useful, but if this functionality presents to much overlap with the `EML` package, pleas disregard this comment.

Right, this is really the scope of the `EML` package.  As the README shows, it is possible to a list-based approach to create a simple (schema-valid) EML file, or make a minor edit, but this is unlikely to scale well without richer functions in EML. 

#### Functionality

> The `as_json`, `as_emld`, `as_xml` functions have a clear purpose and work as expected.

> In addition to testing using the provided code samples, the following checks for a complex EML document were performed for a couple of EML documents such as https://goa.nceas.ucsb.edu/#view/urn:uuid:3249ada0-afe3-4dd6-875e-0f7928a4c171:

> - verified that `as_json` produced valid JSON-LD
> - verified that the following commands produced the original EML file (round trip):

```
x <- as_emld('metadata.xml') # using
as_xml(x, "new.xml")
```

yay, thanks for checking this!

#### R Source

The following potentially unresolved items exist, as show from a quick scan of the source code:

```
$ grep FIXME *
as_emld.R:    ## FIXME technically this assumes only our context
as_xml.R:## FIXME drop NAs too?
eml_validate.R:    ##  FIXME shouldn't have to write to tempfile,
eml_validate.R:  # FIXME technically must be unique inside parent system only...
emld-methods.R:## FIXME: Print method should drop context
```

Good call, these have now been resolved and the notes removed.  (some had already been resolved, or weren't quite accurate)

#### Misc

devtools::check() reports non-standard file `LICENSE.md`

fixed (added to `.Rbuildignore`)
