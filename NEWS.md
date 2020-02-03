# emld 0.5.0

User-facing changes:

- Rewrote logic around how `emld` works with `schemaLocation`:
  - The `schemaLocation` argument on `as_xml` used to fill in a default value that pointed to a local copy of `eml.xsd`. Now, it automatically fills in a web-resolvable location to make validating documents easier out of the box. 
  
  See [#44](https://github.com/ropensci/emld/issues/44) & [#45](https://github.com/ropensci/emld/issues/45).

- Re-worked the logic for handling `schemaLocation` when serializing to XML:
  - When `schemaLocation` is present on the `emld` object, it is used verbatim (no change in API here) regardless of the `schemaLocation` argument of `as_xml`.
  - When `schemaLocation` is absent on the `emld` object:
    - `as_xml(..., schemaLocation = TRUE)` causes a value to be guessed.
    - `as_xml(..., schemaLocation = FALSE)` explicitly prevents a value from being filled in when serialized.
    - `as_xml(..., schemaLocation = "Some value)` explicitly sets the provided value.

  See [#44](https://github.com/ropensci/emld/issues/44) & [#45](https://github.com/ropensci/emld/issues/45).

- `emld::eml_validate` no longer depends on `schemaLocation` to determine the correct XSD to use during schema validation and now uses two helpers (See below) to find the correct schema file. See [#52](https://github.com/ropensci/emld/issues/44) & [#45](https://github.com/ropensci/emld/issues/53).
      
Developer-facing (non-exported) changes:

- Added two new helper methods:
  1. `find_real_root_name(doc : xml_document) : list(prefix : character, name: character)` which returns the namespace prefix and the local name of the root element on an `xml_document`.
  2. `guess_root_schema(doc : xml_document) : list(module : character, version : character, namespace : character)` which returns the module, schema version, and namespace URI of the root element on an `xml_document`.
- `schemaLocation` is now ignored during roundtrip testing because of the new (above) behavior of `emld` with respect to `schemaLocation`.
- Roundtrip testing can now handle documents that are supposed to be invalid but still roundtripped. Specify intentionally invalid files by adding "invalid" (case insensitive) to the filename in `inst/tests`.

# emld 0.4.0

- Fixed serialization bug for `references` attributes [#48](https://github.com/ropensci/emld/issues/48)
- Fixed validation bug: packageId is now used as an identifier for checking uniqueness, and no more errors for annotation elements in additionalMetadata. [#49](https://github.com/ropensci/emld/pull/49)
- Fixed validation bug: XPath was referencing the element rather than the attribute `references`. [#47](https://github.com/ropensci/emld/pull/47)

# emld 0.3.0

- Updated package to support version 2.2.0 of EML. [#40](https://github.com/ropensci/emld/pull/40). See the [EML website](https://eml.ecoinformatics.org/whats-new-in-eml-2-2-0.html) for more information on the 2.2.0 release.
- Fixed a minor XML serialization issue with `TextType` nodes where extra whitespace was being added. [#37](https://github.com/ropensci/emld/pull/37).
- Relaxed `eml_validate`'s behavior when validating custom units. [#35](https://github.com/ropensci/emld/pull/35).

# emld 0.2.0

* Implemented changes requested by rOpenSci review, as detailed in 
  [#30](https://github.com/cboettig/emld/pull/30)

# emld 0.1.1

* Version submitted to rOpenSci review
* Added a `NEWS.md` file to track changes to the package.
