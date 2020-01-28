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
