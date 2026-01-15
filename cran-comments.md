Dear CRAN,

This patch release skips roundtrip validation tests on ARM Macs (aarch64) 
to address the test failures on M1mac. These tests encounter platform-specific 
XML validation issues on Apple Silicon.

Cheers,

Carl Boettiger


