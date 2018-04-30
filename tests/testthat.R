library(testthat)
library(emld)

options("emld_db" = "eml-2.1.1",
        "emld_schemaLocation" = "eml://ecoinformatics.org/eml-2.1.1/")
test_check("emld")
