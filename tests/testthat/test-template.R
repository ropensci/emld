testthat::context("template print method")


test_that("We can show slots for the creator object", {
  expect_output(print(template("creator")), "individualName: \\{\\}")
  expect_output(print(template("creator")), "phone: ~")

})

test_that("template knows about internal classes too", {
  skip("internal class template deprecated")
  ## Only true in emld_db 2.1.1 right now... Probably deprecating this
  expect_output(print(template("ResponsibleParty")), "individualName: \\{\\}")
})

test_that("template knows about internal classes too", {
  options(emld_print =  "json")
  expect_output(print(template("creator")), '"individualName": \\{\\}')
  options(emld_print =  "yaml")

})


