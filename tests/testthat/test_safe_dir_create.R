context("Safe Directory creation")


test_that("Creating a non-existing directory returns TRUE", {
  expect_true(safe.dir.create("a"))
  expect_false(safe.dir.create("a"))
  rm("a")
})
