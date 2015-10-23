context("Safe Directory creation")


test_that("Creating a non-existing directory returns TRUE", {
  path = paste0(tempdir(),  "xxxxxxxxxx")
  unlink(path, TRUE) # in case it exists
  expect_true(safe.dir.create(path))
  expect_false(safe.dir.create(path))
  unlink(path, TRUE)
})
