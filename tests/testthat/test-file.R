context("file")

test_that("missing file gives error", {
  expect_error(read_fdb_csv("missing file"), "File does not exist")
})
