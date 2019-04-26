test_that("missing file gives error", {
  expect_error(read_fdb_csv("missing file"), "does not exist")
})
