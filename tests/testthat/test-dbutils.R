test_that("ephemeral path correct", {
  expect_equal(dbutils.rlib.ephemeral_path(), "file:/usr/lib/R/library")
})
