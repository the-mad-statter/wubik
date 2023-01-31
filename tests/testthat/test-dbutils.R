test_that("ephemeral path correct", {
  expect_equal(dbutils.rlib.ephemeralpath(), "file:/usr/lib/R/library")
})
