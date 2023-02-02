test_that("ephemeral path correct", {
  expect_equal(
    dbutils.rlib.path("ephemeral", "dborker"),
    "/usr/lib/R/dborker-library"
  )
})

test_that("persistent path correct", {
  expect_equal(
    dbutils.rlib.path("persistent", "dborker"),
    "/dbfs/usr/lib/R/dborker-library"
  )
})
