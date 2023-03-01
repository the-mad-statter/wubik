test_that("dbutils.rlib.path() works", {
  expect_equal(
    dbutils.rlib.path("ephemeral", user = "dborker"),
    "/usr/lib/R/dborker-library"
  )
  expect_equal(
    dbutils.rlib.path("ephemeral", "spark", "dborker"),
    "file:/usr/lib/R/dborker-library"
  )
  expect_equal(
    dbutils.rlib.path("persistent", user = "dborker"),
    "/dbfs/usr/lib/R/dborker-library"
  )
  expect_equal(
    dbutils.rlib.path("persistent", "spark", "dborker"),
    "dbfs:/usr/lib/R/dborker-library"
  )
})
