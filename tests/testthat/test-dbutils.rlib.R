test_that("library paths as expected", {
  expect_equal(
    dbutils.rlib.path("ephemeral", user = "dborker"),
    "/usr/lib/R/dborker-library"
  )
  expect_equal(
    dbutils.rlib.path("ephemeral", FALSE, "dborker"),
    "/usr/lib/R/dborker-library"
  )
  expect_equal(
    dbutils.rlib.path("persistent", user = "dborker"),
    "/dbfs/usr/lib/R/dborker-library"
  )
  expect_equal(
    dbutils.rlib.path("persistent", FALSE, "dborker"),
    "/dbfs/usr/lib/R/dborker-library"
  )
})
