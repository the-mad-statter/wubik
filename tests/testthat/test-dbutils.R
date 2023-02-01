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

test_that("abfs home path correct", {
  expect_equal(
    dbutils.rlib.abfs_home(
      user = "dborker",
      host = "file-share@wusmprodadls.dfs.core.windows.net"
    ),
    "abfss://file-share@wusmprodadls.dfs.core.windows.net/data-brokers/dborker"
  )
})
