test_that("dbutils.home.path() works", {
  expect_equal(
    dbutils.home.path(user = "dborker"),
    "/home/dborker"
  )
  expect_equal(
    dbutils.home.path(fmt = "spark", user = "dborker"),
    "file:/home/dborker"
  )
  expect_equal(
    dbutils.home.path("dbfs", user = "dborker"),
    "/dbfs/home/dborker"
  )
  expect_equal(
    dbutils.home.path("dbfs", "spark", "dborker"),
    "dbfs:/home/dborker"
  )
  expect_equal(
    dbutils.home.path("filestore", user = "dborker"),
    "/dbfs/FileStore/dborker"
  )
  expect_equal(
    dbutils.home.path("filestore", "spark", "dborker"),
    "dbfs:/FileStore/dborker"
  )
  expect_equal(
    dbutils.home.path(
      "abfs",
      user = "dborker",
      abfs_host = "file-share-acmeincprodadls.dfs.core.windows.net"
    ),
    paste(
      "abfss://file-share-acmeincprodadls.dfs.core.windows.net",
      "data-brokers",
      "dborker",
      sep = "/"
    )
  )
})
