test_that("file home paths as expected", {
  expect_equal(
    dbutils.fs.home(user = "dborker"),
    "/home/dborker"
  )
  expect_equal(
    dbutils.fs.home(canonical = TRUE, user = "dborker"),
    "file:/home/dborker"
  )
  expect_equal(
    dbutils.fs.home("dbfs", user = "dborker"),
    "/dbfs/home/dborker"
  )
  expect_equal(
    dbutils.fs.home("dbfs", TRUE, "dborker"),
    "dbfs:/home/dborker"
  )
  expect_equal(
    dbutils.fs.home(
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
