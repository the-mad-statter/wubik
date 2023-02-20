test_that("file home paths as expected", {
  expect_equal(
    dbutils.fs.home(user = "dborker"),
    "/home/dborker"
  )
  expect_equal(
    dbutils.fs.home(format = "spark", user = "dborker"),
    "file:/home/dborker"
  )
  expect_equal(
    dbutils.fs.home("dbfs", user = "dborker"),
    "/dbfs/home/dborker"
  )
  expect_equal(
    dbutils.fs.home("dbfs", "spark", "dborker"),
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

test_that("filestore homes as expected", {
  expect_equal(
    dbutils.fs.file_store_home(user = "dborker"),
    "/dbfs/FileStore/dborker"
  )
  expect_equal(
    dbutils.fs.file_store_home("spark", "dborker"),
    "dbfs:/FileStore/dborker"
  )
})

test_that("filestore paths as expected", {
  expect_equal(
    dbutils.fs.file_store_path("out.csv", user = "dborker"),
    "/dbfs/FileStore/dborker/out.csv"
  )
  expect_equal(
    dbutils.fs.file_store_path("out.csv", "spark", "dborker"),
    "dbfs:/FileStore/dborker/out.csv"
  )
})

test_that("filestore url as expected", {
  expect_equal(
    dbutils.fs.file_store_url(
      "out.csv",
      FALSE,
      "dborker",
      "adb-1234567812345678.12.azuredatabricks.net",
      "1234567890123456"
    ),
    paste0(
      "https://adb-1234567812345678.12.azuredatabricks.net",
      "/files/dborker/out.csv?o=1234567890123456"
    )
  )
})
