test_that("dbutils.filestore_url() works", {
  expect_equal(
    dbutils.filestore_url(
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
