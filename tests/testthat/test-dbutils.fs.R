test_that("dbfs home path correct", {
  expect_equal(
    dbutils.fs.home("dbfs", "dborker"),
    "dbfs:/home/dborker"
  )
})

test_that("abfs home path correct", {
  expect_equal(
    dbutils.fs.home("abfs", "dborker"),
    "abfss:///data-brokers/dborker"
  )
})

test_that("file home path correct", {
  expect_equal(
    dbutils.fs.home("file", "dborker"),
    "file:/home/dborker"
  )
})
