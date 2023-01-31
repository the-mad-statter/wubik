test_that("ephemeral path correct", {
  expect_equal(
    dbutils.rlib.ephemeralpath(user = "dborker"),
    "file:/usr/lib/R/dborker-library"
  )
})

test_that("persistent path correct", {
  expect_equal(
    dbutils.rlib.persistentpath(user = "dborker"),
    sprintf(
      "abfss://%s/%s/%s/lib/R/%s-library",
      "file-share@wusmprodadls.dfs.core.windows.net",
      "data-brokers",
      "dborker",
      "dborker"
    )
  )
})
