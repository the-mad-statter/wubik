test_that("dbutils.credentials.current_user() works", {
  expect_equal(
    {
      .GlobalEnv$DATABRICKS_GUID <- "458e791f-b11d-43c9-bfad-4416ec292fbb"
      .GlobalEnv$user <- "dborker@wustl.edu"
      dbutils.credentials.current_user()
    },
    "dborker"
  )
})
