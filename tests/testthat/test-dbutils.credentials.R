test_that("dbutils.credentials.current_user() works", {
  expect_equal(
    dbutils.credentials.current_user("dborker@wustl.edu"),
    "dborker"
  )
})
