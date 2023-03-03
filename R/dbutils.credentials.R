#' Current user
#'
#' @return username of the current user
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.credentials.current_user()
#' }
dbutils.credentials.current_user <- function() {
  x <- if (
    exists("DATABRICKS_GUID", .GlobalEnv) &&
      exists("user", .GlobalEnv)
  ) {
    user
  } else {
    SparkR::collect(SparkR::sql("SELECT current_user()"))[1, 1]
  }

  sub("@wustl.edu", "", x)
}
