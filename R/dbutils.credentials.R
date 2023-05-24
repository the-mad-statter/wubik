#' Current user
#'
#' @param domain logical indicating whether to return username with or without
#' the domain name
#'
#' @return username of the current user
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.credentials.current_user()
#' }
dbutils.credentials.current_user <- function(domain = FALSE) {
  x <- ifelse(
    exists("DATABRICKS_GUID", .GlobalEnv) && exists("user", .GlobalEnv),
    user,
    SparkR::collect(SparkR::sql("SELECT current_user()"))[1, 1]
  )

  ifelse(domain, x, sub("@.+$", "", x))
}
