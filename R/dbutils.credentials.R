#' Current user
#'
#' @return username of the current user
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.credentials.user()
#' }
dbutils.credentials.user <-
  function() {
    current_user <- SparkR::collect(SparkR::sql("SELECT current_user()"))[1, 1]
    sub("@wustl.edu", "", current_user)
  }
