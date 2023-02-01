#' Current user
#'
#' @return username of the current user
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.credentials.current_user()
#' }
dbutils.credentials.current_user <-
  function() {
    current_user <- SparkR::collect(SparkR::sql("SELECT current_user()"))[1, 1]
    sub("@wustl.edu", "", current_user)
  }
