#' Current user
#'
#' @param x current user
#'
#' @return username of the current user
#' @export
#'
#' @examples
#' dbutils.credentials.current_user("dborker@wustl.edu")
dbutils.credentials.current_user <-
  function(x = SparkR::collect(SparkR::sql("SELECT current_user()"))[1, 1]) {
    sub("@wustl.edu", "", x)
  }
