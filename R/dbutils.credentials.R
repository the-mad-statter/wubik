#' Current user
#'
#' @param x current user if known
#'
#' @return username of the current user
#' @export
#'
#' @examples
#' dbutils.credentials.current_user("dborker@wustl.edu")
#' \dontrun{
#' #' dbutils.credentials.current_user()
#' }
dbutils.credentials.current_user <-
  function(x) {
    x <- if (!missing(x)) {
      x
    } else if (exists("user")) {
      user
    } else {
      SparkR::collect(SparkR::sql("SELECT current_user()"))[1, 1]
    }

    sub("@wustl.edu", "", x)
  }
