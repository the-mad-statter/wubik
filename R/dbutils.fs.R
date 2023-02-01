#' Path exists
#'
#' @param x path to test
#'
#' @return TRUE if path exists
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.fs.exists("~")
#' }
dbutils.fs.exists <- function(x) {
  # cannot suppress prettier results message
  result <- try(
    {
      dbutils.fs.ls(x)
    },
    silent = TRUE
  )
  if (inherits(result, "try-error")) {
    msg <- geterrmessage()
    if (grepl("java.io.FileNotFoundException", msg)) {
      FALSE
    } else {
      msg
    }
  } else {
    TRUE
  }
}

#' List directory
#'
#' @param x directory to list
#'
#' @return a [dplyr::tibble()] of directory contents
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.fs.dir("~")
#' }
dbutils.fs.dir <- function(x) {
  purrr::map_dfr(
    dbutils.fs.ls(x),
    ~ dplyr::as_tibble(data.frame(.data))
  )
}

#' Azure blob file system home
#'
#' @param group group to which the user belongs
#' @param user name of the user
#' @param host host name
#'
#' @return path to home in Azure Blob File System
#' @export
#'
#' @examples
#' dbutils.rlib.abfs_home("data-brokers", "dborker")
dbutils.rlib.abfs_home <-
  function(group = "data-brokers",
           user = dbutils.credentials.current_user(),
           host = Sys.getenv("DATABRICKS_ABFSS_HOST")) {
    sprintf("abfss://%s/%s/%s", host, group, user)
  }
