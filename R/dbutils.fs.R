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

#' File system home
#'
#' @param type which file system
#' @param canonical whether to prepend with file system (e.g., file: or dbfs:)
#' @param user name of the user
#' @param abfs_group group to which the user belongs
#' @param abfs_host host name
#'
#' @return path to home directory in desired file system
#' @export
#'
#' @examples
#' dbutils.fs.home(user = "dborker")
#' dbutils.fs.home(canonical = TRUE, user = "dborker")
#' dbutils.fs.home("dbfs", user = "dborker")
#' dbutils.fs.home("dbfs", TRUE, "dborker")
#' dbutils.fs.home(
#'   "abfs",
#'   user = "dborker",
#'   abfs_host = "file-share-acmeincprodadls.dfs.core.windows.net"
#' )
dbutils.fs.home <-
  function(type = c("file", "dbfs", "abfs"),
           canonical = FALSE,
           user = dbutils.credentials.current_user(),
           abfs_group = "data-brokers",
           abfs_host = Sys.getenv("DATABRICKS_ABFSS_HOST")) {
    type <- match.arg(type)

    dplyr::case_when(
      type == "dbfs" & canonical ~ sprintf("dbfs:/home/%s", user),
      type == "dbfs" & !canonical ~ sprintf("/dbfs/home/%s", user),
      type == "file" & canonical ~ sprintf("file:/home/%s", user),
      type == "file" & !canonical ~ sprintf("/home/%s", user),
      type == "abfs" ~ sprintf("abfss://%s/%s/%s", abfs_host, abfs_group, user),
    )
  }
