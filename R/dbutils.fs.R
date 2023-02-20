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
#' @param system which file system
#' @param format how to format the address
#' @param user name of the user
#' @param abfs_group group to which the user belongs
#' @param abfs_host host name
#'
#' @return path to home directory in desired file system
#' @export
#'
#' @examples
#' dbutils.fs.home(user = "dborker")
#' dbutils.fs.home(format = "spark", user = "dborker")
#' dbutils.fs.home("dbfs", user = "dborker")
#' dbutils.fs.home("dbfs", "spark", "dborker")
#' dbutils.fs.home(
#'   "abfs",
#'   user = "dborker",
#'   abfs_host = "file-share-acmeincprodadls.dfs.core.windows.net"
#' )
dbutils.fs.home <-
  function(system = c("file", "dbfs", "abfs"),
           format = c("file", "spark"),
           user = dbutils.credentials.current_user(),
           abfs_group = "data-brokers",
           abfs_host = Sys.getenv("DATABRICKS_ABFSS_HOST")) {
    system <- match.arg(system)
    format <- match.arg(format)

    dplyr::case_when(
      system == "dbfs" & format == "file" ~ sprintf("/dbfs/home/%s", user),
      system == "dbfs" & format == "spark" ~ sprintf("dbfs:/home/%s", user),
      system == "file" & format == "file" ~ sprintf("/home/%s", user),
      system == "file" & format == "spark" ~ sprintf("file:/home/%s", user),
      system == "abfs" ~
        sprintf("abfss://%s/%s/%s", abfs_host, abfs_group, user)
    )
  }

#' FileStore home
#'
#' @param format the desired address format (i.e., file: /dbfs/FileStore/... or
#' spark: dbfs:/FileStore/...)
#' @param user name of the user
#'
#' @return path to the FileStore
#' @export
#'
#' @examples
#' dbutils.fs.file_store(user = "dborker")
#' dbutils.fs.file_store("spark", "dborker")
dbutils.fs.file_store <-
  function(format = c("file", "spark"),
           user = dbutils.credentials.current_user()) {
    format <- match.arg(format)

    dplyr::case_when(
      format == "file" ~ sprintf("/dbfs/FileStore/%s", user),
      format == "spark" ~ sprintf("dbfs:/FileStore/%s", user),
    )
  }

#' FileStore URL
#'
#' @param path to the desired file
#' @param display_html print as clickable link or just the url as text
#' @param user name of the user
#' @param instance the databricks instance
#' @param org_id the organization id
#'
#' @export
#'
#' @examples
#' dbutils.fs.file_store_url(
#'   "out.csv",
#'   FALSE,
#'   "dborker",
#'   "adb-1234567812345678.12.azuredatabricks.net",
#'   "1234567890123456"
#' )
dbutils.fs.file_store_url <-
  function(path,
           display_html = TRUE,
           user = dbutils.credentials.current_user(),
           instance = Sys.getenv("DATABRICKS_INSTANCE"),
           org_id = Sys.getenv("DATABRICKS_ORG_ID")) {
    url <- sprintf("https://%s/files/%s/%s?o=%s", instance, user, path, org_id)

    if (display_html) {
      displayHTML(sprintf("<a href='%s'>%s</a>", url, url))
    } else {
      url
    }
  }
