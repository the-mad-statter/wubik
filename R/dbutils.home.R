#' File system home
#'
#' @param sys which file system
#' @param fmt how to format the address
#' @param user name of the user
#' @param abfs_group group to which the user belongs
#' @param abfs_host host name
#'
#' @return path to home directory in desired file system
#' @export
#'
#' @examples
#' dbutils.home.path(user = "dborker")
#' dbutils.home.path(fmt = "spark", user = "dborker")
#' dbutils.home.path("dbfs", user = "dborker")
#' dbutils.home.path("dbfs", "spark", "dborker")
#' dbutils.home.path("filestore", user = "dborker")
#' dbutils.home.path("filestore", "spark", "dborker")
#' dbutils.home.path(
#'   "abfs",
#'   user = "dborker",
#'   abfs_host = "file-share-acmeincprodadls.dfs.core.windows.net"
#' )
dbutils.home.path <-
  function(sys = c("file", "dbfs", "filestore", "abfs"),
           fmt = c("file", "spark"),
           user = dbutils.credentials.current_user(),
           abfs_group = "data-brokers",
           abfs_host = Sys.getenv("DATABRICKS_ABFSS_HOST")) {
    sys <- match.arg(sys)
    fmt <- match.arg(fmt)

    dplyr::case_when(
      sys == "file" & fmt == "file" ~ sprintf("/home/%s", user),
      sys == "file" & fmt == "spark" ~ sprintf("file:/home/%s", user),
      sys == "dbfs" & fmt == "file" ~ sprintf("/dbfs/home/%s", user),
      sys == "dbfs" & fmt == "spark" ~ sprintf("dbfs:/home/%s", user),
      sys == "filestore" & fmt == "file" ~ sprintf("/dbfs/FileStore/%s", user),
      sys == "filestore" & fmt == "spark" ~ sprintf("dbfs:/FileStore/%s", user),
      sys == "abfs" ~ sprintf("abfss://%s/%s/%s", abfs_host, abfs_group, user)
    )
  }

#' Persist home directory
#'
#' @param ephemeral_path path to ephemeral home location
#' @param persistent_path path to persistent home location
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.home.persist()
#' }
dbutils.home.persist <-
  function(ephemeral_path = dbutils.home.path("file", "spark"),
           persistent_path = dbutils.home.path("dbfs", "spark")) {
    dbutils.fs.cp2(ephemeral_path, persistent_path)
  }

#' Restore home directory
#'
#' @param persistent_path path to persistent home location
#' @param ephemeral_path path to ephemeral home location
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.home.restore()
#' }
dbutils.home.restore <-
  function(persistent_path = dbutils.home.path("dbfs", "spark"),
           ephemeral_path = dbutils.home.path("file", "spark")) {
    dbutils.fs.cp2(persistent_path, ephemeral_path)
  }
