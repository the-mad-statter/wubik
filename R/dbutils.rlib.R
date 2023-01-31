#' Ephemeral path
#'
#' @param full pre-pend "file:" to the path
#'
#' @return path to the recommended ephemeral library location
#' @export
#'
#' @examples
#' dbutils.rlib.ephemeralpath()
#' dbutils.rlib.ephemeralpath(full = TRUE)
dbutils.rlib.ephemeralpath <- function(full = TRUE) {
  ephemeral_path <- "/usr/lib/R/library"
  if (full) {
    ephemeral_path <- paste0("file:", ephemeral_path)
  }
  ephemeral_path
}

#' Persistent path
#'
#' @param group group to which the user belongs
#' @param user name of the user
#' @param host host name
#'
#' @return path to the recommended persistent path in Azure Blob File System
#' @export
#'
#' @examples
#' dbutils.rlib.persistentpath("data-brokers", "dborker")
dbutils.rlib.persistentpath <-
  function(group = "data-brokers",
           user = dbutils.credentials.currentuser(),
           host = "file-share@wusmprodadls.dfs.core.windows.net") {
    sprintf("abfss://%s/%s/%s/lib/R/library", host, group, user)
  }

#' Ephemeral first
#'
#' @param ephemeral_path path to ephemeral library location without "file:"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.ephemeralfirst()
#' }
dbutils.rlib.ephemeralfirst <-
  function(ephemeral_path = dbutils.rlib.ephemeralpath(FALSE)) {
    i <- which(.libPaths() == ephemeral_path)
    if (length(i) == 1) {
      .libPaths(c(ephemeral_path, .libPaths()[-i]))
    }
  }

#' Persist R library
#'
#' @param ephemeral_path path to ephemeral library location
#' @param persistent_path path to persistent library location
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.persist()
#' }
dbutils.rlib.persist <-
  function(ephemeral_path = dbutils.rlib.ephemeralpath(),
           persistent_path = dbutils.rlib.persistentpath()) {
    if (dbutils.fs.exists(persistent_path)) {
      dbutils.fs.rm(persistent_path, TRUE)
    }
    dbutils.fs.mkdirs(persistent_path)
    dbutils.fs.cp(ephemeral_path, persistent_path, TRUE)
  }

#' Restore R library
#'
#' @param ephemeral_path path to ephemeral library location
#' @param persistent_path path to persistent library location
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.restore()
#' }
dbutils.rlib.restore <-
  function(persistent_path = dbutils.rlib.persistentpath(),
           ephemeral_path = dbutils.rlib.ephemeralpath()) {
    dbutils.fs.rm(ephemeral_path, TRUE)
    dbutils.fs.mkdirs(ephemeral_path)
    dbutils.fs.cp(persistent_path, ephemeral_path, TRUE)
  }
