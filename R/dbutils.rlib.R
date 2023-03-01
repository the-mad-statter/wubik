#' Library path
#'
#' @param type the type of path to generate
#' @param fmt how to format the address
#' @param user username of the current user
#'
#' @return path to the recommended ephemeral or persistent library locations
#'
#' @export
#'
#' @examples
#' dbutils.rlib.path(user = "dborker")
#' dbutils.rlib.path(fmt = "spark", user = "dborker")
#' dbutils.rlib.path("persistent", user = "dborker")
#' dbutils.rlib.path("persistent", "spark", "dborker")
dbutils.rlib.path <-
  function(type = c("ephemeral", "persistent"),
           fmt = c("file", "spark"),
           user = dbutils.credentials.current_user()) {
    type <- match.arg(type)
    fmt <- match.arg(fmt)

    dplyr::case_when(
      type == "ephemeral" & fmt == "file" ~ "/usr/lib/R/%s-library",
      type == "ephemeral" & fmt == "spark" ~ "file:/usr/lib/R/%s-library",
      type == "persistent" & fmt == "file" ~ "/dbfs/usr/lib/R/%s-library",
      type == "persistent" & fmt == "spark" ~ "dbfs:/usr/lib/R/%s-library"
    ) %>%
      sprintf(user)
  }

#' Set default library
#'
#' @param path path to desired library
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.set_default()
#' }
dbutils.rlib.set_default <-
  function(path = dbutils.rlib.path("ephemeral", "file")) {
    i <- which(.libPaths() == path)
    if (length(i) == 1) {
      .libPaths(c(path, .libPaths()[-i]))
    } else {
      .libPaths(c(path, .libPaths()))
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
  function(ephemeral_path = dbutils.rlib.path("ephemeral", "spark"),
           persistent_path = dbutils.rlib.path("persistent", "spark")) {
    dbutils.fs.cp2(ephemeral_path, persistent_path)
  }

#' Restore R library
#'
#' @param persistent_path path to persistent library location
#' @param ephemeral_path path to ephemeral library location
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.restore()
#' }
dbutils.rlib.restore <-
  function(persistent_path = dbutils.rlib.path("persistent", "spark"),
           ephemeral_path = dbutils.rlib.path("ephemeral", "spark")) {
    dbutils.fs.cp2(persistent_path, ephemeral_path)
  }

#' List details of installed packages
#'
#' @param libpath path(s) to libraries
#'
#' @return a [dplyr::tibble()] containing package name, path, and version
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.details()
#' }
dbutils.rlib.details <- function(libpath = .libPaths()) {
  libpath %>%
    purrr::map_dfr(~ {
      list.files(.x, full.names = TRUE) %>%
        `[`(!grepl("_cache", .)) %>%
        purrr::map_dfr(~ {
          path <- .x
          package <- basename(path)
          description_path <- file.path(path, "DESCRIPTION")
          package_details <- readLines(description_path)
          version_line <- package_details[grepl("Version:", package_details)]
          version <- sub("Version: ", "", version_line)
          dplyr::tibble(package, path, version) # compareVersion
        })
    })
}

#' Install the R Package Spark
#'
#' @param version version to install
#' @param ... additional arguments passed to [pak::pkg_install()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.install_spark()
#' }
dbutils.rlib.install_spark <- function(version = "3.3.1", ...) {
  pak::pkg_install(sprintf("apache/spark/R/pkg@v%s", version), ...)
}
