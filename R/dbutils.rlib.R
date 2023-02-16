#' Library path
#'
#' @param type the type of path to generate
#' @param canonical whether to prepend with file system (e.g., file: or dbfs:)
#' @param user username of the current user
#'
#' @return path to the recommended ephemeral or persistent library locations
#'
#' @export
#'
#' @examples
#' dbutils.rlib.path("ephemeral", user = "dborker")
#' dbutils.rlib.path("ephemeral", FALSE, "dborker")
#' dbutils.rlib.path("persistent", user = "dborker")
#' dbutils.rlib.path("persistent", FALSE, "dborker")
dbutils.rlib.path <-
  function(type = c("ephemeral", "persistent"),
           canonical = FALSE,
           user = dbutils.credentials.current_user()) {
    dplyr::case_when(
      type == "ephemeral" & canonical ~ "file:/usr/lib/R/%s-library",
      type == "ephemeral" & !canonical ~ "/usr/lib/R/%s-library",
      type == "persistent" & canonical ~ "dbfs:/usr/lib/R/%s-library",
      TRUE ~ "/dbfs/usr/lib/R/%s-library"
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
  function(path = dbutils.rlib.path("ephemeral")) {
    i <- which(.libPaths() == path)
    if (length(i) == 1) {
      .libPaths(c(path, .libPaths()[-i]))
    } else {
      .libPaths(c(path, .libPaths()))
    }
  }

#' Copy R library
#'
#' @param from from path
#' @param to to path
#'
#' @return TRUE if successful
dbutils.rlib.cp <-
  function(from, to) {
    if (dbutils.fs.exists(to)) {
      dbutils.fs.rm(to, TRUE)
    }
    dbutils.fs.mkdirs(to)
    dbutils.fs.cp(from, to, TRUE)
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
  function(ephemeral_path = dbutils.rlib.path("ephemeral", TRUE),
           persistent_path = dbutils.rlib.path("persistent", TRUE)) {
    dbutils.rlib.cp(ephemeral_path, persistent_path)
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
  function(persistent_path = dbutils.rlib.path("persistent", TRUE),
           ephemeral_path = dbutils.rlib.path("ephemeral", TRUE)) {
    dbutils.rlib.cp(persistent_path, ephemeral_path)
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
        purrr::map_dfr(~ {
          path <- .x
          package <- basename(path)
          description_path <- file.path(path, "Description")
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
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.install_spark()
#' }
dbutils.rlib.install_spark <- function(version = "3.3.1") {
  pak::pkg_install(sprintf("apache/spark/R/pkg@v%s", version))
}
