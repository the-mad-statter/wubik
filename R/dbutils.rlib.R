#' Library path
#'
#' @param type the type of path to generate
#' @param user username of the current user
#'
#' @return path to the recommended ephemeral or persistent library locations
#'
#' @export
#'
#' @examples
#' dbutils.rlib.path("ephemeral", "dborker")
#' dbutils.rlib.path("persistent", "dborker")
dbutils.rlib.path <-
  function(type = c("ephemeral", "persistent"),
           user = dbutils.credentials.user()) {
    type <- match.arg(type)
    path <- sprintf("/usr/lib/R/%s-library", user)
    if (type == "persistent") {
      path <- paste0("/dbfs", path)
    }
    path
  }

#' Set default library
#'
#' @param path path to desired library
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.rlib.default()
#' }
dbutils.rlib.default <-
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
dbutils.rlib.copy <-
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
  function(ephemeral_path = dbutils.rlib.path("ephemeral"),
           persistent_path = dbutils.rlib.path("persistent")) {
    dbutils.rlib.copy(ephemeral_path, persistent_path)
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
  function(persistent_path = dbutils.rlib.path("persistent"),
           ephemeral_path = dbutils.rlib.path("ephemeral")) {
    dbutils.rlib.copy(persistent_path, ephemeral_path)
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
