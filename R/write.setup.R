#' Write cluster-scoped init script setup R
#'
#' @param persistent_path path to the persistent library
#' @param ephemeral_path path to the ephemeral library
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' write.setup.r()
#' }
write.setup.r <-
  function(
      persistent_path = dbutils.rlib.path("persistent"),
      ephemeral_path = dbutils.rlib.path("ephemeral")) {
    dbutils.fs.put(
      "/databricks/scripts/setup-r.sh",
      paste(
        c(
          "#!/bin/bash",
          sprintf("mkdir %s", ephemeral_path),
          sprintf("cp -R %s/* %s", persistent_path, ephemeral_path),
          ""
        ),
        collapse = "\n"
      ),
      TRUE
    )
  }

#' Write cluster-scoped init script to generate .Rprofile
#'
#' @param ephemeral_path path to the ephemeral library
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' write.setup.rprofile()
#' }
write.setup.rprofile <-
  function(ephemeral_path = dbutils.rlib.path("ephemeral")) {
    dbutils.fs.put(
      "/databricks/scripts/setup-Rprofile.sh",
      paste(
        c(
          "#!/bin/bash",
          sprintf(
            "echo '.libPaths(c(\x22%s\x22, .libPaths()))' > /root/.Rprofile",
            ephemeral_path
          ),
          ""
        ),
        collapse = "\n"
      ),
      TRUE
    )
  }

#' Write cluster-scoped init script to generate .Renviron
#'
#' @param key_values [dplyr::tibble()] of `key` and `value` pairs.
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' write.setup.renviron(
#'   dplyr::tibble(
#'     key = c("KEY_1", "KEY_2"),
#'     value = c("VALUE_1", "VALUE_2")
#'   )
#' )
#' }
write.setup.renviron <-
  function(key_values) {
    kvs <- purrr::pmap_chr(key_values, function(key, value, ...) {
      sprintf("echo '%s=\x22%s\x22' >> /root/.Renviron", key, value)
    })
    dbutils.fs.put(
      "/databricks/scripts/setup-Renviron.sh",
      paste(
        c(
          "#!/bin/bash",
          kvs,
          ""
        ),
        collapse = "\n"
      ),
      TRUE
    )
  }
