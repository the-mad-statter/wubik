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
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' write.setup.renviron()
#' }
write.setup.renviron <-
  function() {
    dbutils.fs.put(
      "/databricks/scripts/setup-Renviron.sh",
      paste(
        c(
          "#!/bin/bash",
          "echo 'KEY=\x22VALUE\x22' > /root/.Renviron",
          ""
        ),
        collapse = "\n"
      ),
      TRUE
    )
  }
