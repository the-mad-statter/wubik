#' Write cluster-scoped init script
#'
#' @param ini script to write
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write(dbutils.ini.setup_r_sh())
#' dbutils.ini.write(dbutils.ini.setup_rprofile_sh())
#' dbutils.ini.write(dbutils.ini.setup_renviron_sh())
#' dbutils.ini.write(dbutils.ini.setup_databricks_cli_sh())
#' dbutils.ini.write(dbutils.ini.setup_rstudio_server_sh())
#' dbutils.ini.write(dbutils.ini.setup_user_sh())
#' }
dbutils.ini.write <- function(ini) {
  dbutils.fs.put(
    sprintf("/databricks/scripts/%s", attr(ini, "name")),
    ini,
    TRUE
  )
}

#' Cluster-scoped init script setup-R.sh
#'
#' @param persistent_path path to the persistent library
#' @param ephemeral_path path to the ephemeral library
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.setup_r_sh()
#' }
dbutils.ini.setup_r_sh <-
  function(persistent_path = dbutils.rlib.path("persistent", "file"),
           ephemeral_path = dbutils.rlib.path("ephemeral", "file")) {
    x <- paste(
      c(
        "#!/bin/bash",
        sprintf("mkdir %s", ephemeral_path),
        sprintf("cp -R %s/* %s", persistent_path, ephemeral_path),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- "setup-R.sh"
    return(x)
  }

#' Cluster-scoped init script setup-Rprofile.sh
#'
#' @param ephemeral_path path to the ephemeral library
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.setup_rprofile_sh()
#' }
dbutils.ini.setup_rprofile_sh <-
  function(ephemeral_path = dbutils.rlib.path("ephemeral", "file")) {
    x <- paste(
      c(
        "#!/bin/bash",
        sprintf(
          "echo '.libPaths(c(\x22%s\x22, .libPaths()))' > /root/.Rprofile",
          ephemeral_path
        ),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- "setup-Rprofile.sh"
    return(x)
  }

#' Cluster-scoped init script setup-Renviron.sh
#'
#' @param key_values [dplyr::tibble()] of `key` and `value` pairs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.setup_renviron_sh(
#'   dplyr::tibble(
#'     key = c("KEY_1", "KEY_2"),
#'     value = c("VALUE_1", "VALUE_2")
#'   )
#' )
#' }
dbutils.ini.setup_renviron_sh <-
  function(key_values) {
    kvs <- purrr::pmap_chr(key_values, function(key, value, ...) {
      sprintf("echo '%s=\x22%s\x22' >> /root/.Renviron", key, value)
    })

    x <- paste(
      c(
        "#!/bin/bash",
        kvs,
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- "setup-Renviron.sh"
    return(x)
  }

#' Cluster-scoped init script setup-databricks-cli.sh
#'
#' @param host databricks host name
#' @param token databricks token
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.setup_databricks_cli_sh()
#' }
dbutils.ini.setup_databricks_cli_sh <-
  function(host = Sys.getenv("DATABRICKS_HOST"),
           token = Sys.getenv("DATABRICKS_TOKEN")) {
    x <- paste(
      c(
        "#!/bin/bash",
        "pip install databricks-cli",
        "pip install databricks-cli --upgrade",
        "echo '[DEFAULT]' >> /root/.databrickscfg",
        sprintf("echo 'host = https://%s' >> /root/.databrickscfg", host),
        sprintf("echo 'token = %s' >> /root/.databrickscfg", token),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- "setup-databricks-cli.sh"
    return(x)
  }

#' Cluster-scoped init script setup-rstudio-server.sh
#'
#' @param studio_deb_url url for rstudio deb file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.setup_rstudio_server_sh()
#' }
dbutils.ini.setup_rstudio_server_sh <-
  function(studio_deb_url = paste0(
             "https://download2.rstudio.org",
             "/server/bionic/amd64/rstudio-server-2022.12.0-353-amd64.deb"
           )) {
    x <- paste(
      c(
        "#!/bin/bash",
        "echo \"Y\" | apt-get install gdebi-core",
        sprintf("wget %s", studio_deb_url),
        "echo \"y\" | gdebi rstudio-server-2022.12.0-353-amd64.deb",
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- "setup-rstudio-server.sh"
    return(x)
  }

#' Cluster-scoped init script setup-user.sh
#'
#' @param pass password
#' @param user username
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.setup_user_sh("rosebud", "dborker")
#' }
dbutils.ini.setup_user_sh <-
  function(pass, user = dbutils.credentials.current_user()) {
    x <- paste(
      c(
        "#!/bin/bash",
        sprintf(
          "useradd -m %s ; echo -e \"%s\n%s\" | passwd %s",
          user, pass, pass, user
        ),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- "setup-user.sh"
    return(x)
  }
