#' Write cluster-scoped init script
#'
#' @param x script contents to write
#' @param name name of the script
#' @param user current user
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write(dbutils.ini.restore_r_library_sh())
#' dbutils.ini.write(dbutils.ini.write_user_rprofile_sh())
#' dbutils.ini.write(dbutils.ini.write_root_rprofile_sh())
#' kvps <- tibble(key = "key_name", value = "key_value")
#' dbutils.ini.write(dbutils.ini.write_user_renviron_sh(kvps))
#' dbutils.ini.write(dbutils.ini.write_user_renviron_sh(kvps))
#' dbutils.ini.write(dbutils.ini.install_user_databricks_cli_sh("my_db_token"))
#' dbutils.ini.write(dbutils.ini.install_user_databricks_cli_sh("my_db_token"))
#' dbutils.ini.write(dbutils.ini.install_rstudio_server_sh())
#' dbutils.ini.write(dbutils.ini.add_sudo_user_sh("rosebud"))
#' }
dbutils.ini.write <-
  function(x,
           name = attr(x, "name"),
           user = dbutils.credentials.current_user()) {
    p <- sprintf("/databricks/scripts/%s/%s", user, name)
    r <- dbutils.fs.put(p, x[1], TRUE)
    if (r) {
      message(sprintf("Successfully wrote \x22dbfs:%s\x22.", p))
    } else {
      warning(sprintf("Failed to write \x22dbfs:%s\x22", p))
    }
  }

#' Cluster-scoped init script for directory restorations
#'
#' @param persistent_path path to the persistent directory
#' @param ephemeral_path path to the ephemeral directory
#' @param script_name name to use for the script file
dbutils.ini.restore_directory_sh <-
  function(persistent_path, ephemeral_path, script_name) {
    x <- paste(
      c(
        "#!/bin/bash",
        sprintf("mkdir -p %s", ephemeral_path),
        sprintf("cp -R %s/. %s", persistent_path, ephemeral_path),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- script_name
    return(x)
  }

#' Cluster-scoped init script restore-r-library.sh
#'
#' @param persistent_path path to the persistent library
#' @param ephemeral_path path to the ephemeral library
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.restore_r_library_sh()
#' }
dbutils.ini.restore_r_library_sh <-
  function(persistent_path = dbutils.rlib.path("persistent", "file"),
           ephemeral_path = dbutils.rlib.path("ephemeral", "file")) {
    dbutils.ini.restore_directory_sh(
      persistent_path,
      ephemeral_path,
      "restore-r-library.sh"
    )
  }

#' Cluster-scoped init script restore-home-directory.sh
#'
#' @param persistent_path path to the persistent home files
#' @param ephemeral_path path to the ephemeral home files
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.restore_home_directory_sh()
#' }
dbutils.ini.restore_home_directory_sh <-
  function(persistent_path = dbutils.home.path("dbfs", "file"),
           ephemeral_path = dbutils.home.path("file", "file")) {
    dbutils.ini.restore_directory_sh(
      persistent_path,
      ephemeral_path,
      "restore-home-directory.sh"
    )
  }

#' Cluster-scoped init script write-<user>-Rprofile.sh
#'
#' @param x .Rprofile contents
#' @param user user for which to write .Rprofile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write_user_rprofile_sh()
#' }
dbutils.ini.write_user_rprofile_sh <-
  function(x = sprintf(
             ".libPaths(c(\x22%s\x22, .libPaths()))",
             dbutils.rlib.path("ephemeral", "file")
           ),
           user = dbutils.credentials.current_user()) {
    p <- ifelse(user == "root", "/root", sprintf("/home/%s", user))

    x <- paste(
      c(
        "#!/bin/bash",
        sprintf("mkdir -p %s", p),
        sprintf("echo '%s' > %s/.Rprofile", x, p),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- sprintf("write-%s-Rprofile.sh", user)
    return(x)
  }

#' Cluster-scoped init script write-root-Rprofile.sh
#'
#' @param x .Rprofile contents
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write_root_rprofile_sh()
#' }
dbutils.ini.write_root_rprofile_sh <-
  function(x = sprintf(
             ".libPaths(c(\x22%s\x22, .libPaths()))",
             dbutils.rlib.path("ephemeral", "file")
           )) {
    dbutils.ini.write_user_rprofile_sh(x, "root")
  }

#' Cluster-scoped init script write-user-Renviron.sh
#'
#' @param key_values [dplyr::tibble()] of `key` and `value` pairs.
#' @param user user for which to write .Renviron
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write_user_renviron_sh(
#'   dplyr::tibble(
#'     key = c("KEY_1", "KEY_2"),
#'     value = c("VALUE_1", "VALUE_2")
#'   )
#' )
#' }
dbutils.ini.write_user_renviron_sh <-
  function(key_values,
           user = dbutils.credentials.current_user()) {
    p <- ifelse(user == "root", "/root", sprintf("/home/%s", user))

    kvps <- purrr::pmap_chr(key_values, function(key, value, ...) {
      sprintf("echo '%s=\x22%s\x22' >> %s/.Renviron", key, value, p)
    })

    x <- paste(
      c(
        "#!/bin/bash",
        kvps,
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- sprintf("write-%s-Renviron.sh", user)
    return(x)
  }

#' Cluster-scoped init script write-root-Renviron.sh
#'
#' @param key_values [dplyr::tibble()] of `key` and `value` pairs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write_root_renviron_sh(
#'   dplyr::tibble(
#'     key = c("KEY_1", "KEY_2"),
#'     value = c("VALUE_1", "VALUE_2")
#'   )
#' )
#' }
dbutils.ini.write_root_renviron_sh <-
  function(key_values) {
    dbutils.ini.write_user_renviron_sh(key_values, "root")
  }

#' Cluster-scoped init script install-<user>-<token_name>-databricks-cli.sh
#'
#' @param token_name token name
#' @param token_value token value
#' @param user user account for which to write .databrickscfg
#' @param host databricks host name
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.install_user_databricks_cli_sh()
#' }
dbutils.ini.install_user_databricks_cli_sh <-
  function(token_name,
           token_value = Sys.getenv("DATABRICKS_TOKEN"),
           user = dbutils.credentials.current_user(),
           host = Sys.getenv("DATABRICKS_HOST")) {
    p <- ifelse(user == "root", "/root", sprintf("/home/%s", user))

    x <- paste(
      c(
        "#!/bin/bash",
        "/databricks/python/bin/pip install databricks-cli",
        "/databricks/python/bin/pip install databricks-cli --upgrade",
        sprintf("echo '[DEFAULT]' >> %s/.databrickscfg", p),
        sprintf("echo 'host = https://%s' >> %s/.databrickscfg", host, p),
        sprintf("echo 'token = %s' >> %s/.databrickscfg", token_value, p),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- sprintf(
      "install-%s-%s-databricks-cli.sh", user, token_name
    )
    return(x)
  }

#' Cluster-scoped init script install-root-<token_name>-databricks-cli.sh
#'
#' @param token_name token name
#' @param token_value token value
#' @param host databricks host name
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.install_root_databricks_cli_sh("my_default_token")
#' }
dbutils.ini.install_root_databricks_cli_sh <-
  function(token_name,
           token_value = Sys.getenv("DATABRICKS_TOKEN"),
           host = Sys.getenv("DATABRICKS_HOST")) {
    dbutils.ini.install_user_databricks_cli_sh(
      token_name, token_value, "root", host
    )
  }

#' Cluster-scoped init script install-rstudio-server.sh
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.install_rstudio_server_sh()
#' }
dbutils.ini.install_rstudio_server_sh <- function() {
  x <- paste(
    c(
      "#!/bin/bash",
      "echo \"Y\" | apt-get install gdebi-core",
      sprintf(
        "wget %s",
        paste0(
          "https://download2.rstudio.org",
          "/server/bionic/amd64/rstudio-server-2022.12.0-353-amd64.deb"
        )
      ),
      "echo \"y\" | gdebi rstudio-server-2022.12.0-353-amd64.deb",
      ""
    ),
    collapse = "\n"
  )

  attr(x, "name") <- "install-rstudio-server.sh"
  return(x)
}

#' Cluster-scoped init script add-sudo-<user>.sh
#'
#' @param pass password
#' @param user username
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.add_sudo_user_sh("rosebud", "dborker")
#' }
dbutils.ini.add_sudo_user_sh <-
  function(pass, user = dbutils.credentials.current_user()) {
    x <- paste(
      c(
        "#!/bin/bash",
        sprintf(
          "useradd -m %s ; echo -e \"%s\n%s\" | passwd %s",
          user, pass, pass, user
        ),
        sprintf("adduser %s sudo", user),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- sprintf("add-sudo-%s.sh", user)
    return(x)
  }

#' Cluster-scoped init script install-<token_name>-odbc_driver.sh
#'
#' @param token_name token name
#' @param token_value token value
#' @param user token owner
#' @param host databricks host
#' @param port databricks port
#' @param http_path databricks http path
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Generate init script
#' dbutils.ini.install_odbc_driver_sh("my_default_token")
#'
#' ## R usage of driver
#' conn <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' print(DBI::dbGetQuery(
#'   conn,
#'   "SELECT * FROM cleansed.epic_clarity.clarity_orgfilter_pat_enc_hsp LIMIT 2"
#' ))
#' }
dbutils.ini.install_odbc_driver_sh <-
  function(token_name,
           token_value = Sys.getenv("DATABRICKS_TOKEN"),
           user = dbutils.credentials.current_user(),
           host = Sys.getenv("DATABRICKS_HOST"),
           port = 443,
           http_path = Sys.getenv("DATABRICKS_HTTP_PATH")) {
    unixodbc_uri <- "https://www.unixodbc.org/unixODBC-2.3.11.tar.gz"
    unixodbc_bn <- basename(unixodbc_uri)

    driver_uri <- paste0(
      "https://databricks-bi-artifacts.s3.us-east-2.amazonaws.com",
      "/simbaspark-drivers/odbc/2.6.29",
      "/SimbaSparkODBC-2.6.29.1049-Debian-64bit.zip"
    )
    driver_deb <- "simbaspark_2.6.29.1049-2_amd64.deb"
    driver_path <- "/opt/simba/spark/lib/64/libsparkodbc_sb64.so"

    x <- paste(
      c(
        "#!/bin/bash",
        "",
        "# 1. Install unixODBC",
        sprintf("wget %s", unixodbc_uri),
        sprintf("gunzip %s", unixodbc_bn),
        sprintf("tar xvf %s", sub(".gz$", "", unixodbc_bn))
      ),
      sprintf("cd %s", sub(".tar.gz$", "", unixodbc_bn)),
      "./configure",
      "make",
      "make install",
      "",
      "# 2. Download driver",
      "apt-get update -y",
      "apt-get install -y libsasl2-modules-gssapi-mit",
      sprintf("wget %s", driver_uri),
      "",
      "# 3. Install the ODBC driver",
      sprintf(
        "unzip %s", basename(driver_uri),
        sprintf("dpkg -i %s", driver_deb),
        "",
        "# 4. locate the odbc.ini wrt to \x22SYSTEM DATA SOURCES\x22",
        "#odbcinst -j # SYSTEM DATA SOURCES: /etc/odbc.ini",
        "",
        "# 5. open odbc.ini for editing",
        "",
        "# 6. Create an [ODBC Data Sources] section",
        "echo \x22\x22 >> /etc/odbc.ini",
        "echo \x22[ODBC Data Sources]\x22 >> /etc/odbc.ini",
        "echo \x22Databricks=Databricks ODBC Connector\x22 >> /etc/odbc.ini",
        "echo \x22\x22 >> /etc/odbc.ini",
        "",
        "# 7. Create DSN config section",
        "echo \x22[Databricks]\x22 >> /etc/odbc.ini",
        sprintf("echo \x22Driver=x%s\x22 >> /etc/odbc.ini", driver_path),
        sprintf("echo \x22Host=%s\x22 >> /etc/odbc.ini", host),
        sprintf("echo \x22Port=%s\x22 >> /etc/odbc.ini", port),
        sprintf("echo \x22HTTPPath=%s\x22 >> /etc/odbc.ini", http_path),
        "echo \x22ThriftTransport=2\x22 >> /etc/odbc.ini",
        "echo \x22SSL=1\x22 >> /etc/odbc.ini",
        "echo \x22AuthMech=3\x22 >> /etc/odbc.ini",
        "echo \x22UID=token\x22 >> /etc/odbc.ini",
        sprintf("echo \x22PWD=%s\x22 >> /etc/odbc.ini", token_value),
        "echo \x22\x22 >> /etc/odbc.ini",
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- sprintf("install-%s-%s-odbc_driver.sh", user, token_name)
    return(x)
  }

#' Cluster-scoped init script add-facl-<user>-<path>.sh
#'
#' @param path path for which to add user with given permissions
#' @param user user to add
#' @param perms desired permissions to grant
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.add_facl_user_sh()
#' }
dbutils.ini.add_facl_user_sh <-
  function(path, user = dbutils.credentials.current_user(), perms = "rwx") {
    x <- paste(
      c(
        "#!/bin/bash",
        sprintf(
          "setfacl --recursive --modify u:%s:%s,d:u:%s:%s %s",
          user, perms, user, perms, path
        )
      )
    )

    attr(x, "name") <- sprintf("add-facl-%s-%s.sh", user, basename(path))
    return(x)
  }
