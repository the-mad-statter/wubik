#' Print an Init-Script
#'
#' @param x object of class init_script
#' @param ... further arguments to be passed to or from methods
#'
#' @export
print.init_script <- function(x, ...) {
  cat(x[1])
}

#' Write cluster-scoped init script
#'
#' @param x script contents to write
#' @param name name of the script
#' @param user current user
#' @param path path to write script
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' ## Restore R Library for Current User
#' dbutils.ini.write(dbutils.ini.restore_r_library_for_user_sh())
#'
#' ## Write .Rprofile(s)
#' ### for Current User
#' dbutils.ini.write(dbutils.ini.write_rprofile_for_user_sh())
#' ### for Root
#' dbutils.ini.write(dbutils.ini.write_rprofile_for_root_sh())
#'
#' ## Write .Renviron
#' kvps <- tibble(key = "key_name", value = "key_value")
#' ### for Current User
#' dbutils.ini.write(dbutils.ini.write_renviron_for_user_sh(kvps))
#' ### for Root
#' dbutils.ini.write(dbutils.ini.write_renviron_for_root_sh(kvps))
#'
#' ## Databricks CLI
#' ### for Current User
#' dbutils.ini.write(dbutils.ini.install_databricks_cli_for_user_sh())
#' ### for Root
#' dbutils.ini.write(dbutils.ini.install_databricks_cli_for_root_sh())
#'
#' ## Install RStudio Server
#' dbutils.ini.write(dbutils.ini.install_rstudio_server_sh())
#'
#' ## Add Current User as Super User with Password
#' dbutils.ini.write(dbutils.ini.add_sudo_user_sh("rosebud"))
#'
#' ## Install ODBC Driver
#' dbutils.ini.write(dbutils.ini.install_odbc_driver_sh())
#'
#' ## Edit (Home) Directory Permissions for Current User
#' dbutils.ini.write(dbutils.ini.add_facl_user_to_path_sh())
#' }
dbutils.ini.write <-
  function(x,
           name = attr(x, "name"),
           user = dbutils.credentials.current_user(),
           path = sprintf("dbfs:/databricks/scripts/%s/%s", user, name)) {
    stopifnot("init_script" %in% class(x))
    r <- dbutils.fs.put(sub("dbfs:", "", path), x[1], TRUE)
    if (r) {
      message(sprintf("Successfully wrote \x22%s\x22.", path))
    } else {
      warning(sprintf("Failed to write \x22%s\x22", path))
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
    class(x) <- "init_script"
    return(x)
  }

#' Cluster-scoped init script restore-r-library-for-<user>.sh
#'
#' @param user current user
#' @param persistent_path path to the persistent library
#' @param ephemeral_path path to the ephemeral library
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.restore_r_library_for_user_sh()
#' }
dbutils.ini.restore_r_library_for_user_sh <-
  function(user = dbutils.credentials.current_user(),
           persistent_path = dbutils.rlib.path("persistent", "file", user),
           ephemeral_path = dbutils.rlib.path("ephemeral", "file", user)) {
    dbutils.ini.restore_directory_sh(
      persistent_path,
      ephemeral_path,
      sprintf("restore-r-library-for-%s.sh", user)
    )
  }

#' Cluster-scoped init script restore-home-directory-for-<user>.sh
#'
#' @param user current user
#' @param persistent_path path to the persistent home files
#' @param ephemeral_path path to the ephemeral home files
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.restore_home_directory_for_user_sh()
#' }
dbutils.ini.restore_home_directory_for_user_sh <-
  function(user = dbutils.credentials.current_user(),
           persistent_path = dbutils.home.path("dbfs", "file", user),
           ephemeral_path = dbutils.home.path("file", "file", user)) {
    dbutils.ini.restore_directory_sh(
      persistent_path,
      ephemeral_path,
      sprintf("restore-home-directory-for-%s.sh", user)
    )
  }

#' Cluster-scoped init script write-Rprofile-for-<user>.sh
#'
#' @param x .Rprofile contents
#' @param user user for which to write .Rprofile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write_rprofile_for_user_sh()
#' }
dbutils.ini.write_rprofile_for_user_sh <-
  function(
      user = dbutils.credentials.current_user(),
      x = sprintf(
        ".libPaths(c(\x22%s\x22, .libPaths()))",
        dbutils.rlib.path("ephemeral", "file", user)
      )) {
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

    attr(x, "name") <- sprintf("write-Rprofile-for-%s.sh", user)
    class(x) <- "init_script"
    return(x)
  }

#' Cluster-scoped init script write-Rprofile-for-root.sh
#'
#' @param x .Rprofile contents
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write_rprofile_for_root_sh()
#' }
dbutils.ini.write_rprofile_for_root_sh <-
  function(
      x = sprintf(
        ".libPaths(c(\x22%s\x22, .libPaths()))",
        dbutils.rlib.path("ephemeral", "file")
      )) {
    dbutils.ini.write_rprofile_for_user_sh("root", x)
  }

#' Cluster-scoped init script write-Renviron-for-<user>.sh
#'
#' @param key_values [dplyr::tibble()] of `key` and `value` pairs.
#' @param user user for which to write .Renviron
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write_renviron_for_user_sh(
#'   dplyr::tibble(
#'     key = c("KEY_1", "KEY_2"),
#'     value = c("VALUE_1", "VALUE_2")
#'   )
#' )
#' }
dbutils.ini.write_renviron_for_user_sh <-
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

    attr(x, "name") <- sprintf("write-Renviron-for-%s.sh", user)
    class(x) <- "init_script"
    return(x)
  }

#' Cluster-scoped init script write-Renviron-for-root.sh
#'
#' @param key_values [dplyr::tibble()] of `key` and `value` pairs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.write_renviron_for_root_sh(
#'   dplyr::tibble(
#'     key = c("KEY_1", "KEY_2"),
#'     value = c("VALUE_1", "VALUE_2")
#'   )
#' )
#' }
dbutils.ini.write_renviron_for_root_sh <-
  function(key_values) {
    dbutils.ini.write_renviron_for_user_sh(key_values, "root")
  }

#' Cluster-scoped init script install-databricks-cli-for-<user>.sh
#'
#' @param user user account for which to write .databrickscfg
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.install_databricks_cli_for_user_sh()
#' }
dbutils.ini.install_databricks_cli_for_user_sh <-
  function(user = dbutils.credentials.current_user()) {
    p <- ifelse(user == "root", "/root", sprintf("/home/%s", user))

    x <- paste(
      c(
        "#!/bin/bash",
        "/databricks/python/bin/pip install databricks-cli",
        "/databricks/python/bin/pip install databricks-cli --upgrade",
        sprintf('echo "[DEFAULT]" >> %s/.databrickscfg', p),
        sprintf(
          'echo "host = https://$DATABRICKS_HOST" >> %s/.databrickscfg', p
        ),
        sprintf('echo "token = $DATABRICKS_TOKEN" >> %s/.databrickscfg', p),
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- sprintf(
      "install-databricks-cli-for-%s.sh", user
    )
    class(x) <- "init_script"
    return(x)
  }

#' Cluster-scoped init script install-databricks-cli-for-root.sh
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.ini.install_databricks_cli_for_root.sh()
#' }
dbutils.ini.install_databricks_cli_for_root_sh <- function() {
  dbutils.ini.install_databricks_cli_for_user_sh("root")
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
  class(x) <- "init_script"
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
    class(x) <- "init_script"
    return(x)
  }

#' Cluster-scoped init script install-odbc-driver.sh
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Generate init script
#' dbutils.ini.install_odbc_driver_sh()
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
#'
#' @note Because this script will pull values from Spark environment variables:
#' DATABRICKS_HOST, DATABRICKS_HTTP_PATH, and DATABRICKS_TOKEN, ensure these
#' environment variables are present during cluster imitation. To do so it is
#' recommended that these values be stored in an Azure Key Vault and set in the
#' cluster configuration UI: Spark -> Environment variables. For example:
#' `DATABRICKS_TOKEN={{secrets/wusm-prod-biostats-kv/DATABRICKS-TOKEN}}`.
#' Connection can be tested in a terminal with `isql -v Databricks`.
dbutils.ini.install_odbc_driver_sh <-
  function() {
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
        sprintf("tar xvf %s", sub(".gz$", "", unixodbc_bn)),
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
        sprintf("unzip %s", basename(driver_uri)),
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
        sprintf("echo \x22Driver=%s\x22 >> /etc/odbc.ini", driver_path),
        "echo \x22Host=$DATABRICKS_HOST\x22 >> /etc/odbc.ini",
        "echo \x22Port=443\x22 >> /etc/odbc.ini",
        "echo \x22HTTPPath=$DATABRICKS_HTTP_PATH\x22 >> /etc/odbc.ini",
        "echo \x22ThriftTransport=2\x22 >> /etc/odbc.ini",
        "echo \x22SSL=1\x22 >> /etc/odbc.ini",
        "echo \x22AuthMech=3\x22 >> /etc/odbc.ini",
        "echo \x22UID=token\x22 >> /etc/odbc.ini",
        "echo \x22PWD=$DATABRICKS_TOKEN\x22 >> /etc/odbc.ini",
        "echo \x22\x22 >> /etc/odbc.ini",
        ""
      ),
      collapse = "\n"
    )

    attr(x, "name") <- "install-odbc-driver.sh"
    class(x) <- "init_script"
    return(x)
  }

#' Cluster-scoped init script add-facl-<user>-to-<path>.sh
#'
#' @param user user to add
#' @param path path for which to add user with given permissions
#' @param perms desired permissions to grant
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## modify R libary path permissions
#' dbutils.ini.add_facl_user_to_path_sh()
#'
#' ## modify home directory permissions
#' dbutils.ini.add_facl_user_to_path_sh(path = dbutils.home.path())
#' }
dbutils.ini.setfacl_user_to_path_sh <-
  function(user = dbutils.credentials.current_user(),
           path = dbutils.rlib.path(user = user),
           perms = "rwx") {
    x <- paste(
      c(
        "#!/bin/bash",
        sprintf(
          "setfacl --recursive --modify u:%s:%s,d:u:%s:%s %s",
          user, perms, user, perms, path
        )
      ),
      collapse = "\n"
    )

    attr(x, "name") <- sprintf("add-facl-%s-to-%s.sh", user, basename(path))
    class(x) <- "init_script"
    return(x)
  }
