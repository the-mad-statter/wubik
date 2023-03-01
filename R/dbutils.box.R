#' Box FTPS Upload
#'
#' @param local file name or path of the local file to be uploaded.
#' @param remote the path to which the content is to be uploaded.
#' @param home prepended to remote to form the full remote path.
#' @param user Box username (i.e., WashU email)
#' @param pass unique password for external applications. Created at
#' <https://wustl.app.box.com/account>
#' @param verbose emit some progress output
#' @param connecttimeout desired connection timeout in milliseconds
#' @param ... other arguments passed to [curl::handle_setopt()][curl::handle]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.box.ftps_upload("/dbfs/home/my_user/my_img.png")
#' }
dbutils.box.ftps_upload <-
  function(local,
           remote = basename(local),
           home = Sys.getenv("WUSTL_BOX_HOME"),
           user = Sys.getenv("WUSTL_BOX_USER"),
           pass = Sys.getenv("WUSTL_BOX_PASS"),
           verbose = FALSE,
           connecttimeout = 10000,
           ...) {
    if (nchar(home) == 0) {
      stop("Environment variable WUSTL_BOX_HOME not set.")
    }
    if (nchar(user) == 0) {
      stop("Environment variable WUSTL_BOX_USER not set.")
    }
    if (nchar(pass) == 0) {
      stop("Environment variable WUSTL_BOX_PASS not set.")
    }

    r <- curl::curl_upload(
      file = local,
      url = utils::URLencode(
        sprintf("ftps://ftp.box.com:990/%s/%s", home, remote)
      ),
      verbose = verbose,
      connecttimeout = connecttimeout,
      userpwd = sprintf("%s:%s", user, pass),
      use_ssl = TRUE,
      ...
    )

    if (r$status_code == 226) {
      invisible(r)
    } else {
      r
    }
  }

#' Box Write
#'
#' @param x file contents to write
#' @param remote the path to which the content is to be uploaded.
#' @param ... other arguments passed to [curl::handle_setopt()][curl::handle]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.box.write("hello world!", "hello world.txt")
#' }
dbutils.box.write <-
  function(x,
           remote,
           ...) {
    f <- tempfile(fileext = ".box")
    writeLines(x, f)
    dbutils.box.ftps_upload(f, remote, ...)
  }

#' Box FTPS Download
#'
#' @param remote the path from which the content is to be downloaded.
#' @param local file name or path of the local file to write..
#' @param home prepended to remote to form the full remote path.
#' @param user Box username (i.e., WashU email)
#' @param pass unique password for external applications. Created at
#' <https://wustl.app.box.com/account>
#' @param verbose emit some progress output
#' @param connecttimeout desired connection timeout in milliseconds
#' @param ... other arguments passed to [curl::handle_setopt()][curl::handle]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.box.ftps_download(
#'   "my_img.png",
#'   "/dbfs/home/my_user/my_img.png"
#' )
#' }
dbutils.box.ftps_download <-
  function(remote,
           local = basename(remote),
           home = Sys.getenv("WUSTL_BOX_HOME"),
           user = Sys.getenv("WUSTL_BOX_USER"),
           pass = Sys.getenv("WUSTL_BOX_PASS"),
           verbose = FALSE,
           connecttimeout = 10000,
           ...) {
    if (nchar(home) == 0) {
      stop("Environment variable WUSTL_BOX_HOME not set.")
    }
    if (nchar(user) == 0) {
      stop("Environment variable WUSTL_BOX_USER not set.")
    }
    if (nchar(pass) == 0) {
      stop("Environment variable WUSTL_BOX_PASS not set.")
    }

    h <- curl::new_handle()
    curl::handle_setopt(
      handle = h,
      verbose = verbose,
      connecttimeout = connecttimeout,
      userpwd = sprintf("%s:%s", user, pass),
      use_ssl = TRUE,
      ...
    )
    curl::curl_download(
      utils::URLencode(
        sprintf("ftps://ftp.box.com:990/%s/%s", home, remote)
      ),
      local,
      handle = h
    )
  }

#' Box Read
#'
#' @param remote the path from which the contents are to be read.
#' @param ... other arguments passed to [curl::handle_setopt()][curl::handle]
#'
#' @return file contents as character string
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.box.ftps_download("my_sql_file.sql")
#' }
dbutils.box.read <-
  function(remote,
           ...) {
    f <- tempfile(fileext = ".box")
    dbutils.box.ftps_download(remote, f, ...)
    paste(readLines(f), collapse = "\n")
  }
