#' Box FTPS Upload
#'
#' @param local file name or path of the local file to be uploaded.
#' @param remote the path to which the content is to be uploaded.
#' @param home prepended to remote to form the full remote path.
#' @param user Box username (i.e., WashU email)
#' @param pass unique password for external applications. Created at
#' <https://wustl.app.box.com/account>
#' @param verbose emit some progress output
#' @param ... other arguments passed to [curl::handle_setopt()][curl::handle]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' box_ftps_upload("/dbfs/home/my_user/my_img.png")
#' }
box_ftps_upload <-
  function(local,
           remote = basename(local),
           home = Sys.getenv("WUSTL_BOX_HOME"),
           user = Sys.getenv("WUSTL_BOX_USER"),
           pass = Sys.getenv("WUSTL_BOX_PASS"),
           verbose = FALSE,
           ...) {
    curl::curl_upload(
      file = local,
      url = utils::URLencode(
        sprintf("ftps://ftp.box.com:990/%s/%s", home, remote)
      ),
      verbose = verbose,
      userpwd = sprintf("%s:%s", user, pass),
      use_ssl = TRUE,
      ...
    )
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
#' @param ... other arguments passed to [curl::handle_setopt()][curl::handle]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' box_ftps_download(
#'   "my_img.png",
#'   "/dbfs/home/my_user/my_img.png"
#' )
#' }
box_ftps_download <-
  function(remote,
           local = basename(remote),
           home = Sys.getenv("WUSTL_BOX_HOME"),
           user = Sys.getenv("WUSTL_BOX_USER"),
           pass = Sys.getenv("WUSTL_BOX_PASS"),
           verbose = FALSE,
           ...) {
    h <- curl::new_handle()
    curl::handle_setopt(
      handle = h,
      verbose = verbose,
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
#' @param home prepended to remote to form the full remote path.
#' @param user Box username (i.e., WashU email)
#' @param pass unique password for external applications. Created at
#' <https://wustl.app.box.com/account>
#' @param verbose emit some progress output
#' @param ... other arguments passed to [curl::handle_setopt()][curl::handle]
#'
#' @return file contents as character string
#' @export
#'
#' @examples
#' \dontrun{
#' box_read("my_sql_file.sql")
#' }
box_read <-
  function(remote,
           home = Sys.getenv("WUSTL_BOX_HOME"),
           user = Sys.getenv("WUSTL_BOX_USER"),
           pass = Sys.getenv("WUSTL_BOX_PASS"),
           verbose = FALSE,
           ...) {
    f <- tempfile(fileext = ".box")
    box_ftps_download(remote, f, home, user, pass, verbose, ...)
    paste(readLines(f), collapse = "\n")
  }
