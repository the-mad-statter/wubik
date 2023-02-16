#' Box FTP Upload
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
#' box_ftp_upload("/dbfs/home/my_user/my_img.png")
#' }
box_ftp_upload <-
  function(
      local,
      remote = basename(local),
      home = Sys.getenv("WUSTL_BOX_HOME"),
      user = Sys.getenv("WUSTL_BOX_USER"),
      pass = Sys.getenv("WUSTL_BOX_PASS"),
      verbose = FALSE,
      ...) {
    curl::curl_upload(
      file = local,
      url = sprintf("ftp://ftp.box.com/%s/%s", home, remote),
      verbose = verbose,
      userpwd = sprintf("%s:%s", user, pass),
      ...
    )
  }

#' Box FTP Download
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
#' box_ftp_download(
#'   "my_img.png",
#'   "/dbfs/home/my_user/my_img.png"
#' )
#' }
box_ftp_download <-
  function(
      remote,
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
      ...
    )
    curl::curl_download(
      sprintf("ftp://ftp.box.com/%s/%s", home, remote),
      local,
      handle = h
    )
  }
