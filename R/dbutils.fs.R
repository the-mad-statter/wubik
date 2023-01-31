#' Path exists
#'
#' @param x path to test
#'
#' @return TRUE if path exists
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.fs.exists("~")
#' }
dbutils.fs.exists <- function(x) {
  # cannot suppress prettier results message
  result <- try(
    {
      dbutils.fs.ls(x)
    },
    silent = TRUE
  )
  if (inherits(result, "try-error")) {
    msg <- geterrmessage()
    if (grepl("java.io.FileNotFoundException", msg)) {
      FALSE
    } else {
      msg
    }
  } else {
    TRUE
  }
}

#' List directory
#'
#' @param x directory to list
#'
#' @return a [dplyr::tibble()] of directory contents
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.fs.dir("~")
#' }
dbutils.fs.dir <- function(x) {
  purrr::map_dfr(
    dbutils.fs.ls(x),
    ~ dplyr::as_tibble(data.frame(.data))
  )
}
