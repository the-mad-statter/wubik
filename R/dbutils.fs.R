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
      stop("This function only works in a Databricks notebook.")
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
dbutils.fs.ls2 <- function(x) {
  purrr::map_dfr(
    dbutils.fs.ls(x),
    ~ dplyr::as_tibble(data.frame(.data))
  )
}

#' Copy content
#'
#' @param from from path
#' @param to to path
#'
#' @return TRUE if successful
dbutils.fs.cp2 <-
  function(from, to) {
    if (dbutils.fs.exists(to)) {
      dbutils.fs.rm(to, TRUE)
    }
    dbutils.fs.mkdirs(to)
    dbutils.fs.cp(from, to, TRUE)
  }
