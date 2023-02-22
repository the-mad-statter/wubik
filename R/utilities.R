#' As csv
#'
#' @param x object to parse as csv
#' @param ... other arguments passed to [readr::write_csv()][readr::write_csv]
#'
#' @return object x as parsed csv string
#' @export
#'
#' @examples
#' as_csv(mtcars)
as_csv <- function(x, ...) {
  f <- tempfile(fileext = ".csv")
  readr::write_csv(x, f, ...)
  paste(readLines(f), collapse = "\n")
}
