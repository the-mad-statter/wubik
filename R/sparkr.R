#' SQL Query
#'
#' @param sqlQuery character string of sql syntax
#'
#' See `SparkR::[sql][SparkR::sql]` for details.
#'
#' @export
sql <- function(sqlQuery) {
  if (!requireNamespace("SparkR", quietly = TRUE)) {
    stop("Install SparkR to use this function.")
  }

  SparkR::sql(sqlQuery)
}

#' collect: Collects all the elements of a SparkDataFrame and coerces...
#'
#' @param x object to collect
#' @param ... additional arguments
#'
#' See `SparkR::[collect][SparkR::collect]` for details.
#'
#' @export
collect <- function(x, ...) {
  if (!requireNamespace("SparkR", quietly = TRUE)) {
    stop("Install SparkR to use this function.")
  }

  SparkR::collect(x, ...)
}
