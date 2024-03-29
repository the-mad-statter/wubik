#' As csv
#'
#' @param x object to parse as csv
#' @param ... other arguments passed to [write.csv()][utils::write.csv]
#'
#' @return object x as parsed csv string
#' @export
#'
#' @examples
#' as_csv(mtcars)
as_csv <- function(x, ...) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(x, f, na = "", row.names = FALSE, ...)
  paste(readLines(f), collapse = "\n")
}

#' RStudio Server URL
#'
#' @param display_html print as clickable link or just the url as text
#' @param host the databricks host
#' @param org_id the organization id
#' @param cluster_id the cluster id
#' @param port port for the server
#'
#' @export
#'
#' @examples
#' rstudio_server_url(
#'   display_html = FALSE,
#'   host = "adb-1234567812345678.12.azuredatabricks.net",
#'   org_id = "1234567890123456",
#'   cluster_id = "1234-123456-1234abcd"
#' )
rstudio_server_url <-
  function(display_html = TRUE,
           host = Sys.getenv("DATABRICKS_HOST"),
           org_id = Sys.getenv("DATABRICKS_ORG_ID"),
           cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
           port = 8787) {
    url <- sprintf(
      "https://%s/driver-proxy/o/%s/%s/%s/", host, org_id, cluster_id, port
    )

    if (display_html) {
      displayHTML(sprintf("<a href='%s'>%s</a>", url, url))
    } else {
      url
    }
  }

#' Install {wubik}
#'
#' @param pkg wubik github reference
#' @param ... additional arugments passed to [pak::pkg_install()]
#'
#' @return (Invisibly) A data frame with information about the installed
#' package(s).
#' @export
#'
#' @examples
#' \dontrun{
#' wubik_install()
#' }
wubik_install <- function(pkg = "the-mad-statter/wubik", ...) {
  pak::pkg_install(pkg, ...)
}

#' As Datetime mm/dd/yyyy hh:mm:ss mi
#'
#' @param x character representation of datetime
#' @param ... additional arguments to [lubridate::as_datetime]
#'
#' @return datetime representation of the datetime
#' @export
#'
#' @examples
#' as_datetime_mdy_hms_mi("1/1/2001 8:00:32 PM")
#'
as_datetime_mdy_hms_mi <- function(x, ...) {
  lubridate::as_datetime(x, format = "%m/%d/%Y %H:%M:%S %Op", ...)
}
