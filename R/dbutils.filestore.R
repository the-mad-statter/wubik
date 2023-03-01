#' FileStore URL
#'
#' @param path to the desired file
#' @param display_html print as clickable link or just the url as text
#' @param user name of the user
#' @param host the databricks host
#' @param org_id the organization id
#'
#' @export
#'
#' @examples
#' dbutils.filestore_url(
#'   "out.csv",
#'   FALSE,
#'   "dborker",
#'   "adb-1234567812345678.12.azuredatabricks.net",
#'   "1234567890123456"
#' )
dbutils.filestore_url <-
  function(path,
           display_html = TRUE,
           user = dbutils.credentials.current_user(),
           host = Sys.getenv("DATABRICKS_HOST"),
           org_id = Sys.getenv("DATABRICKS_ORG_ID")) {
    url <- sprintf("https://%s/files/%s/%s?o=%s", host, user, path, org_id)

    if (display_html) {
      displayHTML(sprintf("<a href='%s'>%s</a>", url, url))
    } else {
      url
    }
  }
