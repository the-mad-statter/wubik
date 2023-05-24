#' Load secrets
#'
#' @param scope name of the desired scope/key vault
#' @param pattern character string containing a regular expression to select
#' which vault secrets to load
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.load("wusm-prod-biostats-kv", ".+")
#' }
dbutils.secrets.load <-
  function(scope, pattern) {
    setenv <-
      function(var, val) rlang::call2("Sys.setenv", !!rlang::enexpr(var) := val)

    scope %>%
      dbutils.secrets.list() %>%
      unlist() %>%
      unname() %>%
      purrr::walk(~ {
        if (grepl(pattern, .x)) {
          eval(
            setenv(
              !!gsub("-", "_", .x),
              dbutils.secrets.get(scope, .x)
            )
          )
        }
      })
  }

#' Load secrets from the Biostats Production Key Vault
#'
#' @param pattern character string containing a regular expression to select
#' which vault secrets to load
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.load.wusm_prod_biostats_kv()
#' }
#'
dbutils.secrets.load.wusm_prod_biostats_kv <- function(pattern = ".+") {
  dbutils.secrets.load("wusm-prod-biostats-kv", pattern)
}

#' Load secrets from the Databrokers Production Key Vault
#'
#' @param pattern character string containing a regular expression to select
#' which vault secrets to load
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.load.wusm_prod_databrokers_kv()
#' }
#'
dbutils.secrets.load.wusm_prod_databrokers_kv <- function(pattern = ".+") {
  dbutils.secrets.load("wusm-prod-databrokers-kv", pattern)
}

#' Retrieve secrets as a tibble
#'
#' @param scope name of the desired scope/key vault
#' @param pattern character string containing a regular expression to select
#' which vault secrets to include
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.tbl_df("wusm-prod-biostats-kv", ".+")
#' }
dbutils.secrets.tbl_df <-
  function(scope, pattern) {
    scope %>%
      dbutils.secrets.list() %>%
      unlist() %>%
      unname() %>%
      purrr::map_dfr(~ {
        if (grepl(pattern, .x)) {
          dplyr::tibble(
            key = gsub("-", "_", .x),
            value = dbutils.secrets.get(scope, .x)
          )
        }
      })
  }

#' Retrieve secrets as a tibble from the Biostats Production Key Vault
#'
#' @param pattern character string containing a regular expression to select
#' which vault secrets to load
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.tbl_df.wusm_prod_biostats_kv()
#' }
#'
dbutils.secrets.tbl_df.wusm_prod_biostats_kv <- function(pattern = ".+") {
  dbutils.secrets.tbl_df("wusm-prod-biostats-kv", pattern)
}

#' Retrieve secrets as a tibble from the Databrokers Production Key Vault
#'
#' @param pattern character string containing a regular expression to select
#' which vault secrets to load
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.tbl_df.wusm_prod_databrokers_kv()
#' }
#'
dbutils.secrets.tbl_df.wusm_prod_databrokers_kv <- function(pattern = ".+") {
  dbutils.secrets.tbl_df("wusm-prod-databrokers-kv", pattern)
}
