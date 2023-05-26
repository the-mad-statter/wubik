#' Load secrets
#'
#' @param scope name of the desired scope/key vault
#' @param select character string containing a regular expression to be matched
#' when selecting key vault secrets
#' @param pattern character string containing a regular expression to be
#' matched for replacement in the given key vault secret name
#' @param replacement a replacement for matched pattern
#' @param ... further arguments passed to or from other methods
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.load("wusm-prod-biostats-kv", ".+")
#' }
#'
#' @note Azure Key Vault does not allow `_` in secret names while `-` is a
#' non-standard character for R variable names. Therefore, `-` in Key Vault
#' secret names are replaced with `_` for R names after the the desired
#' pattern-replacement has occurred.
dbutils.secrets.load <-
  function(scope, select, pattern = "", replacement = "", ...) {
    Sys.setenv2 <-
      function(var, val) rlang::call2("Sys.setenv", !!rlang::enexpr(var) := val)

    scope %>%
      dbutils.secrets.list() %>%
      unlist() %>%
      unname() %>%
      purrr::walk(~ {
        if (grepl(select, .x, ...)) {
          eval(
            Sys.setenv2(
              !!gsub("-", "_", sub(pattern, replacement, .x, ...), ...),
              dbutils.secrets.get(scope, .x)
            )
          )
        }
      })
  }

#' Load secrets from the Biostats Production Key Vault
#'
#' @inheritParams dbutils.secrets.load
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.load.wusm_prod_biostats_kv()
#' }
#'
dbutils.secrets.load.wusm_prod_biostats_kv <- function(select = ".+", ...) {
  dbutils.secrets.load("wusm-prod-biostats-kv", select, ...)
}

#' Load secrets from the Databrokers Production Key Vault
#'
#' @inheritParams dbutils.secrets.load
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.load.wusm_prod_databrokers_kv()
#' }
#'
dbutils.secrets.load.wusm_prod_databrokers_kv <- function(select = ".+", ...) {
  dbutils.secrets.load("wusm-prod-databrokers-kv", select, ...)
}

#' Retrieve secrets as a tibble
#'
#' @inheritParams dbutils.secrets.load
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.tbl_df("wusm-prod-biostats-kv", ".+")
#' }
dbutils.secrets.tbl_df <-
  function(scope, select, pattern = "", replacement = "", ...) {
    scope %>%
      dbutils.secrets.list() %>%
      unlist() %>%
      unname() %>%
      purrr::map_dfr(~ {
        if (grepl(select, .x, ...)) {
          dplyr::tibble(
            key = gsub("-", "_", sub(pattern, replacement, .x, ...), ...),
            value = dbutils.secrets.get(scope, .x)
          )
        }
      })
  }

#' Retrieve secrets as a tibble from the Biostats Production Key Vault
#'
#' @inheritParams dbutils.secrets.tbl_df
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.tbl_df.wusm_prod_biostats_kv()
#' }
#'
dbutils.secrets.tbl_df.wusm_prod_biostats_kv <- function(select = ".+", ...) {
  dbutils.secrets.tbl_df("wusm-prod-biostats-kv", select, ...)
}

#' Retrieve secrets as a tibble from the Databrokers Production Key Vault
#'
#' @inheritParams dbutils.secrets.tbl_df
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.tbl_df.wusm_prod_databrokers_kv()
#' }
#'
dbutils.secrets.tbl_df.wusm_prod_databrokers_kv <-
  function(select = ".+", ...) {
    dbutils.secrets.tbl_df("wusm-prod-databrokers-kv", select, ...)
  }
