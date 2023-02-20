#' Put secret
#'
#' @param key A unique name to identify the secret.
#' @param value The secret value.
#' @param scope The name of the scope to which the secret will be associated.
#' @param value_type If string_value, the value will be stored in UTF-8 (MB4)
#' form. If bytes_value, the value will be stored as bytes.
#' @param token A databricks API personal access token.
#' @param instance A databricks instance (e.g.,
#' dbc-a1b2345c-d6e7.cloud.databricks.com)
#'
#' @return [httr2::response()]
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.put("ali-baba", "open sesame")
#' }
dbutils.secrets.put <-
  function(key,
           value,
           scope = "data-brokers",
           value_type = c("string_value", "bytes_value"),
           token = Sys.getenv("DATABRICKS_PAT"),
           instance = Sys.getenv("DATABRICKS_INSTANCE")) {
    value_type <- match.arg(value_type)

    version <- "2.0"
    endpoint <- "secrets/put"
    method <- "POST"

    json_list <- list(scope, key, value)
    names(json_list) <- c("scope", "key", value_type)

    httr2::request(
      sprintf("https://%s/api/%s/%s", instance, version, endpoint)
    ) %>%
      httr2::req_auth_bearer_token(token) %>%
      httr2::req_user_agent("wubik/1.0") %>%
      httr2::req_body_raw(
        jsonlite::toJSON(
          json_list,
          auto_unbox = TRUE,
          pretty = TRUE
        ),
        "application/json"
      ) %>%
      httr2::req_method(method) %>%
      httr2::req_perform()
  }

#' List Scopes
#'
#' @param token A databricks API personal access token.
#' @param instance A databricks instance (e.g.,
#' dbc-a1b2345c-d6e7.cloud.databricks.com)
#'
#' @return [httr2::response()]
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.list_scopes()
#' }
dbutils.secrets.list_scopes <-
  function(token = Sys.getenv("DATABRICKS_PAT"),
           instance = Sys.getenv("DATABRICKS_INSTANCE")) {
    version <- "2.0"
    endpoint <- "secrets/scopes/list"
    method <- "GET"

    httr2::request(
      sprintf(
        "https://%s/api/%s/%s",
        instance,
        version,
        endpoint
      )
    ) %>%
      httr2::req_auth_bearer_token(token) %>%
      httr2::req_user_agent("wubric/1.0") %>%
      httr2::req_method(method) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json() %>%
      `[[`(1) %>%
      purrr::map_dfr(~ dplyr::as_tibble(data.frame(.)))[[2, ]]
  }

#' Load secrets
#'
#' @param scope name of the desired scope/key vault
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbutils.secrets.load("wusm-prod-biostats-kv")
#' }
dbutils.secrets.load <-
  function(scope) {
    setenv <-
      function(var, val) rlang::call2("Sys.setenv", !!rlang::enexpr(var) := val)

    scope %>%
      dbutils.secrets.list() %>%
      unlist() %>%
      unname() %>%
      purrr::walk(~ {
        eval(
          setenv(
            !!gsub("-", "_", .),
            dbutils.secrets.get(scope, .)
          )
        )
      })
  }
