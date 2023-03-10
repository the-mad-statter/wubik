% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbutils.secrets.R
\name{dbutils.secrets.put}
\alias{dbutils.secrets.put}
\title{Put secret}
\usage{
dbutils.secrets.put(
  key,
  value,
  scope = "data-brokers",
  value_type = c("string_value", "bytes_value"),
  token = Sys.getenv("DATABRICKS_PAT"),
  host = Sys.getenv("DATABRICKS_HOST")
)
}
\arguments{
\item{key}{A unique name to identify the secret.}

\item{value}{The secret value.}

\item{scope}{The name of the scope to which the secret will be associated.}

\item{value_type}{If string_value, the value will be stored in UTF-8 (MB4)
form. If bytes_value, the value will be stored as bytes.}

\item{token}{A databricks API personal access token.}

\item{host}{A databricks host (e.g.,
dbc-a1b2345c-d6e7.cloud.databricks.com)}
}
\value{
\code{\link[httr2:response]{httr2::response()}}
}
\description{
Put secret
}
\examples{
\dontrun{
dbutils.secrets.put("ali-baba", "open sesame")
}
}
