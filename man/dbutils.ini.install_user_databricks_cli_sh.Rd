% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbutils.ini.R
\name{dbutils.ini.install_user_databricks_cli_sh}
\alias{dbutils.ini.install_user_databricks_cli_sh}
\title{Cluster-scoped init script install-\if{html}{\out{<user>}}-<token_name>-databricks-cli.sh}
\usage{
dbutils.ini.install_user_databricks_cli_sh(
  token_name,
  token_value = Sys.getenv("DATABRICKS_TOKEN"),
  user = dbutils.credentials.current_user(),
  host = Sys.getenv("DATABRICKS_HOST")
)
}
\arguments{
\item{token_name}{token name}

\item{token_value}{token value}

\item{user}{user account for which to write .databrickscfg}

\item{host}{databricks host name}
}
\description{
Cluster-scoped init script install-\if{html}{\out{<user>}}-<token_name>-databricks-cli.sh
}
\examples{
\dontrun{
dbutils.ini.install_user_databricks_cli_sh()
}
}
