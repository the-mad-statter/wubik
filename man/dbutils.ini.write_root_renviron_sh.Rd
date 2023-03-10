% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbutils.ini.R
\name{dbutils.ini.write_root_renviron_sh}
\alias{dbutils.ini.write_root_renviron_sh}
\title{Cluster-scoped init script write-root-Renviron.sh}
\usage{
dbutils.ini.write_root_renviron_sh(key_values)
}
\arguments{
\item{key_values}{\code{\link[dplyr:reexports]{dplyr::tibble()}} of \code{key} and \code{value} pairs.}
}
\description{
Cluster-scoped init script write-root-Renviron.sh
}
\examples{
\dontrun{
dbutils.ini.write_root_renviron_sh(
  dplyr::tibble(
    key = c("KEY_1", "KEY_2"),
    value = c("VALUE_1", "VALUE_2")
  )
)
}
}
