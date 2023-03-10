% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbutils.ini.R
\name{dbutils.ini.restore_r_library_sh}
\alias{dbutils.ini.restore_r_library_sh}
\title{Cluster-scoped init script restore-r-library.sh}
\usage{
dbutils.ini.restore_r_library_sh(
  persistent_path = dbutils.rlib.path("persistent", "file"),
  ephemeral_path = dbutils.rlib.path("ephemeral", "file")
)
}
\arguments{
\item{persistent_path}{path to the persistent library}

\item{ephemeral_path}{path to the ephemeral library}
}
\description{
Cluster-scoped init script restore-r-library.sh
}
\examples{
\dontrun{
dbutils.ini.restore_r_library_sh()
}
}
