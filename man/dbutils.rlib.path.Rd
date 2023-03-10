% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbutils.rlib.R
\name{dbutils.rlib.path}
\alias{dbutils.rlib.path}
\title{Library path}
\usage{
dbutils.rlib.path(
  type = c("ephemeral", "persistent"),
  fmt = c("file", "spark"),
  user = dbutils.credentials.current_user()
)
}
\arguments{
\item{type}{the type of path to generate}

\item{fmt}{how to format the address}

\item{user}{username of the current user}
}
\value{
path to the recommended ephemeral or persistent library locations
}
\description{
Library path
}
\examples{
dbutils.rlib.path(user = "dborker")
dbutils.rlib.path(fmt = "spark", user = "dborker")
dbutils.rlib.path("persistent", user = "dborker")
dbutils.rlib.path("persistent", "spark", "dborker")
}
