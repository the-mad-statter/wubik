% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbutils.box.R
\name{dbutils.box.ftps_upload}
\alias{dbutils.box.ftps_upload}
\title{Box FTPS Upload}
\usage{
dbutils.box.ftps_upload(
  local,
  remote = basename(local),
  home = Sys.getenv("WUSTL_BOX_HOME"),
  user = Sys.getenv("WUSTL_BOX_USER"),
  pass = Sys.getenv("WUSTL_BOX_PASS"),
  verbose = FALSE,
  connecttimeout = 10000,
  ...
)
}
\arguments{
\item{local}{file name or path of the local file to be uploaded.}

\item{remote}{the path to which the content is to be uploaded.}

\item{home}{prepended to remote to form the full remote path.}

\item{user}{Box username (i.e., WashU email)}

\item{pass}{unique password for external applications. Created at
\url{https://wustl.app.box.com/account}}

\item{verbose}{emit some progress output}

\item{connecttimeout}{desired connection timeout in milliseconds}

\item{...}{other arguments passed to \link[curl:handle]{curl::handle_setopt()}}
}
\description{
Box FTPS Upload
}
\examples{
\dontrun{
dbutils.box.ftps_upload("/dbfs/home/my_user/my_img.png")
}
}
