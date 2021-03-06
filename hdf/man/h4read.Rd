% Generated by roxygen2 (4.0.2.9000): do not edit by hand
% Please edit documentation in R/h4read.R
\name{h4read}
\alias{h4read}
\title{Read hdf4 files}
\usage{
h4read(fname, sds.name, sds, ...)
}
\arguments{
\item{fname}{A character containing the name of the hdf4 file.}

\item{sds.name}{An array containing all the SDS or vdata contained in the hdf4 file (provided by h4list).}

\item{sds}{A character indicating the SDS to extract.}
}
\description{
This function returns the content of a hdf4 file as a list for a given SDS.
}
\examples{
fname <- data.hdf
list.sds <- h4list(fname,ignore.vd=FALSE)  ## List the SDS and Vdata contained in data.hdf
value.sds <- h4read(fname,list.sds,"Latitude") ## extract "Latitude" from data.hdf
}
\keyword{hdf4}

