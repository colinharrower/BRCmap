\name{helmert_trans_vars}
\alias{helmert_trans_vars}
\docType{data}
\title{
Helmert transformation parameters required for converting coordinates between between datums
}
\description{
This dataset contains the 7 Helmert transformation parameters required for coverting coordinates between datum commonly used in the UK.
}
\usage{data(helmert_trans_vars)}
\format{
  A data frame with 6 observations on the following 8 variables.
  \describe{
    \item{\code{TRANS}}{a character vector detailing the conversion to which the parameters related, e.g. \code{"OSNItoOSGB"}}
    \item{\code{tx}}{The x translation parameter (in metres)}
    \item{\code{ty}}{The y translation parameter (in metres)}
    \item{\code{tz}}{The z translation parameter (in metres)}
    \item{\code{rx}}{The x rotation parameter (in radians)}
    \item{\code{ry}}{The y rotation parameter (in radians)}
    \item{\code{rz}}{The z rotation parameter (in radians)}
    \item{\code{s}}{The scale factor}
  }
}
\details{
This data in this table can be used to convert datum between coordinates from OSNI and OSGB, OSGB and WGS84 mapping systems. The datum WGS84 is the datum used by most GPS devices and is needed to convert British or Irish coordinates to GPS type Latitude and Longitude values.

Unfortunately there seem to be discrepancy in the parameter values avaiable on the internet for these transformations so accurracy of these values still needs to be fully tested.

}
\source{

}
\references{
For more information on Helmert tranformation:
\cr\url{http://en.wikipedia.org/wiki/Helmert_transformation}
}
\examples{
## Load data
data(helmert_trans_vars)
## View data
helmert_trans_vars
}
\keyword{datasets}
