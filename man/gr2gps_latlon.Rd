\name{gr2gps_latlon}
\alias{gr2gps_latlon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert UK grid references to WGS84 latitude and longitude coordinates
}
\description{
This function converts grid references using one of the standard projections used within the UK to WGS84 latitude and longitudes.
}
\usage{
gr2gps_latlon(gridref, precision = NULL, centre = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
  A character vector containing the grid references to be converted. Grid references can be in any of the standard formats used within the UK (OSGB, OSNI, UTM30).
  }
  \item{precision}{
  Can be either \code{NULL} or a numerical vector of the same length as \code{gridref} containing the precision of each grid reference. In general this can be determined from the grid reference, which is the case in the default \code{precision = NULL}. This argument will only really be used if for some reason the precision to be used is not the same as the precision of the grid reference.
  }
  \item{centre}{
  A logical value determining whether the longitude & latitude returned should be for lower left corner or centre of the supplied grid square. The default \code{centre = TRUE} will return the coordinates of the centre point
  }
}
\details{
This function takes the grid references in the formats most commonly used in the UK (OSGB, OSNI, UTM30) and converts them to WGS 84 latitude and longitude coordinates. WGS 84 latitude and longitude coordinates are what typically what people mean when they talk about latitude and longitude coordinates (i.e. used by GPS), however it should be noted that latitudes and longitudes can be produced for any projection/datum and indeed some of the functions in this package will return latitudes and longitudes that are in the native projections of the grid references (e.g. OSGB) and these are probably not what the user wants.
}
\value{
This function returns a \code{data.frame} containing two columns:
  \item{LATITUDE}{A numerical vector containing the WGS 84 latitudes for each grid reference.}
  \item{LONGITUDE}{A numerical vector containing the WGS 84 longitudes for each grid reference.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Harrower
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{gps_latlon2gr}}
}
\examples{
## The following example converts the following OSNI grid reference for the 
## centre of Dublin to WGS 84 latitude & longitude coordinates

gr2gps_latlon("O1590434801")

## This example takes the grid reference for the Centre of Ecology & 
## Hydrology Wallingford and converts it to WGS 84 latitude & longitude 
## coordinates

gr2gps_latlon("SU6169589648")

## This example takes the grid reference for the Durrell Wildlife Park in 
## Jersey, Channal Islands and converts it WGS 84 latitude & longitude 
## coordinates

gr2gps_latlon("WV6743653296")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
