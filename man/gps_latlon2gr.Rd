\name{gps_latlon2gr}
\alias{gps_latlon2gr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert WGS84 latitude and longitude coordinates to UK grid references
}
\description{
This function converts WGS84 latitude and longitudes to valid UK grid references using one of the standard projections used within the UK.
}
\usage{
gps_latlon2gr(latitude, longitude, out_projection = "OSGB", return_type = "both")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{latitude}{
  A numerical vector containing the latitude values to be converted. Values need to be in the decimal format for latitudes
  }
  \item{longitude}{
  A numerical vector containing the longitude values to be converted. Values need to be in the decimal format for longitudes
  }
  \item{out_projection}{
  A character vector specifying the projection in which the grid references are to be returned. Currently accepted values are \code{("OSGB", "OSNI", "UTM30")}. The default \code{out_projection = "OSGB"} will return grid references based on the OSGB value
  }
  \item{return_type}{
  A character vector determining what to output (see values section below) 
  }
}
\details{
The latitudes and longitudes passed to this function should be latitude and longitudes based on the WGS84 projection/datum. These are the latitudes and longitudes which are what most people will think of as latitudes and longitudes, i.e. the type typically given by GPS devices or used on online mapping (such as google maps). It should be noted however that latitudes and longitudes can be calculated for any projection/datum and that other funcitons  in this package will return latitude and latitudes in the same projection as the original grid references (some of which are used internally within this function).
}
\value{
The function will return a data.frame, that depending on the value given to \code{return_type} in the arguments will have some or all of the following columns.
  \item{GRIDREF}{
  The grid references (in the specified projection) that corespond with the latitude and longitude values given in the arguments
  }
  \item{EASTING}{
  The easting in metres (in the specified projection) that corespond with the latitude and longitude values given in the arguments
  }
  \item{NORTHING}{
  The northing in metres (in the specified projection) that corespond with the latitude and longitude values given in the arguments
  }
If \code{return_type  = "both"} then the data.frame will have all three colums. In contrast if \code{return_type = "gr"} or \code{return_type = "en"} then the returned data.frame will only contain the grid reference column or the eastings & northing columns respectively.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## This example will return the grid reference and eastings & northings for the Centre for Ecology & Hydrology using the Ordnance Survey of Great Britain projection (OSGB)

  gps_latlon2gr(51.602309,-1.110601)

## This example will return only the grid reference for the Radcliffe Camera in Oxford

  gps_latlon2gr(51.753737,-1.254197, return_type = "gr")

## This example will return the grid reference and easting & northing for the Durrell Wildlife Park in Jersey, Channel Islands using the Miltary Grid Reference System (based on UTM30)

  gps_latlon2gr(49.228733,-2.073758, out_projection = "UTM30")

## This example will return the grid reference and easting & northing for the centre of Dublin, Ireland using the Ordnance Survey of Northern Ireland projection ("OSNI")

  gps_latlon2gr(53.350970, -6.260192, out_projection = "OSNI")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
