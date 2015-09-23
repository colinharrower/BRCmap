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
gps_latlon2gr(latitude, longitude, out_projection = "OSGB", return_type = "gr")
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
  A character vector determining what to output (see values section below). Accepted values are \code{"gr","en","both"} to return the a vector of grid references, easting & northings or both grid references and the eastings & northings (see values section for more details). The default \code{return_type = "gr"} is to return the gridrefs.
  }
}
\details{
The latitudes and longitudes passed to this function should be latitude and longitudes based on the WGS84 projection/datum. These are the latitudes and longitudes which are what most people will think of as latitudes and longitudes, i.e. the type typically given by GPS devices or used on online mapping (such as google maps). It should be noted however that latitudes and longitudes can be calculated for any projection/datum and that other funcitons  in this package will return latitude and latitudes in the same projection as the original grid references (some of which are used internally within this function).
}
\value{
The function will either return a character vector or a data.frame, depending on the value given to \code{return_type} in the arguments. When \code{return_type = "gr"} the output will be a vector containg the grid references. With the other output options a data.frame will be returned that have some or all of the following columns.
  \item{GRIDREF}{
  The grid references (in the specified projection) that corespond with the latitude and longitude values given in the arguments
  }
  \item{EASTING}{
  The easting in metres (in the specified projection) that corespond with the latitude and longitude values given in the arguments
  }
  \item{NORTHING}{
  The northing in metres (in the specified projection) that corespond with the latitude and longitude values given in the arguments
  }
If \code{return_type  = "both"} then the data.frame returned will contain all colums detailed above. If the value supplied to the argument \code{return_type} is \code{"en"} then the returned data.frame will contain only the the eastings & northing columns respectively.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Harrower
}
\note{
This function has been modified so the output format and the default output behaviour differ from previous version, which may cause issues if you have existing code. Firstly the default output \code{return_type} is now set to \code{"gr"} to return only the grid references. Additionally when returning only the grid reference the output type is now a vector rather than a 1 column \code{data.frame}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{gr2gps_latlon}},
\code{\link{gps_latlon2mgrs}}
}
\examples{
## This example will return the grid reference and eastings & northings
## for the Centre for Ecology & Hydrology using the Ordnance Survey of 
## Great Britain projection (OSGB)

  gps_latlon2gr(51.602309,-1.110601, return_type = "both")

## This example will return only the grid reference for the 
## Radcliffe Camera in Oxford

  gps_latlon2gr(51.753737,-1.254197, return_type = "gr")
  gps_latlon2gr(51.753737,-1.254197) # this does the same but using the default for return_type

## This example will return the grid reference and easting & northing for
## the Durrell Wildlife Park in Jersey, Channel Islands using the Miltary
## Grid Reference System (based on UTM30)

  gps_latlon2gr(49.228733,-2.073758, out_projection = "UTM30")

## This example will return the grid reference and easting & northing for
## the centre of Dublin, Ireland using the Ordnance Survey of Northern
## Ireland projection ("OSNI")

  gps_latlon2gr(53.350970, -6.260192, out_projection = "OSNI")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Grid reference }
\keyword{ Reprojection }
\keyword{ Latitude & Longitude }