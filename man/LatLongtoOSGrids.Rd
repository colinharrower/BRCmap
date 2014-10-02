\name{LatLongtoOSGrids}
\alias{LatLongtoOSGrids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Latitude & Longitude coordinates to eastings and northings
}
\description{
This function converts latitude and longitude coordinates to easting and northing values. Note the function does not convert between datum/projections (see details for more information).
}
\usage{
LatLongtoOSGrids(Latitude, Longitude, Datum = "OSGB", 
datum_params = NULL, full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Latitude}{
A numerical vector containing the latitude values to be converted
}
  \item{Longitude}{
A numerical vector containing the longitude values to be converted
}
  \item{Datum}{
A character vector containing codes specifying which datum/projection the coordinates are in
}
  \item{datum_params}{
A data.frame containing the parameters required for the conversion. The default \code{datum_params = NULL} will load and use the datasets \code{datum_vars}, which is supplied with the package.
}
  \item{full_output}{
A logical value determining whether to also return the original input latitude and longitude values
}
}
\details{
As mentioned is the description section above this function does not change the datum/projection so the eastings and northings returned will be in same the datum/projection as the latitude and longitude values. This means that this function cannot directly be used to convert latitude and longitude values from GPS devices, google maps, etc to Ordnance Survey eastings and northings as the datum/projection  differ between these coordinate systems (WGS84 in the case of GPS lat lon and OSGB in the case of Ordnance Survey of Great Britain). Instead for this type of conversion use the \code{gps_latlon2gr} function, therefore in most cases users will not want use the function directly but rather use other functions which will internally call this function as part of the conversion process.
}
\value{
The function will return a data.frame containg the eastings and northings values and optionally also the Datum codes and original latitude and longitude values if \code{full_output = TRUE}.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
\item{LATITUDE}{(Optional) The original latitude values}
\item{LONGITUDE}{(Optional) The original longitude values}
\item{DATUM}{(Optional) The datum codes for each set of coordinates}
\item{EASTING}{The easting values in metres}
\item{NORTHING}{The northing values in metres}
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
\code{\link{gps_latlon2gr}},
\code{\link{OSGridstoLatLong}}
}
\examples{
## Example to convert a latitude and longitude to eastings and northings
## Note that the projection will not be tranformed

LatLongtoOSGrids(53.854478831, -1.391831, "OSGB")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Coordinate conversion }
\keyword{ Latitude & Longitude }
\keyword{ Easting & Northing }
