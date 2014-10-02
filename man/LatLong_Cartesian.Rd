\name{LatLong_Cartesian}
\alias{LatLong_Cartesian}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Latitude & Longitude to Cartesian Coordinates
}
\description{
This function converts latitude and longitude coordinates to Cartesian coordinates.
}
\usage{
LatLong_Cartesian(Latitude, Longitude, Datum = "OSGB", 
datum_params = NULL, H = NULL, full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Latitude}{
A numerical vector containing the latitudes of the coordinates to be converted
}
  \item{Longitude}{
A numerical vector containing the longitudes of the coordinates to be converted
}
  \item{Datum}{
A character vector containing codes for the Datum in which the coordinates to be converted. The default value \code{Datum = "OSGB"} is for Ordnance Survey of Great Britain, other possible values are \code{"OSNI"} for the Ordnance Survey of Northern Ireland or \code{"UTM30"} for UTM for Channel Islands.
}
  \item{datum_params}{
The data.frame containing the parameters for datum required in the transformation. The default \code{datum_params = NULL} will load and use the dataset \code{dataum_vars} provided with BRCmap, which contains the parameter required for the three datum commonly used in the UK, if not already loaded into the current session.
}
  \item{H}{
A vector containing the height parameter that will be used in the conversion. The default \code{H = NULL} will use a height of zero and in general it will be best to leave this set to the default.
}
  \item{full_output}{
A logical value determining whether to also return the input latitude and longitude values along with the cartesian coordinates or whether to just return the cartesian coordinates. The default \code{full_output = FALSE} will return only the cartesian coordinates.
}
}
\details{
This function does not convert between projections so the datum/projection of the cartesian coordinates will be the same as the Latitude and Longitude coordiantes that were supplied.
}
\value{
The function returns a data.frame containing the cartesian coordinates and optionally also the original latitude and longitude values if \code{full_output = TRUE}.
\item{x}{the x cartesian coordinates}
\item{y}{the y cartesian coordinates}
\item{z}{the z cartesian coordinates}
\item{LATITUDE}{(optionally) the original latitude values supplied}
\item{LONGITUDE}{(optionally) the original longitude values supplied}
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
}
\examples{
## Example of converting latitude and longitude to cartesian coordinates

# Convert Latitude and Longitude of Grid reference "SP42"
# in orignal datum/projection to cartesian coordinates

LatLong_Cartesian(53.8544788314868, -1.39183128783271)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Coordinate conversion }
\keyword{ Latitude & Longitude }
\keyword{ Cartesian Coordinates }

