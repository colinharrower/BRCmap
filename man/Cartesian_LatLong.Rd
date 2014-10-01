\name{Cartesian_LatLong}
\alias{Cartesian_LatLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to convert cartesian coordinates to latitude and longitude
}
\description{
This function takes cartesian x,y,z coordinates in a specified datum/projection and converts them to latitude and longitude values in the same projection. \bold{NOTE}: as the latitude & longitude returned are in the same projection as the original cartesian coordinates this will not necessarily give latitude & longitude values you might expect (see details below)
}
\usage{
Cartesian_LatLong(x, y, z, Datum = "OSGB", datum_params = NULL, full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numerical vector containing the x coordinates to be converted
}
  \item{y}{
A numerical vector containing the y coordinates to be converted
}
  \item{z}{
A numerical vector containing the z coordinates to be converted
}
  \item{Datum}{
The datum in which the latitude and longitude values are to be returned. The default \code{Datum = OSGB} returns latitude & longitude values using the OSGB Datum/projection
}
  \item{datum_params}{
A data.frame containing the datum/projection parameters required to perform the calculation. The default \code{datum_param = NULL} will load the datum_vars dataset provided with the \code{BRCmap} package, if not already loaded into the current session, which contains the parameters for OSGB, OSNI and UTM30/WGS84 conversions.
}
  \item{full_output}{
A logical variable determining whether the function should return also return the height argument of the conversion or just the new latitude & longitude values. The default \code{full_output = FALSE} will return only the latitude & longitude values, in the majority of cases this is all that is probably required.
}
}
\details{
It is important to understand that this function converts cartesian coordinates to a latitude & longitude \bold{in the same datum/projection} as the original coordinates. 
}
\value{
The function returns a \code{data.frame} containing the following columns:
\item{LATITUDE}{The Latitude values of the cartesian coordinates provided (\bold{in the same projection})}
\item{LONGITUDE}{The Longitude values of the cartesian coordinates provided (\bold{in the same projection})}

Optionally the following column will also be returned when \code{full_output = TRUE}:
\item{HEIGHT}{The height value from the coordinate conversion based on the z value and elipsoid parameters}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
