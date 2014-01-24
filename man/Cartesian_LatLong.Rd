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
Cartesian_LatLong(x, y, z, Datum = "OSGB", datum_params = datum_vars, full_output = FALSE)
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
A data.frame containing the datum/projection parameters required to perform the calculation. The default \code{datum_param = datum_vars} uses the dataset provided with the \code{BRCmap} package, which already includes the parameters for OSGB, OSNI and UTM30/WGS84 conversions
}
  \item{full_output}{
A logical variable determining whether the function should return full output (including input values) or just the new latitude & longitude values
}
}
\details{
It is important to understand that this function converts cartesian coordinates to a latitude & longitude \bold{in the same datum/projection} as the original coordinates. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
