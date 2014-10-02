\name{OSGridstoLatLong}
\alias{OSGridstoLatLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Eastings & Northings to Latitude & Longitude
}
\description{
Converts Easting & Northings to latitude & longitude coordinates in the original projection. See details for warning regarding the latitude & longitude values returned by this function
}
\usage{
OSGridstoLatLong(Easting, Northing, Datum = "OSGB", 
datum_params = NULL, full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Easting}{
A numerical vector containing the easting values for the coordinates to be converted
}
  \item{Northing}{
A numerical vector containing the northing values for the coordinates to be converted
}
  \item{Datum}{
A character value or vector detailing which grid system the easting and northing belong to. The values in this argument are used to determine the conversion parameters required to convert the easting and northings to latitude and longitude values. Permitted values are \code{OSGB}, \code{OSNI}, \code{UTM30}. The default \code{Datum = OSGB} assumes that the easting and northing values are for the Ordnance Survey of Great Britain.
}
  \item{datum_params}{
A data.frame containing the parameters required to convert the current mapping system to eastings & northings. The default (\code{datum_params = NULL}) will load & use the supplied datasets datum_vars, which contains the parameters needed for coversions within the UK. This argument is supplied so that if the easting & northing points are not in one of the three predefined mapping systems (OSGB, OSNI, UTM30) but you know that parameters required to do the transformation you can supply them directly (see details for more information).
}
  \item{full_output}{
A logical variable determining whether the output is a data.frame containing the latitude and longitude values or whether it also contains columns containing the original easting and northing values. The default \code{full_output = FALSE}
}
}
\details{

}
\value{
returns a \code{data.frame} containing the following columns:
\item{Latitude}{
A column containing the latitude values
}
\item{Longitude}{
A column containing the longitude values
}
Optionally the data.frame can also contain the columns:
\item{Easting}{
A column containing the original easting values
}
\item{Northing}{
A column containing the original northing values
}
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
\code{\link{LatLongtoOSGrids}},
\code{\link{gr2gps_latlon}}
}
\examples{
## Example converting a range of eastings & northings from OSGB, OSNI,
## & UTM30 to latitude & longitude values in the original projection

# Vector of grid refs that will be converted
gridrefs <- c("SP49","NO58","WV55","H47","NN29","WV55","WA50","NT20","TL97"
,"NH33","SE25","SN22","SH87","SO00","N20","M85","T01","SX59","NY78")

# First convert to native eatings & northings
gr_en = gr_let2num(gridrefs, return_projection = TRUE)

# Convert the native eastings & northings native latitute & longitude
OSGridstoLatLong(gr_en$EASTING, gr_en$NORTHING,Datum = gr_en$PROJECTION)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Easting & Northing }
\keyword{ Latitude & Longitude }
\keyword{ Coordinate conversion }