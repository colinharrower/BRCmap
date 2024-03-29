\name{OSgridReprojection}
\alias{OSgridReprojection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert UK, Irish or Channel Islands coordinates to another coordinate system.
}
\description{
A function to convert UK, Irish or Channel Islands coordinates (given as easting & northing) to another mapping system.
}
\usage{
OSgridReprojection(Easting, Northing, org_grid = "OSNI", 
out_grid = "OSGB", full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Easting}{
 A vector of easting coordinates to be converted
}
  \item{Northing}{
A vector of northing coordinates to be converted
}
  \item{org_grid}{
A code determining the original mapping system. Valid values are; \code{"OSGB"}, \code{"OSNI"}, \code{"UTM30"}. The default \code{org_grid = "OSNI"} assumes intput coordinates are easting & northing on the Irish Grid
}
  \item{out_grid}{
A code determing the mapping system to which the coordinates are to be converted. As above the valid values are; \code{"OSGB"}, \code{"OSNI"}, code{"UTM30"}. The default (\code{org_grid = "OSGB"}) assumes coordinates returned are to be easting & northing on the British Grid
}
  \item{full_output}{
Logical determining whether the output data.frame is to contain the full range of input and output columns or only the new coordinates. The default (\code{full_output} \code{= FALSE}) will return only columns containing the new eastings and northing coordinates.
}
}
\details{
At present the function will only convert between these the UK, Irish, Channel Islands and WGS84 coordinates systems.
}
\value{
A \code{data.frame} containing the following columns:
\item{EASTING}{The easting after conversion to the new coordinate system}
\item{NORTHING}{The northing after conversion to the new coordinate system}
In addition if \code{full_output = TRUE} then the following columns will also be returned:
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
\code{\link{helmert_trans}},
\code{\link{plotUK_gr}}
}
\examples{
## Example converting a range of eastings & northings from OSGB, OSNI,
## & UTM30 to eastings & northings on the OSGB grid

# Vector of grid refs that will be converted
gridrefs <- c("SP49","NO58","WV55","H47","NN29","WV55","WA50","NT20","TL97"
,"NH33","SE25","SN22","SH87","SO00","N20","M85","T01","SX59","NY78")

# First convert to native eatings & northings
gr_en = gr_let2num(gridrefs, return_projection = TRUE)

# Convert the native eastings & northings to OSGB eastings & northings
OSgridReprojection(gr_en$EASTING,gr_en$NORTHING,org_grid=gr_en$PROJECTION,
out_grid = "OSGB")
}
\keyword{ Reprojection }
\keyword{ Easting & Northing }