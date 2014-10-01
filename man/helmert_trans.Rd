\name{helmert_trans}
\alias{helmert_trans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform Helmert Tranforformation on x,y,z cartesian coordinates
}
\description{
This function performs a Helmert transformation to convert cartesian coordinates between mapping systems using different datum.
}
\usage{
helmert_trans(x, y, z, trans = "OSNItoOSGB", trans_params = NULL, full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Numerical vector contain x cartesian coordinates to be converted
}
  \item{y}{
  Numerical vector contain y cartesian coordinates to be converted
}
  \item{z}{
  Numerical vector contain z cartesian coordinates to be converted
}
  \item{trans}{
  Vector of text stings determining which transformation parameters to use for each grid reference. This should be the \code{row.name} of the row in \code{trans_params} which contains the helmert paramters for the transformation you wish to use for that grid reference. The default \code{trans = "OSNItoOSGB"} converts cartesian coordinates from the Irish datum to the British Datum
}
  \item{trans_params}{
  A \code{data.frame} containing Herlert transformation parameters. The default \code{trans_params = NULL} will load & use the built in dataset \code{hermert_trans_vars} which contains the parameters need to convert the most common mapping systems using in the UK
}
  \item{full_output}{
A logical variable determining whether to return all columns or only to return the new transformed x,y,z coordinates. The default \code{full_output = FALSE} will return only the transformed x,y,z coordinates.
}
}
\details{
In theory this function can be used to peform Helmert transformation as long as you have the herlmert parameters for this transformation. The default set of parameters included in the \code{BRCmap} package included parameters to convert coordiantes between UK, Irish, Channel Islands, and WGS84 datum.

The helert datum transformation is only one step in converting grid reference from one map system to another. The full process involves; 
\cr 1) Convert original grid reference to eastings and northings, 
\cr 2) convert easting and northing to latitude and longitude in original datum, 
\cr 3) convert latitude & longitude to cartesian coordinates in original datum, 
\cr 4) transform cartesian coordinates form original datum to new datum, 
\cr 5) convert new cartesian cooridinates to latitude & longitude in new datum, 
\cr 6) convert new latitude and longitude to eastings and northings, 
\cr 7) optionally convert new eastings & northings back to grid reference in new mapping system

Therefore to convert between mapping systems you will need to use several function in the \code{BRCmap} package. To do this you can either manually use each function, use one of the pre-written wrapper functions provided in the \code{BRCmap} packge (if one exists for the transformations you require), or write you own custom wrapper function.
}
\value{
A \code{data.frame} containing the following columns:
\item{x}{The transformed x cateisian coordinate(s)}
\item{y}{The transformed y cateisian coordinate(s)}
\item{z}{The transformed z cateisian coordinate(s)}
}
\references{
Code is translated from Javascript written under open source attribution licence by Chris Veness, and provided on the following website:
\url{http://www.movable-type.co.uk/scripts/latlong-convert-coords.html}
}
\author{
Colin Harrower
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{OSgridReprojection}}, 
\code{\link{OSGridstoLatLong}}, 
\code{\link{LatLong_Cartesian}},
\code{\link{Cartesian_LatLong}},
\code{\link{LatLongtoOSGrids}}
}
\examples{
  ## Examples to follow
}