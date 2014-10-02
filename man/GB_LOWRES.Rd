\name{GB_LOWRES}
\alias{GB_LOWRES}
\docType{data}
\title{
A Spatial Polygons object containing the GB outline
}
\description{
This object contains the data required to plot the outline of the GB. The data contained in the object is in a SpatialPolygons format (see \code{sp} package). 
}
\usage{data(GB_LOWRES)}
\format{
 A mentioned above the object is a spatial polygons class object, see documentation for \code{sp} package for more details on this object class.
}
\details{
The polygons in this datasets are at a much lower resolution that the polygons for the GB provided in \code{UK} dataset. NOTE this low resolution version is only GB and does not include Ireland or the Channel Islands.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\seealso{
\code{\link{UK}}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
# Load outline data into R

  data(GB-LOWRES)
  
# Plot outline data

  plot_GIS(GB_LOWRES)
}
\keyword{datasets}
\keyword{ Mapping }
