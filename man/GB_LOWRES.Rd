\name{GB_LOWRES}
\alias{GB_LOWRES}
\docType{data}
\title{
List of Spatial Polygons containing the GB outline
}
\description{
This list contains the data required to plot the outline of the GB. The data contained in the list is in a SpatialPolygons format (see \code{sp} package). The polygons in this datasets are at a much lower resolution that the polygons in UK dataset. Note this low resolution version does not include Ireland or the Channel Islands.
}
\usage{data(GB_LOWRES)}
\format{
 The list contains three \code{SpatialPolygons} objects:

\code{britain} a \code{SpatialPolygons} object containing the polygons required to draw the british outline.

\code{ireland} a \code{SpatialPolygons} object containing the polygons required to draw the Irish outline.

\code{ci} a \code{SpatialPolygons} object containing the polygons required to draw the Channel Islands outline.
}
\details{
This dataset contains a lower resolution outline for the UK than the one contained within the \code{UK} dataset.
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
  data(UK)
# Plot outline data
  plot_GIS(UK)
}
\keyword{datasets}
