\name{UK_old}
\alias{UK_old}
\docType{data}
\title{
List of Spatial Polygons containing UK outlines
}
\description{
This list contains the data required to plot the outline of the UK. The data contained in the list is in a SpatialPolygons format (see sp package)
 (\code{ci}).
}
\usage{data(UK_old)}
\format{
 The list contains three \code{SpatialPolygons} objects:

\code{britain} a \code{SpatialPolygons} object containing the polygons required to draw the british outline.

\code{ireland} a \code{SpatialPolygons} object containing the polygons required to draw the Irish outline.

\code{ci} a \code{SpatialPolygons} object containing the polygons required to draw the Channel Islands outline.
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\seealso{
\code{\link{GB_LOWRES}}
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
\keyword{ Mapping }