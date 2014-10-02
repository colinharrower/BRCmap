\name{plotUK_gr_points}
\alias{plotUK_gr_points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add points to a map using grid references
}
\description{
A function to take a list of grid references, convert them to eastings & nortings on the British grid and then add standard R plotting points at the coordinates
}
\usage{
plotUK_gr_points(gridref, centre = TRUE, gr_prec = NULL, 
ci_insert = FALSE, ci_origin = c(-180000,30000), unit_conv = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A character vector containing the grid references to be added to the map
}
\item{centre}{
A logical variable determing whether the point should be added at the centre of the specified grid reference or at the lower left corner. The default \code{centre = TRUE} will plot the point at the centre of the square.
}
\item{gr_prec}{
A numerical vector specifying the precision of each grid reference. The default \code{gr_prec = NULL} will determine the precision directly from the grid reference. This variable will only be needed if the default precision needs to be overridden.
}
\item{ci_insert}{
A logical variable determining whether Channel Islands grid references are to be plotted in an insert box to the left of the UK. The default \code{ci_insert = FALSE} will plot the Channel Islands grid references in their true position to the south of the UK.
}
\item{ci_origin}{
A vector of two values giving the x & y cooridinates for the origin of the Channel Islands grid relative to the the UK.
}
\item{unit_conv}{
A legacy argument that will be removed in the next update.
}
\item{...}{
Any other arguments that will be passed to the \code{points} function used to add the points to the map. Examples of typical arguments that you may want to pass through are \code{pch, col, cex}.
}
}
\details{
This function converts UK grid references strings (i.e. "SP4386") to x, y (eastings and northing) values and then uses the base \code{points} fuction to add points at these coordinates to an existing plot. The coordinate system used in the Ordnace Survey of Great Britain (OSGB), grid reference from Ireland (OSNI) or the Channel Islands are reprojected onto the British grid.

This function is essentially a wrapper that sequentially uses several other functions in this package to convert and reproject grid references where necesary.
}
\value{
By default this function does not return anything, but if the output from the function is assigned it will return the x,y values (eastings & northings on OSGB) of the grid references via the \code{invisible} function.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Harrower
}
\note{
It is important to note that the points added are not scaled relative to the precision of the grid reference but are simply the default points sizes in R.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{plotUK_gr}}, \code{\link{plotUK_gr_cats}}
}
\examples{
## Example of using plotUK_gr_points function to add a couple of dots to an existing map

  # Load UK data
    data(UK)
    
  # Plot UK outline
    plot_GIS(UK, new.window = FALSE)
    
  # Create a vector of grid references to be added
    gridrefs <- c("SP4297","WV569079","H49280101","NN29","NT20S","TL97NW")
    
  # Add points to map
    plotUK_gr_points(gridrefs, col="red", pch = 16)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mapping }
\keyword{ Grid reference }
\keyword{ Channel Islands }