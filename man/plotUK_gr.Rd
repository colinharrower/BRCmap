\name{plotUK_gr}
\alias{plotUK_gr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot UK grid references
}
\description{
This function will plot valid UK grid references onto an exisiting map
}
\usage{
plotUK_gr(gridref, gr_prec = NULL, ci_insert = FALSE, 
ci_origin = c(-180000, 30000), unit_conv = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A character vector containing the grid references to added to the map 
}
  \item{gr_prec}{
A numerical value or numerical vector (of the same length as \code{gridref}) giving the precision of the grid references in \code{gridref}. The default \code{gr_prec = NULL} will determine the precision from the grid reference.
}
  \item{ci_insert}{
A logical value determining whether CI values are being plotted in a insert box or in their true position. The default \code{ci_insert = FALSE} will plot the grid references in their true position.
}
  \item{ci_origin}{
A numerical vector containg the x,y values of the false CI origin if using an insert box
}
  \item{unit_conv}{
A numerical value determining the unit conversion if the units for the map to which the points are being added is not metres. In most situation this argument will not be needed.
}
  \item{\dots}{
Other arguments which are passed to the \code{\link{polygon}} function used to plot the grid references. Examples of arguments you may wish to pass are \code{col} or \code{border} to change the fill or border columns of the polygons representing the grid refences.
}
}
\details{
As mentioned in the description this function will added grid references as square polygons to an existing map. It will not create map and will therefore produce an error if there is no existing plot.

It should be noted that if plotting at the UK level it is probably best to to try and avoid plotting grid references smaller than 10km as this make the points quite hard to see. More precise grid reference can be plotted however if the area to be mapped is more restricted and therefore the area more magnified. 
}
\value{
The function can return a data.frame containing the x,y values (easting & northing in OSGB) of the lower left corner of grid references if the output is assigned to an object. If not assigned no output, other than the plotting will be produced
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
Colin Harrower
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{plotUK_gr_cats}}, \code{\link{plotUK_gr_points}}
}
\examples{
## Example of using plotUK_gr_points function to add a couple of dots to an existing map

  # Create a vector of grid references to be plotted
    gridrefs <- c("SP49","WV57","H40","NN29","NT20","TL97")
  
  # Load UK data
    data(UK)
  
  # Plot UK outline
    plot_GIS(UK, new.window = FALSE)
  
  # Add these grid references to an exisiting plot
    plotUK_gr(gridrefs, col="darkgreen")
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
