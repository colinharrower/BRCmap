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
plotUK_gr_points(gridref, centre = TRUE, gr_prec = NULL, ci_insert = FALSE, ci_origin = c(-180000,30000), unit_conv = NULL, ...)
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
Any other arguments that will be passed to the points function used to add the points to the map. Examples of typical arguments that you may want to pass through are \code{pch, col, cex}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
