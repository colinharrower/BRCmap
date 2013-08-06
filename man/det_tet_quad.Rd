\name{det_tet_quad}
\alias{det_tet_quad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine Tetrad and Quadrant
}
\description{
Determine the tetrad and/or quadrant for a grid reference
}
\usage{
det_tet_quad(gridref, precision = NULL, prec_out = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A vector of valid UK, Irish or Channel Islands Grid References (see details).
}
  \item{precision}{
A value, or vector of same length as \code{gridref}, detailing the precision of the grid reference(s) supplied in \code{gridref}.
}
  \item{prec_out}{
A value giving the required precision of the Grid References to be returned (in meters). The default (\code{prec_out = NULL}) is to return both tetrad and quadrant information. Valid values of \code{prec_out} are: 5000 or 2000.
}
}
\details{
This function is more likely to be used via \code{reformat_gr} which provides a wrapper to this function when asked to output tetrad or quadrant grid references.
Note: The tetrad and quadrants returned are the full tetrad and quadrant grid references and not just the tetrad/quadrant letter codes.

The grid references accepted by this function are valid OSGB (Ordnance suvery of Great Britain, e.g. SP4209, TN389207), OSNI (Ordnance Survey of Northern Ireland, e.g. D0803, N0138) or UTM (Universal Transverse Mercator, e.g. WV595475, WA569079) grid references. Note that the UTM grid reference can only be used for the Channel Islands and are only in the format as shown by the example above. The function can also accept tetrad or quadrant grid references (e.g. SP40T, SP40NW)
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
Colin Harrower
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{reformat_gr}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Create a vector of grid references
gridrefs <- c("SP4297","WV569079","H49280101","NN29","NT20S","TL97NW")

## determine tetrad and quadrants for the grid references
det_tet_quad(gridrefs)

## Extract tetrad and quadrant codes from grid references
det_tet_quad(gridrefs, prec_out = 2000)
