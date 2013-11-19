\name{reformat_gr}
\alias{reformat_gr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reformat Grid References
}
\description{
Reformat UK, Irish or Channel Islands grid references from one precision to another precision
}
\usage{
reformat_gr(gridref, prec_out = 10000, precision = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
  A vector of valid UK, Irish or Channel Islands Grid References (see details).
}
  \item{prec_out}{
  A value giving the required precision of the Grid References to be returned (in meters). The default is to return 10km Grid References (10000m). Valid values of \code{prec_out} are: 100000,10000,5000, 2000, 1000,100, 10, 1.
}
  \item{precision}{
  A value, or vector of same length as \code{gridref}, detailing the precision of the Grid Reference(s) supplied in \code{gridref}.
}
}
\details{
Grid references will be reformated to the required precision as long as their precision is higher than or equal to the precision required. For instance a 1km grid reference can be reformated to return a 10km grid reference but not to return a 100m grid reference. In cases where a grid reference cannot be reformated to the required precision then an NA will be returned for those grid references.

The grid references accepted by this function are valid OSGB (Ordnance suvery of Great Britain, e.g. SP4209, TN389207), OSNI (Ordnance Survey of Northern Ireland, e.g. D0803, N0138) or UTM (Universal Transverse Mercator, e.g. WV595475, WA569079) grid references. Note that the UTM grid reference can only be used for the Channel Islands and are only in the format as shown by the example above. The function can also accept tetrad or quadrant grid references (e.g. SP40T, SP40NW)


}
\value{
A vector of the same length as \code{gridref} containing the reformated grid reference 
}
\references{
For more information on grid references see:\cr
  \url{http://www.ordnancesurvey.co.uk}, 
  \cr\url{http://en.wikipedia.org/wiki/Ordnance_Survey_of_Northern_Ireland},
  \cr\url{http://www.conchsoc.org/node/2064}
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
}
\examples{
## Create a vector of grid references
gridrefs <- c("SP4297","WV569079","H49280101","NN29","NT20S","TL97NW")

## Reformat these grid references to 10km precision
gr10km = reformat_gr(gridrefs)

## Reformat the grid references to 1km precision (note the NA values for the 10km grid reference which couldn't be formated to required precision)
gr1km = reformat_gr(gridrefs, prec_out = 1000)

## Reforamt the grid reference to tetrad precision (2000m)
grtet = reformat_gr(gridrefs, prec_out = 2000)


