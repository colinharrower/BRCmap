\name{gr_num2let}
\alias{gr_num2let}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Eastings & Northings to Grid References
}
\description{
A function to convert UK, Irish or Channel Islands easting and northing coordinates to grid references.
}
\usage{
gr_num2let(easting, northing, OSgrid = "OSGB", keep_precision = TRUE, min_10km = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{easting}{
A vector of eastings from the mapping system origin (in metres)
}
  \item{northing}{
A vector of northings from the mapping system origin (in metres)
}
  \item{OSgrid}{
A character string, or vector of the same length as \code{easting} & \code{northing} detailing the mapping system from which the eastings and northing are derived. Possible values are \code{OSGB, OSNI, UTM30} for UK, Irish or Channel Island grid references respectively. The default \code{OSgrid = "OSGB"} assumes easting and nothing
}
  \item{keep_precision}{
Logical determining whether the precision in the easting and northing should be preserved. The default \code{keep_precision = TRUE} assumes every digit is meaningful and output grid reference are all 1m precision. If set to FALSE then digit pairs that are both zero will be removed (see details for example)
}
  \item{min_10km}{
Logical determining whether when reducing precision of the grid reference to stop at 10km
}
}
\details{
This function takes valid UK, Irish or Channel Islands coordinates as eastings and northings (i.e. x,y from a origin measured in metres) and convert them to their corresponding grid references. During this process false precision resulting from eastings and northings being given in m, can be optionally removed by removing digit pairs from the right where both numbers are zero. 
}
\value{
A vector of the same length as the \code{easting} and \code{northing} vectors containing the grid references
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
\code{\link{gr_let2num}}
}
\examples{
}