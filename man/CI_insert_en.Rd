\name{CI_insert_en}
\alias{CI_insert_en}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reposition Channel Islands coordinates to a false origin
}
\description{
This function is used reposition Channel Islands coordinates to a false position to produce an insert box on the map. The default false position is to the lower left of the mainland of Great Britain (under Ireland if present).
}
\usage{
CI_insert_en(easting, northing, origin = c(-180000, 30000))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{easting}{
A numerical vector containing the easting values of the points to be repositioned
}
  \item{northing}{
A numerical vector containing the northing values of the points to be repositioned
}
  \item{origin}{
The false origin relative to which the points will be repositioned
}
}
\details{
In most cases you will not need to use this function but will instead use \code{CI_insert_gr} which will take grid references rather than easting and northing values.
}
\value{
  This function returns a \code{data.frame} containing the following columns:
  \item{EASTING}{The new easting values for the repositioned points}
  \item{NORTHING}{The new northing values for the repositioned points}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Harrower
}
\note{
Although this function is designed for Channel Islands coordinates it could potentially be used to reposition other coordinates for the purpose of a insert box.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{CI_insert_gr}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
