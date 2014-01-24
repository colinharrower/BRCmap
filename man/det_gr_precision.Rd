\name{det_gr_precision}
\alias{det_gr_precision}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine grid reference precision
}
\description{
Determine the precision of a grid reference
}
\usage{
det_gr_precision(gridref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A character vector containing a valid UK, Irish or Channel Islands grid reference
}
}
\details{
The number of digits in a grid reference is generally important as it should indicate the precision of the grid reference. The exception to this is when grid references have been padded with zeros, either to force the grid reference to be a certain length, or when a easting and northing with an unknown precision have been converted to a grid reference.
}
\value{
The function returns a vector of precision values (in metres) of the same length as \code{gridref}
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
}
\examples{
det_gr_precision("SP40")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
