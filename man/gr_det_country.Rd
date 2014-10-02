\name{gr_det_country}
\alias{gr_det_country}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine the projection of a grid reference
}
\description{
This function determines the projection/datum of grid references for the grid reference systems commonly used in the UK
}
\usage{
gr_det_country(gridref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A character vector containing the grid references for which the projection/datum is to be determined
}
}
\details{
This function attempts to determine the projection/datum to which the grid references supplied belong. 
}
\value{
This function returns a character vector containing the codes for the commonly encountered projection/datums in the UK ("OSGB", "OSNI", "UTM30").
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Harrower
}
\note{
This function is probably going to be superseded by \code{gr_components} function which does the same as this and the \code{fmt_gridref} function but at present a couple of functions in the package internally make use of this function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{gr_components}}, 
  \code{\link{fmt_gridref}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### This example will use the function to determine the datum precision of a 
### vector of grid references

## Create a vector of random grid references, including one broken grid 
## reference (with one missing digit)
  test_grs = c("SP42","N2329","WV6753","SK38","NT3002","N34T","H38NE")
  
## Now determine precision/datum
  gr_det_country(test_grs)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Grid reference }

