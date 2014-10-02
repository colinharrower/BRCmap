\name{fmt_gridref}
\alias{fmt_gridref}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to check the format of grid references
}
\description{
This function checks a series of grid references to determine if they follow the expected rules for the commonly used grid reference systems in the UK. The function will also remove spacing characters that are commonly encountered when dealing with grid references (i.e. spaces and hyphens) as well as any control characters that may be present.
}
\usage{
fmt_gridref(gridref, gr_fmt = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
  A character vector containing the grid references to be checked
  }
  \item{gr_fmt}{
  A numerical value determining which parts of valid grid references are to be returned. The default \code{gr_fmt = NULL} will return the whole grid reference, minus any spacing or control characters. Other possible values are 1 = Whole gridref minus any tet/quad codes, 2 = Inital letter(s) only, 3 = Digits only, 4 = Tetrad/Quad codes only (if present), 5 = Tetrad only (if present), 6 = Quadrant only (if present). 
  }
}
\details{
As mentioned above this function attempts to check the formating of any grid references to avoid failures in other functions.
}
\value{
The function returns are character vector containing all the valid grid references (minus any spacing and/or control characters) replacing values that do not match the formats for a valid (OSGB, OSNI, UTM30) grid references with \code{NA}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Harrower
}
\note{
This function is probably going to be superseded by \code{gr_components} function which effectively does the same as this function but at present a couple of functions in the package still internally make use of this function hence is continued existence.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{gr_components}}, 
  \code{\link{gr_det_country}}
}
\examples{
## This example will check the following grid references and remove entries 
##that are in a non-valid format. 
##Note it will also remove spaces and hyphens in the grid references if present.

  ## Create a vector of random grid references
  ## including one non-valid grid reference 
  test_grs = c("SP42", "N2329", "WV6753", "SOJ458", "SK3-8-", "NT 30 02",
  "N34T", "H38NE")
  
  # Now check these grid references
  fmt_gridref(test_grs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Grid reference }

