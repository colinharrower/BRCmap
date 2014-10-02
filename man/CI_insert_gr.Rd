\name{CI_insert_gr}
\alias{CI_insert_gr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reposition Channel Islands Grid References to a false origin
}
\description{
This function is used reposition Channnel Islands grid references to that they can be plotted onto a map in which the Channel Islands has been moved to a false position to act as an insert. The default false position is to the lower left of the mainland of Great Britain (under Ireland if present).
}
\usage{
CI_insert_gr(gridref, origin = c(-180000, 30000))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A character vector containing the grid references to be repositioned
}
  \item{origin}{
The coordinates of the false origin relative to which the supplied grid references will be repositioned.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
Although this function is designed for Channel Islands grid references it could potentially be used to reposition other coordinates for the purpose of a insert box.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{CI_insert_en}}
}
\examples{
## Example of how to convert a Channel Islands grid references to an easting 
## & northing based on a new false origin to allow plotting inside a insert 
##box

# Convert a Channel Island gridref to false easting & northing
  ins_en = CI_insert_gr("WV55")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Channel Islands }
\keyword{ Mapping }
\keyword{ Grid reference }
\keyword{ Coordinate conversion }
\keyword{ Insert box }
