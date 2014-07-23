\name{OSgrid2GB_EN}
\alias{OSgrid2GB_EN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to convert British grid references to eastings & northings on the GB ordnance survey system
}
\description{
This function will convert British grid references (OSGB, OSNI, UTM30) to eastings & Northings on the Ordnance Survey for Great Britain projection. In the case of Irish & Channel Islands grid references they will be reprojected to place them onto the GB ordnance survey system to allow comparisons, simple distance measures or plotting onto maps in the GB projection. GB grid reference are simply converted to their native projection.
}
\usage{
OSgrid2GB_EN(gridref, centre = TRUE, gr_prec = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A character vector containing the grid references to be converted
}
\item{Centre}{
A logical variable determining whether the coordinates returned are for the grid centre or the bottom left corner. The default \code{centre = TRUE} will return the coordinates for the grid reference centre.
}
\item{gr_prec}{
A numerical vector determining the precision of the grid references. The default \code{gr_prec = NULL} will determine the precision directly from the grid reference
}
}
\details{
This function is a wrapper for several other functions in the the BRCmap package. It essential takes grid references in the commonly used British systems (OSGB, OSNI, UTM30) and will convert them all to eastings and northings in the OSGB projection, reprojecting the OSNI & UTM30 grid references
}
\value{
The function returns a \code{data.frame} containing two columns
\item{EASTING}{The easting in metres}
\item{NORTHING}{The northing in metres}
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
\code{\link{gr_let2num}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
