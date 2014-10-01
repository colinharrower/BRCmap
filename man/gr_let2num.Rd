\name{gr_let2num}
\alias{gr_let2num}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Grid References to eastings and northings
}
\description{
A function to convert UK, Irish or Channel Islands grid references to eastings and northings in their respective coordinate system
}
\usage{
gr_let2num(gridref, centre = FALSE, gr_prec = NULL, return_projection = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
Valid UK, Irish or Channel islands grid references (see details)
}
  \item{centre}{
Logical variable determining whether the eastings and northing returned should be for the bottom left of the grid reference square or for the centre point. The default \code{centre = FALSe} returns the coordinates for bottom left corner of the square. 
}
  \item{gr_prec}{
A vector of the same length as \code{gridref} giving the precision associated with the grid reference. The default \code{gr_prec = NULL} results in the precision being interpreted directly from the grid reference, use this argument if you need to override this intpretation.
}
  \item{return_projection}{
Logical variable determing whether the projection of the grid reference should be returned. The default \code{return_projection = FALSE} will not return the projection. See value section for information on projection values that can be returned
}
}
\details{
The function takes valid UK, Irish or Channel Islands grid references and converts them to eastings and northings (i.e. x,y coordinates) on the respective mapping projections. The grid references accepted by this function are valid OSGB (Ordnance suvery of Great Britain, e.g. SP4209, TN389207), OSNI (Ordnance Survey of Northern Ireland, e.g. D0803, N0138) or UTM (Universal Transverse Mercator, e.g. WV595475, WA569079) grid references. Note that the UTM grid reference can only be used for the Channel Islands and are only in the format as shown by the example above. The function can also accept tetrad or quadrant grid references (e.g. SP40T, SP40NW).

In addition the function can also return the projection associated with each grid reference, this is especially important when dealing collectively with grid references from grid reference types listed above (UK, Irish, Channel Islands) as grid reference from each could results in the same eastings and nortings due to the different origins for each system.
}
\value{
a \code{data.frame} containing the follow columns:
\item{EASTING}{The easting from the mapping system origin (in metres)}
\item{NORTHING}{The norting from the mapping system origin (in metres)}
optionally an addition column can also be returned (depending on value of \code{return_projection}:
\item{PROJECTION}{The mapping system for the grid reference. Possible values are \code{OSGB, OSNI or UTM30} for UK, Irish or Channel Island grid references}
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
\code{\link{gr_num2let}}
}
\examples{
## Create a vector of grid references

gridrefs <- c("SP4297","WV569079","H49280101","NN29","NT20S","TL97NW")

## Convert grid reference to easting and northing in native projection

gr_let2num(gridrefs)

## Convert grid reference to easting and northing and return projection,
## but this time for the centre of the grid reference square

gr_let2num(gridrefs, centre = TRUE, return_projection = TRUE)
}