\name{gr_components}
\alias{gr_components}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract rid Reference components
}
\description{
A function to split a UK, Irish or Channel Islands grid reference into its main components
}
\usage{
gr_components(gridref, output_col = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
  Valid UK, Irish or Channel islands grid references 
}
  \item{output_col}{
  The name(s) of the output column(s) containg the components you wish to have returned (see details). The default, when \code{output_col = NULL}, is to return all of the components. Valid values are: \code{VALID_GR, PRECISION, CHARS, DIGITS, DIGITS_EAST, DIGITS_NORTH, TETRAD, QUADRANT}
}
}
\details{
This function is primarily an accessory function used by other functions in the BRCmap package (such as \code{reformat_gr}) and may not be of much use to a user. It essentially splits the grid reference string into the component parts such as the inital characters, the digits (which are also sub-divided into the easting and northing digits), and finally any tetrad or quadrant codes which were present.

The function can also be used to check the format of a grid reference and see that it matches the expected syntax. Any grid reference which do not meet the expected sytax will have show \code{NA} in the column \code{VALID_GR}

The grid references accepted by this function are valid OSGB (Ordnance suvery of Great Britain, e.g. SP4209, TN389207), OSNI (Ordnance Survey of Northern Ireland, e.g. D0803, N0138) or UTM (Universal Transverse Mercator, e.g. WV595475, WA569079) grid references. Note that the UTM grid reference can only be used for the Channel Islands and are only in the format as shown by the example above. The function can also accept tetrad or quadrant grid references (e.g. SP40T, SP40NW)
}
\value{
A \code{data.frame} with \code{gridref} rows containing some or all of the following columns (depending on value of \code{output_col}):
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{GRIDREF}{The original grid reference passed to the function}
\item{VALID_GR}{The standardised grid reference (i.e. with spaces removed), if the grid reference was valid, otherwise \code{NA}}
\item{PRECISION}{The precision of the standardised grid reference (in meters)}
\item{CHARS}{The intial characters (two characters if from GB or Channel Lands, or only 1 if Irish)}
\item{DIGITS}{The numberical digits from the grid reference}
\item{DIGITS_EAST}{The numberical digits giving the easting from the grid reference}
\item{DIGITS_NORTH}{The numberical digits giving the northing from the grid reference}
\item{TETRAD}{The tetrad code if present, otherwise \code{NA}}
\item{QUADRANT}{The quadrant code if present, otherwise \code{NA}}
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
}
\examples{
## Create a vector of grid references
gridrefs <- c("SP4297","WV569079","H49280101","NN29","NT20S","TL97NW")

## Extract components of grid references
gr_components(gridrefs)

## Extract tetrad and quadrant codes from grid references
gr_components(gridrefs, output_col = c("TETRAD", "QUADRANT"))