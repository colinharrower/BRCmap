\name{MGRStoUTM}
\alias{MGRStoUTM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Military Grid Reference System to UTM
}
\description{
This function converts Military Grid Reference System (MGRS) grid references to UTM coordinates. In the UK  a modified version of MGRS grid references are often used for the Channel Islands (see details for more information). This function converts MGRS grid references to UTM eastings and northings which can then be dealt with by the other functions in this package.
}
\usage{
MGRStoUTM(gridref, zone = 30, zdl = "U", spos = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A character vector containing the MGRS grid references to be converted. Note the zone number and letter should not be contained in this grid reference at present.
}
  \item{zone}{
A numerical value specifying the UTM zone number. Note at present all grid references passed to the function need to be for the same zone and zone letter (which is the case for the Channel Islands).
}
  \item{zdl}{
A character string given the zone letter. Note at present all grid references passed to the function need to be for the same zone and zone letter (which is the case for the Channel Islands).
}
  \item{spos}{
A logical variable that modifies the northing but I can't remember why
}
}
\details{
In the UK a modified version of MGRS grid references are commonly used to identify locations in the Channel Islands. They differ from the standard MGRS grid reference in excluding the zone number & letter from the start of the grid reference. The zone number & letter are most likely excluded as all of the Channel Islands fall within zone 30 U in the Military Grid Reference System.

The modified MGRS grid references look like Ordnance Survey of Great Britain grid references in that they are two characters followed by an even number of digits. They can however be distinguished by the fact that the Channel Islands grid references start with either WA or WV.

For example the grid reference "WV6548" specifies a 1km grid reference in St. Helier. The full MGRS grid references for this location would be 30UWV6548 (or WV6500048000 if not truncating zero digit pairs).
}
\value{
The function returns a \code{data.frame} containing the following columns:
\item{Easting}{The easting values for the coordinates (in metres)}
\item{Northing}{The northing values for the coordinates (in metres)}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Harrower
}
\note{
It should be noted that at present the \code{gr_let2num} function does not use this function to convert Channel Islands grid references to eastings and northings. Instead it uses a simplier process that will only work for those from the Channel Islands. This function should work for any MGRS grid references not just those from the Channel Islands.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gr_let2num}}
}
\examples{
### Examples of converting MGRS to UTM easting/northing
## The Channel Islands
# Using the MGRStoUTM function
MGRStoUTM("WV6548")

# Using the gr_let2num function
gr_let2num("WV6548")


## Outside of the UK
# Avery Island, Louisiana (home of Tabasco Sauce)
# MGRS gridref 15RXP0509
MGRStoUTM("XP0509",zone = 15,zdl="R")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
