\name{gr_num2let}
\alias{gr_num2let}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Eastings & Northings to Grid References
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gr_num2let(easting, northing, OSgrid = "OSGB", keep_precision = TRUE, min_10km = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{easting}{
%%     ~~Describe \code{easting} here~~
}
  \item{northing}{
%%     ~~Describe \code{northing} here~~
}
  \item{OSgrid}{
%%     ~~Describe \code{OSgrid} here~~
}
  \item{keep_precision}{
%%     ~~Describe \code{keep_precision} here~~
}
  \item{min_10km}{
%%     ~~Describe \code{min_10km} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (easting, northing, OSgrid = "OSGB", keep_precision = TRUE, 
    min_10km = TRUE) 
{
    if (length(easting) != length(northing)) {
        stop("Length of Easting & Northing Vectors do not match")
    }
    inv_grid = which(!OSgrid \%in\% c("OSGB", "OSNI", "UTM30"))
    if (length(inv_grid) > 0) {
        stop(paste("Invalid Grid type (", OSgrid[inv_grid], ")", 
            sep = ""))
    }
    if (length(OSgrid) == 1) {
        OSgrid = rep(OSgrid, length(easting))
    }
    ret_obj = rep(NA, length(easting))
    easting = format(easting, scientific = FALSE, trim = TRUE)
    northing = format(northing, scientific = FALSE, trim = TRUE)
    easting[easting == "0"] = "00000"
    northing[northing == "0"] = "00000"
    dig_east = substr(easting, (nchar(easting) - 5) + 1, nchar(easting))
    dig_north = substr(northing, (nchar(northing) - 5) + 1, nchar(northing))
    if (!keep_precision) {
        pair_trim = pmin(nchar(dig_east) - nchar(gsub("[0]*$", 
            "", dig_east)), nchar(dig_north) - nchar(gsub("[0]*$", 
            "", dig_north)))
        dig_east[pair_trim > 0] = substr(dig_east[pair_trim > 
            0], 1, nchar(dig_east[pair_trim > 0]) - pair_trim[pair_trim > 
            0])
        dig_north[pair_trim > 0] = substr(dig_north[pair_trim > 
            0], 1, nchar(dig_north[pair_trim > 0]) - pair_trim[pair_trim > 
            0])
    }
    if (min_10km) {
        dig_east[nchar(dig_east) == 0] = "0"
        dig_north[nchar(dig_north) == 0] = "0"
    }
    cty_inds = which(OSgrid == "OSGB")
    if (length(cty_inds) > 0) {
        eX500k = as.numeric(easting[cty_inds])/5e+05
        nX500k = as.numeric(northing[cty_inds])/5e+05
        L500k = (21 - (5 * floor(nX500k)) + floor(eX500k)) - 
            3
        eX100k = round((eX500k - floor(eX500k)) * 5, 5)
        nX100k = round((nX500k - floor(nX500k)) * 5, 5)
        L100k = 21 - (5 * floor(nX100k)) + floor(eX100k)
        ret_obj[cty_inds] = paste(LETTERS[-9][L500k], LETTERS[-9][L100k], 
            dig_east[cty_inds], dig_north[cty_inds], sep = "")
    }
    cty_inds = which(OSgrid == "OSNI")
    if (length(cty_inds) > 0) {
        eX100k = round(as.numeric(easting[cty_inds])/1e+05, 2)
        nX100k = round(as.numeric(northing[cty_inds])/1e+05, 
            2)
        L100k = 21 - (5 * floor(nX100k)) + floor(eX100k)
        ret_obj[cty_inds] = paste(LETTERS[-9][L100k], dig_east[cty_inds], 
            dig_north[cty_inds], sep = "")
    }
    cty_inds = which(OSgrid == "UTM30")
    if (length(cty_inds) > 0) {
        chr_east = substr(easting, 1, (nchar(easting) - 5))
        chr_north = substr(northing, 1, (nchar(northing) - 5))
        part_inds = which(OSgrid == "UTM30" & chr_north == 55)
        if (length(part_inds) > 0) {
            ret_obj[part_inds] = paste("WA", dig_east[part_inds], 
                dig_north[part_inds], sep = "")
        }
        part_inds = which(OSgrid == "UTM30" & chr_north == 54)
        if (length(part_inds) > 0) {
            ret_obj[part_inds] = paste("WV", dig_east[part_inds], 
                dig_north[part_inds], sep = "")
        }
    }
    return(ret_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
