\name{OSgridReprojection}
\alias{OSgridReprojection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reproject UK Eastings & Northings from one projection to another (OSNI, OSGB, UTM30)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OSgridReprojection(Easting, Northing, org_grid = "OSNI", out_grid = "OSGB", full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Easting}{
%%     ~~Describe \code{Easting} here~~
}
  \item{Northing}{
%%     ~~Describe \code{Northing} here~~
}
  \item{org_grid}{
%%     ~~Describe \code{org_grid} here~~
}
  \item{out_grid}{
%%     ~~Describe \code{out_grid} here~~
}
  \item{full_output}{
%%     ~~Describe \code{full_output} here~~
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
function (Easting, Northing, org_grid = "OSNI", out_grid = "OSGB", 
    full_output = FALSE) 
{
    len_east = length(Easting)
    if (length(Northing) != len_east) {
        stop("ERROR: 'Easting' and 'Northing' differ in length")
    }
    if (full_output) {
        ret_obj = data.frame(ORG_EASTING = Easting, ORG_NORTHING = Northing, 
            ORG_GRID = org_grid, NEW_GRID = out_grid, EASTING = rep(NA, 
                len_east), NORTHING = rep(NA, len_east), stringsAsFactors = FALSE)
    }
    else {
        ret_obj = data.frame(EASTING = rep(NA, len_east), NORTHING = rep(NA, 
            len_east), stringsAsFactors = FALSE)
    }
    org_latlon = OSGridstoLatLong(Easting, Northing, org_grid)
    org_cart = LatLong_Cartesian(org_latlon$LATITUDE, org_latlon$LONGITUDE, 
        gsub("WGS84", "UTM30", org_grid))
    if (!org_grid \%in\% c("WGS84", "UTM30")) {
        helm_tran = helmert_trans(x = org_cart$x, y = org_cart$y, 
            z = org_cart$z, trans = paste(org_grid, "toWGS84", 
                sep = ""))
    }
    else {
        helm_tran = org_cart
    }
    if (!out_grid \%in\% c("WGS84", "UTM30")) {
        helm_tran = helmert_trans(x = helm_tran$x, y = helm_tran$y, 
            z = helm_tran$z, trans = paste("WGS84to", out_grid, 
                sep = ""))
    }
    out_latlon = Cartesian_LatLong(helm_tran$x, helm_tran$y, 
        helm_tran$z, out_grid)
    out_en = LatLongtoOSGrids(out_latlon$LATITUDE, out_latlon$LONGITUDE, 
        out_grid)
    ret_obj[, c("EASTING", "NORTHING")] = data.frame(out_en$EASTING, 
        out_en$NORTHING)
    return(ret_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
