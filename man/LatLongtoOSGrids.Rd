\name{LatLongtoOSGrids}
\alias{LatLongtoOSGrids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Latitude & Longitude (WGS84) to UK grid refs (OSGB, OSNI, UTM30)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LatLongtoOSGrids(Latitude, Longitude, Datum = "OSGB", datum_params = datum_vars, full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Latitude}{
%%     ~~Describe \code{Latitude} here~~
}
  \item{Longitude}{
%%     ~~Describe \code{Longitude} here~~
}
  \item{Datum}{
%%     ~~Describe \code{Datum} here~~
}
  \item{datum_params}{
%%     ~~Describe \code{datum_params} here~~
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
function (Latitude, Longitude, Datum = "OSGB", datum_params = datum_vars, 
    full_output = FALSE) 
{
    lat_len = length(Latitude)
    if (length(Longitude) != lat_len) {
        stop("ERROR: 'Latitude' & 'Longitude' are of different lengths")
    }
    if (!length(Datum) \%in\% c(1, lat_len)) {
        stop("ERROR: Length of 'Datum' does not match length of Latitude/Longitude values")
    }
    datum_list = unique(Datum)
    miss_datum = which(!datum_list \%in\% datum_params$Datum)
    if (length(miss_datum) > 0) {
        stop(paste("ERROR: Datum present in data for which parameters have not been given (", 
            paste(sQuote(datum_list[miss_datum]), collapse = ","), 
            ")", sep = ""))
    }
    if (full_output) {
        ret_obj = data.frame(LATITUDE = Latitude, LONGITUDE = Longitude, 
            DATUM = Datum, EASTING = rep(NA, lat_len), NORTHING = rep(NA, 
                lat_len), stringsAsFactors = FALSE)
    }
    else {
        ret_obj = data.frame(EASTING = rep(NA, lat_len), NORTHING = rep(NA, 
            lat_len), stringsAsFactors = FALSE)
    }
    for (i_datum in 1:length(datum_list)) {
        if (length(Datum) == 1) {
            dat_inds = 1:lat_len
        }
        else {
            dat_inds = which(Datum == datum_list[i_datum])
        }
        par_ind = which(datum_params$Datum == datum_list[i_datum])
        if (length(par_ind) > 1) {
            stop(paste("ERROR: More than one match for current Datum (", 
                sQuote(datum_list[i_datum]), ") found in datum_params data frame", 
                sep = ""))
        }
        a = datum_params$a[par_ind]
        b = datum_params$b[par_ind]
        F0 = datum_params$F0[par_ind]
        lat0 = datum_params$lat0[par_ind] * (pi/180)
        lon0 = datum_params$lon0[par_ind] * (pi/180)
        N0 = datum_params$N0[par_ind]
        E0 = datum_params$E0[par_ind]
        e2 = 1 - (b^2)/(a^2)
        n = (a - b)/(a + b)
        n2 = n^2
        n3 = n^3
        lat = Latitude[dat_inds] * (pi/180)
        lon = Longitude[dat_inds] * (pi/180)
        cosLat = cos(lat)
        sinLat = sin(lat)
        nu = (a * F0) * (1 - e2 * sinLat^2)^-0.5
        rho = (a * F0) * (1 - e2) * (1 - e2 * sinLat^2)^-1.5
        eta2 = (nu/rho) - 1
        Ma = (1 + n + ((5/4) * n2) + ((5/4) * n3)) * (lat - lat0)
        Mb = ((3 * n) + (3 * n2) + ((21/8) * n3)) * sin(lat - 
            lat0) * cos(lat + lat0)
        Mc = (((15/8) * n2) + ((15/8) * n3)) * sin(2 * (lat - 
            lat0)) * cos(2 * (lat + lat0))
        Md = ((35/24) * n3) * sin(3 * (lat - lat0)) * cos(3 * 
            (lat + lat0))
        M = (b * F0) * (Ma - Mb + Mc - Md)
        cos3lat = cosLat^3
        cos5lat = cosLat^5
        tan2lat = tan(lat)^2
        tan4lat = tan(lat)^4
        I = M + N0
        II = (nu/2) * sinLat * cosLat
        III = (nu/24) * sinLat * cos3lat * (5 - tan2lat + (9 * 
            eta2))
        IV = (nu/720) * sinLat * cos5lat * (61 - (58 * tan2lat) + 
            tan4lat)
        V = nu * cosLat
        VI = (nu/6) * cos3lat * ((nu/rho) - tan2lat)
        VII = (nu/120) * cos5lat * (5 - (18 * tan2lat) + tan4lat + 
            (14 * eta2) - (58 * tan2lat * eta2))
        dLon = lon - lon0
        N = floor(I + (II * dLon^2) + (III * dLon^4) + (IV * 
            dLon^6))
        E = floor(E0 + V * dLon + VI * dLon^3 + VII * dLon^5)
        ret_obj[dat_inds, c("EASTING", "NORTHING")] = data.frame(E, 
            N)
    }
    return(ret_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
