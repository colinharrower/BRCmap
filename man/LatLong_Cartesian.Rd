\name{LatLong_Cartesian}
\alias{LatLong_Cartesian}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Latitude & Longitude to Cartesian Coordinates
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LatLong_Cartesian(Latitude, Longitude, Datum = "OSGB", datum_params = datum_vars, H = NULL, full_output = FALSE)
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
  \item{H}{
%%     ~~Describe \code{H} here~~
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
    H = NULL, full_output = FALSE) 
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
            DATUM = Datum, x = rep(NA, lat_len), y = rep(NA, 
                lat_len), z = rep(NA, lat_len), stringsAsFactors = FALSE)
    }
    else {
        ret_obj = data.frame(x = rep(NA, lat_len), y = rep(NA, 
            lat_len), z = rep(NA, lat_len), stringsAsFactors = FALSE)
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
        if (is.null(H)) {
            H = 0
        }
        else {
            H = H
        }
        lat = Latitude[dat_inds] * (pi/180)
        lon = Longitude[dat_inds] * (pi/180)
        e2 = (a^2 - b^2)/a^2
        v = a/sqrt(1 - e2 * sin(lat)^2)
        x = (v + H) * cos(lat) * cos(lon)
        y = (v + H) * cos(lat) * sin(lon)
        z = (((1 - e2) * v) + H) * sin(lat)
        ret_obj[dat_inds, c("x", "y", "z")] = data.frame(x, y, 
            z)
    }
    return(ret_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
