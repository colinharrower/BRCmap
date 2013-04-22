\name{Cartesian_LatLong}
\alias{Cartesian_LatLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to convert cartesian coordinates to latitude and longitude
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Cartesian_LatLong(x, y, z, Datum = "OSGB", datum_params = datum_vars, full_output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
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
function (x, y, z, Datum = "OSGB", datum_params = datum_vars, 
    full_output = FALSE) 
{
    len_x = length(x)
    if (length(y) != len_x | length(z) != len_x) {
        stop("ERROR: input variables 'x', 'y' and 'z' are of different lengths")
    }
    if (!length(Datum) \%in\% c(1, len_x)) {
        stop("ERROR: Length of 'Datum' does not match length of input 'x','y' & 'z' values")
    }
    datum_list = unique(Datum)
    miss_datum = which(!datum_list \%in\% datum_params$Datum)
    if (length(miss_datum) > 0) {
        stop(paste("ERROR: Datum present in data for which parameters have not been given (", 
            paste(sQuote(datum_list[miss_datum]), collapse = ","), 
            ")", sep = ""))
    }
    if (full_output) {
        ret_obj = data.frame(x = x, y = y, z = z, DATUM = Datum, 
            LATITUDE = rep(NA, len_x), LONGITUDE = rep(NA, len_x), 
            HEIGHT = rep(NA, len_x), stringsAsFactors = FALSE)
    }
    else {
        ret_obj = data.frame(LATITUDE = rep(NA, len_x), LONGITUDE = rep(NA, 
            len_x), stringsAsFactors = FALSE)
    }
    prec = 1
    for (i_datum in 1:length(datum_list)) {
        if (length(Datum) == 1) {
            dat_inds = 1:len_x
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
        e2 = (a^2 - b^2)/a^2
        lon = atan(y[dat_inds]/x[dat_inds])
        p = sqrt(x[dat_inds]^2 + y[dat_inds]^2)
        lat = atan(z[dat_inds]/(p * (1 - e2)))
        v = a/sqrt(1 - e2 * sin(lat)^2)
        lat2 = atan((z[dat_inds] + e2 * v * sin(lat))/p)
        d_lat = lat - lat2
        iter_inds = which(d_lat > prec)
        while (length(iter_inds) > 0) {
            lat[iter_inds] = lat2[iter_inds]
            v[iter_inds] = a[dat_inds][iter_inds]/sqrt(1 - e2 * 
                sin(lat[iter_inds])^2)
            lat2[iter_inds] = atan((z[dat_inds][iter_inds] + 
                e2 * v[iter_inds] * sin(lat[iter_inds]))/p[iter_inds])
            d_lat[iter_inds] = lat[iter_inds] - lat2[iter_inds]
            iter_inds = which(d_lat > prec)
        }
        H = (p/cos(lat)) - v
        lat = lat * (180/pi)
        lon = lon * (180/pi)
        if (full_output) {
            ret_obj[dat_inds, c("LATITUDE", "LONGITUDE", "HEIGHT")] = data.frame(lat, 
                lon, H)
        }
        else {
            ret_obj[dat_inds, c("LATITUDE", "LONGITUDE")] = data.frame(lat, 
                lon)
        }
    }
    return(ret_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
