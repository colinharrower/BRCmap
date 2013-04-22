\name{MGRStoUTM}
\alias{MGRStoUTM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Miltary Grid Reference System to UTM
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MGRStoUTM(gridref, zone = 30, zdl = "U", spos = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
%%     ~~Describe \code{gridref} here~~
}
  \item{zone}{
%%     ~~Describe \code{zone} here~~
}
  \item{zdl}{
%%     ~~Describe \code{zdl} here~~
}
  \item{spos}{
%%     ~~Describe \code{spos} here~~
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
function (gridref, zone = 30, zdl = "U", spos = TRUE) 
{
    zdl_chr = c("C", "D", "E", "F", "G", "H", "J", "K", "L", 
        "M", "N", "P", "Q", "R", "S", "T", "U", "V", "W", "X")
    mgrs_chr = c("A", "B", "C", "D", "E", "F", "G", "H", "J", 
        "K", "L", "M", "N", "P", "Q", "R", "S", "T", "U", "V", 
        "W", "X", "Y", "Z")
    for (i in seq(1, along.with = gridref)) {
        if (zdl == "X") {
            med_lat = 78 - 1
        }
        else {
            med_lat = which(zdl_chr == zdl) - 1
        }
        med_lat = -76 + (8 * med_lat)
        temp_gridref = gsub(" ", "", toupper(gridref[i]))
        temp_gridref = gsub("-", "", temp_gridref)
        l1 = which(mgrs_chr == substr(temp_gridref, 1, 1)) - 
            1
        l2 = which(mgrs_chr == substr(temp_gridref, 2, 2)) - 
            1
        gr_nums = substr(temp_gridref, 3, nchar(temp_gridref))
        east_num = substr(gr_nums, 1, nchar(gr_nums)/2)
        north_num = substr(gr_nums, (nchar(gr_nums)/2) + 1, nchar(gr_nums))
        d_len = nchar(east_num)
        if (nchar(north_num) != d_len) {
            stop("Easting and Northing Digits must be the same length")
        }
        E0 = 1 + l1\%\%8
        N0 = (20 + l2 - ifelse(zone\%\%2 != 0, 0, 5))\%\%20
        appN = med_lat * 100/90
        N0 = N0 + round((appN - N0)/20) * 20
        E = E0 * 1e+05 + as.numeric(east_num) * 10^(5 - d_len)
        N = N0 * 1e+05 + as.numeric(north_num) * 10^(5 - d_len)
        if (spos & which(zdl_chr == zdl) <= 11) {
            N = N + 1e+07
        }
        if (i == 1) {
            ret_obj = data.frame(Easting = E, Northing = N)
        }
        else {
            ret_obj[i, ] = data.frame(Easting = E, Northing = N)
        }
        return(ret_obj)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
