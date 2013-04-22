\name{gr_let2num}
\alias{gr_let2num}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Grid references to eastings and northings
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gr_let2num(gridref, centre = FALSE, gr_prec = NULL, return_projection = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
%%     ~~Describe \code{gridref} here~~
}
  \item{centre}{
%%     ~~Describe \code{centre} here~~
}
  \item{gr_prec}{
%%     ~~Describe \code{gr_prec} here~~
}
  \item{return_projection}{
%%     ~~Describe \code{return_projection} here~~
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
function (gridref, centre = FALSE, gr_prec = NULL, return_projection = FALSE) 
{
    spmod = function(x, mod) {
        ret_obj = x\%\%mod
        ret_obj[ret_obj == 0] = mod
        return(ret_obj)
    }
    gridref = gsub("[ -]", "", toupper(gridref))
    len_grvec = length(gridref)
    if (return_projection) {
        ret_obj = data.frame(EASTING = rep(NA, len_grvec), NORTHING = rep(NA, 
            len_grvec), PROJECTION = rep(NA, len_grvec), row.names = NULL)
    }
    else {
        ret_obj = data.frame(EASTING = rep(NA, len_grvec), NORTHING = rep(NA, 
            len_grvec), row.names = NULL)
    }
    cty_inds = which(grepl("^[[:upper:]]{2}[[:digit:]]{2,}$", 
        gridref) & !grepl("^(WA)|(WV)[[:digit:]]{2,}$", gridref))
    if (length(cty_inds) > 0) {
        l1 = match(substr(gridref[cty_inds], 1, 1), LETTERS[-9])
        l2 = match(substr(gridref[cty_inds], 2, 2), LETTERS[-9])
        e = (spmod(l1, 5) - 1) * 5
        n = floor(abs(l1 - 25)/5) * 5
        e = e + (spmod(l2, 5) - 1)
        n = n + floor(abs(l2 - 25)/5)
        e = e - 10
        n = n - 5
        gr_nums = substr(gridref[cty_inds], 3, nchar(gridref[cty_inds]))
        east_num = substr(gr_nums, 1, nchar(gr_nums)/2)
        north_num = substr(gr_nums, (nchar(gr_nums)/2) + 1, nchar(gr_nums))
        east_num = gsub(" ", "0", format(east_num, width = 5))
        north_num = gsub(" ", "0", format(north_num, width = 5))
        e = paste(e, east_num, sep = "")
        n = paste(n, north_num, sep = "")
        ret_obj[cty_inds, c("EASTING", "NORTHING")] = c(as.numeric(e), 
            as.numeric(n))
        if (return_projection) {
            ret_obj[cty_inds, "PROJECTION"] = "OSGB"
        }
    }
    cty_inds = which(grepl("^[[:upper:]]{1}[[:digit:]]{2,}$", 
        gridref))
    if (length(cty_inds) > 0) {
        l2 = match(substr(gridref[cty_inds], 1, 1), LETTERS[-9])
        e = spmod(l2, 5) - 1
        n = floor(abs(l2 - 25)/5)
        gr_nums = substr(gridref[cty_inds], 2, nchar(gridref[cty_inds]))
        east_num = substr(gr_nums, 1, nchar(gr_nums)/2)
        north_num = substr(gr_nums, (nchar(gr_nums)/2) + 1, nchar(gr_nums))
        east_num = gsub(" ", "0", format(east_num, width = 5))
        north_num = gsub(" ", "0", format(north_num, width = 5))
        e = paste(e, east_num, sep = "")
        n = paste(n, north_num, sep = "")
        ret_obj[cty_inds, c("EASTING", "NORTHING")] = c(as.numeric(e), 
            as.numeric(n))
        if (return_projection) {
            ret_obj[cty_inds, "PROJECTION"] = "OSNI"
        }
    }
    cty_inds = which(grepl("^(WA)|(WV)[[:digit:]]{2,}$", gridref))
    if (length(cty_inds) > 0) {
        e = rep(5, length(cty_inds))
        n = ifelse(grepl("^(WA)[[:digit:]]{2,}$", gridref[cty_inds]), 
            55, 54)
        gr_nums = substr(gridref[cty_inds], 3, nchar(gridref[cty_inds]))
        east_num = substr(gr_nums, 1, nchar(gr_nums)/2)
        north_num = substr(gr_nums, (nchar(gr_nums)/2) + 1, nchar(gr_nums))
        east_num = gsub(" ", "0", format(east_num, width = 5))
        north_num = gsub(" ", "0", format(north_num, width = 5))
        e = paste(e, east_num, sep = "")
        n = paste(n, north_num, sep = "")
        ret_obj[cty_inds, c("EASTING", "NORTHING")] = c(as.numeric(e), 
            as.numeric(n))
        if (return_projection) {
            ret_obj[cty_inds, "PROJECTION"] = "UTM30"
        }
    }
    if (centre) {
        if (is.null(gr_prec)) {
            gr_prec = det_gr_precision(gridref)
        }
        ret_obj[, c("EASTING", "NORTHING")] = ret_obj[, c("EASTING", 
            "NORTHING")] + (gr_prec/2)
    }
    return(ret_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
