\name{helmert_trans}
\alias{helmert_trans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform Helmert Tranforformation on x,y,z cartesian coordinates
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
helmert_trans(x, y, z, trans = "OSNItoOSGB", trans_params = helmert_trans_vars, full_output = FALSE)
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
  \item{trans}{
%%     ~~Describe \code{trans} here~~
}
  \item{trans_params}{
%%     ~~Describe \code{trans_params} here~~
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
function (x, y, z, trans = "OSNItoOSGB", trans_params = helmert_trans_vars, 
    full_output = FALSE) 
{
    len_x = length(x)
    if (length(y) != len_x | length(z) != len_x) {
        stop("ERROR: input variables 'x', 'y' and 'z' are of different lengths")
    }
    if (!length(trans) \%in\% c(1, len_x)) {
        stop("ERROR: Length of 'trans' does not match length of input 'x','y' & 'z' values")
    }
    trans_list = unique(trans)
    miss_trans = which(!trans_list \%in\% trans_params$TRANS)
    if (length(miss_trans) > 0) {
        stop(paste("ERROR: Transformation requested for which parameters have not been given (", 
            paste(sQuote(trans_list[miss_trans]), collapse = ","), 
            ")", sep = ""))
    }
    if (full_output) {
        ret_obj = data.frame(org_x = x, org_y = y, org_z = z, 
            TRANS = Datum, x = rep(NA, len_x), y = rep(NA, len_x), 
            y = rep(NA, len_x), stringsAsFactors = FALSE)
    }
    else {
        ret_obj = data.frame(x = rep(NA, len_x), y = rep(NA, 
            len_x), z = rep(NA, len_x), stringsAsFactors = FALSE)
    }
    for (i_trans in 1:length(trans_list)) {
        if (length(trans) == 1) {
            dat_inds = 1:len_x
        }
        else {
            dat_inds = which(trans == trans_list[i_trans])
        }
        tp = trans_params[trans_params$TRANS == trans_list[i_trans], 
            ]
        tp[, c("rx", "ry", "rz")] = (tp[, c("rx", "ry", "rz")]/3600) * 
            (pi/180)
        tp[, "s"] = tp$s * 1e-06
        x_out = x[dat_inds] + (x[dat_inds] * tp$s) - (y[dat_inds] * 
            tp$rz) + (z[dat_inds] * tp$ry) + tp$tx
        y_out = (x[dat_inds] * tp$rz) + y[dat_inds] + (y[dat_inds] * 
            tp$s) - (z[dat_inds] * tp$rx) + tp$ty
        z_out = (-1 * x[dat_inds] * tp$ry) + (y[dat_inds] * tp$rx) + 
            z[dat_inds] + (z[dat_inds] * tp$s) + tp$tz
        ret_obj[dat_inds, c("x", "y", "z")] = data.frame(x = x_out, 
            y = y_out, z = z_out)
    }
    return(ret_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
