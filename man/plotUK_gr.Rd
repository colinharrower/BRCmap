\name{plotUK_gr}
\alias{plotUK_gr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot UK grid references
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotUK_gr(gridref, gr_prec = 10000, ci_insert = TRUE, ci_origin = c(-180000, 30000), unit_conv = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
%%     ~~Describe \code{gridref} here~~
}
  \item{gr_prec}{
%%     ~~Describe \code{gr_prec} here~~
}
  \item{ci_insert}{
%%     ~~Describe \code{ci_insert} here~~
}
  \item{ci_origin}{
%%     ~~Describe \code{ci_origin} here~~
}
  \item{unit_conv}{
%%     ~~Describe \code{unit_conv} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (gridref, gr_prec = 10000, ci_insert = TRUE, ci_origin = c(-180000, 
    30000), unit_conv = NULL, ...) 
{
    gridref = unique(gridref)
    gr_len = length(gridref)
    if (is.null(gr_prec)) {
        gr_prec = det_gr_precision(gridref)
    }
    gr_poly = data.frame(EASTING = rep(NA, gr_len * 5), NORTHING = rep(NA, 
        gr_len * 5))
    gr_poly[seq(1, by = 5, length.out = gr_len), ] = gr_let2num(gridref)
    gr_poly[seq(2, by = 5, length.out = gr_len), ] = gr_poly[seq(1, 
        by = 5, length.out = gr_len), ] + data.frame(rep(0, gr_len), 
        rep(gr_prec, gr_len))
    gr_poly[seq(3, by = 5, length.out = gr_len), ] = gr_poly[seq(1, 
        by = 5, length.out = gr_len), ] + data.frame(rep(gr_prec, 
        gr_len), rep(gr_prec, gr_len))
    gr_poly[seq(4, by = 5, length.out = gr_len), ] = gr_poly[seq(1, 
        by = 5, length.out = gr_len), ] + data.frame(rep(gr_prec, 
        gr_len), rep(0, gr_len))
    ir_inds = which(grepl("(^[[:upper:]]{1}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1}[[:digit:]]{2,}$)", 
        rep(gridref, each = 5)) & !is.na(gr_poly$EASTING))
    if (length(ir_inds) > 0) {
        gr_poly[ir_inds, ] = OSgridReprojection(gr_poly$EASTING[ir_inds], 
            gr_poly$NORTHING[ir_inds], org_grid = "OSNI", out_grid = "OSGB")
    }
    ci_inds = which(grepl("(^(WA|WV)[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^(WA|WV)[[:digit:]]{2,}$)", 
        rep(gridref, each = 5)) & !is.na(gr_poly$EASTING))
    if (length(ci_inds) > 0) {
        if (ci_insert) {
            gr_poly[ci_inds, ] = CI_insert_en(gr_poly$EASTING[ci_inds], 
                gr_poly$NORTHING[ci_inds])
        }
        else {
            gr_poly[ci_inds, ] = OSgridReprojection(gr_poly$EASTING[ci_inds], 
                gr_poly$NORTHING[ci_inds], org_grid = "UTM30", 
                out_grid = "OSGB")
        }
    }
    if (!is.null(unit_conv)) {
        gr_poly = gr_poly * unit_conv
    }
    polygon(gr_poly$EASTING, gr_poly$NORTHING, ...)
    invisible(data.frame(GRIDREF = rep(gridref, each = 5), gr_poly))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
