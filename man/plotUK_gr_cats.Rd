\name{plotUK_gr_cats}
\alias{plotUK_gr_cats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot UK grid references using categorical variable to colour squares
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotUK_gr_cats(gridref, att, breaks, legend_pos = "topleft", leg_cex = 0.7, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
%%     ~~Describe \code{gridref} here~~
}
  \item{att}{
%%     ~~Describe \code{att} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{legend_pos}{
%%     ~~Describe \code{legend_pos} here~~
}
  \item{leg_cex}{
%%     ~~Describe \code{leg_cex} here~~
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
function (gridref, att, breaks, legend_pos = "topleft", leg_cex = 0.7, 
    ...) 
{
    if (length(gridref) != length(att)) {
        stop("Length of Grid Refs does not match length of Attribute")
    }
    att_cols = rev(heat.colors(length(breaks) - 1))
    for (i in 1:(length(breaks) - 1)) {
        if (i == length(breaks) - 1) {
            row_inds = which(att >= breaks[i] & att <= breaks[i + 
                1])
        }
        else {
            row_inds = which(att >= breaks[i] & att < breaks[i + 
                1])
        }
        if (length(row_inds) > 0) {
            plotUK_gr(gridref[row_inds], col = att_cols[i], ...)
        }
    }
    if (is.null(legend_pos) == FALSE) {
        legend(x = legend_pos, legend = paste(breaks[1:(length(breaks) - 
            1)], "-", breaks[2:length(breaks)]), fill = att_cols, 
            cex = leg_cex, bg = "white", inset = 0.015)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
