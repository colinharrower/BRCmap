\name{add_channel_isles}
\alias{add_channel_isles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An R function To Add the Channel Islands to the existing map
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add_channel_isles(CI_outline_gr, insert_origin = c(-180000, 30000), insert_dimen = c(1e+05, 1e+05), ins_border = NULL, ins_col = "white", col = NA, border = NULL, show_gridline = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CI_outline_gr}{
%%     ~~Describe \code{CI_outline_gr} here~~
}
  \item{insert_origin}{
%%     ~~Describe \code{insert_origin} here~~
}
  \item{insert_dimen}{
%%     ~~Describe \code{insert_dimen} here~~
}
  \item{ins_border}{
%%     ~~Describe \code{ins_border} here~~
}
  \item{ins_col}{
%%     ~~Describe \code{ins_col} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{border}{
%%     ~~Describe \code{border} here~~
}
  \item{show_gridline}{
%%     ~~Describe \code{show_gridline} here~~
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
function (CI_outline_gr, insert_origin = c(-180000, 30000), insert_dimen = c(1e+05, 
    1e+05), ins_border = NULL, ins_col = "white", col = NA, border = NULL, 
    show_gridline = FALSE) 
{
    rect(insert_origin[1], insert_origin[2], insert_origin[1] + 
        insert_dimen[1], insert_origin[2] + insert_dimen[2], 
        border = ins_border, col = ins_col)
    if (show_gridline) {
        gr_line = CI_insert_gr("WA00", origin = insert_origin)$NORTHING
        segments(x0 = insert_origin[1], gr_line, insert_origin[1] + 
            insert_dimen[1], gr_line, col = "grey")
    }
    polygon(CI_insert_gr(CI_outline_gr, origin = insert_origin), 
        col = col, border = border)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
