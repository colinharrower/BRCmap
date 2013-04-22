\name{det_gr_precision}
\alias{det_gr_precision}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine grid reference precision
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
det_gr_precision(gridref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
%%     ~~Describe \code{gridref} here~~
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
function (gridref) 
{
    gridref = toupper(gridref)
    prec_out = rep(NA, length(gridref))
    v_inds = which(grepl("(^[[:upper:]]{1,2}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1,2}[[:digit:]]{2,}$)", 
        gridref) & nchar(gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", 
        "\\3", gridref))\%\%2 == 0)
    gr_char = gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", 
        "\\2", gridref[v_inds])
    gr_digits = gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", 
        "\\3", gridref[v_inds])
    gr_tet = gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", 
        "\\5", gridref[v_inds])
    gr_quad = gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", 
        "\\6", gridref[v_inds])
    n_pairs = nchar(gr_digits)/2
    gr_prec = 10^5/10^n_pairs
    gr_prec[gr_tet \%in\% LETTERS[-15]] = 2000
    gr_prec[gr_quad \%in\% c("NW", "NE", "SW", "SE")] = 5000
    prec_out[v_inds] = gr_prec
    return(prec_out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
