\name{plotUK_gr_cats}
\alias{plotUK_gr_cats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot UK grid references using categorical variable to colour squares
}
\description{
Plot grid reference squares onto a map colouring the squares based on the values in a supplied vector. The supplied vector can contain categorical or continuous values (see below for more details).
}
\usage{
plotUK_gr_cats(gridref, att, breaks, legend_pos = "topleft", leg_cex = 0.7, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A character vector containing the grid references to be plotted
}
  \item{att}{
A vector containing the information used to seperate the grid references into categories (see below for more details)
}
  \item{breaks}{
A numerical vector specifying the breakpoints used to convert the att column to categories if the values in the att column are continuous values. The default \code{breaks = NULL} will assume that values in att are already categorical.
}
  \item{legend_pos}{
A character string giving the position of the legend. Accepted values are \code{"topleft"}, \code{"topright"},\code{"bottomleft"}, \code{"bottomright"}. The default \code{"topleft"} will put the legened in the topleft hand corner of the plot area with a slight inset.
}
  \item{leg_cex}{
A numerical value giving the relative sizing of the legend (see \code{\link{par}}). The default \code{0.7} means that the legend text, etc will be slightly smaller than the main plot
}
  \item{\dots}{
Any other arguments 
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
Colin Harrower
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
