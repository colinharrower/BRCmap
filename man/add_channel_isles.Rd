\name{add_channel_isles}
\alias{add_channel_isles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An R function To Add the Channel Islands to the existing map
}
\description{
This functions can be used to add the Channel Islands outline to a existing map as an insert box
}
\usage{
add_channel_isles(CI_outline_gr, insert_origin = c(-180000, 30000), insert_dimen = c(1e+05, 1e+05), ins_border = NULL, ins_col = "white", col = NA, border = NULL, show_gridline = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CI_outline_gr}{
The name of an R object containing the shape file information/polygon information for the outline of the Channel Islands
}
  \item{insert_origin}{
A integer vector of length 2 giving the x,y position for the origin of the insert box. Default is to put the box to the lower left corner of the default UK map
}
  \item{insert_dimen}{
A numerical vector of length 2 givng the dimensions of the insert box. The default is 10km (10,000m) by 10km insert box.
}
  \item{ins_border}{
A value determining the line type used to border the insert box. Default \code{NULL} will use a solid line.
}
  \item{ins_col}{
This argument specifies The background colour of the insert box. Any valid form of specifying colours in R should work here. The default \code{white} means that the insert box will cover any points/lines underneath.
}
  \item{col}{
This argument specifies the colour in which the channel outline will be filled. Any valid form of specifying colours in R should work here. The default \code{NA} means that the outline will contain a transparent fill.
}
  \item{border}{
This argument species the colour of the lines used to plot the outline of the Channel Islands. Any valid form of specifying colours in R should work here. The default \code{NULL} will results in a black outline.
}
  \item{show_gridline}{
%%     ~~Describe \code{show_gridline} here~~
}
}
\details{
Generally this funciton will only be used to add the Channel Islands onto the map at a false position in an insert box. If you wish to plot the Channel Islands at it's true position then you should ideally use the standard shape file plotting.
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Channel Islands }
\keyword{ Insert box }% __ONLY ONE__ keyword per line
