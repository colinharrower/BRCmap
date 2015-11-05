\name{neigh_smooth}
\alias{neigh_smooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine Neighbouring 10km squares
}
\description{
Determine the neighbouring 10km grid references around a series of focal 10km grid squares.
}
\usage{
neigth_smooth(gridref, smooth_radius = 1, square_list = NULL, sq_dimen = 10000, output_type = "neigh_only")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
A vector of grid references
}
  \item{smooth_radius}{
An interger value determining the extent of the neighbourhood, measured in numbers of squares in each direction around the focal square(s).The default \code{smooth_radius = 1} will return a neighbourhood containing the eight 10km squares that are 1 square in each direction from each forcal sites while a value of 2 will return a neighbourhood containing the 24 squares that are within 2 squares in each direction.
}
  \item{square_list}{
A vector of permissable 10km grid references. This argument allows the neighbourhood to be constrained if there are areas that do not want to be in a neighbourhood (i.e. squares in the sea). When this argument is supplied the list of neighbourhood squares determined by the function will be filtered to return only those that are also present in this square_list. The default \code{square_list = NULL} will not filter the neighbourhood squares in anyway so will return any valid grid reference.
}
  \item{sq_dimen}{
A numerical value determining the dimesions of the grid squares. At present this function only works for 10km grid references hence the default \code{sq_dimen = 10000}. In a future update this will be adapted to work for other dimensions.
}
  \item{output_type}{
A character value determining what is to be returned from the function. The default \code{output_type = "neigh_only"} will return a vector containing only the neighbouring squares for the series of gridrefs supplied to \code{gridref}. The only other output type recognised by the function at present is \code{"both"} which will return a 2 column data.frame with the columns columns \code{GRIDREF}; containing both the original focal grid references and the neighbouring grid references, and \code{OBSERVED} containing 1 for rows corresponding to the supplied focal grid references and 0 for rows corresponding to determined neighbouring grid references.
}
}
\details{
As mentioned above this function only works with 10km grid references so do not pass other resolutions of grid reference as the function will not work/work properly.
}
\value{
As mentioned above the function will either return a vector containing only the neighbouring squares for the series of gridrefs supplied to \code{gridref} or a 2 column data.frame with the columns columns:
\item{GRIDREF}{a character vector containing the original focal grid references and the neighbouring grid references}
\item{OBSERVED}{a integer vector determining whether the corresponding grid reference was a supplied focal grid references or a neighbouring grid reference. A value of 1 indicates the grid references was a supplied focal grid references while a value of 0 indicates the grid reference was a neighbouring grid reference}
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
\code{\link{plotUK_gr}},
\code{\link{plotUK_gr_cats}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## In this example we will determine & plot the neighbouring squares around a set of focal grid references

# Create a vector of grid references to test
  focal_gr = c("SU90","SU65","SU12","NT28","H38")

# Determine the 10km squares 1 square in each direction around the focal square
  ne_gr = neigh_smooth(focal_gr)

# Create a blank plot of the UK
  plot_GIS(UK, new.window = FALSE)

# Add the focal squares to the map (in red)
  plotUK_gr(focal_gr, col="red")

# Add the neighbouring squares to the map (in light green)
  plotUK_gr(ne_gr, col="lightgreen")

# Create a vector of some other grid references
  focal_gr2 = c("NY12","S87","SK38","SN48")

# Determine neighbours 2 squares in each direction (returning both sets of squares)
  ne_gr2 = neigh_smooth(focal_gr2, smooth_radius = 2, output_type = "both")

# Add these points to the map
  plotUK_gr_cats(ne_gr2$GRIDREF,att = ne_gr2$OBSERVED, att_col = c("red","lightgreen"))
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
