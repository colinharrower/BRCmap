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
plotUK_gr_cats(gridref, att, breaks = NULL, legend_pos = "topleft", 
leg_cex = 0.7, att_col = NULL, ...)
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
\item{att_col}{
A vector giving colour values to be used for colouring the squares based on the attribute vector. Note if specified the number of values in att_col needs to match the number of category in, or derived from, the attribute vector. The default \code{att_col = NULL} will use the default colour scheme, which is a reversed version of R's \code{\link{heat.colors}} pallete (values going from light yellow to dark red).
}
  \item{\dots}{
Any other arguments that are passed to the \code{polygon} function used to actually plot the grid references
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
## Example of using plotUK_gr_cats function to plot a map of uk then
## add grid squares colouring by geographic region

# Load UK data
data(UK)

# Plot UK outline
plot_GIS(UK, new.window = FALSE, show.grid = FALSE, show.axis = FALSE, 
xlab = "", ylab = "")

# Create a vector of grid references to be added
gridrefs <- c("SP49","NO58","WV55","H47","NN29","WV55","WA50","NT20",
"TL97","NH33","SE25","SN22","SH87","SO00","N20","M85","T01","SX59","NY78")

# Create a vector of regions associated with the countries
gr_reg = c("England","Scotland","Channel Isles","Ireland","Scotland",
"Channel Isles","Channel Isles","Scotland","England","Scotland","England",
"Wales","Wales","Wales","Ireland","Ireland","Ireland","England","England")

# Add squares to map colouring by region
plotUK_gr_cats(gridrefs, gr_reg, att_col = rainbow(5))


## Example of using plotUK_gr_cats function to plot a map of uk then
## add grid squares colouring by some numerical vector (such as counts) 
## broken down into categories

# Plot UK outline
plot_GIS(UK, new.window = FALSE, show.grid = FALSE, show.axis = FALSE, xlab = "", ylab = "")

# Create a vector of grid references to be added
gridrefs <- c("SP49","NO58","WV50","H47","NN29","WV55","WA50","NT20",
"TL97","NH33","SE25","SN22","SH87","SO00","N20","M85","T01","SX59","NY78")

# Create a vector of regions associated with the countries
gr_vals = rpois(length(gridrefs),10)

# Add squares to map colouring by region
plotUK_gr_cats(gridrefs, gr_vals, breaks = c(0,5,10,15,20))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mapping }
\keyword{ Grid reference }
\keyword{ Channel Islands }