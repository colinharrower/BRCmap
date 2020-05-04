\name{plot_GIS}
\alias{plot_GIS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot map by providing a list of outlines to be plotted 
Plot GIS Map
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function take a Spatial Object or a list of spatial objects (such as coastlines) and plots the outlines as the base for Atlas style map.
}
\usage{
plot_GIS(gis_data, main = "", xlab = "Easting (km)", ylab = "Northing (km)",
xlim = NULL, ylim = NULL, show.axis = TRUE, show.grid = TRUE,
grid.div = 1e+05, grid.col = "grey", fill.col = NA, line.col = par("fg"),
bg.col = "white", box.col = NA, new.window = TRUE, no.margin = FALSE,
max.dimen = 13, cex.main = 1.2, cex.lab = 1, cex.axis = 0.8, 
blank.plot = FALSE, plot.shape = TRUE, additions = FALSE, 
return.dimen = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gis_data}{
Spatial Object, or list of such objects that are to be plotted. Usually spatial objects are obtained by reading in Shape file of coastlines/outline of area to be mapped
}
  \item{main}{
character string giving the main title that will be given to the plot. Default is to have no title
}
  \item{xlab}{
character string giving the label used on the x axis of the plot. Default is "Northing (km)"
}
  \item{ylab}{
character string giving the label used on the y axis of the plot. Default is "Northing (km)"
}
  \item{xlim}{
numerical vector given the limits of the x axis. If null the limits will be determined from gis_data.
}
  \item{ylim}{
numerical vector given the limits of the y axis. If null the limits will be determined from gis_data.
}
  \item{show.axis}{
logical, indicating whether the plots is to have a visible axis.
}
  \item{show.grid}{
logical, indicating whether grid lines are to be added to the plot
}
  \item{grid.div}{
numerical value given the spacing between grid lines (if show.grid == \code{TRUE}).
}
  \item{grid.col}{
variable determining the colour of the grid lines. Argument takes inputs accepted by \code{col} argument supplied to \code{par}. Default is \code{"grey"}
}
  \item{fill.col}{
variable determining the colour used to fill the gis_data outlines. Default (\code{NA}) results in no fill.
}
  \item{line.col}{
variable determining the colour of the line used to plot the gis_data outlines. Default (\code{par("fg")}) results in the par default line colour (usually black)
}
  \item{bg.col}{
variable determing the background colour of the plot area. Default = \code{"white"}.
}
  \item{box.col}{
variable determining the colour of the border surrounding the plot area. Default = \code{NA} results in no border to plotting region.
}
  \item{new.window}{
logical, determing whether plot should force a new device window to be opened. Default = \code{FALSE}. If new window is opened dimensions are set based on dimensions of object to be plotted.
}
  \item{no.margin}{
logical, determing whether plot is to include a margin. Default = \code{FALSE}. NOTE title, xlab and ylab are written to margin area so will no show if margin removed.
}
  \item{max.dimen}{
numerical, determining the maximum dimensions of the new device that will be opened. Default = \code{13}
}
  \item{cex.main}{
numerical variable determining the relative sizing of the main plot title (default = \code{1.2})
}
  \item{cex.lab}{
numerical variable determining the relative sizing of the axis labels (default = \code{1})
}
  \item{cex.axis}{
numerical variable determining the relative sizing of the axis values (default = \code{0.8})
}
  \item{blank.plot}{
logical variable determining whether function should finish once blank plot window created or if it should continue further with the plotting
}
  \item{plot.shape}{
logical variable determining whether to plot lines from shape file or whether to only setup the background of the plot (i.e grid lines, axis) to which outline can be added later, useful where outline is to cover plotting symbols/colours)
}
  \item{additions}{
logical variable determining whether plot to be created from scratch or whether components (i.e. axes, gridlines, outline) to be added to existing plot
}
  \item{return.dimen}{
logical, determining whether function should return plot dimensions
}
}
\details{
This function is quite complex as there are lots of argument, however for a basic idea of what the function can do see the examples below.
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
## NOTE: Example 5 shows how plot_GIS function can be used to set up a plot 
## of the UK to which additional points,lines,text, etc can then be added 
## using functions such as 'points()', 'lines()', 'segments()', 'polygons
## ()', etc

## Load shapefile data for UK (dataset in BRCmap package)

  data(UK)

## Example 1 - Standar plot_GIS plot using all defaults (except for the 
## main plot title which has been set to "UK")
  
  plot_GIS(UK, main = "UK")   

## Example 2 - Plot Scotland only with grid using 10km divisons and colour 
## plot background and fill landmass

  plot_GIS(UK$britain, main = "Scotland", xlim = c(0,500000), 
  ylim = c(600000,1000000), grid.div = 10000, fill.col = "lightgreen",
  bg.col = "lightblue")

## Example 3 - Plot Shetland Isles using fill colour and coloured plot 
## area overlaid with 10km grid
  
  plot_GIS(UK$britain, main = "Shetland", xlim = c(400000,500000), 
  ylim = c(1100000,1250000), grid.div = 10000, fill.col = "lightgreen", 
  bg.col = "lightblue") 

## Example 4 - Plot England without fill, background colour, or gridlines

  plot_GIS(UK$britain, main = "England", xlim = c(100000,700000), 
  ylim = c(0,700000), show.grid = FALSE)

## Example 5 - Plot UK as map (i.e. without axis, grid, labels, margin etc)

  plot_GIS(UK, xlab="", ylab="", show.axis = FALSE, show.grid = FALSE, 
  fill.col = "lightgreen", no.margin = TRUE)

## Example 6 - Plot UK (excluding shetland) with fill colour and background
## colour but no grid then add points and labels highlighting the locations
## of the capital cities

  plot_GIS(UK, main="UK Capital Cities", ylim = c(0, 1100000), 
  show.grid = FALSE, fill.col = "lightgreen", bg.col = "lightblue")
  
  # Create vectors of x and y values for main cities
  city.x = c(532000, 327000, 317000, 118000, 157000)
  city.y = c(181000, 672000, 175000, 385000, 529000)

  # Add points for cities  
  points(city.x,city.y,pch=16, col="red")
    
  # Label points
  text(city.x[1:2], city.y[1:2], labels = c("London", "Edinburgh"), 
  col="black", pos=1)
  
  text(city.x[3], city.y[3], labels = "Cardiff", col="black", pos=3)
  
  text(city.x[4:5], city.y[4:5], labels = c("Dublin", "Belfast"), 
  col="black", pos=2)

## Example 7 - Plot Forth Estuary (this plot highlights that in plot.GIS
## the real xlim and ylim for the plot are determined by taking user
## specified values and rounding to the nearest grid division allowing
## given coordinates to be plotted)

## NOTE: in example red dashed box represents area specified by user using 
## xlim and ylim attributes and plots highlights that plot xlim and ylim 
## have been rounded to nearest 10km that contains the user specified axis 
## limits
  
  plot_GIS(UK, main = "Forth Estuary - Scotland", xlim=c(281797,374931),
  ylim = c(657948,713754), grid.div = 10000, fill.col = "lightgreen", 
  bg.col = "lightblue")
  
  # Add box to plot to highlight area
  rect(281797,657948,374931,713754, border ="red", lty=2)
    
  # Add point to map
  points(x = 327000, y = 672000, col="red", pch=16, cex = 1.7)
    
  # Label point
  text(x = 327000, y = 672000, labels = "Edinburgh", pos = 1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mapping }
