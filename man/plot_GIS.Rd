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
plot_GIS(gis_data, main = "", xlab = "Easting (km)", ylab = "Northing (km)", xlim = NULL, ylim = NULL, show.axis = TRUE, show.grid = TRUE, grid.div = 1e+05, grid.col = "grey", fill.col = NA, line.col = NULL, bg.col = "white", box.col = NA, new.window = TRUE, no.margin = FALSE, max.dimen = 13, cex.main = 1.2, cex.lab = 1, cex.axis = 0.8, blank.plot = FALSE, plot.shape = TRUE, additions = FALSE, return.dimen = TRUE)
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
variable determining the colour of the line used to plot the gis_data outlines. Default (\code{NULL}) results in par default line colour (usually black)
}
  \item{bg.col}{
variable determing the background colour of the plot area. Default = \code{"white"}.
}
  \item{box.col}{
variable determining the colour of the border surrounding the plot area. Default = \code{NA} results in no border to plotting region.
}
  \item{new.window}{
logical, determing whether plot should force a new device window to be opened. Default = \code{TRUE}. If new window is opened dimensions are set based on dimensions of object to be plotted.
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
function (gis_data, main = "", xlab = "Easting (km)", ylab = "Northing (km)", 
    xlim = NULL, ylim = NULL, show.axis = TRUE, show.grid = TRUE, 
    grid.div = 1e+05, grid.col = "grey", fill.col = NA, line.col = NULL, 
    bg.col = "white", box.col = NA, new.window = TRUE, no.margin = FALSE, 
    max.dimen = 13, cex.main = 1.2, cex.lab = 1, cex.axis = 0.8, 
    blank.plot = FALSE, plot.shape = TRUE, additions = FALSE, 
    return.dimen = TRUE) 
{
    if (is.null(xlim)) {
        if (is.list(gis_data)) {
            for (i in 1:length(gis_data)) {
                temp = attributes(gis_data[[i]])$bbox[1, ]
                if (i == 1) {
                  xlim = temp
                }
                else {
                  xlim = c(min(xlim[1], temp[1]), max(xlim[2], 
                    temp[2]))
                }
            }
        }
        else {
            xlim = attributes(gis_data)$bbox[1, ]
        }
    }
    xlim = c(floor(xlim[1]/grid.div) * grid.div, ceiling(xlim[2]/grid.div) * 
        grid.div)
    if (is.null(ylim)) {
        if (is.list(gis_data)) {
            for (i in 1:length(gis_data)) {
                temp = attributes(gis_data[[i]])$bbox[2, ]
                if (i == 1) {
                  ylim = temp
                }
                else {
                  ylim = c(min(ylim[1], temp[1]), max(ylim[2], 
                    temp[2]))
                }
            }
        }
        else {
            ylim = attributes(gis_data)$bbox[2, ]
        }
    }
    ylim = c(floor(ylim[1]/grid.div) * grid.div, ceiling(ylim[2]/grid.div) * 
        grid.div)
    fill_col <- fill.col
    line_col <- line.col
    x.rat = abs(xlim[1] - xlim[2])/1e+05
    y.rat = abs(ylim[1] - ylim[2])/1e+05
    aspect.ratio = x.rat/y.rat
    if (aspect.ratio <= 1) {
        y.part = max.dimen - y.rat
        x.part = x.rat + (y.part * aspect.ratio)
        plot.dimen = data.frame(height = max.dimen, width = x.part)
    }
    else {
        aspect.ratio = 1/aspect.ratio
        x.part = max.dimen - x.rat
        y.part = y.rat + (x.part * aspect.ratio)
        plot.dimen = data.frame(height = y.part, width = max.dimen)
    }
    if (new.window | dev.cur() == 1) {
        dev.new(height = plot.dimen$height, width = plot.dimen$width)
    }
    if (additions == FALSE) {
        if (no.margin) {
            par(mar = c(0, 0, 0, 0))
        }
        plot(0, 0, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", 
            xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", 
            bty = "n", asp = 1)
        rect(xlim[1], ylim[1], xlim[2], ylim[2], col = bg.col, 
            border = NA)
    }
    if (blank.plot == FALSE) {
        if (plot.shape) {
            if (is.list(gis_data)) {
                for (i in 1:length(gis_data)) {
                  plot(gis_data[[i]], add = TRUE, col = fill.col, 
                    border = line.col)
                }
            }
            else {
                plot(gis_data, add = TRUE, col = fill.col, border = line.col)
            }
        }
        if (show.grid) {
            segments(x0 = xlim[1], y0 = seq(ylim[1], ylim[2], 
                by = grid.div), x1 = xlim[2], col = grid.col)
            segments(x0 = seq(xlim[1], xlim[2], by = grid.div), 
                y0 = ylim[1], y1 = ylim[2], col = grid.col)
        }
        if (show.axis) {
            axis(1, at = seq(xlim[1], xlim[2], by = grid.div), 
                labels = seq(xlim[1]/1000, xlim[2]/1000, by = grid.div/1000), 
                cex.axis = cex.axis)
            axis(2, at = seq(ylim[1], ylim[2], by = grid.div), 
                labels = seq(ylim[1]/1000, ylim[2]/1000, by = grid.div/1000), 
                cex.axis = cex.axis)
        }
        title(main = main, xlab = xlab, ylab = ylab, cex.main = cex.main, 
            cex.lab = cex.lab)
        if (is.na(box.col) == FALSE) {
            box(col = box.col)
        }
    }
    if (return.dimen) {
        invisible(plot.dimen)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
