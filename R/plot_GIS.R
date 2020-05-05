#'Produce a base map using a spatial outject to which grid reference can later
#'be added
#'
#'@description This function take a Spatial Object containing basemap data, such
#'  as coastlines or Vice county boundaries, and creates a plot that can be use
#'  as the base for adding grid reference data to produce atlas style maps.
#'
#'@param gis_data a spatial object that is to be plotted and used as the base
#'  map. BRCmap includes several spatial objects containing coastlines, country
#'  boundaries, or Watsonian Vice County boundaries for Britain & Ireland that
#'  can be used as base maps or you can use other spatial objects
#'@param main character string giving the main title that will be given to the
#'  plot. The default \code{main = ""} is to have no title
#'@param xlab character string giving the label to be used on the x axis of the
#'  plot. The default \code {xlab = ""} is to have no x label
#'@param ylab character string giving the label to be used on the y axis of the
#'  plot. The default \code{ylab = ""} is to have no y label
#'@param xlim numerical vector of length 2 giving the limits of the x axis of
#'  the plot. If null (the default) the limits will be determined from
#'  \code{gis_data}
#'@param ylim numerical vector of length 2 giving the limits of the y axis of
#'  the plot. If null (the default) the limits will be determined from
#'  \code{gis_data}
#'@param show.axis logical value determining whether the plot is to have visible
#'  axes. The default \code{show.axis = FALSE} is to not shown the axes
#'@param show.grid logical value determining wehther the plot is to include a
#'  regular grid on top of \code{gis_data}. The default \code{show.grid = FALSE}
#'  is to not include the grid
#'@param grid.div numerical value giving the spacing between grid lines on the
#'  grid shown on top of \code{gis_data} (if \code{show.grid = TRUE}). The
#'  default \code{grid.div = 1e5} is for 10,000m grid
#'@param grid.col determines the colour of the grid lines (if \code{show.grid =
#'  TRUE}). Arguments takes any inputs accepted by \code{col} arugment supplied
#'  to \code{par}. Default \code{grid.col = "grey"}
#'@param fill.col determines the colour used to fill \code{gis_data}. The
#'  default \code{fill.col = NA} results in no fill
#'@param line.col determines the colour of the border line used for
#'  \code{gis_data}. The default \code{line.col = par("fg")} results in the par
#'  default colour being used (typically \code{"black"})
#'@param bg.col determines the background colour of the plot area. The default
#'  \code{bg.col = "white"} is to use white
#'@param box.col determines the colour of the border surround the plot area.
#'  Default \code{box.col = NA} results in no border to the plotting region
#'@param new.window logical value determing wehtehr the plot should be created
#'  in a new window. If a new window is ty be opened the dimensions of the
#'  window  are set basede on the dimensions of object to be plotted (\code
#'  {gis_data}). The default \code{new.window = FALSE} is to not open a new
#'  window
#'@param no.margin logical value determining whether the plot is to include a
#'  margin. The default \code{no.margin = FALSE} will include a margin. NOTE the
#'  title, xlab, ylab are written to the margin area so will not show if the
#'  margin is removed. This is a legacy argument it is better pracitice to
#'  control margins yourself using the mar agrumnet in \code{par()}
#'@param max.dimen numerical value determining the maximum dimension of the
#'  window that will be opened (if \code{new.window = TRUE})
#'@param cex.main numerical value determining the relative sizing of the main
#'  plot title. The default is \code{cex.main = 1.2}
#'@param cex.lab numerical variable determining the relative sizing of the axis
#'  labels. The default is \code{cex.lab = 1}
#'@param cex.axis numerical variable determining the relative sizing of the axis
#'  values. The default is \code{cex.axis = 0.8}
#'@param blank.plot logical value determining wehther the function should setup
#'  the device but not actually plot anthing . The default \code{blank.plot =
#'  FALSE} is to plot everthing required, but you may want to use change this if
#'  you want setup the plot but for the background outline to be plotted on top
#'  of the grid reference data
#'@param plot.shape logical value determining whether to plot the data in
#'  \code{gis_data} or whether to only setup the background of the plot (i.e
#'  grid lines, axis) to which outline can be added later, useful where the
#'  outline is to cover plotting symbols/colours. this argument is similar to
#'  \code{blank.plot} only differing if axes, titles or elements in addition to
#'  the \code{gis_data} are to be plotted, which is not typically the case
#'  unless values other than the defaults for other arguments are used. The
#'  default \code{plot.shape = TRUE} will plot everything requested
#'@param additions logical variable determining whether the plot is to be a new
#'  plot (clearing the existing plot on the device) or if it is to add to an
#'  existing plot. The default \code{additions = FALSE} will clear any exising
#'  plot (or open in a new blank window if \code{new.window = TRUE}).
#'@param return.dimen logical, determining whether function should return the
#'  plot dimensions
#'
#'@return typically returns nothing unless \code{return_dimen = TRUE} in which
#'  case the plotting dimensions will be returned
#'@export
#'
#'@details This function has alot of arguments allowing the base plot to be
#'  tailored in a number of ways, however the defaults for most arguments should
#'  be the most commonly required options only requiring specification if you
#'  need something slightly different. Although there is a fair degree of
#'  flexibility in this function you may find it easier produce the base map
#'  yourself usign other R mapping functions (e.g. the default \code{plot}
#'  method for the spatial object class you are using), particularly if you want
#'  something bespoke.
#'
#'@author Colin Harrower
#'
#'@seealso \code{\link{plotUK_gr}}, \code{\link{UK}},
#'  \code{\link{UK_countries}},
#'  code{\link{UK_VC}},\code{\link{UK_lowres}},\code{\link{UK_countries_lowres}}
#'  code{\link{UK_VC_lowres}}
#'
#' @examples
#'
#' ## Example 1 - Standard plot_GIS plot using all defaults (except for the
#' ## main plot title which has been set to "UK"). The call uses one of the
#' ## spatial datasets included with BRCmap that has the coastline of the
#' ## Britian and Ireland adn is a lower resolution suitable for plotting
#' ## large areas of Britain/Ireland
#'
#' plot_GIS(UK_lowres, main = "UK")
#'
#' ## Example 2 - Plot Scotland, add a grid using 10km divisons, and colour
#' ## plot background and fill. This time using an included spatial dataset
#' ## with the country outlines for Britain & Ireland but subsetting to only
#' ## Scotland
#'
#' plot_GIS(UK_countries[UK_countries$COUNTRY == "Scotland",], main = "Scotland", grid.div = 10000, fill.col = "lightgreen", bg.col = "lightblue")
#'
#' ## Example 3 - Plot Scotland but set xlim & ylims so plot area only shows
#' ## Shetland Isles
#'
#' plot_GIS(UK_countries[UK_countries$COUNTRY == "Scotland",], main = "Shetland", xlim = c(400000,500000), ylim = c(1100000,1250000), grid.div = 10000, fill.col = "lightgreen", bg.col = "lightblue")
#'
#' ## Example 4 - Plot England without fill, background colour, or gridlines
#'
#' plot_GIS(UK_countries[UK_countries$COUNTRY == "England",], main = "England")
#'
#' ## Example 5 - Plot UK without axes, grid, labels, margins but with fill
#'
#' plot_GIS(UK_lowres, fill = "lightgreen")
#'
#' ## Example 6 - Plot UK with fill and background and then add points &
#' ## labels for capital cities
#'
#' plot_GIS(UK, main = "UK Captial Cities", fill.col = "lightgreen",bg.col="lightblue")
#'
#' # Create vectors of grid references for main cities
#'
#' city_gr = c(London = "TQ3080",Edinburgh = "NT2573", Cardiff = "ST1876", Dublin = "O1534", Belfast = "J3374")
#'
#' # Add points for cities
#' plotUK_gr_points(city_gr,pch=16, col="red")
#'
#' # In order to label points using text need to get x,y coordinates for cities
#' # in m on OSGB grid as this is the system used by the background plot
#'
#' city_en = OSgrid2GB_EN(city_gr)
#'
#' # Now use these coordinates to add labels
#' text(city_en, labels = names(city_gr), col="black", pos = c(1,1,3,2,2))
#' 


plot_GIS <-
function(
	gis_data, main = "", 
	xlab = "", 
	ylab = "", 
	xlim = NULL, 
	ylim = NULL, 
	show.axis = FALSE, 
	show.grid = FALSE, 
	grid.div = 100000, 
	grid.col = "grey",  
	fill.col = NA, 
	line.col = par("fg"), 
	bg.col = "white", 
	box.col = NA, 
	new.window = FALSE, 
	no.margin = FALSE, 
	max.dimen = 13, 
	cex.main = 1.2, 
	cex.lab = 1, 
	cex.axis = 0.8,
	blank.plot = FALSE,
	plot.shape = TRUE,
	additions = FALSE,
	return.dimen = TRUE
){
    # Determine dimesions of plot
    if(is.null(xlim)){
      if(is.list(gis_data)){
        for(i in 1:length(gis_data)){
          temp = attributes(gis_data[[i]])$bbox[1,]
          if(i == 1){
            xlim = temp
          } else {
            xlim = c(min(xlim[1], temp[1]), max(xlim[2], temp[2])) 
          }
        }
      } else {
        xlim = attributes(gis_data)$bbox[1,]
      }
      
    }
    if(show.grid){
      xlim = c(floor(xlim[1]/grid.div) * grid.div, ceiling(xlim[2]/grid.div) * grid.div ) # Round xlim to nearest grid divisions that include xlims specified
    }
    
    if(is.null(ylim)){
      if(is.list(gis_data)){
        for(i in 1:length(gis_data)){
          temp = attributes(gis_data[[i]])$bbox[2,]
          if(i == 1){
            ylim = temp
          } else {
            ylim = c(min(ylim[1], temp[1]), max(ylim[2], temp[2])) 
          }
        }
      } else {
        ylim = attributes(gis_data)$bbox[2,]
      }
      
    }
    if(show.grid){
      ylim = c(floor(ylim[1]/grid.div) * grid.div, ceiling(ylim[2]/grid.div) * grid.div ) # Round xlim to nearest grid divisions that include xlims specified
    }
  
    fill_col <- fill.col
    line_col <- line.col
    
	x.rat = abs(xlim[1] - xlim[2])/100000
	y.rat = abs(ylim[1] - ylim[2])/100000
	aspect.ratio = x.rat / y.rat
	if(aspect.ratio <= 1){
	  y.part = max.dimen - y.rat
	  x.part = x.rat + (y.part * aspect.ratio)
	  #dev.new(height = max.dimen, width = x.part)
	  plot.dimen = data.frame(height = max.dimen, width = x.part)
	} else {
	  aspect.ratio = 1/aspect.ratio
	  x.part = max.dimen - x.rat
	  y.part = y.rat + (x.part * aspect.ratio)
	  #dev.new(height = y.part, width = max.dimen)
	  plot.dimen = data.frame(height = y.part, width = max.dimen)
	}
		
	# Open new window if new.window == TRUE
    if(new.window | dev.cur() == 1){
		dev.new(height = plot.dimen$height, width = plot.dimen$width)
    }
    
	# If function to be run in additions mode then do not run commands to create blank plot
	if(additions == FALSE){
		if(no.margin){
		  par(mar = c(0,0,0,0))
		}
		 
		# Set up blank plot
		plot(0, 0, xlim=xlim, ylim=ylim, xaxs='i', yaxs='i', xaxt='n', yaxt='n', xlab="",ylab="", type='n', bty="n", asp=1) # Plot covering all of uk
		rect(xlim[1],ylim[1], xlim[2], ylim[2],col=bg.col, border = NA)
	}
    
	# If blank.plot == FALSE then continue and plot gridlines/shape data
	if(blank.plot == FALSE){
	
		# Plot data from shape file(s) if plot.shape == TRUE
		if(plot.shape){
			# Plot GIS data onto plot
			if(is.list(gis_data)){
			  for(i in 1:length(gis_data)){
				plot(gis_data[[i]], add = TRUE, col = fill.col, border = line.col)
			  }
			} else {
			  plot(gis_data, add=TRUE, col = fill.col, border = line.col)
			}
		}
		
	  
		
		# Add Axis and gridlines to plot
		if(show.grid){
		  #abline(h = seq(ylim[1],ylim[2], by = grid.div), v = seq(xlim[1],xlim[2], by=grid.div), col= grid.col)
		  # Horizontal lines
		  segments(x0 = xlim[1], y0 = seq(ylim[1],ylim[2], by = grid.div), x1 = xlim[2], col=grid.col)
		  # Vertical lines
		  segments(x0 = seq(xlim[1], xlim[2], by = grid.div), y0 = ylim[1], y1 = ylim[2], col=grid.col)
		}
		if(show.axis){
		  axis(1, at=seq(xlim[1],xlim[2], by=grid.div), labels = seq(xlim[1]/1000,xlim[2]/1000,by=grid.div/1000), cex.axis = cex.axis)
		  axis(2, at=seq(ylim[1],ylim[2], by=grid.div), labels = seq(ylim[1]/1000,ylim[2]/1000,by=grid.div/1000), cex.axis= cex.axis)
		}
		title(main=main, xlab=xlab, ylab=ylab, cex.main= cex.main, cex.lab= cex.lab)
		
		if(is.na(box.col) == FALSE){
		  box(col = box.col)
		}
	}
	
	if(return.dimen){
		invisible(plot.dimen)
	}
  
  }
