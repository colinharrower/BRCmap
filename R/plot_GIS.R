#'Produce a base map using a spatial outject to which grid reference can later
#'be added
#'
#'@description This function take a Simple Features spatial object containing
#'  basemap data, such as coastlines or Vice county boundaries, and creates a
#'  plot that can be use as the base for adding grid reference data to produce
#'  atlas style maps.
#'
#'@param gis_data a spatial object that is to be plotted and used as the base
#'  map. BRCmap includes several spatial objects containing coastlines, country
#'  boundaries, or Watsonian Vice County boundaries for Britain & Ireland that
#'  can be used as base maps or you can use other spatial objects
#'@param blank_plot logical value determining whether the function should setup
#'  the device but not actually plot anything . The default \code{blank_plot =
#'  FALSE} is to plot everything required, but you may want to use change this if
#'  you want setup the plot/device but not to plot the actual background outline
#'  yet, for instance so it can be plotted later and lie on top of the grid
#'  reference data
#'@param show_grid logical value determining wehther the plot is to include a
#'  regular grid on top of \code{gis_data}. The default \code{show_grid = FALSE}
#'  is to not include the grid
#'@param grid_div numerical value giving the spacing between grid lines on the
#'  grid shown on top of \code{gis_data} (if \code{show.grid = TRUE}). The
#'  default \code{grid_div = 10000} is for 10,000m grid
#'@param grid_col determines the colour of the grid lines (if \code{show.grid =
#'  TRUE}). Arguments takes any inputs accepted by \code{col} arugment supplied
#'  to \code{par}. Default \code{grid_col = "grey"}
#'@param grid_lty determines the line type of the grid lines (if \code{show.grid
#'  = TRUE}). Arguments takes any inputs accepted by \code{lty} argument
#'  supplied to \code{segments}. Default \code{grid_lty = 1}'
#'@param grid_lwd determines the line width of the grid lines (if
#'  \code{show.grid = TRUE}). Arguments takes any inputs accepted by \code{lwd}
#'  argument supplied to \code{segments}. Default \code{grid_lwd = 1}'
#'@param ... Other arguments that will be passed to the `sf.plot` method

#'
#'@return returns nothing
#'@export
#'
#'
#'@details This function is now effectively a wrapper funciton for the plot
#'  function provided in the `sf` package. Previous versions of this function
#'  contained a number of arguments allowing flexibility in the way the base map
#'  was created. The majority of those abilities are available via the default
#'  plot method for `sf` objects so this function has been rewritten to provide
#'  these features via that route, passing any supplied arguments on to sf::plot
#'  methods. See the examples below for code demonstrating how to set colours
#'  for the plot background, the fill colour, border line colour, x and y limts,
#'  etc. '
#'
#'@author Colin Harrower
#'
#'@seealso \code{\link[sf]{plot}}, \code{\link{plotUK_gr}}, \code{\link{UK}},
#'  \code{\link{UK_countries}}, \code{\link{UK_VC}}, \code{\link{UK_lowres}},
#'  \code{\link{UK_countries_lowres}} \code{\link{UK_VC_lowres}}
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
#' plot_GIS(UK_countries[UK_countries$COUNTRY == "Scotland",], main = "Scotland", 
#' grid.div = 10000, col = "lightgreen", bgc = "lightblue")
#'
#' ## Example 3 - Plot Scotland but set xlim & ylims so plot area only shows
#' ## Shetland Isles and add white 10,000m grid lines
#'
#' plot_GIS(UK_countries[UK_countries$COUNTRY == "Scotland",], main = "Shetland", 
#' xlim = c(400000,500000), ylim = c(1100000,1250000), show_grid = TRUE, 
#' grid_div = 10000, grid_col = "white", col = "lightgreen", bgc = "lightblue")
#'
#' ## Example 4 - Plot England without fill, background colour, or gridlines
#'
#' plot_GIS(UK_countries[UK_countries$COUNTRY == "England",], main = "England")
#'
#' ## Example 5 - Plot UK without axes, grid, labels, margins but with fill
#'
#' plot_GIS(UK_lowres, col = "lightgreen")
#'
#' ## Example 6 - Plot UK with fill and background and then add points &
#' ## labels for capital cities
#'
#' plot_GIS(UK, main = "Captial Cities", col = "lightgreen",bgc="lightblue")
#'
#' # Create vectors of grid references for main cities
#'
#' city_gr = c(London = "TQ3080",Edinburgh = "NT2573", Cardiff = "ST1876", 
#' Dublin = "O1534", Belfast = "J3374")
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
	gis_data,
	blank_plot = FALSE,
	show_grid = FALSE,
	grid_div = 10000,
	grid_col = "grey",
	grid_lty = 1,
	grid_lwd = 1,
	...
){
  # If blank plot then use plot_sf to only setup the plotting device but not to actually plot the spatial outline
  if(blank_plot){
    sf::plot_sf(gis_data,...)
  } else{
    # Otherwise setup plot and & geometry
      plot(sf::st_geometry(gis_data),...)
  }
  
  # If show_grid TRUE
  if(show_grid){
    # See if ... includes xlim or ylim
      d_args = list(...)
    if("xlim" %in% names(d_args)){
      x_lims = d_args$xlim
    } else {
      x_lims = sf::st_bbox(gis_data)[c("xmin","xmax")]
    }
    if("ylim" %in% names(d_args)){
      y_lims = d_args$ylim
    } else {
      y_lims = sf::st_bbox(gis_data)[c("ymin","ymax")]
    }
      
    # Determine points from gridlines
      x_l = seq(from = floor(x_lims[1]/grid_div)*grid_div, to = ceiling(x_lims[2]/grid_div)*grid_div, by = grid_div)
      y_l = seq(from = floor(y_lims[1]/grid_div)*grid_div, to = ceiling(y_lims[2]/grid_div)*grid_div, by = grid_div)
      
    # Now add gridlines as segments
      segments(x0 = x_l, y0 = y_lims[1], y1 = y_lims[2], col=grid_col, lty = grid_lty, lwd = grid_lwd)
      segments(x0 = x_lims[1],x1 = x_lims[2], y0 = y_l, col=grid_col, lty = grid_lty, lwd = grid_lwd)
      
  }
  
}
