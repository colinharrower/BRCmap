
#' @title Add a regular polgon to the existing plot
#' @description \code{draw_polyon} draw a regular polyon with n vertices centred on the coordinates specified by x,y
#' onto the existing plot
#'
#' @param x a single numerical value giving the x coordinate for the central
#'   point.
#' @param y a single numerical value giving the y coordinate for the central
#'   point.
#' @param r a single numerical value or a vector of values giving the radii of
#'   the vertices from the central point.
#' @param n a number specifying the number of vertices
#' @param rot a numerical value specifying the rotational poisition of the first
#'   point. The default \code{rot = 0} means the first point will be at the 12
#'   o'clock position.
#' @param ... any other arugments will be passed through the \code{polygon}
#'   function used to actually add the polygon to the plot, e.g. \code{col =
#'   "red"}, code{border = "blue"}, etc.
#' 
#' @details In most cases this function plots a regular polygon, i.e. where all 
#'   angles are the same (equiangular) and where all sides are the same length 
#'   (equilateral). An exception is made however in the case of a triangle 
#'   (\code{n = 3}) where this function returns an isosceles triangle rather
#'   than an equilateral one. This alteration is made so that the resulting
#'   triangle fills as much of the area occupied by a square with dimensions r*2
#'   by r*2.
#'   
#'   This function is also used by BRCmap to draw circular symbols of specified 
#'   dimensions by using a polygon with sufficient vertices to approximate a 
#'   circle. There is a trade-off in the number of vertices required to 
#'   approximate a cirlce will with larger vectices producing a smoother polygon
#'   but having increasing complexity.
#'   
#'   This funciton is an internal functiont that is used by other functions in 
#'   BRCmap to plot symbols of specified dimesions onto maps. This function only
#'   draws a single polyon but you can use the wrapper function 
#'   \code{draw_polygons} that uses mapply to plot multiple polygons.
#'
#' @return The coordinates of the vertices are returned invisibly
#' @author Colin Harrower
#' @seealso \code{\link{draw_polygons}}, \code{\link{draw_symbols}}
#' @export
#'
#' @examples 
#'   ## An example of adding a regular polygon to a plot
#' 
#'   # First create a blank plot
#'     plot(1:60,1:60, type="n")
#'   # Now add a couple of different polygons
#'     # Triangle (note this is an isosceles traingle not equilateral)
#'       draw_polygon(10,10,r=3,n=3, col="red")
#'     # Square (need to alter rotation of first point so that this is a square not diamond)
#'       draw_polygon(20,20,r=3,n=4,rot=45,col="blue")
#'       draw_polygon(30,30,r=3,n=4,rot=,col="blue")
#'     # Pentagon
#'       draw_polygon(40,40,r=3,n=5,rot=,col="green")
#'     # Hexagon
#'       draw_polygon(50,50,r=3,n=6,rot=,col="orange")
#'  
#' 
draw_polygon = function(x,y, r, n, rot = 0, ...){
  # Determine angles required to make polygon with n number of points (note default for rotate is from x axis proceeding anticlockwise, hence 90 and -rot in formula)
  if(n==3){
    # To have a triangle that fills the same area as a square with dimensions n*2 then it needs to be an isosceles traingle so need to modify a and r values
      a = c(0,135,225)+90
    # Calculate length of longer sides
      l_r = sqrt(r^2+r^2)
    # Now create vector of radius values to be used
      r = c(r,l_r,l_r)
  } else {
    a = (-rot + 90 + (0:(n-1) * (360/n)) ) %% 360
  }
  # Determine x coordinates for points between which polygon will be draw
  x = round(x + r * cos(a*(pi/180)),4)
  # Determine y coordinates for points between which polygon will be draw
  y = round(y + r * sin(a*(pi/180)),4)
  # Draw polygon
  polygon(x,y,...)
  # return points invisibly
  return(invisible(data.frame(x,y)))
}

#' @title Add a series of regular polygons to the existing plot
#'   
#' @param x a single numerical value giving the x coordinate for the central 
#'   point.
#' @param y a single numerical value giving the y coordinate for the central 
#'   point.
#' @param r a single numerical value or a vector of values giving the radii of 
#'   the vertices from the central point.
#' @param n a number specifying the number of vertices.
#' @param rot a numerical value specifying the rotational poisition of the first
#'   point. The default \code{rot = 0} means the first point will be at the 12 
#'   o'clock position.
#' @param ... any other arugments will be passed through the \code{polygon} 
#'   function used to actually add the polygon to the plot, e.g. \code{col = 
#'   "red"}, code{border = "blue"}, etc.
#'   
#' @details In most cases this function plots a regular polygon, i.e. where all 
#'   angles are the same (equiangular) and where all sides are the same length 
#'   (equilateral). An exception is made however in the case of a triangle 
#'   (\code{n = 3}) where this function returns an isosceles triangle rather 
#'   than an equilateral one. This alteration is made so that the resulting 
#'   triangle fills as much of the area occupied by a square with dimensions r*2
#'   by r*2.
#'   
#'   This function is also used by BRCmap to draw circular symbols of specified 
#'   dimensions by using a polygon with sufficient vertices to approximate a 
#'   circle. There is a trade-off in the number of vertices required to 
#'   approximate a cirlce will with larger vectices producing a smoother polygon
#'   but having increasing complexity.
#'   
#'   This funciton is an internal functiont that is used by other functions in 
#'   BRCmap to plot symbols of specified dimesions onto maps.
#'  
#' @author Colin Harrower    
#' @seealso \code{\link{draw_polygon}}, \code{\link{draw_symbols}}
#' @export
#' 
#' @examples
#'   # Examples of adding regular polygons to a plot
#'   # First create a blank plot
#'     plot(1:60,1:60, type="n")
#'   # Now add a series of pentagons across the diagonal
#'     xy_vals = seq(10,50,by=10)
#'     draw_polygons(xy_vals, xy_vals, r=3,n=5, col="red")
#'   
#'   # Open a blank plot
#'     plot(1:60,1:60, type="n")
#'   # Now add a series of polygons across the diagonal starting with
#'   ## a triangle increasing the vertices by 1 each time
#'     draw_polygons(xy_vals, xy_vals, r=3, n = 3:7, col=1:5)
#'
draw_polygons = function(x,y,r,n,rot=0, ...){
  # Determine addition arguments that match polygons
  matched_args = list(...)[intersect( names(list(...)), names(formals(polygon))) ]
  # Build x,y,r,n, rot into a data.frame
  temp = mapply(draw_polygon, x, y, r, n, rot, ...)	
}

#' @title Add symbols to existing plot
#' @description \code{draw_symbols} draws symbols of specified type and size
#'   onto the existing plot
#'
#' @param x a numerical vector specifying the x coordinates of the central
#'   points for each symbol.
#' @param y a numerical vector specifying the y coordinates of the central
#'   points for each symbol.
#' @param symbol A text string specifying the type of symbols to be draw. The 
#'   default \code{symbol = "circle"} draws a circle with radius dimen/2. Other
#'   possible values are \code{triangle}, \code{square}, \code{diamond}.
#' @param dimen The dimensions of the symbol. This is given as the maximum 
#'   height/length of the total polygon and not the radii from the central point
#' @param n_circle A number specifying the number number of vertices in the
#'   polygon that will be used to approximate a polygon. The default
#'   \code{n_circle = 32} will give a good approximation of a circle in most
#'   cases, however if you require smooth approximations then inccrease this
#'   value.
#' @param ... any other arugments will be passed through the \code{polygon} 
#'   function used to actually add the polygon to the plot, e.g. \code{col = 
#'   "red"}, code{border = "blue"}, etc.
#'
#' @details This function is used by functions in BRCmap (see See Also seciton
#'   below) to place plotting symbols of a given size at coordinates specified.
#'   At present only a couple of symbols (circle, triangle, square, diamond) are
#'   possible at present. Other commonly used mapping symbols will be added in
#'   the future.
#'   
#'   It should be noted that the 'circles' produced are actually n-vertice
#'   polygons that in most cases should give acceptable approximations for a
#'   true circle. If you require a smoother circle increase the number of
#'   vertices in the polygon used to approximate the circle (via
#'   \code{n_circle}). It should be noted that symbols can appear more jagged in
#'   the default R device that they will when plotted to other devices such as
#'   pdf.
#' @author Colin Harrower
#' @seealso \code{\link{plotUK_gr_symbols}}
#' @export
#'
#' @examples
#'   ## Example of adding symbols to a plot
#'   # First create a blank plot
#'     plot(1:60,1:60, type="n")
#'   # Now add a series of pentagons across the diagonal
#'     xy_vals = seq(10,50,by=10)
#'     draw_symbols(xy_vals, xy_vals, "circle", dimen = 3, col="red")
#'     
draw_symbols = function(x, y, symbol = "circle", dimen, n_circle = 32, ...){
    # Ensure values passed to symbol are lower case
      symbols = tolower(symbol)
    
    switch(symbol, 
          circle = draw_polygons(x,y,r=dimen/2,n=n_circle, ...), 
          triangle = draw_polygons(x,y,r=dimen/2,n=3, ...),
          square = draw_polygons(x,y,r = 0.5*sqrt(dimen^2+dimen^2),n=4,rot=45, ...),
          diamond = draw_polygons(x,y, r= 0.5*sqrt(dimen^2+dimen^2),n=4,rot=0, ...),
          stop("Invalid option for argument 'symbol'")
    )
}

#' @title Add symbols to existing map
#' @description Function to plot valud UK grid references as symbols onto a existing map
#'
#' @param gridref A character vector containg the grid refrences to be added to the map.
#' @param symbol A text string specifying the type of symbol to be added. The default
#' \code{symbol="circle"} will add a circle (see details). Other possible values are; 
#' \code{triangle}, \code{square}, \code{diamond}.
#' @param dimen A vector of numerical values specifying the dimensions for the
#'   symbols to be plotted. The default \code{dimen = NULL} means that the
#'   dimensions of each symbols will be the dimensions of the corresponding grid
#'   references, e.g. a 10km grid square will be represented by a symbols with
#'   dimensions of 10km
#' @param dimen_type A text string specifying how the dimensions are being
#'   specified. The default \code{dimen_type = "abs"} indicates that the
#'   dimensions are being given in plotting units. The alternative
#'   \code{dimen_type = "prop"} indicates that the dimensions are being
#'   specified as a proportion of the grid reference dimension, e.g. 0.9 means
#'   that the dimensions of the symbols will be 90% of dimensions of the
#'   relevant grid square.
#' @param centre A logical variable determing whether the symbol will be 
#'   centered on the centre point of the grid reference or at the lower left 
#'   corner. The default \code{centre = TRUE} will plot the symbol so that it is
#'   centered on the central point of the square.
#' @param gr_prec A numerical vector specifying the precision of each grid
#'   reference. The default \code{gr_prec = NULL} will determine the precision directly
#'   from the grid reference. This variable will only be needed if the default
#'   precision needs to be overridden.
#' @param ci_insert A logical variable determining whether Channel Islands grid
#'   references are to be plotted in an insert box to the left of the UK. The
#'   default \code{ci_insert = FALSE} will plot the Channel Islands grid references in
#'   their true position to the south of the UK.
#' @param ci_origin A vector of two values giving the x & y cooridinates for the
#'   origin of the Channel Islands grid relative to the the UK when using the
#'   insert box.
#' @param ... Any other arguments that will be passed to the functions
#'   used to add the symbols to the map. Examples of typical arguments that you
#'   may want to pass through are \code{col}, \code{border}.
#'
#' @return By default this function does not return anything, but if the output
#'   from the function is assigned it will return the x,y values (eastings &
#'   northings on OSGB) of the grid references via the invisible function.
#' @author Colin Harrower   
#' @seealso \code{\link{plotUK_gr}}, \code{\link{plotUK_gr_cats}}, \code{\link{plotUK_gr_points}}
#' @export
#'
#' @examples
#'   ## Example of adding grid references to a map as symbols
#'   # Create blank map of UK
#'     plot_GIS(UK, show.grid = FALSE, xlab = "", ylab="", show.axis = FALSE)
#'     
#'   # Add some points as circles
#'     plotUK_gr_symbols(c("SU68","SP50","SU77"), col="red", border="darkred")
#'   # Add some other points this time as triangles
#'     plotUK_gr_symbols(c("NT27","NS56","NJ90"), symbol = "triangle", col="blue", border="darkblue")
#'   # Add some more as diamonds
#'    plotUK_gr_symbols(c("O13","J37"), symbol = "diamond", col="green", border="darkgreen")
#'     
plotUK_gr_symbols <-
  function(gridref, symbol = "circle", dimen = NULL, dimen_type = "abs", centre = TRUE, gr_prec = NULL, ci_insert = FALSE, ci_origin = c(-180000,30000), ...){
    if(!dimen_type %in% c("abs","prop")){
      stop("invalid option for dimen_type (recognised values are 'prop' or 'abs')")
    }
    # Check that values have been passed
    if(length(gridref) > 0){
      # Extract only unique gridrefs
      # If gr_prec is single value then can deal directly with grid ref, if not then need to look for unique gridref/gr_prec combinations
      if( length(gr_prec) == 1 | is.null(gr_prec) ){
        gridref = unique(gridref)	
      } else if( length(gridref) == length(gr_prec) ) {
        temp = unique(data.frame(gridref = gridref, gr_prec = gr_prec, stringsAsFactors = FALSE))
        gridref = temp$gridref
        gr_prec = temp$gr_prec
      } else {
        stop("Invalid Value for gr_prec: accepted values are NULL, a single value (used for all gridrefs), or a vector of values the same length as gridref vector")
      }
      
      
      # Determine number of unique gridrefs
      gr_len = length(gridref)
      
      # If gr_prec is null then determine precision from gridref, if not check whether gr_prec is 1 value (if so create vector of length gr_len) or a vector of length gr_len
      if(is.null(gr_prec)){
        gr_prec = det_gr_precision(gridref)
      } else if(length(gr_prec) == 1) {
        gr_prec = rep(gr_prec, gr_len)
      }
      
      # Determine eastings and northings of point in the centre of the grid square
      gr_points = gr_let2num(gridref, centre = centre)
      
      # Find Irish gridrefs
      ir_inds = which(grepl("(^[[:upper:]]{1}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1}[[:digit:]]{2,}$)", gridref) & !is.na(gr_points$EASTING))
      if(length(ir_inds) > 0){
        gr_points[ir_inds,] = OSgridReprojection(gr_points$EASTING[ir_inds], gr_points$NORTHING[ir_inds], org_grid = "OSNI", out_grid = "OSGB")
      }
      
      # If ci_insert = TRUE then find channel islands gridrefs and convert to insert position
      # Find channel islands grid refs
      ci_inds = which(grepl("(^(WA|WV)[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^(WA|WV)[[:digit:]]{2,}$)", gridref) & !is.na(gr_points$EASTING))
      if(length(ci_inds) > 0){	
        if(ci_insert){	
          # Convert eastings and northings for these gridrefs to the insert positions
          gr_points[ci_inds,] = CI_insert_en(gr_points$EASTING[ci_inds], gr_points$NORTHING[ci_inds])
        } else {
          # If no ci_insert the plot in normal position relative to UK
          gr_points[ci_inds,] = OSgridReprojection(gr_points$EASTING[ci_inds], gr_points$NORTHING[ci_inds], org_grid = "UTM30", out_grid = "OSGB")
        }
      }
      
      # Figure out symbol dimensions
      if(is.null(dimen)){
        dimen = gr_prec 
      } else {
        if(dimen_type=="prop"){
          # Replace dimen with dimen * gr precision
          dimen = gr_prec*dimen  
        }
      }
      
      # Plot symbols
      draw_symbols(gr_points$EASTING, gr_points$NORTHING, symbol = symbol, dimen=dimen, ...)
      
      
      # Return gr_points invisibly
      invisible(gr_points)
    }
  }