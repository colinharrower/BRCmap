#' @title Add a regular polgon to the existing plot
#' @description \code{draw_polyon} draw a regular polyon with n vertices centred
#'   on the coordinates specified by x,y onto the existing plot
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
#'   "red"}, \code{border = "blue"}, etc.
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
#'   approximate a circle will with larger vectices producing a smoother polygon
#'   but having increasing complexity.
#'
#'   This function is an internal function that is used by other functions in
#'   BRCmap to plot symbols of specified dimensions onto maps. This function
#'   only draws a single polygon but you can use the wrapper function
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
#'     # Square (alter rotation of first point for square rather than diamond)
#'       draw_polygon(20,20,r=3,n=4,rot=45,col="blue")
#'       draw_polygon(30,30,r=3,n=4,rot=,col="blue")
#'     # Pentagon
#'       draw_polygon(40,40,r=3,n=5,rot=,col="green")
#'     # Hexagon
#'       draw_polygon(50,50,r=3,n=6,rot=,col="orange")
#'
#' 
draw_polygon = function(x,y, r, n, rot = 0, ...){
  # Determine angles required to make polygon with n number of points (note
  # default for rotate is from x axis proceeding anticlockwise, hence 90 and
  # -rot in formula)
  if(n==3){
    # To have a triangle that fills the same area as a square with dimensions
    # n*2 then it needs to be an isosceles traingle so need to modify a and r
    # values
      a = c(0,135,225)-rot+90
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
#' @param x a numerical vector giving the x coordinates for the central points
#'   of the symbols.
#' @param y a numerical vector giving the y coordinates for the central points
#'   of the symbols.
#' @param r a single numerical value or a vector of values giving the radii of
#'   the vertices from the central point.
#' @param n a number specifying the number of vertices of the polygon to be
#'   plotted. For example n = 4 will draw a diamond or square (depending on
#'   \code{rot}), while n = 6 will plot a hexagon
#' @param rot a numerical value specifying the rotational poisition of the first
#'   point. The default \code{rot = 0} means the first point will be at the 12
#'   o'clock position.
#' @param ... any other arguments will be passed through the \code{polygon}
#'   function used to actually add the polygon to the plot, e.g. \code{col =
#'   "red"}, \code{border = "blue"}, etc.
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
#'   approximate a cirlce will with larger vertices producing a smoother polygon
#'   but having increasing complexity.
#'
#'   This function is an internal function that is used by other functions in
#'   BRCmap to plot symbols of specified dimensions onto maps.
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
draw_polygons = function(x,y,r,n=5,rot=0, ...){
  # Determine addition arguments that match polygons
  matched_args = list(...)[intersect( names(list(...)), names(formals(polygon))) ]
  # Build x,y,r,n, rot into a data.frame
  temp = mapply(draw_polygon, x, y, r, n, rot, ...)	
}

#' Add a simple star polygon to the plot
#'
#' @param x a single numerical value giving the x value for the center of the
#'   star.
#' @param y a single numerical value giving the y value for the center of the
#'   star.
#' @param r a numerical value specifying the radius to the outer points of the
#'   star.
#' @param nrays A numerical value specifying the number of rays or points of the
#'   star. The default \code{nrays = 5} will draw a star with 5 rays or points.
#' @param in_ratio A numerical value, typically between 0 and 1, specifying the
#'   inner ratio for the radius of the inner section of the star (i.e the base
#'   of the rays/points) relative to radius for the outer vertices\code{r}. The
#'   default \code{r=0.5} the inner section will be half the distance of the
#'   rays from the center of the star.
#' @param ... any other arguments will be passed through the \code{draw_polygon}
#'   or \code{polygon} functions used to actually add the star to the plot, e.g.
#'   \code{col = "red"}, \code{border = "blue"}, etc.
#'
#' @details This function creates a simple star polygon where the vertices
#'   vertices are arranged at regular angles around the central point but at
#'   alternating radii from the central point. The vertices will be placed
#'   alternately at \code{r} and \code{r * in_ratio} distance from the central
#'   point. The default inner ratio \code{in_ratio = 0.5} will produce a star
#'   where the inner vertices are positioned half of the distance to the outer
#'   vertices.
#'
#'   Note that when plotting a 3 ray star with default in_ratio \code{in_ratio =
#'   0.5} the symbol plotted will be a triangle. To have a more typical star 
#'   with 3 rays use a value of \code{in_ratio < 0.5}.
#'
#'   This function is an internal function that is used by other functions in
#'   BRCmap to plot symbols of specified dimensions onto maps.
#'
#' @returns The x,y values of the point are returned invisibly
#' @author Colin Harrower
#' @seealso \code{\link{draw_polygon}}, \code{\link{draw_symbols}}
#' @export
#'
#' @examples
#'   ## An example of adding a regular star to a plot
#'
#'   # First create a blank plot
#'   # Set asp = 1 so stars do not look warped
#'     plot(1:60,1:60, type="n",asp = 1)
#'   # Now add a couple of different polygons
#'     # 5 ray/pointed star
#'       draw_star(10,10,r=3,nrays=5, col="red")
#'     # 8 ray/pointed star
#'       draw_star(20,20,r=3,nrays=8,col="blue")
#'     # 5 ray/pointed star with smaller inner section
#'       draw_star(30,30,r=3,nrays=5, in_ratio= 0.3,col="purple")
#'     # 3 ray star but with smaller in_ratio
#'     # NOTE if in_ratio left at default= 0.5 then it will plot a triangle
#'       draw_star(40,40,r=3,nrays=3, in_ratio= 0.3,col="orange")
#'       # Now draw the same star but change the rotation
#'       draw_star(50,50,r=3,nrays=3,in_ratio= 0.3,rot = 180,col="orange")
#' 
draw_star = function(x,y,r,nrays = 5,in_ratio = 0.5,...){
	# Use draw polygon to plot star by determining the positions of inner and outer
	# vertices using R and in_ratio
	draw_polygon(x,y,n = nrays*2,r = r * rep(c(1,in_ratio),nrays),...)
  # return points invisibly
  return(invisible(data.frame(x,y)))
}

#' Add simple star polygon(s) to the plot
#'
#' @param x numerical vector giving the x values for the central points at which
#'   stars will be plotted.
#' @param y numerical vector giving the y values for the central points at which
#'   stars will be plotted.
#' @param r numerical vector specifying the radii for the outer vertices of the
#'   star.
#' @param nrays numerical value or vector specifying the number of rays or
#'   points the star will have
#' @param in_ratio numerical value specifying the ratio for radii of the inner
#'   vertices of the star relative to \code{r}, the radii specified for the
#'   outer vertices.
#' @param ... any other arguments will be passed through the \code{draw_polygon}
#'   or \code{polygon} functions used to actually add the star to the plot, e.g.
#'   \code{col = "red"}, \code{border = "blue"}, etc.
#'
#' @details This function creates simple star polygon(s), at point(s) x,y, where
#'   the vertices vertices are arranged at regular angles around the central
#'   point but at alternating radii from the central point. The vertices will be
#'   placed alternately at \code{r} and \code{r * in_ratio} distance from the
#'   central point. The default inner ratio \code{in_ratio = 0.5} will produce a
#'   star where the inner vertices are positioned half of the distance to the
#'   outer vertices.
#'
#'   This function is an internal function that is used by other functions in
#'   BRCmap to plot symbols of specified dimensions onto maps.
#'
#'
#' @author Colin Harrower
#' @seealso \code{\link{draw_star}}, \code{\link{draw_polygons}},
#'   \code{\link{draw_symbols}}
#'
#' @export
#'
#' @examples
#'   # Examples of adding simple stars to a plot
#'   # First create a blank plot
#'     plot(1:60,1:60, type="n",asp = 1)
#'   # Now add a series of stars across the diagonal
#'     xy_vals = seq(10,50,by=10)
#'     draw_stars(xy_vals, xy_vals, r=3,nrays=5, col="red")
#'
#'   # Open a blank plot
#'     plot(1:60,1:60, type="n",asp = 1)
#'   # Now add a series of stars across the diagonal starting with
#'   # a 3 rayed star increasing the number of rays by 1 each time
#'     draw_stars(xy_vals, xy_vals, r=3, 
#'       nrays = 3:7, in_ratio = c(0.3,0.5,0.5,0.5,0.5), col=1:5
#'     )
#'   # Add some extra 5 ray/pointed stars with different in_ratios above
#'   # below existing 5 ray star
#'     draw_stars(
#'       xy_vals[3], xy_vals[c(2,4)], r=3, 
#'       nrays = 5, in_ratio = c(0.3,0.7), col=3
#'     )
#'     
draw_stars = function(x,y,r,nrays=5,in_ratio = 0.5,...){
  # Determine addition arguments that match polygons
  matched_args = list(...)[intersect( names(list(...)), names(formals(polygon))) ]
  # Build x,y,r,n, rot into a data.frame
  temp = mapply(draw_star, x, y, r, nrays, in_ratio, ...)	
}

#' Add a rotated line based symbol to the plot
#'
#' @param x the x value at which the line symbol will be plotted.
#' @param y the y value as which the line symbol will be plotted.
#' @param r the length of the lines specified as a radius from the center of the
#'   symbol.
#' @param nlines the number of individual lines that the symbol is comprised off
#' @param rot the rotation of symbol in degrees. The default \code{rot = 0} will
#'   plot the first line, of length \code{r*2}, vertically across the central
#'   point from top to bottom.
#' @param ... any other arguments will be passed through to the
#'   \code{segements()} used internally to add the line symbol to the plot, 
#'   e.g. \code{col ="red"}, \code{lwd = 2}, etc.
#'
#' @details This function creates simple rotated line plotting symbol at point
#'   x,y. The points are comprised of a series of linear lines, of length
#'   \code{r*2}, rotated evenly around the central point of the symbol. For
#'   example \code{nlines = 2} will produce a + (plus) sign or a cross,
#'   depending on the value of \code{rot}, while \code{nlines = 3} will draw an
#'   asterisk.
#'
#'   This function is an internal function that is used by other functions in
#'   BRCmap to plot symbols of specified dimensions onto maps.
#'
#' @export
#'
#' @author Colin Harrower
#' @seealso \code{\link{draw_star}}, \code{\link{draw_symbols}}
#'
#' @examples
#'   ## An example of adding a rotated line symbols
#'
#'   # First create a blank plot
#'   # Note set asp = 1 to ensure symbols do not look warped
#'     plot(1:60,1:60, type="n",asp = 1)
#'   # Now add a couple of different rotated line symbols
#'     # plus sign
#'       draw_line_sym(10,10,r=3, nlines=2, col="red")
#'     # cross
#'       draw_line_sym(20,20,r=3,nlines=2, rot = 45, col="blue")
#'     # smaller cross with heavier line weight
#'       draw_line_sym(30,30,r=1.5,nlines=2, rot = 45, lwd = 2, col="blue")
#'     # asterisk
#'       draw_line_sym(40,40,r=3,nlines=3, col="purple")
#'     # 5 lined star like symbol
#'       draw_line_sym(50,50,r=2,nlines=5, lwd = 2, col="orange")
#' 
draw_line_sym = function(x,y,r,nlines = 4, rot = 0,...){
  # The number of points to join is nlines * 2
	n = nlines * 2
  # Determine angles required to add that number of points to the full 360 around the x,y points
    a = (-rot + 90 + (0:(n-1) * (360/n)) ) %% 360
  # Determine x coordinates for points between which polygon will be draw
	x = round(x + r * cos(a*(pi/180)),4)
  # Determine y coordinates for points between which polygon will be draw
	y = round(y + r * sin(a*(pi/180)),4)
  # Determine pairs of lines (180 degrees appart)
	segments(x0 = x[1:nlines],y0=y[1:nlines],x1 = x[(nlines+1):n], y1 = y[(nlines+1):n],...)
}

#' Add rotated line based symbol(s) to the plot
#'
#' @param x vector of x values giving the center points of the symbol(s).
#' @param y vector of y values giving the center points of the symbol(s).
#' @param r numerical value or vector specifying the radii of the circle(s) that
#'   the lines will intersect, i.e. length of the line = \code{2*r}.
#' @param nlines numerical value(s) specifying the number of rotated lines that
#'   will comprise the symbol being plotted. The default \code{nlines=4} will
#'   plot a plus sign.
#' @param rot the rotation of the first line given as degrees. The default
#'   \code{rot=0} will draw the first line vertically from top to the bottom.
#' @param ... any other arguments will be passed through to
#'   \code{draw_line_sym()} or \code{segements()} used internally to add the
#'   line symbols to the plot, e.g. \code{col ="red"}, \code{lwd = 2}, etc.
#'
#' @details This function creates rotated line plotting symbol(s) at the
#'   point(s) specified by the coordinates \code{x} and \code{y}. The points are
#'   comprised of series of linear lines, of length \code{r*2}, rotated evenly
#'   around the central point of the symbol.
#'
#'   Using the default \code{nlines = 2} will produce a + (plus) sign or a
#'   cross, depending on the value of \code{rot}, while \code{nlines = 3} will
#'   draw an asterisk.
#'
#'   This function is an internal function that is used by other functions in
#'   BRCmap to plot symbols of specified dimensions onto maps.
#'
#' @export
#'
#' @examples
#'   ## An example of adding a rotated line symbols
#'
#'   # First create a blank plot
#'   # note asp = 1 so symbols do not look warped/streched
#'     plot(1:60,1:60, type="n",asp = 1)
#'   # Now add a series of stars across the diagonal
#'     xy_vals = seq(10,50,by=10)
#'   # Now add a series of line symbols across the plot starting with
#'   # 2 line based symbols (plus & cross)
#'   # and incrementing for subsequent rows
#'     draw_line_syms(rep(xy_vals,each = 5), rep(xy_vals,5), 
#'       r=rep(seq(1,by=0.5, length.out = 5),each = 5),
#'       rot = c(0,45,0,0,0), lwd = 2,
#'       nlines = c(2,2:5), col=1:5
#'     )
#' 
draw_line_syms = function(x,y,r,nlines=4,rot=0,...){
  # Determine addition arguments that match polygons
  matched_args = list(...)[intersect( names(list(...)), names(formals(segments))) ]
  # Build x,y,r,n, rot into a data.frame
  temp = mapply(draw_line_sym, x, y, r, nlines = nlines, rot = rot, ...)	
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
#'   possible values are \code{triangle}, \code{square}, \code{polygon},
#'   \code{diamond}, \code{star}, \code{plus}, \code{cross}, \code{asterisk},
#'   \code{lines}.
#' @param dimen The dimensions of the symbol. This is given as the maximum
#'   height/length of the total polygon and not the radii from the central point
#' @param rot A numerical value specifying the rotation of the symbol in
#'   degrees. The default \code{rot = 0} means that the symbol will be plotted
#'   from 12 o'clock working around the central point going clockwise.  An
#'   exception to this is when \code{symbol = "square"} in which case the
#'   vertices are rotated by 45 degrees so that the top edge is parallel with
#'   the x axis.
#' @param n_circle A number specifying the number number of vertices in the
#'   polygon that will be used to approximate a polygon. The default
#'   \code{n_circle = 32} will give a good approximation of a circle in most
#'   cases, however if you require smooth approximations then inccrease this
#'   value.
#' @param ... any other arugments will be passed through the
#'   \code{draw_polygons()}, \code{draw_stars()}, \code{draw_line_syms()}
#'   functions used to actually add the polygon to the plot, e.g. \code{col =
#'   "red"}, \code{border = "blue"}, etc.
#'
#' @details This function is used by functions in BRCmap (see See Also seciton
#'   below) to place plotting symbols of a given size at coordinates specified.
#'   At present the available options for symbol types are circle, triangle,
#'   square, diamond, polygon, star, plus, cross, asterisk, lines are possible
#'   at present.
#'
#'   This is a wrapper function for a set of more specific functions that do the
#'   plotting for the different symbol types. Each of these individual functions
#'   have their own specific arguments related to their symbol type that can be
#'   specified and passed through \code{...}. For example the the function that
#'   draws stars \code{draw_stars} has the arugments \code{nrays} that specifies
#'   the number of rays or points that the star will have. \code{draw_polygons}
#'   is used for plotting circles, triangles, squares and diamonds;
#'   \code{draw_stars} for stars; \code{draw_line_syms} for plus, cross,
#'   asterisk and other rotated line symbols.
#'
#'   It should be noted that the 'circles' produced are actually n-vertice
#'   polygons that in most cases should give acceptable approximations for a
#'   true circle. If you require a smoother circle increase the number of
#'   vertices in the polygon used to approximate the circle (via
#'   \code{n_circle}). It should be noted that symbols can appear more jagged in
#'   the default R device that they will when plotted to other devices such as
#'   pdf.
#' @author Colin Harrower
#' @seealso \code{\link{plotUK_gr_symbols}}, \code{\link{draw_polygons}},
#'   \code{\link{draw_stars}}, \code{\link{draw_line_syms}}
#' @export
#'
#' @examples
#'   ## Example of adding symbols to a plot
#'   # First create a blank plot
#'     plot(1:60,1:60, type="n",asp=1)
#'   # Now series of plot types
#'     xy_vals = seq(10,50,by=10)
#'     draw_symbols(xy_vals, xy_vals[1], "circle", dimen = 3, col="red")
#'     draw_symbols(xy_vals, xy_vals[2], "triangle", dimen = 3, col="blue")
#'     draw_symbols(xy_vals, xy_vals[3], "square", dimen = 3, col="orange")
#'     draw_symbols(xy_vals, xy_vals[4],
#'       "star", nrays = c(3,4,5,6,7), dimen = 3, col="purple"
#'     )
#'     draw_symbols(xy_vals, xy_vals[5], "lines", dimen = 3, col="red")
#'   #
#'     
draw_symbols = function(x, y, symbol = "circle", dimen, rot=0, n_circle = 32, ...){
    # Ensure values passed to symbol are lower case
      symbols = tolower(symbol)
    
    switch(symbol, 
      circle = draw_polygons(x,y,r=dimen/2,n=n_circle,rot=rot, ...), 
      triangle = draw_polygons(x,y,r=dimen/2,n=3,rot=rot, ...),
      square = draw_polygons(x,y,r = 0.5*sqrt(dimen^2+dimen^2),n=4,rot=rot+45, ...),
      diamond = draw_polygons(x,y, r= 0.5*sqrt(dimen^2+dimen^2),n=4,rot=rot, ...),
		  polygon = draw_polygons(x,y, r= 0.5*sqrt(dimen^2+dimen^2),rot=rot, ...),
		  star = draw_stars(x,y,r=dimen/2,rot = rot, ...),
		  plus = draw_line_syms(x,y,r = dimen/2,nlines = 2, rot = rot,...),
		  cross = draw_line_syms(x,y,r = dimen/2,nlines = 2, rot = rot+45,...),
		  asterisk = draw_line_syms(x,y,r = dimen/2,nlines = 3, rot = rot,...),
		  lines = draw_line_syms(x,y,r = dimen/2,rot = rot,...),
      stop("Invalid option for argument 'symbol'")
    )
}

#' @title Add symbols to existing map
#' @description Function to plot valud UK grid references as symbols onto a
#'   existing map
#'
#' @param gridref A character vector containg the grid refrences to be added to
#'   the map.
#' @param symbol A text string specifying the type of symbol to be added. The
#'   default \code{symbol="circle"} will add a circle (see details). Other
#'   possible values are; \code{triangle}, \code{square}, \code{diamond}.
#' @param dimen A vector of numerical values specifying the dimensions for the
#'   symbols to be plotted. The default \code{dimen = NULL} means that the
#'   dimensions of each symbols will be the dimensions of the corresponding grid
#'   references, e.g. a 10km grid square will be represented by a symbols with
#'   dimensions of 10km
#' @param dimen_type A text string specifying how the dimensions are being
#'   specified. The default \code{dimen_type = "prop"} indicates that the
#'   dimensions are being specified as a proportion of the grid reference
#'   dimension, e.g. 0.9 means that the dimensions of the symbols will be 90"\%" of
#'   dimensions of the relevant grid square. The alternative \code{dimen_type =
#'   "abs"} indicates that the dimensions are being given in plotting units
#' @param rot A numerical value specifying the rotation of the symbol in
#'   degrees. The default \code{rot = 0} means that the symbol will be plotted
#'   from 12 o'clock working around the central point going clockwise. An
#'   exception to this is when \code{symbol = "square"} in which case the
#'   vertices are rotated by 45 degrees so that the top edge is parallel with
#'   the x axis.
#' @param centre A logical variable determing whether the symbol will be
#'   centered on the centre point of the grid reference or at the lower left
#'   corner. The default \code{centre = TRUE} will plot the symbol so that it is
#'   centered on the central point of the square.
#' @param gr_prec A numerical vector specifying the precision of each grid
#'   reference. The default \code{gr_prec = NULL} will determine the precision
#'   directly from the grid reference. This variable will only be needed if the
#'   default precision needs to be overridden.
#' @param ci_insert A logical variable determining whether Channel Islands grid
#'   references are to be plotted in an insert box to the left of the UK. The
#'   default \code{ci_insert = FALSE} will plot the Channel Islands grid
#'   references in their true position to the south of the UK.
#' @param add_irish_rot A numberical value specifying the additional rotation to
#'   be applied to symbols used to plot Irish grid references. This rotation is
#'   applied in addition to the rotation given to all grid references specified
#'   by the argument \code{rot}. The default \code{add_irish_rot = 5} will
#'   rotate Irish grid references and extra 5 degrees clockwise to account for
#'   the difference in alignment between the GB and Irish grids when plotted
#'   together, meaning that symbols in Ireland should appear correctly aligned
#'   relative to the Irish grid even thoughy they are actually being plotted on
#'   the GB grid. To avoid this additional rotation for Irish grid reference use
#'   a value of 0.
#' @param ci_origin A vector of two values giving the x & y cooridinates for the
#'   origin of the Channel Islands grid relative to the the UK when using the
#'   insert box.
#' @param ... Any other arguments that will be passed to the functions used to
#'   add the symbols to the map. Examples of typical arguments that you may want
#'   to pass through are \code{col}, \code{border}.
#'
#' @return By default this function does not return anything, but if the output
#'   from the function is assigned it will return the x,y values (eastings &
#'   northings on OSGB) of the grid references via the invisible function.
#' @author Colin Harrower
#' @seealso \code{\link{plotUK_gr}}, \code{\link{plotUK_gr_cats}},
#'   \code{\link{plotUK_gr_points}}
#' @export
#'
#' @examples
#'   ## Example of adding grid references to a map as symbols
#'   # Create blank map of UK
#'     plot_GIS(UK)
#'   # Add some points as circles
#'     plotUK_gr_symbols(c("SU68","SP50","SU77"), col="red", border="darkred")
#'   # Add some other points this time as triangles
#'     plotUK_gr_symbols(
#'       c("NT27","NS56","NJ90"), symbol = "triangle", 
#'       col="blue", border="darkblue"
#'     )
#'   # Add some more as diamonds
#'     plotUK_gr_symbols(
#'       c("O13","J37"), symbol = "diamond", 
#'       col="green", border="darkgreen"
#'     )
#'   # Add some plus and cross symbols
#'     plotUK_gr_symbols(
#'       c("NY32","NY58","NY98"), symbol = "plus", 
#'       col="black", lwd = 2
#'     )
#'     plotUK_gr_symbols(
#'       c("SE19","SE94","SE45"), symbol = "cross", 
#'       col="black", lwd = 2
#'     )  
#'     
plotUK_gr_symbols = function(
    gridref, 
    symbol = "circle", 
    dimen = NULL, 
    dimen_type = "prop", 
    rot = 0,  
    centre = TRUE, 
    gr_prec = NULL, 
    add_irish_rot = 5, 
    ci_insert = FALSE, 
    ci_origin = c(-180000,30000), 
    ...
){
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
        # If Irish grid references found add add_irish_rot != 0 then will need to covert rot to a vector of the same length as the grid refs and then add additional irish rotation to the existing rotation values
        if(add_irish_rot != 0){
          rot = rep(rot,nrow(gr_points))
          rot[ir_inds] = rot[ir_inds] + add_irish_rot
        }
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
      draw_symbols(gr_points$EASTING, gr_points$NORTHING, symbol = symbol, dimen=dimen,rot=rot, ...)
      
      
      # Return gr_points invisibly
      invisible(gr_points)
    }
  }