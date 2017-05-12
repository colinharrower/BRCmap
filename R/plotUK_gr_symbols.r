draw_polygon = function(x,y, r, n, rot = 0, ...){
  # Determine angles required to make polygon with n number of points (note default for rotate is from x axis proceeding anticlockwise, hence 90 and -rot in formula)
  a = (-rot + 90 + (0:(n-1) * (360/n)) ) %% 360
  # Determine x coordinates for points between which polygon will be draw
  x = round(x + r * cos(a*(pi/180)),4)
  # Determine y coordinates for points between which polygon will be draw
  y = round(y + r * sin(a*(pi/180)),4)
  # Draw polygon
  polygon(x,y,...)
  # return points invisibly
  return(invisible(data.frame(x,y)))
}

draw_polygons = function(x,y,r,n,rot=0, ...){
  # Determine addition arguments that match polygons
  matched_args = list(...)[intersect( names(list(...)), names(formals(polygon))) ]
  # Build x,y,r,n, rot into a data.frame
  temp = mapply(draw_polygon, x, y, r, n, rot, ...)	
}

draw_triangle = function(x,y,dimen,rot=0, ...){
  # First determine coordinates of square as that give lower 2 points (assuming 1st point is at 0 degrees)
  n = 4
  sq_rot = rot+45
  r = 0.5*sqrt(dimen^2+dimen^2)
  # Determine angles required to make polygon with n number of points (note default for rotate is from x axis proceeding anticlockwise, hence 90 and -rot in formula)
  a = (-sq_rot + 90 + (0:(n-1) * (360/n)) ) %% 360
  # Determine x coordinates for points between which polygon will be draw
  x = round(x + r * cos(a*(pi/180)),4)
  # Determine y coordinates for points between which polygon will be draw
  y = round(y + r * sin(a*(pi/180)),4)
  # Now average first 2 x values and same for y values while keeping the other 2 unchanged
  x = c(mean(x[1:2]),x[3:4])
  y = c(mean(y[1:2]),y[3:4])
  polygon(x,y,...)
  # return points invisibly
  return(invisible(data.frame(x,y)))
}

draw_triangles = function(x,y,dimen,rot=0,...){
  # Determine addition arguments that match polygons
  matched_args = list(...)[intersect( names(list(...)), names(formals(polygon))) ]
  # Build x,y,r,n, rot into a data.frame
  temp = mapply(draw_triangle, x, y, dimen, rot, ...)
}


# Wrapper function to drawn common sympbols of a specified dimension (many are plotted as n pointed polygons
draw_symbol = function(x, y, symbol = "circle", dimen, n = NULL, ...){
    symbols = tolower(symbol)
    switch(symbol, 
          circle = draw_polygons(x,y,r=dimen/2,n=32, ...), 
          triangle = draw_triangles(x,y,dimen=dimen, ...),
          square = draw_polygons(x,y,r = 0.5*sqrt(dimen^2+dimen^2),n=4,rot=45, ...),
          diamond = draw_polygons(x,y, r= 0.5*sqrt(dimen^2+dimen^2),n=4,rot=0, ...)
    )
}

plotUK_gr_symbols <-
  function(gridref, symbol = "circle", dimen = NULL, dimen_type = "prop", centre = TRUE, gr_prec = NULL, ci_insert = FALSE, ci_origin = c(-180000,30000), unit_conv = NULL, ...){
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
      # Apply unit conversion if necessary
      if(!is.null(unit_conv)){
        gr_points = gr_points * unit_conv
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
      draw_symbol(gr_points$EASTING, gr_points$NORTHING, symbol = symbol, dimen=dimen, ...)
      
      
      # Return gr_points invisibly
      invisible(gr_points)
    }
  }