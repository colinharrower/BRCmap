plotUK_gr_points <-
function(gridref, centre = TRUE, gr_prec = NULL, ci_insert = FALSE, ci_origin = c(-180000,30000), unit_conv = NULL, ...){
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
			
		# Plot polygons
			points(gr_points$EASTING, gr_points$NORTHING, ...)
		
		# Return gr_points invisibly
			invisible(gr_points)
	}
}
