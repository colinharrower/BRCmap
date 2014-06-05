OSgrid2GB_EN <-
function(gridref, centre = TRUE, gr_prec = NULL){
	# Check that values have been passed
	if(length(gridref) > 0){
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
					
		# Find Irish gridrefs & reproject
			ir_inds = which(grepl("(^[[:upper:]]{1}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1}[[:digit:]]{2,}$)", gridref) & !is.na(gr_points$EASTING))
			if(length(ir_inds) > 0){
				gr_points[ir_inds,] = OSgridReprojection(gr_points$EASTING[ir_inds], gr_points$NORTHING[ir_inds], org_grid = "OSNI", out_grid = "OSGB")
			}
			
		# Find channel islands gridrefs and convert & reproject
			# Find channel islands grid refs
				ci_inds = which(grepl("(^(WA|WV)[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^(WA|WV)[[:digit:]]{2,}$)", gridref) & !is.na(gr_points$EASTING))
			if(length(ci_inds) > 0){	
					gr_points[ci_inds,] = OSgridReprojection(gr_points$EASTING[ci_inds], gr_points$NORTHING[ci_inds], org_grid = "UTM30", out_grid = "OSGB")
			}
			
		# Return gr points
			return(gr_points)
	}
}
