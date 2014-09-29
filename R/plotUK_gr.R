plotUK_gr <-
function(gridref, gr_prec = NULL, ci_insert = FALSE, ci_origin = c(-180000,30000), unit_conv = NULL, ...){
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
		
	# Setup dataframe to hold data
		gr_poly = data.frame(EASTING = rep(NA,gr_len*5), NORTHING = rep(NA,gr_len*5))
	
	# Determine origin of square and asign one value to every 5th row
		gr_poly[seq(1,by=5, length.out=gr_len),] = gr_let2num(gridref)
		
	# Determine other three corners and assign to every fith row in data.frame (i.e. 2nd, 7th for TL corner, 3rd, 8th for TR, etc)
		# Top Left
		gr_poly[seq(2,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(0,gr_len),gr_prec)
		# Top Right
		gr_poly[seq(3,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(gr_prec,gr_prec)
		# Bottom Right
		gr_poly[seq(4,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(gr_prec, rep(0,gr_len))
		
	# Find Irish gridrefs
		ir_inds = which(grepl("(^[[:upper:]]{1}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1}[[:digit:]]{2,}$)", rep(gridref,each=5)) & !is.na(gr_poly$EASTING))
		if(length(ir_inds) > 0){
			gr_poly[ir_inds,] = OSgridReprojection(gr_poly$EASTING[ir_inds], gr_poly$NORTHING[ir_inds], org_grid = "OSNI", out_grid = "OSGB")
		}
		
	# If ci_insert = TRUE then find channel islands gridrefs and convert to insert position
		# Find channel islands grid refs
			ci_inds = which(grepl("(^(WA|WV)[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^(WA|WV)[[:digit:]]{2,}$)", rep(gridref,each=5)) & !is.na(gr_poly$EASTING))
		if(length(ci_inds) > 0){	
			if(ci_insert){	
				# Convert eastings and northings for these gridrefs to the insert positions
					gr_poly[ci_inds,] = CI_insert_en(gr_poly$EASTING[ci_inds], gr_poly$NORTHING[ci_inds])
			} else {
				# If no ci_insert the plot in normal position relative to UK
					gr_poly[ci_inds,] = OSgridReprojection(gr_poly$EASTING[ci_inds], gr_poly$NORTHING[ci_inds], org_grid = "UTM30", out_grid = "OSGB")
			}
		}
	# Apply unit conversion if necessary
		if(!is.null(unit_conv)){
			gr_poly = gr_poly * unit_conv
		}
		
	# Plot polygons
		polygon(gr_poly$EASTING, gr_poly$NORTHING, ...)
	
	# Return gr_poly invisibly
		invisible(data.frame(GRIDREF = rep(gridref, each = 5), gr_poly))
}
