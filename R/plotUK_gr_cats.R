plotUK_gr_cats <-
function(gridref, att, breaks = NULL, legend_pos = "topleft", leg_cex = 0.7, att_col = NULL, ...){
	# Check gridref is same length as att
	if(length(gridref) != length(att)){
		stop("Length of Grid Refs does not match length of Attribute")
	}
	
	if(is.null(breaks)){
		# If breaks not specified then assume att column is factor or should be treated as factor
		if(class(att) == "factor"){
			# If factor then get levels (so that level order is used for legend & colour scheme)
			att_cats = levels(att)
		} else {
			# If not factor then get unique values (order for legend & colour scheme will be allocated in order of occurrence in vector)
			att_cats = unique(att)
			if(length(att_cats) > 10){
				# Print a warning to screen when 
				warning("WARNIGN: No breaks specified - treating att column as factor/categorical")
			}
		}
		
		# If att_col is null then set to use heat.colours otherwise use values supplied
		if(is.null(att_col)){
			# Get colours from heat.colours
				att_cols = rev(heat.colors(length(att_cats)))
		} else {
			# Check that att_cols is same length as breaks -1 if not stop
			if(length(att_col) != length(att_cats)){
				stop("length of att_cols needs to be same as number of categories in atts")
			} else {
				att_cols = att_col
			}
		}
		
		# Loop through categories and plot
		for(i in 1:(length(att_cats))){
			row_inds = which(att == att_cats[i])
			if(length(row_inds) > 0){
				plotUK_gr(gridref[row_inds], col = att_cols[i], ...)
			}
		}
		
		# Add legend
		if(is.null(legend_pos) == FALSE){
			legend(x = legend_pos, legend = att_cats, fill=att_cols, cex = leg_cex, bg = "white", inset = 0.015)
		}
		
	} else {
		# If att_col is null then set to use heat.colours otherwise use values supplied
		if(is.null(att_col)){
			# Get colours from heat.colours
				att_cols = rev(heat.colors(length(breaks)-1))
		} else {
			# Check that att_cols is same length as breaks -1 if not stop
			if(length(att_col) != length(breaks)-1){
				stop("length of att_cols needs to be 1 less than length of breaks")
			} else {
				att_cols = att_col
			}
		}
		
		# Loop through breaks (exluding last value) and find gridrefs where att >= breaks[i] but < breaks[i+1]
		for(i in 1:(length(breaks)-1)){
			if(i == length(breaks)-1){
				row_inds = which(att >= breaks[i] & att <= breaks[i+1])
			} else {
				row_inds = which(att >= breaks[i] & att < breaks[i+1])
			}
			if(length(row_inds) > 0){
				plotUK_gr(gridref[row_inds], col = att_cols[i], ...)
			}
		}
		# Add legend
		if(is.null(legend_pos) == FALSE){
			legend(x = legend_pos, legend = paste(breaks[1:(length(breaks)-1)],"-", breaks[2:length(breaks)]), fill=att_cols, cex = leg_cex, bg = "white", inset = 0.015)
		}
	}
		#att_cols = rev(rainbow(length(breaks)))
	
}
