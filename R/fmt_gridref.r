fmt_gridref = function(gridref, gr_fmt = NULL){
	# Setup object to hold output grid refs
		gr_out = rep(NA, length(gridref))
	# convert gridref string to upper case
		gridref = toupper(gridref)
	# Replace any spaces, punctuation or control characters
		gridref = gsub("[[:space:][:cntrl:][:punct:]]", "", gridref)
	# Check that gridref conforms to grid reference pattern after removals
		# Get indices of gridrefs which are in a valid format
			gr_inds = which(grepl("^([[:digit:]]{1,2}[[:upper:]]{1})?[[:upper:]]{1,2}[[:digit:]]{2,}([[:upper:]]?|[[:upper:]]{2})?$", gridref))
		# Copy valid gridrefs to output object
			gr_out[gr_inds] = gridref[gr_inds]
	# Extract components where gr_fmt is not NULL (1 = Whole gridref minus tet/quad codes inc utm zone&band if present,2 = UTM zone & band, 3 = whole gridref minus tet/quad codes & utm zone & band, 4 = Inital letter(s), 5 = Digits only, 6 = Tetrad/Quad only, 7 = Tetrad only, 8 = Quadrant only)
	if(!is.null(gr_fmt)){
		gr_out = gsub("^(([[:digit:]]{1,2}[[:upper:]]{1})?(([[:upper:]]{1,2})([[:digit:]]{2,})))(([[:upper:]]?)|([[:upper:]]{2}))?$", paste("\\",gr_fmt, sep=""), gr_out)
	}
	# Return formatted gridref
		return(gr_out)
}