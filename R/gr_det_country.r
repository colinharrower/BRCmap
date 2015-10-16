gr_det_country = function(gridref){
	# Create variable to store output
	cty_out = rep(NA, length(gridref))
  
	# Find British Gridrefs
	cty_out[grepl("^[[:upper:]]{1,2}[[:digit:]]{2,}([[:upper:]]?|[[:upper:]]{2})?$",gridref) & !grepl("^(WA)|(WV)",gridref)] = "OSGB"
	
	# Find Irish Gridrefs
	cty_out[grepl("^[[:upper:]]{1}[[:digit:]]{2,}([[:upper:]]?|[[:upper:]]{2})?$",gridref)] = "OSNI"
	
	# Find Channel Islands Gridrefs
	cty_out[grepl("^(WA)|(WV)[[:digit:]]{2,}([[:upper:]]?|[[:upper:]]{2})?$",gridref)] = "UTM30"
	
	# Find MGRS grid reference types
	cty_out[grepl("^[[:digit:]]{1,2}[[:upper:]]{1}[[:upper:]]{1,2}[[:digit:]]{2,}([[:upper:]]?|[[:upper:]]{2})?$", gridref)] = "MGRS"
	
	# Return output object
	return(cty_out)
}