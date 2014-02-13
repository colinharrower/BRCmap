gr_det_country = function(gridref){
	# Create variable to store output
	cty_out = rep(NA, length(gridref))
	
  
	"(^[[:upper:]]{1,2}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1,2}[[:digit:]]{2,}$)"
  
	# Find British Gridrefs
	cty_out[grepl('(^[[:upper:]]{2}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{2}[[:digit:]]{2,}$)',gridref) & !grepl('^(WA)|(WV)',gridref)] = "OSGB"
	
	# Find Irish Gridrefs
	cty_out[grepl('(^[[:upper:]]{1}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1}[[:digit:]]{2,}$)',gridref)] = "OSNI"
	
	# Find Channel Islands Gridrefs
	cty_out[grepl('(^(WA)|(WV)[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^(WA)|(WV)[[:digit:]]{2,}$)',gridref)] = "UTM30"
	
	# Return output object
	return(cty_out)
}