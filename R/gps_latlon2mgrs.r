gps_latlon2mgrs = function(lat, lon, output_type = "mgrs"){
	# Convert to utm coordinates
		utm_str = gps_latlon2utm(lat, lon)
	# Convert utm coordinates to MGRS grid reference
		mgrs_str = utm2mgrs(utm_str)
	# Determine output type
	if(tolower(output_type) == "mgrs"){
		ret_obj = mgrs_str
	} else if(tolower(output_type) == "both"){
		ret_obj = data.frame(UTM = utm_str, MGRS = mgrs_str)
	} else {
		stop("Unknown output type: valid options are 'mgrs' or 'both'")
	}
	# Return mgrs
		return(ret_obj)
}