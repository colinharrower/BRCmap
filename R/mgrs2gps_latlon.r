mgrs2gps_latlon = function(mgrs){
	# Convert MGRS to UTM coordinates also returning precision value of mgrs grid reference
		utm = mgrs2utm(mgrs, return_precision = TRUE)
	# Now convert utm string to lat lon
		lat_lon = utm2gps_latlon(utm$UTM, precision = utm$PRECISION)
	# Return lat lon value
		return(lat_lon)
}	