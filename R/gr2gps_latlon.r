gr2gps_latlon = function(gridref, precision = NULL, centre = TRUE){
	# NOTE FOR UTM30 gridrefs then no need to use helmert transformation as lat long are already in correct projection
	
	# Setup up variable to hold final output
		out_latlon = data.frame(LATITUDE = rep(NA, length(gridref)), LONGITUDE = rep(NA, length(gridref)))
			
	# Determine eastings & northings
		org_en = gr_let2num(gridref, centre = centre, gr_prec = precision, return_projection = TRUE)
	
	# Determine lat lon (original projection)
		out_latlon = OSGridstoLatLong(org_en$EASTING, org_en$NORTHING, org_en$PROJECTION)
	
	# Determine indices of gridrefs that are not UTM30 (which will need transformed)
	# NOTE also need to stop transformations being attempted for records with NA in projection hence the NA in the search vector
		i_trans = which(!org_en$PROJECTION %in% c("UTM30","WGS84", NA))
	# If any gridrefs which need reprojection are found the do so
  if(length(i_trans) > 0){
  	# Determine Cartesian (Original projection)
  		org_cart = LatLong_Cartesian(out_latlon$LATITUDE[i_trans], out_latlon$LONGITUDE[i_trans], org_en$PROJECTION[i_trans])
  	
  	# Apply Helmert transformation to convert original projections to WGS84 (Cartesian in WGS84)
  		helm_tran = helmert_trans(x =org_cart$x, y = org_cart$y, z = org_cart$z, trans = paste(org_en$PROJECTION[i_trans],"toWGS84", sep=""))
  		
  	# Convert Cartesian coordinates to Latitude/Longitude (Lat Lon in WGS84)
  		out_latlon[i_trans,] = Cartesian_LatLong(helm_tran$x, helm_tran$y, helm_tran$z, "UTM30")
  }
	# Return output
		return(out_latlon)
}