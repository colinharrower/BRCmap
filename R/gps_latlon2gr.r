gps_latlon2gr = function(latitude, longitude, out_projection = "OSGB", return_type = "gr"){
	# Determine number of coordinates
		n_coords = length(latitude)
		# Check lengths are the same if not stop
		if(n_coords != length(longitude)){
			stop("ERROR - Lengths of latitude and longitude do not match")
		}
		

	# Convert to Cartesian
		org_cart = LatLong_Cartesian(latitude, longitude, "UTM30")
		
	# At some point should set up some ranges to split UK, Irish & Channel Islands grid refs (and do follow stages in loop for different projections)
		# If doing multiple projections will need to setup output variable before hand!
	
  # Note helmert transformations not needed when out_projection = UTM30
  if(out_projection != "UTM30"){
  	# Perform projection/datum transformation (helmert)
  		helm_tran = helmert_trans(x =org_cart$x, y = org_cart$y, z = org_cart$z, trans = paste("WGS84to", out_projection, sep=""))
  	# Convert to UK lat lon
  		out_latlon = Cartesian_LatLong(helm_tran$x, helm_tran$y, helm_tran$z, out_projection)
  } else {
    out_latlon = data.frame(LATITUDE = latitude, LONGITUDE = longitude)
  }
  
	# Convert to UK easting and northing
		out_en = LatLongtoOSGrids(out_latlon$LATITUDE, out_latlon$LONGITUDE, out_projection)
	
	# Convert to UK gridref
		out_gr = gr_num2let(out_en$EASTING, out_en$NORTHING, OSgrid = out_projection)
	
	# Determine output
	if(return_type == "both"){
		out_obj = data.frame(GRIDREF = out_gr, EASTING = out_en$EASTING, NORTHING = out_en$NORTHING, stringsAsFactors = FALSE)
	} else if(return_type == "en"){
		out_obj = data.frame(EASTING = out_en$EASTING, NORTHING = out_en$NORTHING, stringsAsFactors = FALSE)
	} else if(return_type == "gr"){
		out_obj = out_gr # Return a vector rather than a 1 column data.frame
	} else {
		stop("ERROR - Unknown return type (valid entries are \"both\", \"en\", \"gr\")")
	}
	
	# Return output object
	return(out_obj)
}