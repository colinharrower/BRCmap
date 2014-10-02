OSgridReprojection <-
function(Easting, Northing, org_grid = "OSNI", out_grid = "OSGB", full_output = FALSE){
	# Determine length of easting and check same as northing
		len_east = length(Easting)
		if(length(Northing) != len_east){
			stop("ERROR: 'Easting' and 'Northing' differ in length")
		}
		
	# Setup object to hold output variable
		ret_obj = data.frame(ORG_EASTING = Easting, ORG_NORTHING = Northing, ORG_GRID = org_grid, NEW_GRID = out_grid, EASTING = rep(NA, len_east), NORTHING = rep(NA, len_east), stringsAsFactors = FALSE)
		
		
	# Where org_grid == out_grid then copy across
		inds = which(ret_obj$ORG_GRID == ret_obj$NEW_GRID)
		if(length(inds) > 0){
			ret_obj[inds,c("EASTING","NORTHING")] = ret_obj[inds,c("ORG_EASTING","ORG_NORTHING")]
		}
		
	# Determine which grids will need reprojected
		inds = which(ret_obj$ORG_GRID != ret_obj$NEW_GRID)
		if(length(inds) > 0){
			# Determine Lat/Lon (Original projection)
				org_latlon = OSGridstoLatLong(ret_obj$ORG_EASTING[inds], ret_obj$ORG_NORTHING[inds], ret_obj$ORG_GRID[inds])
			# Determine Cartesian (Original projection)
				org_cart = LatLong_Cartesian(org_latlon$LATITUDE, org_latlon$LONGITUDE, gsub("WGS84","UTM30", ret_obj$ORG_GRID[inds]))
				# Add old and new grids to org_cart
				org_cart[,c("ORG_GRID","NEW_GRID")] = ret_obj[inds,c("ORG_GRID","NEW_GRID")]
			# First need to convert all to WGS84 as only have params for required conversion via WGS84 so which which org_grid not WGS84/UTM30
				new_inds = which(!org_cart$ORG_GRID %in% c("WGS84","UTM30"))
				
				if(length(new_inds) > 0){
					# For these gridrefs replace org_cart with tranformed values
						org_cart[new_inds,c("x","y","z")] = helmert_trans(x =org_cart$x[new_inds], y = org_cart$y[new_inds], z = org_cart$z[new_inds], trans = paste(org_cart$ORG_GRID[new_inds],"toWGS84", sep=""))
				}
			# If out_grid not WGS84/UTM30 then need to convert all Cartesian coords to output grid
				new_inds = which(!org_cart$NEW_GRID %in% c("WGS84","UTM30"))
				
				if(length(new_inds)  > 0){
					org_cart[new_inds,c("x","y","z")] = helmert_trans(x =org_cart$x[new_inds], y = org_cart$y[new_inds], z = org_cart$z[new_inds], trans = paste("WGS84to",org_cart$NEW_GRID[new_inds], sep=""))
				}
			# Convert Cartesian coordiantes to Latitude/Longitude (New projection)
				out_latlon = Cartesian_LatLong(org_cart$x, org_cart$y, org_cart$z, out_grid)
			# Now covert lat lons to Easting & Northing (new projection)
				out_en = LatLongtoOSGrids(out_latlon$LATITUDE, out_latlon$LONGITUDE, out_grid)
			# Finally write values to output data.frame
				ret_obj[inds,c("EASTING","NORTHING")] = out_en[,c("EASTING","NORTHING")]
		}
	
	# If full_output = FALSE then remove extra columns
	if(!full_output){
		ret_obj = ret_obj[,c("EASTING","NORTHING")]
	}
		
	return(ret_obj)
}
