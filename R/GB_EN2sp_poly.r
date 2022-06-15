# Fnction to convert OSGB easting and northing values to WGS84 latitude and longitude values (allows conversion of easting & northing for areas off the typical OSGB grid coverage and where meaningful grid references can't really be allocated).
GB_EN2gps_latlon = function(easting,northing){
	# NOTE FOR UTM30 gridrefs then no need to use helmert transformation as lat long are already in correct projection
	# Check easting & northing are same length
	stopifnot(length(easting) == length(northing))
	# Setup up variable to hold final output
		out_latlon = data.frame(LATITUDE = rep(NA, length(easting)), LONGITUDE = rep(NA, length(easting)))
	
	# Determine lat lon (original projection)
		out_latlon = OSGridstoLatLong(easting, northing, "OSGB")
	
  	# Determine Cartesian (Original projection)
  		org_cart = LatLong_Cartesian(out_latlon$LATITUDE, out_latlon$LONGITUDE, "OSGB")
  	
  	# Apply Helmert transformation to convert original projections to WGS84 (Cartesian in WGS84)
  		helm_tran = helmert_trans(x =org_cart$x, y = org_cart$y, z = org_cart$z, trans = "OSGBtoWGS84")
  		
  	# Convert Cartesian coordinates to Latitude/Longitude (Lat Lon in WGS84)
  		out_latlon = Cartesian_LatLong(helm_tran$x, helm_tran$y, helm_tran$z, "UTM30")
		
	# Return output
		return(out_latlon)
}

# A temporary function that uses the herlmet transformations and parameters in BRCmap to convert/reproject OSGB easting & northing data to WGS84 lat lon values in order to WGS84 spatial polygons object till I can figure out a way to get round the issues/inaccuracy intorduced into spTransform since the changes to the way it handles older CRS stings
GB_EN2wgs84_sp_poly = function(easting, northing, precision, gr_atts = NULL){
	# Requires rgdal package in addition to standard BRCmaps packages
	
	# Check eastings & northing are the same length
	stopifnot(length(easting)==length(northing))
	
	# Check if precision is same length as easting/northing or is 1
	if(length(precision) != length(easting)){
		if(length(precision) == 1){
			precision = rep(precision,length(easting))
		} else {
			stop("precision needs to be either the same length as easting & northing vectors or a single value that is assumed to apply to all easting/northings")
		}
	}
	
	# If gr_atts is supplied then check that the length/nrows matches number of squares
		if(!is.null(gr_atts)){
			# if gr_atts is a vector then convert to a 1 column data.frame
			if(is.vector(gr_atts)){
				gr_atts = data.frame(gr_atts)
			}
			# Check the lengths against easting (already checked that easting == northing)
			if(length(easting) != nrow(gr_atts)){
				stop("Supplied attributes info (gr_atts) does not match the length number of easting & northing vectors")
			}
		}
	
	
	# Create a object to store the output values
		
		# Determine length of en_pairs
			gr_len = length(easting)
				
		# Setup dataframe to hold data
			gr_poly = data.frame(EASTING = rep(NA,gr_len*5), NORTHING = rep(NA,gr_len*5))
		
		# Determine origin of square and asign one value to every 5th row
			gr_poly[seq(1,by=5, length.out=gr_len),] = cbind(easting,northing)
			
		# Determine other three corners and assign to every fith row in data.frame (i.e. 2nd, 7th for TL corner, 3rd, 8th for TR, etc)
			# Top Left
			gr_poly[seq(2,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(0,gr_len),precision)
			# Top Right
			gr_poly[seq(3,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(precision,precision)
			# Bottom Right
			gr_poly[seq(4,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(precision, rep(0,gr_len))
			# Finally back to bottom left
			gr_poly[seq(5,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),]
	
		
		# Convert these polygon coordinates to WGS84 lat lon values
			temp = GB_EN2gps_latlon(gr_poly$EASTING,gr_poly$NORTHING)
			gr_poly = temp[,c("LONGITUDE","LATITUDE")]
					
		# Determine row inds for each polygon
			poly_no = rep(1:gr_len, each = 5)
		# Create a list of Polygon objects (one for each grid reference)
			sp_pol = by(gr_poly,poly_no, FUN = sp::Polygon)
			# For some reason this is now adding a names attribute that is being carried through and has issues when the final spatial object is plotted by leaflet
			# Set this attribute to NULL effictively removing it while retaining any other attributes
				attributes(sp_pol)$names = NULL
			rm(gr_poly)
		# Convert list of Polygon objects to a a Polygons object where each polygon has its own ID
			# Need to make sure gr_ids are unique across all grid refs types so they can be combined if necessary
			gr_ids = 1:gr_len
			sp_pols = mapply(sp::Polygons,lapply(sp_pol, list),gr_ids)
			# rm polygon list
				rm(sp_pol)
		# Convert Polygons object to Spatial Polygons
			sp_spol = sp::SpatialPolygons(sp_pols, proj4string = sp::CRS(SRS_string = "EPSG:4326"))
			# remove polygons object
				rm(sp_pols)
		# Convert SpatialPolygons to SpatialPolygonsDateFrame
			sp_df = data.frame(EASTING = easting, NORTHING = northing, PRECISION = precision)
			if(!is.null(gr_atts)){
				sp_df[,names(gr_atts)] = gr_atts  # TODO: Need to check that length of atts and gridrefs match at start of script
			}
			row.names(sp_df) = gr_ids
			sp_polydf = sp::SpatialPolygonsDataFrame(sp_spol, data = sp_df)
			rm(sp_df, sp_spol)	
	
	# Return output object
	return(sp_polydf)
}

# This function should work but there seems to be accuracy issues when converting between projects that seem to results from spTransform now handling proj4 stings different and resulting in much worse reprojections
GB_EN2sp_poly = function(easting, northing, precision, gr_atts = NULL,out_proj4 = NULL){
	# Requires rgdal package in addition to standard BRCmaps packages
	# Check out_proj4 if supplied
	if(!is.null(out_proj4)){
		test = rgdal::checkCRSArgs_ng(SRS_string = out_proj4)
		# Check to see if CRS check returns true (a valid CRS value) otherwise produce error and stop
		if(!test[[1]]){
			stop("PROJ.4 string passed to out_proj4 is not a recognised CRS")
		}
	} 
	
	# Check eastings & northing are the same length
	stopifnot(length(easting)==length(northing))
	
	# Check if precision is same length as easting/northing or is 1
	if(length(precision) != length(easting)){
		if(length(precision) == 1){
			precision = rep(precision,length(easting))
		} else {
			stop("precision needs to be either the same length as easting & northing vectors or a single value that is assumed to apply to all easting/northings")
		}
	}
	
	# If gr_atts is supplied then check that the length/nrows matches number of squares
		if(!is.null(gr_atts)){
			# if gr_atts is a vector then convert to a 1 column data.frame
			if(is.vector(gr_atts)){
				gr_atts = data.frame(gr_atts)
			}
			# Check the lengths against easting (already checked that easting == northing)
			if(length(easting) != nrow(gr_atts)){
				stop("Supplied attributes info (gr_atts) does not match the length number of easting & northing vectors")
			}
		}
	
	
	# Create a object to store the output values
		
		# Determine length of en_pairs
			gr_len = length(easting)
				
		# Setup dataframe to hold data
			gr_poly = data.frame(EASTING = rep(NA,gr_len*5), NORTHING = rep(NA,gr_len*5))
		
		# Determine origin of square and asign one value to every 5th row
			gr_poly[seq(1,by=5, length.out=gr_len),] = cbind(easting,northing)
			
		# Determine other three corners and assign to every fith row in data.frame (i.e. 2nd, 7th for TL corner, 3rd, 8th for TR, etc)
			# Top Left
			gr_poly[seq(2,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(0,gr_len),precision)
			# Top Right
			gr_poly[seq(3,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(precision,precision)
			# Bottom Right
			gr_poly[seq(4,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(precision, rep(0,gr_len))
			# Finally back to bottom left
			gr_poly[seq(5,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),]
	
		
					
		# Determine row inds for each polygon
			poly_no = rep(1:gr_len, each = 5)
		# Create a list of Polygon objects (one for each grid reference)
			sp_pol = by(gr_poly,poly_no, FUN = sp::Polygon)
			# For some reason this is now adding a names attribute that is being carried through and has issues when the final spatial object is plotted by leaflet
			# Set this attribute to NULL effictively removing it while retaining any other attributes
				attributes(sp_pol)$names = NULL
			rm(gr_poly)
		# Convert list of Polygon objects to a a Polygons object where each polygon has its own ID
			# Need to make sure gr_ids are unique across all grid refs types so they can be combined if necessary
			gr_ids = 1:gr_len
			sp_pols = mapply(sp::Polygons,lapply(sp_pol, list),gr_ids)
			# rm polygon list
				rm(sp_pol)
		# Convert Polygons object to Spatial Polygons
			sp_spol = sp::SpatialPolygons(sp_pols, proj4string = sp::CRS(SRS_string = "EPSG:27700"))
			# remove polygons object
				rm(sp_pols)
		# Convert SpatialPolygons to SpatialPolygonsDateFrame
			sp_df = data.frame(EASTING = easting, NORTHING = northing, PRECISION = precision)
			if(!is.null(gr_atts)){
				sp_df[,names(gr_atts)] = gr_atts  # TODO: Need to check that length of atts and gridrefs match at start of script
			}
			row.names(sp_df) = gr_ids
			sp_polydf = sp::SpatialPolygonsDataFrame(sp_spol, data = sp_df)
			rm(sp_df, sp_spol)	
		# Test whether out_proj4 is not NULL (i.e. it was supplied or either sep_proj_layers = FALSE or there is only 1 grid ref type present)		
		if(!is.null(out_proj4)){
			# Test whether current projection set matches output project if not reproject
			if(all(rgdal::checkCRSArgs_ng(SRS_string = "EPSG:27700")[[1]] & rgdal::checkCRSArgs_ng(SRS_string=out_proj4)[[1]]) & rgdal::checkCRSArgs_ng(SRS_string = "EPSG:27700")[[3]] != rgdal::checkCRSArgs_ng(SRS_string=out_proj4)[[3]]){
				sp_polydf = sp::spTransform(sp_polydf, sp::CRS(SRS_string = out_proj4))
			}
		}
		
	
	# Return output object
	return(sp_polydf)
}