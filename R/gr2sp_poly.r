gr2sp_poly = function(gridref, gr_atts = NULL, sep_proj_layers = FALSE, out_proj4 = NULL){
	# Requires rgdal package in addition to standard BRCmaps packages
	# Check out_proj4 if supplied
	if(!is.null(out_proj4)){
		test = checkCRSArgs(out_proj4)
		# Check to see if CRS check returns true (a valid CRS value) otherwise produce error and stop
		if(!test[[1]]){
			stop("PROJ.4 string passed to out_proj4 is not a recognised CRS")
		}
	} 
	
	# If gr_atts is supplied then check that the length/nrows matches gridref
		if(!is.null(gr_atts)){
			# if gr_atts is a vector then convert to a 1 column data.frame
			if(is.vector(gr_atts)){
				gr_atts = data.frame(gr_atts)
			}
			# Check the lengths
			if(length(gridref) != nrow(gr_atts)){
				stop("Supplied attributes info (gr_atts) does not match the length of the grid ref vector")
			}
		}
	
	# Find any non-valid grid refs and replace with NA
		gridref = fmt_gridref(gridref)
	
	# Determine number of unique gridrefs
		gr_len = length(gridref)
	
	# Determine the system/country from which the grid ref originates
		gr_proj = gr_det_country(gridref)
		if(any(!gr_proj[!is.na(gridref)] %in% c("OSGB","OSNI","UTM30"))){
			stop("gridref vector contains values that are not a recognised grid reference format")
		}
		
	# Process grid refs and create determine corner coordinates in native eastings & northings 
	# Determine number of grid ref types in dataset
		gr_tp = unique(gr_proj)	
		
		# Determine which gr_tp has the most records
		if(is.null(out_proj4)){
			cnt_tp = tapply(gr_proj, gr_proj, length)
			max_tp = names(cnt_tp)[which.max(cnt_tp)]
			if(max_tp == "OSGB"){
				out_proj4 = "+init=epsg:27700"
			} else if(max_tp == "OSNI"){
				out_proj4 = "+init=epsg:29902"
			} else if(max_tp == "UTM30"){
				out_proj4 = "+init=epsg:32630"
			} else {
				# If grid ref type with most records is unrecognised (or NA) then set to OSGB
				out_proj4 = "+init=epsg:27700"
			}
			rm(cnt_tp, max_tp)
		}	
		
	# Create a object to store the output values
	
	# Loop through grid ref types in dataset and create spatial objects
	for(i in 1:length(gr_tp)){	
		inds = which(gr_proj == gr_tp[i])
		# Determine length of gridref vector
			gr_len = length(inds)
			
		# Determine grid ref precision
			gr_prec = det_gr_precision(gridref[inds])
		
		# Setup dataframe to hold data
			gr_poly = data.frame(EASTING = rep(NA,gr_len*5), NORTHING = rep(NA,gr_len*5))
		
		# Determine origin of square and asign one value to every 5th row
			gr_poly[seq(1,by=5, length.out=gr_len),] = gr_let2num(gridref[inds])
			
		# Determine other three corners and assign to every fith row in data.frame (i.e. 2nd, 7th for TL corner, 3rd, 8th for TR, etc)
			# Top Left
			gr_poly[seq(2,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(0,gr_len),gr_prec)
			# Top Right
			gr_poly[seq(3,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(gr_prec,gr_prec)
			# Bottom Right
			gr_poly[seq(4,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(gr_prec, rep(0,gr_len))
			# Finally back to bottom left
			gr_poly[seq(5,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),]
	
		
		# When processing OSGB grid refs
		if(gr_tp[i] == "OSGB"){
			tp_proj4 = "+init=epsg:27700"
		} else if(gr_tp[i] == "OSNI"){
			tp_proj4 = "+init=epsg:29902"
		} else if(gr_tp[i] == "UTM30"){
			tp_proj4 = "+init=epsg:32630"
		} else {
			stop("gridref vector contains values that are not a recognised grid reference format (see help for details)")
		}
					
		# Determine row inds for each polygon
			poly_no = rep(1:gr_len, each = 5)
		# Create a list of Polygon objects (one for each grid reference)
			sp_pol = by(gr_poly,poly_no, FUN = Polygon)
			rm(gr_poly)
		# Convert list of Polygon objects to a a Polygons object where each polygon has its own ID
			# Need to make sure gr_ids are unique across all grid refs types so they can be combined if necessary
			gr_ids = inds
			sp_pols = mapply(Polygons,lapply(sp_pol, list),gr_ids)
			# rm polygon list
				rm(sp_pol)
		# Convert Polygons object to Spatial Polygons
			sp_spol = SpatialPolygons(sp_pols, proj4string = CRS(tp_proj4))
			# remove polygons object
				rm(sp_pols)
		# Convert SpatialPolygons to SpatialPolygonsDateFrame
			sp_df = data.frame(GRIDREF = gridref[inds])
			if(!is.null(gr_atts)){
				sp_df[,names(gr_atts)] = gr_atts[inds,]  # TODO: Need to check that length of atts and gridrefs match at start of script
			}
			row.names(sp_df) = gr_ids
			sp_polydf = SpatialPolygonsDataFrame(sp_spol, data = sp_df)
			rm(sp_df, sp_spol)	
		# Test whether current projection set matches output project if not reproject
		if(all(checkCRSArgs(tp_proj4)[[1]] & checkCRSArgs(out_proj4)[[1]]) & checkCRSArgs(tp_proj4)[[2]] != checkCRSArgs(out_proj4)[[2]]){
			sp_polydf = spTransform(sp_polydf, CRS(out_proj4))
		}
		
		# Add current spatial polygons data frame to output object
		if(length(gr_tp) == 1 | (length(gr_tp) > 1 & i == 1 & !sep_proj_layers))	{
			out_obj = sp_polydf
		} else if(length(gr_tp) > 1 & !sep_proj_layers){
			# If there are multiple grid ref types but only one layer is too be returned and i > 1 then need to merge with existing
			out_obj = spRbind(out_obj,sp_polydf)
		} else {
			if(i == 1){
				out_obj = list(sp_polydf)
			} else {
				out_obj[[i]] = sp_polydf
			}
		}
	}
	
	# Return output object
	return(out_obj)
}