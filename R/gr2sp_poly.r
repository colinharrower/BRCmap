gr2sp_poly = function(gridref, gr_atts = NULL, gr_prec, sep_proj_layers = FALSE, out_proj4 = NULL){
	# Requires rgdal package in addition to standard BRCmaps packages
	# Check out_proj4 if supplied
	if(!is.null(out_proj4)){
		test = checkCRSArgs(out_proj4)
		# Check to see if CRS check returns true (a valid CRS value) otherwise produce error and stop
		if(!test[[1]]){
			stop("PROJ.4 string passed to out_proj4 is not a recognised CRS")
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
	
	# If gr_prec is null then determine precision from gridref, if not check whether gr_prec is 1 value (if so create vector of length gr_len) or a vector of length gr_len
		if(is.null(gr_prec)){
			gr_prec = det_gr_precision(gridref)
		} else if(length(gr_prec) == 1) {
			gr_prec = rep(gr_prec, gr_len)
		}
		
	# Need to initally create seperate spatial objects for different grid reference system/country
	# OSGB
		
		gr_len = length(inds)
	
	# Setup dataframe to hold data
		gr_poly = data.frame(EASTING = rep(NA,gr_len*5), NORTHING = rep(NA,gr_len*5))
	
	# Determine origin of square and asign one value to every 5th row
		gr_poly[seq(1,by=5, length.out=gr_len),] = gr_let2num(gridref)
		
	# Determine other three corners and assign to every fith row in data.frame (i.e. 2nd, 7th for TL corner, 3rd, 8th for TR, etc)
		# Top Left
		gr_poly[seq(2,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(0,gr_len),gr_prec)
		# Top Right
		gr_poly[seq(3,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(gr_prec,gr_prec)
		# Bottom Right
		gr_poly[seq(4,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(gr_prec, rep(0,gr_len))
		# Finally back to bottom left
		gr_poly[seq(5,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),]
	
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
		}	
		
	# Create a object to store the output values
		out_obj = NULL
	# Loop through grid ref types in dataset and create spatial objects
	for(i in 1:length(gr_tp)){	
		inds = which(gr_proj == gr_tp[i])
		
		# When processing OSGB grid refs
		if(gr_tp[i] == "OSGB"){
			tp_id = "EPSG:27700"
			tp_desc = "Ordnance Survey of Great Britian Grid References"
			tp_proj4 = "+init=epsg:27700"
		}
		
		# OSNI grid refs
		if(gr_tp[i] == "OSNI"){
			tp_id = "EPSG: 29902"
			tp_desc = "Ordnance Survey Ireland Grid References"
			tp_proj4 = "+init=epsg:29902"
		}
		
		# Channel Islands grid reference
		if(gr_tp[i] == "UTM30"){
			tp_id = "EPSG: 32630"
			tp_desc = "Channel Islands Grid References"
			tp_proj4 = "+init=epsg:32630"
		}
					
		# Determine row inds for each polygon
			poly_inds = lapply((0:(gr_len-1))*5+1, seq, by = 1, length.out = 5)
			poly_no = rep(1:gr_len, each = 5)
		# Create a list of Polygon objects (one for each grid reference)
			sp_pol = by(gr_poly,poly_no, FUN = Polygon)
		# Convert list of Polygon objects to a a Polygons object where each polygon has its own ID
			gr_ids = paste0("GR_",1:length(sp_pol))
			sp_pols = mapply(Polygons,sp_pol,gr_ids)
			# rm polygon list
				rm(sp_pol)
		# Convert Polygons object to Spatial Polygons
			sp_spol = SpatialPolygons(sp_pols, proj4string = CRS(tp_proj4))
			# remove polygons object
				rm(sp_pols)
		# Convert SpatialPolygons to SpatialPolygonsDateFrame
			sp_df = data.frame(GRIDREF = gridref)
			if(!is.null(gr_atts)){
				sp_df[,names(atts)] = atts  # TODO: Need to check that length of atts and gridrefs match at start of script
			}
			row.names(sp_df) = gr_ids
			sp_polydf = SpatialPolygonsDataFrame(sp_spol, data = sp_df)
				
		# Test whether current projection set matches output project if not reproject
		if(all(checkCRSArgs(tp_proj4)[[1]] & checkCRSArgs(out_proj4)[[1]]) & checkCRSArgs(tp_proj4)[[2]] != checkCRSArgs(out_proj4)[[2]]){
			sp_polydf = spTransform(sp_polydf, CRS(out_proj4))
		}
		
		# Add current spatial polygons data frame to output object
		if(length(gr_tp) == 1 | i == 1){
			out_obj = sp_polydf
		} else if(length(gr_tp) > 1 and !sep_proj_layers){
			# If there are multiple grid ref types but only one layer is too be returned and i > 1 then need to merge with existing
		}
	}
}