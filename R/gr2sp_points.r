gr2sp_points = function(gridref, gr_atts = NULL, sep_proj_layers = FALSE, out_proj4 = NULL){
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
		gr_tp = unique(na.omit(gr_proj))
		
		# Determine which gr_tp has the most records
		if(is.null(out_proj4) & length(gr_tp) > 1 & !sep_proj_layers){
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
		
				
		# Determine easting & northing of square centre
			gr_points = gr_let2num(gridref[inds], centre = TRUE)
				
		
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
					
		# Create a SpatialPoints object from the gr points data.frame
			#sp_points = SpatialPoints(gr_points, proj4string = CRS(tp_proj4))
			#rm(gr_points)
		
		# Convert SpatialPoints to SpatialPointsDateFrame
			sp_df = data.frame(GRIDREF = gridref[inds])
			if(!is.null(gr_atts)){
				sp_df[,names(gr_atts)] = gr_atts[inds,]  # TODO: Need to check that length of atts and gridrefs match at start of script
			}
			#row.names(sp_df) = NULL
			sp_pointsdf = SpatialPointsDataFrame(coords = gr_points, data = sp_df,proj4string = CRS(tp_proj4))
			rm(sp_df)	
		# Test whether out_proj4 is not NULL (i.e. it was supplied or either sep_proj_layers = FALSE or there is only 1 grid ref type present)		
		if(!is.null(out_proj4)){
			# Test whether current projection set matches output project if not reproject
			if(all(checkCRSArgs(tp_proj4)[[1]] & checkCRSArgs(out_proj4)[[1]]) & checkCRSArgs(tp_proj4)[[2]] != checkCRSArgs(out_proj4)[[2]]){
				sp_pointsdf = spTransform(sp_pointsdf, CRS(out_proj4))
			}
		}
		
		# Add current spatial polygons data frame to output object
		if(length(gr_tp) == 1 | (length(gr_tp) > 1 & i == 1 & !sep_proj_layers))	{
			out_obj = sp_pointsdf
		} else if(length(gr_tp) > 1 & !sep_proj_layers){
			# If there are multiple grid ref types but only one layer is too be returned and i > 1 then need to merge with existing
			out_obj = spRbind(out_obj,sp_pointsdf)
		} else {
			if(i == 1){
				out_obj = list(sp_pointsdf)
			} else {
				out_obj[[i]] = sp_pointsdf
			}
		}
	}
	
	# Return output object
	return(out_obj)
}