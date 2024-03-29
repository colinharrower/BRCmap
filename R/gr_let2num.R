gr_let2num <-
function(gridref, centre = FALSE, gr_prec = NULL, return_projection = FALSE){
	# Function required to calculate easting in Letter Grid
	spmod = function(x, mod){
		ret_obj = x %% mod
		ret_obj[ret_obj == 0] = mod
		return(ret_obj)
	}
	
	# Validate grid reference format (removing any spaces or hyphens)
		gr_comps = gr_components(gridref)
		gridref = gsub("[ -]","",toupper(gridref))
	
	# Setup variable to hold output
		len_grvec = nrow(gr_comps)
		if(return_projection){
			ret_obj = data.frame( EASTING = rep(NA,len_grvec), NORTHING = rep(NA, len_grvec), PROJECTION = rep(NA, len_grvec), row.names = NULL ) # row.names set to null to stop duplicate row names error
		} else {
			ret_obj = data.frame( EASTING = rep(NA,len_grvec), NORTHING = rep(NA, len_grvec), row.names = NULL ) # row.names set to null to stop duplicate row names error
		}
		
	# First find all British Gridrefs
		cty_inds = which(grepl('(^[[:upper:]]{2}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{2}[[:digit:]]{2,}$)',gr_comps$VALID_GR) & !grepl('^(WA)|(WV)',gr_comps$VALID_GR))

		# If British Gridrefs found then calc easting and northings
		if(length(cty_inds) > 0){
			# Get position of gridref letters in grid
				l1 = match(substr(gr_comps$CHARS[cty_inds],1,1), LETTERS[-9])
				l2 = match(substr(gr_comps$CHARS[cty_inds],2,2), LETTERS[-9])
			# Determine initial easting northing digits based on 500km square
				e = (spmod(l1,5) - 1)*5
				n = floor(abs(l1 - 25)/5)*5
			# Modify initial easting/northing digits based on 100km square
				e = e + (spmod(l2,5) - 1)
				n = n + floor(abs(l2 - 25)/5)
			# Recalulate for false origin (SV) of British Grid
				e = e - 10
				n = n - 5
			# Extend so easting and northing digits so that nchars of east_num/north_num = 5 right padded with zeros
				east_num = gsub(" ","0", format(gr_comps$DIGITS_EAST[cty_inds], width = 5))
				north_num = gsub(" ","0", format(gr_comps$DIGITS_NORTH[cty_inds], width = 5))
			# append numeric part of references to grid index
				e = paste(e,east_num, sep="")
				n = paste(n,north_num, sep="")
			# Overwrite placeholder values in ret_obj
				ret_obj[cty_inds,c("EASTING", "NORTHING")] = c(as.numeric(e), as.numeric(n))
				if(return_projection){
					ret_obj[cty_inds,"PROJECTION"] = "OSGB"
				}
		}
		
	# Find all Irish Gridrefs
		cty_inds = which(grepl('(^[[:upper:]]{1}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1}[[:digit:]]{2,}$)',gr_comps$VALID_GR))
		# If Irish Gridrefs found the calc easting and northings
		if(length(cty_inds) > 0){
			# Get position of gridref letters in grid
				l2 = match(substr(gr_comps$CHARS[cty_inds],1,1), LETTERS[-9])
			# Determine initial easting/northing digits based on 100km square
				e = spmod(l2,5) - 1
				n = floor(abs(l2 - 25)/5)
			# Extend so easting and northing digits so that nchars of east_num/north_num = 5 right padded with zeros
				east_num = gsub(" ","0", format(gr_comps$DIGITS_EAST[cty_inds], width = 5))
				north_num = gsub(" ","0", format(gr_comps$DIGITS_NORTH[cty_inds], width = 5))
			# append numeric part of references to grid index
				e = paste(e,east_num, sep="")
				n = paste(n,north_num, sep="")
			# Overwrite placeholder values in ret_obj
				ret_obj[cty_inds,c("EASTING", "NORTHING")] = c(as.numeric(e), as.numeric(n))
				if(return_projection){
					ret_obj[cty_inds,"PROJECTION"] = "OSNI"
				}
		}
	
	# Find all Channel Islands Gridrefs
		cty_inds = which(grepl('(^(WA)|(WV)[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^(WA)|(WV)[[:digit:]]{2,}$)',gr_comps$VALID_GR))
		# If CI gridrefs found then calc easting and northings
		if(length(cty_inds) > 0){
			# Determine initial easting/northing based on letters
				e = rep(5, length(cty_inds))
				n = ifelse(grepl('^(WA)[[:digit:]]{2,}([[:upper:]]{1,2})?$',gr_comps$VALID_GR[cty_inds]),55,54)
			# Extend so easting and northing digits so that nchars of east_num/north_num = 5 right padded with zeros
				east_num = gsub(" ","0", format(gr_comps$DIGITS_EAST[cty_inds], width = 5))
				north_num = gsub(" ","0", format(gr_comps$DIGITS_NORTH[cty_inds], width = 5))
			# append numeric part of references to grid index
				e = paste(e,east_num, sep="")
				n = paste(n,north_num, sep="")
			# Overwrite placeholder values in ret_obj
				ret_obj[cty_inds,c("EASTING", "NORTHING")] = c(as.numeric(e), as.numeric(n))
				if(return_projection){
					ret_obj[cty_inds,"PROJECTION"] = "UTM30"
				}
		}
	
	# If any grid refs contained a tetrad or quadrant code then need to modify the easting and northing
		# Find all grid refs with a tetrad code
			cty_inds = which(!is.na(gr_comps$TETRAD))
		# For these grid refs modify the eastings and northings accordingly
		if(length(cty_inds) > 0){
			# Get list of tetrad codes (all letters but O)
				tet_codes = LETTERS[-15]
			# Determine the position in the tetrad codes vector for each code
				code_match = match(gr_comps$TETRAD[cty_inds], tet_codes)
			# Modify easting
				ret_obj[cty_inds, "EASTING"] = ret_obj[cty_inds,"EASTING"] + ((code_match-1) %/% 5)*2000
			# Modify northing
				ret_obj[cty_inds, "NORTHING"] = ret_obj[cty_inds,"NORTHING"] + ((code_match-1) %% 5)*2000
		}
		
		# Find all grid refs with a quadrant code
			cty_inds = which(!is.na(gr_comps$QUADRANT))
		# For these grid refs modify the eastings and northings accordingly
		if(length(cty_inds) > 0){
			# Get list of tetrad codes (all letters but O)
				quad_codes = c("SW","NW","SE","NE")
			# Determine the position in the tetrad codes vector for each code
				code_match = match(gr_comps$QUADRANT[cty_inds], quad_codes)
			# Modify easting
				ret_obj[cty_inds, "EASTING"] = ret_obj[cty_inds,"EASTING"] + ((code_match-1) %/% 2)*5000
			# Modify northing
				ret_obj[cty_inds, "NORTHING"] = ret_obj[cty_inds,"NORTHING"] + ((code_match-1) %% 2)*5000
		}
	
	# If centre is true the determine precision and give easting and northing for centre of gridref
	if(centre){
		# Determine precision (if gr_prec not supplied)
			if(is.null(gr_prec)){
				gr_prec = gr_comps$PRECISION
			}
		# Add half of precision to easting and northing
			ret_obj[,c("EASTING", "NORTHING")] = ret_obj[,c("EASTING", "NORTHING")] + (gr_prec/2)
	}
	# Return easting and Northings	
	return(ret_obj)
}
