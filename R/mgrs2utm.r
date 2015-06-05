mgrs2utm = function(mgrs, centre = TRUE){
	# Convert mgrs to uppercase
		mgrs = toupper(mgrs)
	#Define MGRS latitude bands
		MGRS_lat_bands = 1:20
		names(MGRS_lat_bands) = c("C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X")
	# Define letters for e100km & n100km
		lets_e100km = LETTERS[-c(9,15)]
		lets_n100km = c(LETTERS[-c(9,15, 23:26)], LETTERS[c(6:8,10:14,16:22,1:5)])
		
	# Setup variables to hold components
		zone = rep(NA, length(mgrs))
		band = rep(NA, length(mgrs))
		easting = rep(NA, length(mgrs))
		northing = rep(NA, length(mgrs))
		l_e = rep(NA, length(mgrs))
		l_n = rep(NA, length(mgrs))
		precision = rep(NA, length(mgrs))
		utm_str = rep(NA, length(mgrs))
		
	# Check UTM string matches expected pattern
		good_inds = which(grepl("^[[:digit:]]{1,2}[ ]?[[:alpha:]]{1}[ ]?[[:alpha:]]{2}[[:digit:]]{2,}([[:upper:]]{1}|[[:upper:]]{2})?$", mgrs) & 
			nchar(gsub("^([[:digit:]]{1,2}[ ]?[[:alpha:]]{1}[ ]?[[:alpha:]]{2})([[:digit:]]{2,})([[:upper:]]{1}|[[:upper:]]{2})?$", "\\2", mgrs)) %% 2 == 0)
		if(length(good_inds) == 0){
			stop("None of the supplied strings were recognised as MGRS grid references")
		} else if(length(good_inds) != length(utm_str)){
			warning("One or more of the supplied strings were not recognised as MGRS grid references")
			utm_str = utm_str[good_inds]
		}

	# Split MGRS grid ref into components
		temp = gr_components(mgrs[good_inds])
		zone[good_inds] = temp$ZONE
		band[good_inds] = temp$BAND
		l_e = substr(temp$CHARS,1,1)
		l_n = substr(temp$CHARS,2,2)
		easting = as.numeric(gsub(" ","0", format(temp$DIGITS_EAST, width = 5)))
		northing = as.numeric(gsub(" ","0", format(temp$DIGITS_NORTH, width = 5)))
		
		# Determine easting specified by e100km
			int_e = match(l_e,lets_e100km) - ((as.numeric(zone) - 1) %% 3)*8
			int_n = match(l_n, lets_n100km[(((as.numeric(zone) - 1) %% 2)*20+2):40])
			
		# Determine median latitude from latitude
			med_lat = MGRS_lat_bands[band] - 1
			names(med_lat) = NULL
			inds = which(band == "X")
			if(length(inds) > 0){
				med_lat[inds] = 77
			}
			med_lat = -76 + (8 * med_lat)
			
		# Determine approximate median northing of ZDL (100km)
			appN = med_lat*100/90
		
		# Now modify int_n by med_lat
			int_n = int_n + round((appN - int_n)/20)*20
			
		
		# Any mgrs with a tetrad code will need modified
		inds = good_inds[which(!is.na(temp$TETRAD))]
		if(length(inds) > 0){
			# Get list of tetrad codes (all letters but O)
				tet_codes = LETTERS[-15]
			# Determine the position in the tetrad codes vector for each code
				code_match = match(temp$TETRAD, tet_codes)
			# Modify easting
				easting[inds] = easting[inds] + ((code_match-1) %/% 5)*2000
			# Modify northing
				northing[inds] = northing[inds] + ((code_match-1) %% 5)*2000
			# Clean up
				rm(tet_codes)
				rm(code_match)
		}
		
		# Any mgrs with a quadrant code will need modified
		inds = good_inds[which(!is.na(temp$QUADRANT))]
		if(length(inds) > 0){
			# Get list of quadrant codes in correct order
				quad_codes = c("SW","NW","SE","NE")
			# Determine the position in the tetrad codes vector for each code
				code_match = match(temp$QUADRANT, quad_codes)
			# Modify easting
				easting[inds] = easting[inds] + ((code_match-1) %/% 2)*5000
			# Modify northing
				northing[inds] = northing[inds] + ((code_match-1) %% 2)*5000
		}
		
		
		# Now bring all the parts together to figure out UTM northing & eastings
			easting[good_inds] = int_e * 1e5 + easting
			northing[good_inds] = int_n * 1e5 + northing
			
		# If centre is true then need to also modify by 1/2 of precision to get centres
		if(centre){
			easting[good_inds] = easting[good_inds] + 0.5*temp$PRECISION
			northing[good_inds] = northing[good_inds] + 0.5*temp$PRECISION
		}
		
		# Now create utm strings
		utm_str[good_inds] = sprintf("%s%s %i %i",zone, band, easting, northing)
		
	# Return utm_str
	return(utm_str)
}