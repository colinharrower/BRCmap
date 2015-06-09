mgrs2utm = function(mgrs, centre = FALSE, return_precision = FALSE){
	# Convert mgrs to uppercase
		mgrs = toupper(mgrs)
	#Define MGRS latitude bands
		MGRS_lat_bands = 1:20
		names(MGRS_lat_bands) = c("C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X")
	# Define letters for e100km & n100km
		lets_e100km = LETTERS[-c(9,15)]
		lets_n100km_odd = LETTERS[-c(9,15, 23:26)]
		lets_n100km_even = LETTERS[c(6:8,10:14,16:22,1:5)]
		
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
		zone[good_inds] = as.numeric(temp$ZONE)
		band[good_inds] = temp$BAND
		l_e = substr(temp$CHARS,1,1)
		l_n = substr(temp$CHARS,2,2)
		easting = as.numeric(gsub(" ","0", format(temp$DIGITS_EAST, width = 5)))
		northing = as.numeric(gsub(" ","0", format(temp$DIGITS_NORTH, width = 5)))
		
		# Determine easting specified by e100km
			int_e = match(l_e,lets_e100km) - ((zone[good_inds] - 1) %% 3)*8
		# Determine northing specified by n100km
			int_n = rep(NA, length(good_inds))
			# If zone is odd then use lets starting at A else use lets starting at F
			inds = which(zone[good_inds] %% 2 == 0)
			if(length(inds) > 0){
				int_n[inds] = match(l_n[inds], lets_n100km_even)-1
			}
			inds = which(zone[good_inds] %% 2 != 0)
			if(length(inds) > 0){
				int_n[inds] = match(l_n[inds], lets_n100km_odd)-1
			}
			
			#int_n = match(l_n, lets_n100km[(((as.numeric(zone[good_inds]) - 1) %% 2)*2+2):40])
			
		# Determine easting & northing of 100km square
			e100km = int_e * 1e5
			n100km = int_n * 1e5
			
		# get latitude of bottom of band
			latband = ((MGRS_lat_bands[band] - 1) - 10)*8
			
		# 100km grid square row letters repeat every 2000 km north, add enough 2000km blocks to get into required band
			# Get northing for bottom of latitude band
				#nband = gps_latlon2utm(latband, rep(0,length(latband)), out_string = FALSE)$NORTHING
				lon_mid = -180 + (int_e *6) + 3 # Had to calculate longitude midpoint for zone and add that to the line estimating northing of bottom of the latitude band as bands not even across longitude
				nband = gps_latlon2utm(latband, lon_mid, out_string = FALSE)$NORTHING
			# Set intial value for northing of 2,000km blocks
				n2m = rep(0,length(good_inds))
			# Look for grid refs where the northing has not yet met the northing of the band
			while(any(n2m + n100km + northing < nband)){
				inds = which(n2m + n100km + northing < nband)
				n2m[inds] = n2m[inds] + 2000000
			}
			# Modify n100km based on final value of n2m
			n100km = n100km + n2m
			
		
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
			easting[good_inds] = e100km + easting
			northing[good_inds] = n100km + northing
		
			
		# If centre is true then need to also modify by 1/2 of precision to get centres
		if(centre){
			easting[good_inds] = easting[good_inds] + floor(0.5*temp$PRECISION)
			northing[good_inds] = northing[good_inds] + floor(0.5*temp$PRECISION)
		}
		
		# Now create utm strings
			utm_str[good_inds] = sprintf("%i%s %.0f %.0f",zone, band, easting, northing)
		
		# Add precision if return_precision == TRUE
		if(return_precision){
			precision[good_inds] = temp$PRECISION
			ret_obj = data.frame(UTM = utm_str, PRECISION = precision)
		} else {
			ret_obj = utm_str
		}
		
	# Return utm_str
	return(ret_obj)
}