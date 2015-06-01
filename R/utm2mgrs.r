utm2mgrs = function(utm_str, output_type = "full_gr"){
	# Setup object to store output
		zone = rep(NA, length(utm_str))
		band = rep(NA, length(utm_str))
		easting = rep(NA, length(utm_str))
		northing = rep(NA, length(utm_str))
		l_e = rep(NA, length(utm_str))
		l_n = rep(NA, length(utm_str))
		no_dec = rep(NA, length(utm_str))
	
	# Define letters for e100km & n100km
		lets_e100km = LETTERS[-c(9,15)]
		lets_n100km = c(LETTERS[-c(9,15, 23:26)], LETTERS[c(6:8,10:14,16:22,1:5)])
	
	# Check UTM string matches expected pattern
	bad_inds = which(!grepl("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$",utm_str))
	if(length(bad_inds) > 0){
		warning("One or more of the supplied strings were not recognised as UTM coordinates")
	}

	# Split UTM str into components
		zone[-bad_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\1", utm_str[-bad_inds])
		band[-bad_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\2", utm_str[-bad_inds])
		easting[-bad_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\3", utm_str[-bad_inds])
		northing[-bad_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\5", utm_str[-bad_inds])
		
		# Determine decimal precision of easting & northing values
			no_dec[-bad_inds] = pmax(nchar(gsub("^([[:digit:]]+)([.]([[:digit:]]*))?$", "\\3", easting[-bad_inds])), nchar(gsub("^([[:digit:]]+)([.]([[:digit:]]*))?$", "\\3", northing[-bad_inds])))	
	
	# Converting easting & northing to numeric vector
		easting = as.numeric(easting)
		northing = as.numeric(northing)
	
	# Determine easting & northing letters
		# Easting
		l_e = lets_e100km[((as.numeric(zone) - 1)%%3)*8 + floor(easting/1e5)]
		# Northing
		l_n = lets_n100km[ ((as.numeric(zone) - 1) %% 2)*20 + ((floor(northing/1e5) %% 20) + 1)]
		
	# Determine easting & northing within 100km square
		east_100km = easting %% 1e5
		north_100km = northing %% 1e5
	
	# Create output string
	if(tolower(output_type) == "full_gr"){
		ret_obj = sprintf("%s%s%s%s%05.0f%05.0f",zone,band,l_e,l_n,floor(east_100km),floor(north_100km))
		ret_obj[bad_inds] = NA
	} else if(tolower(output_type) == "split_gr"){
		ret_obj = data.frame(GZD = paste(zone,band,sep=""), GRIDREF = sprintf("%s%s%05.0f%05.0f",l_e,l_n,floor(east_100km),floor(north_100km)))
		ret_obj[bad_inds,] = c(NA,NA)
	} else if(tolower(output_type) == "atomised"){
		ret_obj = data.frame(ZONE = zone, BAND = band, SQ100KM_ID = paste(l_e,l_n, sep=""), EASTING = round(east_100km,no_dec), NORTHING = round(north_100km, no_dec))
		ret_obj$SQ100KM_ID[bad_inds] = NA
	} else {
		stop("Unknown output type: valid options are 'full_gr', 'split_gr', or 'atomised'")
	}
	
	# Return output object
	return(ret_obj)
		
}