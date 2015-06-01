utm2mgrs = function(utm_str, output_type = "full_gr"){
	# Define letters for e100km & n100km
		lets_e100km = LETTERS[-c(9,15)]
		lets_n100km = c(LETTERS[-c(9,15, 23:26)], LETTERS[c(6:8,10:14,16:22,1:5)])
	
	# Convert to upper case if not already
		utm_str = toupper(utm_str)
	# Check UTM string matches expected pattern
	if(!grepl("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$",utm_str)){
		stop("ERROR: supplied string does not match expected UTM format, e.g. 30U 421937.838, 5564409.771")
	}

	# Split UTM str into components
		zone = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\1", utm_str)
		band = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\2", utm_str)
		easting = as.numeric(gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\3", utm_str))
		northing = as.numeric(gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\5", utm_str))
		
		# Determine decimal precision of easting & northing values
			no_dec = max(c(nchar(easting) - nchar(floor(easting))-1, nchar(northing) - nchar(floor(northing))-1))
			if(no_dec < 0){
				no_dec = 0
			}
		
		
	
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
	} else if(tolower(output_type) == "split_gr"){
		ret_obj = data.frame(GZD = paste(zone,band,sep=""), GRIDREF = sprintf("%s%s%05.0f%05.0f",l_e,l_n,floor(east_100km),floor(north_100km)))
	} else if(tolower(output_type) == "atomised"){
		ret_obj = data.frame(ZONE = zone, BAND = band, SQ100KM_ID = paste(l_e,l_n, sep=""), EASTING = round(east_100km,no_dec), NORTHING = round(north_100km, no_dec))
	} else {
		stop("Unknown output type: valid options are 'full_gr', 'split_gr', or 'atomised'")
	}
	
	# Return output object
	return(ret_obj)
		
}