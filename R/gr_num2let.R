gr_num2let <-
function(easting,northing, OSgrid = "OSGB", keep_precision = TRUE, min_10km = TRUE){
	# Check easting and northing are same length
	if(length(easting)!=length(northing)){
		stop("Length of Easting & Northing Vectors do not match")
	}
	
	# Check OSgrid is valid or NA
	inv_grid = which(!OSgrid %in% c("OSGB","OSNI","UTM30",NA))
	if(length(inv_grid) > 0){
		stop(paste("Invalid Grid type (",OSgrid[inv_grid],")", sep=""))
	}
	
	# Where OSgrid is NA then check that easting & northing are also NA
	inv_grid = which(is.na(OSgrid) & !is.na(easting) & !is.na(northing))
	if(length(inv_grid) > 0){
    # Print warning to the screen and set eastings & northings to NA
	    warning("OSgrid contained NA value(s) corresponding to non-NA easting/northing values, an NA will be returned in these cases")
	    easting[inv_grid] = NA
	    northing[inv_grid] = NA
	}
	
	#If OSgrid is length 1 then expand to be same length as easting/northing
	if(length(OSgrid) == 1){
		OSgrid = rep(OSgrid, length(easting))
	}
	
	# Where easting | northing are NA then set OSgrid to NA
		na_inds = which(is.na(easting) | is.na(northing))
		if(length(na_inds) > 0){
			OSgrid[na_inds] = NA
		}

	# Setup variable to store gridrefs
		ret_obj = rep(NA, length(easting))

	# Convert easting/northing to strings and make sure strings are at least 5 chars or if not left pad with zeros
		easting = sprintf("%05i",as.integer(easting))
		northing = sprintf("%05i",as.integer(northing))
		
	# Extract component of easting/northing that will remain as digits
		dig_east = substr(easting,(nchar(easting)-5)+1,nchar(easting))
		dig_north = substr(northing,(nchar(northing)-5)+1,nchar(northing))
	
	# Determine if precision of easting/northing to be kept or if can trim gridref to only positve value
		#(i.e. easting/northing 507000, 281000 could be presented as TL0700081000 (keeping the precision) or presented as TL0781 (removing matching zero pairs)
	if(!keep_precision){
		# Strip excess digits from ends of dig_east/dig_north (So gridrefs donn't come out with loads of zeros)
			# Can only trim digit from east/north if both east and northing end in a 0
			pair_trim = pmin( nchar(dig_east) - nchar(gsub("[0]*$","", dig_east)), nchar(dig_north) - nchar(gsub("[0]*$","", dig_north)) )
			dig_east[pair_trim > 0] = substr(dig_east[pair_trim > 0],1,nchar(dig_east[pair_trim > 0])-pair_trim[pair_trim > 0])
			dig_north[pair_trim > 0] = substr(dig_north[pair_trim > 0], 1, nchar(dig_north[pair_trim > 0])-pair_trim[pair_trim > 0])
	}
	
	# If dig_east and dig_north are length zero and min_10km == TRUE then replace with a "0"
	if(min_10km){
		dig_east[nchar(dig_east) == 0] = "0"
		dig_north[nchar(dig_north) == 0] = "0"
	}
		
	# Determine gridref of British Easting Northings
	cty_inds = which(OSgrid == "OSGB")
	if(length(cty_inds) > 0){
		
		# Determine easting and northing from orgin in 500km squares
		eX500k = as.numeric(easting[cty_inds])/500000
		nX500k = as.numeric(northing[cty_inds])/500000
		
		# Determine first letter based on 500km easting/northing (-3 at end is to deal with the 500km grid starting at s
		L500k = (21 - (5*floor(nX500k)) + floor(eX500k))-3
			# Check for letter indices out with the expected values (1 to 25), if such values exist then remove the indices for these rows from cty_inds and corresponding values from eX500k,nX500k and L500k
			rm_inds  = which(L500k <= 0 | L500k > 25)
			if(length(rm_inds) > 0){
				cty_inds = cty_inds[-rm_inds]
				eX500k = eX500k[-rm_inds]
				nX500k = nX500k[-rm_inds]
				L500k = L500k[-rm_inds]
			}
		
		# Determine easting and northing within given 500km square (round to 5 decimal places to ensure floor in next step works (issues with HP60 gridref)
		eX100k = round((eX500k - floor(eX500k)) * 5,5)
		nX100k = round((nX500k - floor(nX500k)) * 5,5)
		
		# Determine second letter based on 100km easting/northing
		L100k = 21 - (5*floor(nX100k))  + floor(eX100k)
		
		# Fill ret_obj with grid references using letters and digits (not exlude 9th letter from LETTERS as I not used)
		ret_obj[cty_inds] = paste(LETTERS[-9][L500k], LETTERS[-9][L100k], dig_east[cty_inds], dig_north[cty_inds], sep="")
		
	}
	
	# Determine gridref of Irish Easting/Northings
	cty_inds = which(OSgrid == "OSNI")
	if(length(cty_inds) > 0){
		# Determine easting and northing from origin in 100km squares (even though Ireland doesn't use 500km squares
		eX100k = round(as.numeric(easting[cty_inds])/100000,5)
		nX100k = round(as.numeric(northing[cty_inds])/100000,5)
		
		# Determine second letter based on 100km easting/northing
		L100k = 21 - (5*floor(nX100k))  + floor(eX100k)
		ret_obj[cty_inds] = paste(LETTERS[-9][L100k], dig_east[cty_inds], dig_north[cty_inds], sep="")
	}
	
	# Determine gridref of Channel Islands Easting/Northings
	cty_inds = which(OSgrid == "UTM30")
	if(length(cty_inds) > 0){
		chr_east = substr(easting,1,(nchar(easting)-5))
		chr_north = substr(northing,1,(nchar(northing)-5))
		# First square
		part_inds = which(OSgrid == "UTM30" & chr_north == 55)
		if(length(part_inds) > 0){
			ret_obj[part_inds] = paste("WA",dig_east[part_inds],dig_north[part_inds], sep="")
		}
		# Second square
		part_inds = which(OSgrid == "UTM30" & chr_north == 54)
		if(length(part_inds) > 0){
			ret_obj[part_inds] = paste("WV",dig_east[part_inds],dig_north[part_inds], sep="")
		}
	}
	
	return(ret_obj)
}
