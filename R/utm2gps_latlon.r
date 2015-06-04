utm2gps_latlon = function(utm_str){

	# Set false easting & northing
		false_e = 500e3
		false_n = 10000e3
	
	# Set datum params for WGS84 ellipsoid (perhaps replace this at some point to use datum_vars table included with BRCmap)
		a = 6378137
		b = 6356752.314245
		f = 1/298.257223563
		k0 = 0.9996 # UTM scale on central meridian
		
		# Determine eccentricity
			e = sqrt(f*(2-f))
		# 3rd flattening
			n = f / (2 - f)
			n2 = n*n
			n3 = n*n2
			n4 = n*n3
			n5 = n*n4
			n6 = n*n5
			
		
		# Determine A
		A = ( a/(1+n) ) * (1 + 1/4*n2 + 1/64*n4 + 1/256*n6) # 2piA is the circumference of a meridian - equation 14
		
	# Define MGRS latitude bands
		MGRS_lat_bands = 1:20
		names(MGRS_lat_bands) = c("C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X")
	
	# Check UTM string matches expected pattern
		good_inds = which(grepl("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$",utm_str))
		if(length(good_inds) == 0){
			stop("None of the supplied strings were recognised as UTM coordinates")
		} else if(length(good_inds) != length(utm_str)){
			warning("One or more of the supplied strings were not recognised as UTM coordinates")
			utm_str = utm_str[good_inds]
		}
		
	# Determine components from utm_str
		# Setup object to store output
		zone = rep(NA, length(utm_str))
		band = rep(NA, length(utm_str))
		easting = rep(NA, length(utm_str))
		northing = rep(NA, length(utm_str))
		no_dec = rep(NA, length(utm_str))
		lat = rep(NA, length(utm_str))
		lon = rep(NA, length(utm_str))
		conv = rep(NA, length(utm_str))
		v_scale = rep(NA, length(utm_str))
		
		
	
		# Split UTM str into components
		zone[good_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\1", utm_str[good_inds])
		band[good_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\2", utm_str[good_inds])
		easting[good_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\3", utm_str[good_inds])
		northing[good_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\5", utm_str[good_inds])
		
		# Determine decimal precision of easting & northing values
			no_dec[good_inds] = pmax(nchar(gsub("^([[:digit:]]+)([.]([[:digit:]]*))?$", "\\3", easting[good_inds])), nchar(gsub("^([[:digit:]]+)([.]([[:digit:]]*))?$", "\\3", northing[good_inds])))	
	
	# Converting easting & northing to numeric vector and store in x, y
		x = as.numeric(easting[good_inds])
		y = as.numeric(northing[good_inds])
		
	# Determine hemisphere
		s_hemi = rep(FALSE, length(good_inds))
		s_hemi = MGRS_lat_bands[band[good_inds]] < 11
		names(s_hemi) = NULL
	
	# Convert x & y values relative to cm and equator
		x = x - false_e
		y = y - (false_n *  s_hemi) # for northern hemisphere s_hemi = FALSE == 0 therefore y is not modified
	
	# Determine eta and xi
		eta = x / (k0*A)
		xi = y / (k0*A)
		
	# Determine beta (note beta is an R function so use a different name)
		v_beta = c(
			0,
			1/2*n - 2/3*n2 + 37/96*n3 - 1/360*n4 - 81/512*n5 + 96199/604800*n6,
			1/48*n2 + 1/15*n3 - 437/1440*n4 + 46/105*n5 - 1118711/3870720*n6,
			17/480*n3 - 37/840*n4 - 209/4480*n5 + 5569/90720*n6,
			4397/161280*n4 - 11/504*n5 - 830251/7257600*n6,
			4583/161280*n5 - 108847/3991680*n6,
			20648693/638668800*n6
		)
		j = 0:(length(v_beta)-1)
		
			n_xy = length(x) # Determine number of x,y values
			n_bet = length(v_beta) # Determine number of v_beta values

	# Determine xi_prime and eta_prime
		xi_prime = xi - rowSums(matrix(rep(v_beta,n_xy) * sin(2*rep(j,n_xy)*rep(xi,each = n_bet)) * cosh(2*rep(j,n_xy)*rep(eta,each = n_bet)), nrow = n_xy, ncol = n_bet, byrow = TRUE)) #
		
		eta_prime = eta - rowSums(matrix(rep(v_beta,n_xy) * cos(2*rep(j,n_xy)*rep(xi,each = n_bet)) * sinh(2*rep(j,n_xy)*rep(eta,each = n_bet)), nrow = n_xy, ncol = n_bet, byrow = TRUE)) # Vectorised form of equation
	
	# Calculate tau_prime
		tau_prime = sin(xi_prime) / sqrt(sinh(eta_prime)^2 + cos(xi_prime)^2)
	
	# Convergence loop
	# Setup tau_i which will be the test condition for the while loop
		tau_i = tau_prime
		sigma_i = sinh(e*atanh(e*tau_i/sqrt(1+tau_i^2)))
		tau_i_prime = tau_i * sqrt(1 + sigma_i^2) - sigma_i * sqrt(1+tau_i^2)
		gamma_tau_i = (tau_prime - tau_i_prime)/sqrt(1+tau_i_prime^2) * (1 + (1-e^2)*tau_i^2) / ((1-e^2)*sqrt(1+tau_i^2))
		tau_i = tau_i + gamma_tau_i
	while(any(abs(gamma_tau_i) > 1e-12)){
		inds = which(abs(gamma_tau_i) > 1e-12)
		sigma_i[inds] = sinh(e*atanh(e*tau_i[inds]/sqrt(1+tau_i[inds]^2)))
		tau_i_prime[inds] = tau_i[inds] * sqrt(1 + sigma_i[inds]^2) - sigma_i[inds] * sqrt(1+tau_i[inds]^2)
		gamma_tau_i[inds] = (tau_prime[inds] - tau_i_prime[inds])/sqrt(1+tau_i_prime[inds]^2) * (1 + (1-e^2)*tau_i[inds]^2) / ((1-e^2)*sqrt(1+tau_i[inds]^2))
		tau_i[inds] = tau_i[inds] + gamma_tau_i[inds]
	}
	
	v_tau = tau_i
	
	phi = atan(v_tau)
	
	lambda = atan2(sinh(eta_prime), cos(xi_prime))
	
	# Convergernce - Karney 2011 eq 26, 27
		p = 1 - rowSums(matrix(2*rep(j,n_xy)*rep(v_beta,n_xy)*cos(2*rep(j,n_xy)*rep(xi,each = n_bet))*cosh(2*rep(j,n_xy)*rep(eta,each = n_bet)), nrow = n_xy, ncol = n_bet, byrow = TRUE))
		
		v_q = rowSums(matrix(2*rep(j,n_xy)*rep(v_beta,n_xy)*sin(2*rep(j,n_xy)*rep(xi,each = n_bet))*sinh(2*rep(j,n_xy)*rep(eta,each = n_bet)), nrow = n_xy, ncol = n_bet, byrow = TRUE))
		
		nu_prime = atan(tan(xi_prime) * tanh(eta_prime))
		nu_pp = atan2(v_q,p)
		
		nu = nu_prime + nu_pp
	
	# Scale - Karney 2011 eq 28
		k_prime = sqrt(1 - e^2*sin(phi)^2) * sqrt(1+v_tau^2) * sqrt(sinh(eta_prime)^2 + cos(xi_prime)^2)
		k_pp = A / a / sqrt(p^2 + v_q^2)
		
		k = k0 * k_prime * k_pp
		
	# Move from zonal to global coordinates
		# Offset for current zone
			lambda0 = ((as.numeric(zone) - 1)*6 - 180 + 3) * (pi/180)
		# Adjust lambda to be relative to global system from zone
			lambda = lambda + lambda0 
			
	# Round to reasonable precision
		lat[good_inds] = round(phi * (180/pi), 11)
		lon[good_inds] = round(lambda * (180/pi), 11)
		conv[good_inds] = round(nu * (180/pi), 9)
		v_scale[good_inds] = round(k,12)
	
	# Return latitudes and longitudes
		ret_obj = data.frame(LATITUDE = lat, LONGITUDE = lon)
		return(ret_obj)
	
}
