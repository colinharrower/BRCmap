# Adapted from Script created by Chris Verness ((c) Chris Veness 2014-2015 / MIT Licence)
# This script is provided on moveable type website (http://www.movable-type.co.uk/scripts/latlong-utm-mgrs.html) under MIT licence for
# free use and adaptation.
# It is Based on Karney 2011 ‘Transverse Mercator with an accuracy of a few nanometers’, building on 
# Krüger 1912 ‘Konforme Abbildung des Erdellipsoids in der Ebene’.          
 
gps_latlon2UTM = function(lat, lon){
	if(! lat >= -80 & lat <= 84){
		stop("Supplied value outside of UTM limits")
	}
	false_e = 500e3
	false_n = 10000e3
	
	# Determine the UTM zone
		zone = floor((lon + 180)/6) + 1
	# Determine the longitude of central meridian
		lon_cm = ( ((zone - 1)*6) - 180 + 3 ) * (pi/180)
	# Determine MGRS lat grid
		# Grid zones are 8 degrees tall; 0 degrees north is offset 10 into latitude bands array
			# Define MGRS latitude bands
				MGRS_lat_bands = c("C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X","X")
			# Determine MGRS lat band for current latitude
				lat_band = MGRS_lat_bands[floor(lat/8 + 10)+1]
		# Deal with Norway/Svalbard exceptions
			if(zone == 31 & lat_band == "V" & lon >= 3){
				zone = zone + 1
				lon_cm = lon_cm + (6 * (pi/180))
			}
			if(zone == 32 & lat_band == "X" & lon < 9){
				zone = zone - 1
				lon_cm = lon_cm - (6 * (pi/180))
			}
			if(zone == 32 & lat_band == "X" & lon >= 9){
				zone = zone + 1
				lon_cm = lon_cm + (6 * (pi/180))
			}
			if(zone == 34 & lat_band == "X" & lon < 21){
				zone = zone - 1
				lon_cm = lon_cm - (6 * (pi/180))
			}
			if(zone == 34 & lat_band == "X" & lon >= 21){
				zone = zone + 1
				lon_cm = lon_cm + (6 * (pi/180))
			}
			if(zone == 36 & lat_band == "X" & lon < 33){
				zone = zone - 1
				lon_cm = lon_cm - (6 * (pi/180))
			}
			if(zone == 36 & lat_band == "X" & lon >= 33 ){
				zone = zone + 1
				lon_cm = lon_cm + (6 * (pi/180))
			}
			
		# Latitude from equator
			phi = lat * (pi/180)
		# Longitude from central meridian
			lambda = (lon * (pi/180)) - lon_cm
			
		# define Parameters for WGS84 ellipsoid
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
		
		# Cos lambda
			cos_lambda = cos(lambda)
			sin_lambda = sin(lambda)
			tan_lambda = tan(lambda)
}		# Tau
			v_tau = tan(phi)
			sigma = sinh(e*atanh((e*v_tau)/sqrt(1+v_tau*v_tau)))
			
			tau_prime = v_tau * sqrt(1+sigma*sigma) - sigma*sqrt(1+v_tau*v_tau)
			xi_prime = atan2(tau_prime, cos_lambda)
			eta_prime = asinh(sin_lambda / sqrt(tau_prime*tau_prime + cos_lambda*cos_lambda))
			
			A = a/(1+n) * (1 + 1/4*n2 + 1/64*n4 + 1/256*n6) # 2piA is the circumference of a meridian
			alpha = c(
				0,
				1/2*n - 2/3*n2 + 5/16*n3 + 41/180*n4 - 127/288*n5 + 7891/37800*n6,
				13/48*n2 - 3/5*n3 + 557/1440*n4 + 281/630*n5 - 1983433/1935360*n6,
				61/240*n3 - 103/140*n4 + 15061/26880*n5 + 167603/181440*n6,
				49561/161280*n4 - 179/168*n5 + 6601661/7257600*n6,
				34729/80640*n5 - 3418889/1995840*n6,
				212378941/319334400*n6
			)
			xi = xi_prime
			for(j in 1:6){
				xi = xi + alpha[j] * sin(2*j*xi_prime) * cosh(2*j*eta_prime)
			}
			eta = eta_prime
			for(j in 1:6){
				eta = eta + alpha[j] * cos(2*j*xi_prime) * sinh(2*j*eta_prime)
			}
			
			x = k0 * A * eta
			y = k0 * A * xi
		
		# Convergence (Karney 2011 eq 23, 24)
			p_prime = 1
			for(j in 1:6){
				p_prime = p_prime + 2*j*alpha[j] * cos(2*j*xi_prime) * cosh(2*j*eta_prime)
			}
			q_prime = 0
			for(j in 1:6){
				q_prime = q_prime + 2*j*alpha[j] * sin(2*j*xi_prime) * sinh(2*j*eta_prime)
			}
			
			nu_prime = atan(tau_prime / sqrt(1 + tau_prime*tau_prime) * tan_lambda)
			nu_pp = atan2(q_prime,p_prime)
			
			nu = nu_prime + nu_pp
			
		# Scale (Karney 2011 eq 25)
			sin_phi = sin(phi)
			k_prime = sqrt(1 - e*e*sin_phi*sin_phi) * sqrt(1 + v_tau*v_tau) / sqrt(tau_prime*tau_prime + cos_lambda*cos_lambda)
			k_pp = A / a * sqrt(p_prime*p_prime + q_prime*q_prime)
			
			k = k0 * k_prime * k_pp
			
		# Shift x/y to false origins
			x = x + false_e # Make x relative to false easting
			if(y < 0){
				y = y + false_n # Make y in southern hemisphere relative to false northing
			}
			
			# Convert to character, round to reasonable precision and pad to have 5 digits prior to decimal point
				#x = sprintf("%5.0f",x)
				#y = sprintf("%5.0f",y)
				
		# Build output string
			ret_obj = sprintf("%i%s% .3f% .3f",zone, lat_band, x, y)