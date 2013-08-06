reformat_gr <- function(
	gridref,
	prec_out = 10000,
	precision = NULL
){
	# Setup output object to store reformated grid refs
		gr_out = rep(NA, length(gridref))
	
	# Determine if required precision is possible from gridref	
	if (prec_out %in% c(100000,10000,1000,100, 10, 1)) {
	
		# Extract components from valid grid refs
		gr_comps = gr_components(gridref)
		
		# Determine number of digits to give required precision
			dig_req = 5 - log10(prec_out)
			
		# Find gridrefs for which new precision can be calculated
			if(!is.null(precision)){
				i_ref = which(pmax(gr_comps$PRECISION, precision) <= prec_out)
			} else {
				i_ref = which(gr_comps$PRECISION <= prec_out)
			}
		
		if(length(i_ref) > 0 ){
			# For gridrefs to be reformated use output from gr_components to extract easting and northings of correct length 
				east_digit = substr(gr_comps$DIGITS_EAST[i_ref], 1, dig_req)
				north_digit = substr(gr_comps$DIGITS_NORTH[i_ref], 1, dig_req)
			
			# Combine new components to create grid ref of required precision and insert inot gr_out
				gr_out[i_ref] = paste(gr_comps$CHARS[i_ref],east_digit, north_digit, sep="")
		}
	} else if (prec_out %in% c(2000,5000)){
		# Use det_tet_quad to reformat_gr
		gr_out = det_tet_quad(gridref, precision =precision,  prec_out = prec_out)
	} else {
		stop("Not a valid output precision")
	}
	# Return output string
		return(gr_out)
}