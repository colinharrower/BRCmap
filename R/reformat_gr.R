reformat_gr <- function(
	gridref,
	prec_out = 10000,
	precision = NULL,
	pad_gr = FALSE
){
	# Setup output object to store reformated grid refs
		gr_out = rep(NA, length(gridref))
		
	# If precision is a single value and not a recognised precision then stop
	if(length(prec_out) == 1 & !prec_out %in% c(100000,10000,5000,2000,1000,100,10,1)){
	  stop("Value supplied in prec_out is not a valid output precision")
	}
  # If precision is not a single value then check it is same length as gridref and that all values are recognised precisions
	if(length(prec_out) > 1){
	  if(length(prec_out) != length(gridref)){
	    stop("Length of prec_out does not match length of gridref")
	  }
	  inv_inds = which(!prec_out %in% c(100000,10000,5000,2000,1000,100,10,1))
	  # If any invalid prec_out values found then reset to NA
	  if(length(inv_inds) > 0){
	    warning("prec_out contains invalid output precisions (offending values have been replaced by NA)")
	    prec_out[inv_inds] = NA
	  }
	} else if(length(prec_out) == 1) {
	  prec_out = rep(prec_out, length(gridref))
	} else {
	  stop("prec_out need to be supplied and have a length >= 1")  
	}
		
  # Extract components from valid grid refs
    gr_comps = gr_components(gridref)
    # Create a copy of the precision values estimated from the grid refs
    comb_prec = gr_comps$PRECISION
    
  # Check calculated values agaist supplied precision if not null
  if(!is.null(precision)){
    inv_inds = which(gr_comps$PRECISION > precision)
    if(length(inv_inds) > 0){
      warning("Supplied precision value(s) are higher resolution than the corresponding grid reference (grid ref precision will be used instead)")
      comb_prec[inv_inds] = ifelse(length(precision) == 1, precision, precision[inv_inds])
    }
  }
  
  
  # Determine which grid refs can be reformatted to required precision and where the prec_out is not tetrad/quadrant
  i_ref = which(comb_prec <= prec_out & prec_out %in% c(100000,10000,1000,100, 10, 1))
  if(length(i_ref) > 0){
    # Determine number of digits to give required precision
      dig_req = 5 - log10(prec_out[i_ref])   
    # For gridrefs to be reformated use output from gr_components to extract easting and northings of correct length 
      east_digit = substr(gr_comps$DIGITS_EAST[i_ref], 1, dig_req)
      north_digit = substr(gr_comps$DIGITS_NORTH[i_ref], 1, dig_req)
    # Combine new components to create grid ref of required precision and insert inot gr_out
      gr_out[i_ref] = paste0(gr_comps$CHARS[i_ref],east_digit, north_digit) 
  }
  
  # Find cases where a MGRS zone was present and gr_out is not null
  inds = which(!is.na(gr_comps$ZONE) & !is.na(gr_out))
  if(length(inds) > 0){
    gr_out[inds] = paste0(gr_comps$ZONE[inds],gr_out[inds])
  }
	
  # Determine which grid refs can be reformatted to required precision and where the prec_out is tetrad/quadrant 
  i_ref = which(comb_prec <= prec_out & prec_out %in% c(2000,5000))
  if(length(i_ref) > 0){
		# Use det_tet_quad to reformat_gr
		gr_out[i_ref] = det_tet_quad(gridref[i_ref], precision =precision,  prec_out = prec_out[i_ref])
  }
  
  # Where prec_out > comb_prec and pad_gr is true then pad the grid ref
  if(pad_gr){
    warning("Setting pad_gr = TRUE will reformat grid references to a higher resolution than the original resulting in false precision:\n\tOnly use this option if you are sure this is what you want!")
    # Find gridrefs which need padded to non-tetrad/quadrant
    i_ref = which(comb_prec > prec_out & prec_out %in% c(1000,100, 10, 1))
    if(length(i_ref) > 0){
      # Determine number of digits to give required precision
        dig_req = 5 - log10(prec_out[i_ref])
      # Extract digits padding to required length
        east_digit = gsub(" ","0", format(gr_comps$DIGITS_EAST[i_ref], width = dig_req))
        north_digit = gsub(" ","0", format(gr_comps$DIGITS_NORTH[i_ref], width = dig_req))
      # Combine components to create gridref
        gr_out[i_ref] = paste0(gr_comps$CHARS[i_ref],east_digit, north_digit)
        
    }
    # Find gridrefs which need padded to tetrad/quadrant
    i_ref = which(comb_prec > prec_out & prec_out %in% c(5000,2000))
    if(length(i_ref) > 0){
      # Determine number of digits to give required precision
      dig_req = 5 - log10(rep(1000,i_ref))
      # Extract digits padding to required length
      east_digit = gsub(" ","0", format(gr_comps$DIGITS_EAST[i_ref], width = dig_req))
      north_digit = gsub(" ","0", format(gr_comps$DIGITS_NORTH[i_ref], width = dig_req))
      # Combine components to create gridref
      gr_out[i_ref] = det_tet_quad(paste0(gr_comps$CHARS[i_ref],east_digit, north_digit), precision = 1000, prec_out = prec_out[i_ref])
    }
  }
	# Return output string
		return(gr_out)
}