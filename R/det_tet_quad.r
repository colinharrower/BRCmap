det_tet_quad <- function(
	gridref,
	precision = NULL,
	prec_out = NULL
){
	# Setup output object
		out_obj = data.frame(TETRAD_GR = rep(NA, length(gridref)), QUADRANT_GR = NA)
	
	# Split gridref into components (will also check gridref sytax is correct)
		gr_comps = gr_components(gridref)

		gr_inds = which(nchar(gr_comps$DIGITS) >= 2 & !is.na(gr_comps$VALID_GR))	
		sq10 = rep(NA, length(gridref))
		sq10[gr_inds] = paste(gr_comps$CHARS[gr_inds], substr(gr_comps$DIGITS_EAST[gr_inds],1,1), substr(gr_comps$DIGITS_NORTH[gr_inds],1,1), sep="")
	
	# All gridrefs that have valid tetrad/quadrant codes can be filled directly to out_obj
		# Tetrads
		gr_inds = which(!is.na(gr_comps$TETRAD))
		if(length(gr_inds) > 0){
			out_obj[gr_inds,"TETRAD_GR"] = paste(sq10[gr_inds], gr_comps$TETRAD[gr_inds], sep="")
		}
		#Quadrants
		gr_inds = which(!is.na(gr_comps$QUADRANT))
		if(length(gr_inds) > 0){
			out_obj[gr_inds,"QUADRANT_GR"] = paste(sq10[gr_inds], gr_comps$QUADRANT[gr_inds], sep="")
		}
		
	# If original gridref had tetrad code then where can be mapped directly to quadrant then do so
		# Find gridrefs with calculated precision of 2000 i.e. those that had a tetrad code in the grid ref
		gr_inds = which(gr_comps$PRECISION == 2000)
		if(length(gr_inds) > 0){
			# SW
			gr_inds = which(gr_comps$PRECISION == 2000 & gr_comps$TETRAD %in% c("A","B","F","G"))
			if(length(gr_inds) > 0){
				out_obj[gr_inds,"QUADRANT_GR"] = paste(sq10[gr_inds], "SW", sep="") 
			}
			# NW
			gr_inds = which(gr_comps$PRECISION == 2000 & gr_comps$TETRAD %in% c("D","E","J","I"))
			if(length(gr_inds) > 0){
				out_obj[gr_inds,"QUADRANT_GR"] = paste(sq10[gr_inds], "NW", sep="") 
			}
			# SE
			gr_inds = which(gr_comps$PRECISION == 2000 & gr_comps$TETRAD %in% c("Q","R","V","W"))
			if(length(gr_inds) > 0){
				out_obj[gr_inds,"QUADRANT_GR"] = paste(sq10[gr_inds], "SE", sep="") 
			}
			# NE
			gr_inds = which(gr_comps$PRECISION == 2000 & gr_comps$TETRAD %in% c("T","U","Y","Z"))
			if(length(gr_inds) > 0){
				out_obj[gr_inds,"QUADRANT_GR"] = paste(sq10[gr_inds], "NE", sep="") 
			}
		}		
	
		
	# Find all valid gridrefs where precision is high enough
		gr_inds = which(gr_comps$PRECISION <= 1000)
	
	if(length(gr_inds) > 0){
		
	# Extract relevant digits for tetrad/quadrant estimation
		code_e = as.numeric(substr(gr_comps$DIGITS_EAST[gr_inds],2,2))
		code_n = as.numeric(substr(gr_comps$DIGITS_NORTH[gr_inds],2,2))
		
	# Determine tetrad code from easting and northing digits (can perhaps
		# Get list of tetrad codes (no O)
			tet_codes = LETTERS[-15]
		# Get tetrad code for each gridref of suitable precision
			tets = tet_codes[(code_e%/%2)*5 + (code_n%/%2)+1]
			out_obj[gr_inds,"TETRAD_GR"] = paste0(sq10[gr_inds],tets)
			
		# Find cases where a MGRS zone was present and out_obj$TETRAD_GR is not null
			inds = which(!is.na(gr_comps$ZONE) & !is.na(out_obj$TETRAD_GR))
			if(length(inds) > 0){
				out_obj$TETRAD_GR[inds] = paste0(gr_comps$ZONE[inds],out_obj$TETRAD_GR[inds])
			}
		
	# Determine quadrant codes from easting and northing digits
		# Build vector of quadrant codes (in correct order)
			quad_codes = c("SW","NW","SE","NE")
		# Get tetrad code for each gridref of suitable precision
			quads = quad_codes[(code_e%/%5)*2 + (code_n%/%5)+1]
			out_obj[gr_inds,"QUADRANT_GR"] = paste0(sq10[gr_inds],quads)
			
		# Find cases where a MGRS zone was present and out_obj$QUADRANT_GR is not null
			inds = which(!is.na(gr_comps$ZONE) & !is.na(out_obj$QUADRANT_GR))
			if(length(inds) > 0){
				out_obj$QUADRANT_GR[inds] = paste0(gr_comps$ZONE[inds],out_obj$QUADRANT_GR[inds])
			}
			
	} 
	
	# If precision is supplied determine if it matches value estimated from gridref
	if(!is.null(precision)){
		# Check that precision is either a vector of length gridref or a single value
		if( !(length(precision) == 1 | length(precision) == length(gridref)) ){
			stop("precision does not match gridref length, supply either single value or vector of same length as gridref")
		}
		# Find non-matching precisions
			# Find non-matching non padded gridrefs (i.e. ones that shouldn't be wrong)
			gr_inds = which(gr_comps$PRECISION != precision & gr_comps$PRECISION != 1000 & (precision != 2000 | precision != 5000) )
			# Find all non-matched precision values
			if(length(gr_inds) > 0){
				stop("Precision supplied does not match determined precision for", length(gr_inds), " grid refs")
			}
			# If estimated precision = 1000 and supplied prec = 2000 or 5000 then assume tetrad/quadrant padded values (e.g. BRC type data)
			# Padded tetrads
				# Note if tetrad grid ref and tetrad code in (C,H,K,L,M,N,P,S,X) then quadrant cannot be uniquely determined
				# Find grid refs matching this criteria
				gr_inds = which(gr_comps$PRECISION != precision & gr_comps$PRECISION == 1000 & precision == 2000 & grepl("^[[:alpha:]]{1,2}[[:digit:]]{2}[CHKLMNPSX]$",out_obj$TETRAD_GR))
				# Set quadrant to NA
				if(length(gr_inds) > 0){
					out_obj[gr_inds,"QUADRANT_GR"] = NA
				}
			# Padded Quadrant
				# For all padded quadrant codes tetrad cannot be uniquely determined
				gr_inds = which(gr_comps$PRECISION != precision & gr_comps$PRECISION == 1000 & precision == 5000)
				# Set quadrant to NA
				if(length(gr_inds) > 0){
					out_obj[gr_inds,"TETRAD_GR"] = NA	
				}
			
	}		
	
	# Build final output based on value(s) in prec_out
		if(is.null(prec_out)){
		  final_out = out_obj
		} else if (length(prec_out) == 1 & length(gridref) > 1) {
		  if(prec_out == 2000){
		    final_out = out_obj$TETRAD_GR
		  }
		  if(prec_out == 5000){
		    final_out = out_obj$QUADRANT_GR  
		  }
		} else {
		  # If there are more than 1 prec out then select the corresponding tetrad/quadrants
		  # Set all to NA first
		    final_out = rep(NA, nrow(out_obj))
		  # Find all entries where the output precision is to be tetrad and update them
		  inds = which(prec_out == 2000)
		  if(length(inds) > 0){
		    final_out[inds] = out_obj$TETRAD_GR[inds]  
		  }
		  # Find all entries where the output precision is to be quadrant and update them
		  inds = which(prec_out == 5000)
		  if(length(inds) > 0){
		    final_out[inds] = out_obj$QUADRANT_GR[inds]  
		  }
		}
  # Return final output
		return(final_out)
}
