det_gr_precision <- function(
	gridref
){
	# Convert letters to uppercase
		gridref = toupper(gridref)
		
	# Set up variable to store output
		prec_out = rep(NA,length(gridref))
	
	# Find valid gridrefs
		v_inds = which(grepl("(^[[:upper:]]{1,2}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1,2}[[:digit:]]{2,}$)", gridref) & nchar(gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", "\\3", gridref)) %% 2 == 0)
		
	# Split into components
		gr_char = gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", "\\2", gridref[v_inds])
		gr_digits = gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", "\\3", gridref[v_inds])
		gr_tet = gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", "\\5", gridref[v_inds])
		gr_quad = gsub("^(([[:upper:]]{1,2})([[:digit:]]{2,}))(([[:upper:]]?)|([[:upper:]]{2}))$", "\\6", gridref[v_inds])
	
	# Determine number of digits pairs
		n_pairs = nchar(gr_digits)/2
		
	# Determine precison based on gr
		gr_prec = 10^5 / 10^n_pairs
		
	# If gr_tet contains valid letter then ignore precision based on gridref length and assign 2000
		gr_prec[gr_tet %in% LETTERS[-15]] = 2000
		# If not valid tetrad code in gr_tet then set to NA (i.e. O is not a valid tetrad code)
		gr_prec[!gr_tet %in% LETTERS[-15] & gr_tet != ""] = NA
		
	# If gr_quad contains valid letter then ignore precision based on gridref length and assign 5000
		gr_prec[gr_quad %in% c("NW","NE","SW","SE")] = 5000
		# If not valid quadrant code in gr_tet then set to NA
		gr_prec[!gr_quad %in% c("NW","NE","SW","SE") & gr_quad != ""] = NA
		
	# Write gr_prec values to output variable
		prec_out[v_inds] = gr_prec
	
	# Return output variable
	return(prec_out)
}