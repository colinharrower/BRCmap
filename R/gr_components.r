	gr_components <- function(gridref, output_col = NULL){

		# Convert letters to uppercase
			gridref = toupper(gridref)
			
		# Set up variable to store output
			gr_comps = data.frame(GRIDREF = gridref, VALID_GR = NA, PRECISION = NA, CHARS = NA, DIGITS = NA, DIGITS_EAST = NA, DIGITS_NORTH = NA, TETRAD = NA, QUADRANT = NA, ZONE = NA, BAND = NA)
			
		# Check values for output_col if supplied
		if(!is.null(output_col)){	
			if(!all(toupper(output_col) %in% names(gr_comps))){
				stop("Supplied output column name not recognised, valid values are:\n\t", paste(shQuote(names(gr_comps)), collapse = ", "))
			}
		}
		
		# Find valid gridrefs
			v_inds = which(grepl("^(([[:digit:]]{1,2}[[:upper:]]{1})?(([[:upper:]]{1,2})([[:digit:]]{2,})))(([[:upper:]]?)|([[:upper:]]{2}))?$", gsub("[[:space:][:cntrl:][:punct:]]", "", gridref)) & nchar(gsub("^(([[:digit:]]{1,2}[[:upper:]]{1})?(([[:upper:]]{1,2})([[:digit:]]{2,})))(([[:upper:]]?)|([[:upper:]]{2}))?$", "\\5", gsub("[[:space:][:cntrl:][:punct:]]", "", gridref))) %% 2 == 0)
			
		if(length(v_inds) > 0){
		
		# Update valid gridrefs to data.frame
			gr_comps[v_inds,"VALID_GR"] = gsub("[[:space:][:cntrl:][:punct:]]", "", gridref[v_inds])
			
		# Split into components
			# Characters
			gr_comps[v_inds,"CHARS"] = gsub("^(([[:digit:]]{1,2}[[:upper:]]{1})?(([[:upper:]]{1,2})([[:digit:]]{2,})))(([[:upper:]]?)|([[:upper:]]{2}))?$", "\\4", gr_comps$VALID_GR[v_inds])
			# Digits
			gr_comps[v_inds,"DIGITS"] = gsub("^(([[:digit:]]{1,2}[[:upper:]]{1})?(([[:upper:]]{1,2})([[:digit:]]{2,})))(([[:upper:]]?)|([[:upper:]]{2}))?$", "\\5", gr_comps$VALID_GR[v_inds])
				# split digits into east and north
					len_digit = nchar(gr_comps$DIGITS[v_inds])
					gr_comps[v_inds, "DIGITS_EAST"] = substr(gr_comps$DIGITS[v_inds], 1, len_digit/2)
					gr_comps[v_inds, "DIGITS_NORTH"] = substr(gr_comps$DIGITS[v_inds], (len_digit/2)+1, len_digit)
				# Determine precision based on digits
					gr_comps[v_inds,"PRECISION"] = 10^5 / 10^(len_digit/2)
			# Tetrad
			gr_comps[v_inds,"TETRAD"] = gsub("^(([[:digit:]]{1,2}[[:upper:]]{1})?(([[:upper:]]{1,2})([[:digit:]]{2})))(([[:upper:]]?)|([[:upper:]]{2}))?$", "\\7", gr_comps$VALID_GR[v_inds])
				# Find non-valid values in gr_tet and replace with NA
				na_inds = which(!gr_comps$TETRAD %in% LETTERS[-15])
				if(length(na_inds) > 0){
					gr_comps[na_inds,"TETRAD"] = NA
				}
			# Quadrant
			gr_comps[v_inds,"QUADRANT"] = gsub("^(([[:digit:]]{1,2}[[:upper:]]{1})?(([[:upper:]]{1,2})([[:digit:]]{2})))(([[:upper:]]?)|([[:upper:]]{2}))?$", "\\8", gr_comps$VALID_GR[v_inds])
				# Find non-valid values in gr_tet and replace with NA
				na_inds = which(!gr_comps$QUADRANT %in% c("NW","NE","SW","SE"))
				if(length(na_inds) > 0){
					gr_comps[na_inds,"QUADRANT"] = NA
				}
				
			# Modify precision if tetrad or quadrant is not null
				# Tetrad
				gr_comps[grepl("^([[:digit:]]{1,2}[[:upper:]]{1})?[[:upper:]]{1,2}[[:digit:]]{2}[[:upper:]]{1,2}$", gr_comps$VALID_GR) & !is.na(gr_comps$TETRAD),"PRECISION"] = 2000
				# quadrant
				gr_comps[grepl("^([[:digit:]]{1,2}[[:upper:]]{1})?[[:upper:]]{1,2}[[:digit:]]{2}[[:upper:]]{1,2}$", gr_comps$VALID_GR) & !is.na(gr_comps$QUADRANT),"PRECISION"] = 5000
			
			# Modify valid_gr to remove any gridrefs that had non-valid tetrad/quadrant codes
				na_inds = which(grepl("^[[:upper:]]{1,2}[[:digit:]]{2,}[[:upper:]]{1,2}$", gr_comps$VALID_GR) & is.na(gr_comps$TETRAD) & is.na(gr_comps$QUADRANT))
				if(length(na_inds) > 0){
					gr_comps[na_inds,-1] = NA
				}
				
			# Zone (for MGRS gridrefs only)
			gr_comps[v_inds, "ZONE"] = gsub("^$",NA, gsub("^((([[:digit:]]{1,2})([[:upper:]]{1}))?(([[:upper:]]{1,2})([[:digit:]]{2,})))(([[:upper:]]?)|([[:upper:]]{2}))?$", "\\3", gr_comps$VALID_GR[v_inds])) # First gsub used to replace blank strings (i.e. where no zone found in str) with NA values
			gr_comps[v_inds, "BAND"] = gsub("^$",NA, gsub("^((([[:digit:]]{1,2})([[:upper:]]{1}))?(([[:upper:]]{1,2})([[:digit:]]{2,})))(([[:upper:]]?)|([[:upper:]]{2}))?$", "\\4", gr_comps$VALID_GR[v_inds])) # First gsub used to replace blank strings (i.e. where no zone found in str) with NA values
		}	

		
		
		# If output_col is null then output all columns otherwise only output requested columns
		if(is.null(output_col)){
			out_obj = gr_comps
		} else {
			out_obj = gr_comps[,toupper(output_col)]
		}
		
		# Return output object
		return(out_obj)
	}