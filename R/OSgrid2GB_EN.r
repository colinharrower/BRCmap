#' Function to convert British & Irish grid references to eastings & northings on the GB ordnance survey system
#' @description This function will convert grid references typically used in Britain (OSGB, OSNI, UTM30) to eastings & northings on the Ordnance Survey for Great Britain projection. In the case of Irish & Channel Islands grid references they will be reprojected to place them onto the GB ordnance survey system (OSGB) to allow comparisons, simple distance measures or plotting onto maps in the GB projection. GB grid reference are simply converted to eastings and northings in their native projection.
#'
#' @param gridref A character vector containing the grid references to be converted
#' @param centre 	A logical variable determining whether the coordinates returned are for the grid centre or the bottom left corner. The default centre = TRUE will return the coordinates for the grid reference centre.
#' @param gr_prec A numerical vector determining the precision of the grid references. The default gr_prec = NULL will determine the precision directly from the grid reference
#'
#' @return The function returns a data.frame containing two columns EASTING and NORTHING which give the eastings and northing in meters from the OSGB origin.
#' 
#' @export
#'
#' @author Colin Harrower
#'
#' @seealso \code{\link{gr_let2num}}
#'
#' @examples
#' ## Example converting a range of UK grid references (OSGB, OSNI, & UTM30)
#' ## to eastings & northings on the OSGB grid
#' 
#' # Create a vector of grid refs to be converted to OSGB easting & northing
#' gridrefs <- c("SP49","NO58","WV55","H47","NN29","WV55","WA50","NT20","TL97"
#'               ,"NH33","SE25","SN22","SH87","SO00","N20","M85","T01","SX59","NY78")
#' 
#' # Convert the gridrefs to OSGB eastings & northings
#' OSgrid2GB_EN(gridrefs)


OSgrid2GB_EN <-
function(gridref, centre = TRUE, gr_prec = NULL){
	# Check that values have been passed
	if(length(gridref) > 0){
		# Determine number of unique gridrefs
			gr_len = length(gridref)
			
		# If gr_prec is null then determine precision from gridref, if not check whether gr_prec is 1 value (if so create vector of length gr_len) or a vector of length gr_len
			if(is.null(gr_prec)){
				gr_prec = det_gr_precision(gridref)
			} else if(length(gr_prec) == 1) {
				gr_prec = rep(gr_prec, gr_len)
			}
		
		# Determine eastings and northings of point in the centre of the grid square
			gr_points = gr_let2num(gridref, centre = centre)
					
		# Find Irish gridrefs & reproject
			ir_inds = which(grepl("(^[[:upper:]]{1}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1}[[:digit:]]{2,}$)", gridref) & !is.na(gr_points$EASTING))
			if(length(ir_inds) > 0){
				gr_points[ir_inds,] = OSgridReprojection(gr_points$EASTING[ir_inds], gr_points$NORTHING[ir_inds], org_grid = "OSNI", out_grid = "OSGB")
			}
			
		# Find channel islands gridrefs and convert & reproject
			# Find channel islands grid refs
				ci_inds = which(grepl("(^(WA|WV)[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^(WA|WV)[[:digit:]]{2,}$)", gridref) & !is.na(gr_points$EASTING))
			if(length(ci_inds) > 0){	
					gr_points[ci_inds,] = OSgridReprojection(gr_points$EASTING[ci_inds], gr_points$NORTHING[ci_inds], org_grid = "UTM30", out_grid = "OSGB")
			}
			
		# Return gr points
			return(gr_points)
	}
}
