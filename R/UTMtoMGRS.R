# Adapted from Script created by Chris Verness ((c) Chris Veness 2014-2015 / MIT Licence)
# This script is provided on moveable type website (http://www.movable-type.co.uk/scripts/latlong-utm-mgrs.html) under MIT licence for
# free use and adaptation.
# It is Based on Karney 2011 ‘Transverse Mercator with an accuracy of a few nanometers’, building on 
# Krüger 1912 ‘Konforme Abbildung des Erdellipsoids in der Ebene’.



#' @title Convert UTM to Miltary Grid Reference System (MGRS)
#' @description Takes one or more UTM coordinates and converts them to Miltary Grid Reference System (MGRS) grid references
#'
#' @param utm_str A character vector containing the UTM coordinates.
#' Note UTM coordinates should include MGRS band (see details for more information)
#'
#' @return A character vector containing the MGRS grid references
#' @details The UTM character strings accepted by this function need to include the MGRS band and be in the 
#' format \code{ZoneBand Easting Northing} e.g. \code{30U 630855 5718493}. These aren't technically true
#' UTM coordinates as bands aren't part of UTM but MGRS bands are sometimes used in relation to UTM
#' in place of the traditional hemisphere code (\code{N} or \code{S}). The 
#' \href{https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system}{UTM wikipedia page} has more details. 
#' The functions in BRC map that output UTM coordinates should output the UTM strings in the required format, including 
#' the MGRS band.
#' 
#' The Miltary Grid Reference System (MGRS) is a geocoordinate system used by NATO militaries. It is a system that
#' covers the entire globe. A MMGRS grid reference is composed of several components; a grid zone designation, 100,000 
#' metre (100km) square indentifer, and a numerical location specifying the location in that 100km square. An 
#' example MGRS grid reference is 30UXC3085518493 where the grid zone is 30U, XC is the 100km square and
#' the coordiantes 3085518493 refer to a point 30855m east and 18493m west of the origin of that 100km square.
#' 
#' The grid zones are areas where the 6 degree wide UTM longitudinal zones are intersected by latitudinal bands that 
#' are 8 degrees tall. These grid zones are desingated by concatinating the UTM zone number with the band letter. 
#' The UTM zone numbers range from 1 to 60 while the band use letters running alphabetically from C in the South
#' to X in the North (but excluding I and O), for example grid zone 30U which contains a large part of the UK. The 
#' are within the grid zones are divided up into 100km squares (100km x 100km), where the corners of the squares
#' have UTM coordinates that are muitples of 100,000m. These squares are indentified within each zone via a
#' column letter (A-Z, excluding I and O) and a row letter (A-V, excluding I and O). For example the 100km square XC 
#' within grid zone 30U covers Oxford, UK. The exact way these row and column letters are layed out across 
#' the grid zones is relatively complex but if you are interested then the 
#' \href{https://en.wikipedia.org/wiki/Military_Grid_Reference_System}{MGRS wikipedia page} contains more information. 
#' The numerical location given after the 100km square reference a point by giving the deviation east and north 
#' from the origin (lower left corner) of the square.
#' 
#' The code for this function is modified version of javascript code created by Chris Vernes and published on his website
#' \href{http://www.movable-type.co.uk/scripts/latlong-utm-mgrs.html}{Moveable Type Scripts} under MIT licence 
#'  for free use and adaption ( (c) Chris Veness 2014-2015 / MIT Licence). According to his websites Chris's code 
#'  is based on Karney 2011 "Transverse Mercator with an accuracy of a few nanometers". 
#' 
#' @author Colin Harrower
#' 
#' @export
#'
#' @seealso \code{\link{MGRStoUTM}}
#' @examples 
#' ## Example converting a UTM grid reference to MGRS
#'    UTMtoMGRS("30U 630855 5718493")
#'    
UTMtoMGRS = function(utm_str){
	# Look for valid format UTM
		valid_utm = grepl("^([[:digit:]]{1,2})([[:alpha:]]{1})[ ]([[:digit:].]{1,})[ ]([[:digit:].]{1,})$",utm_str)
	# Set all non-valid to NA
	if(sum(valid_utm) != length(utm_str)){
		warning("utm_str contains ",sum(!valid_utm)," values that are not non-valid UTM strings")
		utm_str[!valid_utm] = NA
	}
	# Extract components from UTM
		# Extract zone from UTM string
			zone = as.numeric(gsub("^([[:digit:]]{1,2})([[:alpha:]]{1})[ ]([[:digit:].]{1,})[ ]([[:digit:].]{1,})$","\\1",utm_str))
			# Zones should be between 1 and 60 so add checking for this later
		# Band (note band technically not part of UTM coordinates but often given and R function in BRCmap that converts lat lon to UTM adds bands to coordinates)
			band = gsub("^([[:digit:]]{1,2})([[:alpha:]]{1})[ ]([[:digit:].]{1,})[ ]([[:digit:].]{1,})$","\\2",utm_str)
			# If required to calculate band then need to calculate lat lon then band = floor(lat/8)+10 (each band is 8 degrees long with 0degN being the 10th band)
		# Extract easting & northing
			e = as.numeric(gsub("^([[:digit:]]{1,2})([[:alpha:]]{1})[ ]([[:digit:].]{1,})[ ]([[:digit:].]{1,})$","\\3",utm_str))
			n = as.numeric(gsub("^([[:digit:]]{1,2})([[:alpha:]]{1})[ ]([[:digit:].]{1,})[ ]([[:digit:].]{1,})$","\\4",utm_str))
	# Calculate 100km letters
		# Easting letter (columns)
			# Eastings uses letters A-Z excluding I and O
			mgrs_e_lets = LETTERS[c(-9,-15)] 
			let_col = floor(e/100e3)
			e100k = mgrs_e_lets[(((zone-1) %% 3)*8)+let_col]
		# Northing letter (rows)
			# Modern MGRS using different letter patterns depending on whether zone is even or odd
			# Northing when odd uses A-V (excluding I and O)
				mgrs_n_lets_odd = mgrs_e_lets[1:20]
			# Northing when even uses F-V before wrapping around to A-E
				mgrs_n_lets_even = mgrs_e_lets[c(6:20,1:5)]
			# Determine which row
				let_row = (floor(n / 100e3) %% 20)+1
			# Now determine letter
				n100k = rep(NA,length(utm_str))
				# Even zones
				inds = which(zone %% 2 == 0)
				if(length(inds) > 0){
					n100k[inds] = mgrs_n_lets_even[let_row]
				}
				# Odd zones
				inds = which(zone %% 2 == 1)
				if(length(inds) > 0){
					n100k[inds] = mgrs_n_lets_odd[let_row]
				}
	# Determine easting & northings
		mgrs_e = floor(e) %% 100e3
		mgrs_n = floor(n) %% 100e3
	# Build out grid reference ensuring MGRS easting and northing include 5 digits each
		mgrs_out = rep(NA,length(utm_str))
		mgrs_out[valid_utm] = paste0(zone[valid_utm],band[valid_utm],e100k[valid_utm],n100k[valid_utm],sprintf("%05d%05d",mgrs_e[valid_utm], mgrs_n[valid_utm]))
	# Return the MGRS grid references
		return(mgrs_out)
}

# Wrapper function to convert from WGS84 latitude longitude values to MGRS values
#' @title Convert WGS84 Latitude & Longitude values to Miltary Grid Reference System (MGRS)
#' @description This function takes vectors of latitude & longitude values and converts them 
#' to Miltary Grid Reference System (MGRS) grid references
#'
#' @param lat a numerical vector containing latitude values
#' @param lon a numerical vector containing longitude values
#'
#' @details This function is essentially a wrapper function that uses other functions 
#' within \code{BRCmap} to do the conversion (listed in See Also section).The vectors supplied to \code{lat} and 
#' \code{lon} need to be paired, so that 1st values from each are the latitude and 
#' longitude coordinates for the 1st WGS84 point, the 2nd values from each vector are
#' the coordiantes for the 2nd WGS84 point, and so on.
#'
#' @return A character vector containing the MGRS grid references
#' @author Colin Harrower
#' 
#' @export
#' @seealso \code{\link{UTMtoMGRS}}
#' @examples
#' ## Converting WGS84 latitude and longitude values to MGRS grid references
#'    gps_latlon2mgrs(51.602382,-1.110548)
#'    
gps_latlon2mgrs = function(lat,lon){
	# Convert lat lon to UTM coords
		utm_str = gps_latlon2utm(lat,lon)
	# Convert UTM coords to MGRS 
		mgrs_gr = UTMtoMGRS(utm_str)
	# Return the mgrs values
		return(mgrs_gr)
}
