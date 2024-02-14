#' @title Convert UTM to Miltary Grid Reference System (MGRS)
#' @description Takes one or more UTM coordinates and converts them to Miltary
#'   Grid Reference System (MGRS) grid references
#' 
#' @param utm_str A character vector containing the UTM coordinates. Note UTM
#'   coordinates must include MGRS band (see details for more information)
#' @param output_type A character vector specifying the desired option for the
#'   output format. Valid options are \code{"full_gr"}, \code{"split_gr"}, or
#'   \code{"atomised"} with \code{full_gr} being the default. See the Value
#'   section for more information on these options
#' 
#' @details The UTM character strings accepted by this function need to include
#'   the MGRS band and be in the format \code{ZoneBand Easting Northing} e.g.
#'   \code{30U 630855 5718493}. These aren't technically true UTM coordinates as
#'   bands aren't part of UTM but MGRS bands are sometimes used in relation to
#'   UTM in place of the traditional hemisphere code (\code{N} or \code{S}). The
#'   \href{https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system}{UTM
#'   wikipedia page} has more details. The functions in BRC map that output UTM
#'   coordinates should output the UTM strings in the required format, including
#'   the MGRS band.
#'   
#'   The Miltary Grid Reference System (MGRS) is a geocoordinate system used by
#'   NATO militaries. It is a system that covers the entire globe. A MMGRS grid
#'   reference is composed of several components; a grid zone designation,
#'   100,000 metre (100km) square indentifer, and a numerical location
#'   specifying the location in that 100km square. An example MGRS grid
#'   reference is 30UXC3085518493 where the grid zone is 30U, XC is the 100km
#'   square and the coordiantes 3085518493 refer to a point 30855m east and
#'   18493m west of the origin of that 100km square.
#'   
#'   The grid zones are areas where the 6 degree wide UTM longitudinal zones are
#'   intersected by latitudinal bands that are 8 degrees tall. These grid zones
#'   are desingated by concatinating the UTM zone number with the band letter. 
#'   The UTM zone numbers range from 1 to 60 while the band use letters running
#'   alphabetically from C in the South to X in the North (but excluding I and
#'   O), for example grid zone 30U which contains a large part of the UK. The 
#'   are within the grid zones are divided up into 100km squares (100km x
#'   100km), where the corners of the squares have UTM coordinates that are
#'   muitples of 100,000m. These squares are indentified within each zone via a 
#'   column letter (A-Z, excluding I and O) and a row letter (A-V, excluding I
#'   and O). For example the 100km square XC within grid zone 30U covers Oxford,
#'   UK. The exact way these row and column letters are layed out across the
#'   grid zones is relatively complex but if you are interested then the 
#'   \href{https://en.wikipedia.org/wiki/Military_Grid_Reference_System}{MGRS
#'   wikipedia page} contains more information. The numerical location given
#'   after the 100km square reference a point by giving the deviation east and
#'   north from the origin (lower left corner) of the square.
#'   
#'   The code for this function is modified version of javascript code created
#'   by Chris Vernes and published on his website 
#'   \href{http://www.movable-type.co.uk/scripts/latlong-utm-mgrs.html}{Moveable
#'   Type Scripts} under MIT licence for free use and adaption ( (c) Chris
#'   Veness 2014-2015 / MIT Licence). According to his websites Chris's code is
#'   based on Karney 2011 "Transverse Mercator with an accuracy of a few
#'   nanometers".
#'
#' @return This function can return either a character vector or a data.frame 
#'   depending on the value given to the \code{output_type} argument. In the 
#'   default case (\code{output_type = "full_gr"} a character vector is returned
#'   wwhere the each values contains a full MGRS grid references. When 
#'   \code{output_type = "split_gr"} the MGRS grid references are returned as a
#'   \code{data.frame} with the first column (GZD) containing the grid zone 
#'   designation and the second column (GRIDREF) containing the 100km grid 
#'   reference. The final output type \code{"atomised"} fully breaks down the 
#'   MGRS grid reference into its compoent parts and returns a data.frame with 
#'   columns for the Zone, Band, 100km square identifier, Easting and Northing.
#' @export
#' 
#' @author Colin Harrower
#'
#' @seealso \code{\link{mgrs2utm}}, \code{\link{gps_latlon2utm}}
#' @examples 
#' ## Example converting a UTM grid reference to MGRS
#'    utm2mgrs("30U 630855 5718493")
#'    
#' ## Example converting UTM to MGRS but this time outputing the MGRS as an
#' ## atomised compnonents in a data.frame
#'    utm2mgrs("30U 630855 5718493", output_type="atomised")
#'    
utm2mgrs = function(utm_str, output_type = "full_gr"){
	# Setup object to store output
		zone = rep(NA, length(utm_str))
		band = rep(NA, length(utm_str))
		easting = rep(NA, length(utm_str))
		northing = rep(NA, length(utm_str))
		l_e = rep(NA, length(utm_str))
		l_n = rep(NA, length(utm_str))
		no_dec = rep(NA, length(utm_str))
	
	# Define letters for e100km & n100km
		lets_e100km = LETTERS[-c(9,15)]
		lets_n100km = c(LETTERS[-c(9,15, 23:26)], LETTERS[c(6:8,10:14,16:22,1:5)])
	
	# Check UTM string matches expected pattern
	good_inds = which(grepl("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$",utm_str))
	if(length(good_inds) == 0){
		stop("None of the supplied strings were recognised as UTM coordinates")
	} else if(length(good_inds) != length(utm_str)){
		warning("One or more of the supplied strings were not recognised as UTM coordinates")
		utm_str = utm_str[good_inds]
	}

	# Split UTM str into components
		zone[good_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\1", utm_str[good_inds])
		band[good_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\2", utm_str[good_inds])
		easting[good_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\3", utm_str[good_inds])
		northing[good_inds] = gsub("^([[:digit:]]{1,2})[ ]?([[:alpha:]]{1})[ ]?([[:digit:]]*([.][[:digit:]]*)?)[ ,]{1,2}(([[:digit:]]*([.][[:digit:]]*)?))$","\\5", utm_str[good_inds])
		
		# Determine decimal precision of easting & northing values
			no_dec[good_inds] = pmax(nchar(gsub("^([[:digit:]]+)([.]([[:digit:]]*))?$", "\\3", easting[good_inds])), nchar(gsub("^([[:digit:]]+)([.]([[:digit:]]*))?$", "\\3", northing[good_inds])))	
	
	# Converting easting & northing to numeric vector
		easting = as.numeric(easting)
		northing = as.numeric(northing)
	
	# Determine easting & northing letters
		# Easting
		l_e = lets_e100km[((as.numeric(zone) - 1)%%3)*8 + floor(easting/1e5)]
		# Northing
		l_n = lets_n100km[ ((as.numeric(zone) - 1) %% 2)*20 + ((floor(northing/1e5) %% 20) + 1)]
		
	# Determine easting & northing within 100km square
		east_100km = easting %% 1e5
		north_100km = northing %% 1e5
	
	# Create output string
	if(tolower(output_type) == "full_gr"){
		ret_obj = sprintf("%s%s%s%s%05.0f%05.0f",zone,band,l_e,l_n,floor(east_100km),floor(north_100km))
		if(length(good_inds) < length(zone)){
			ret_obj[-good_inds] = NA
		}
	} else if(tolower(output_type) == "split_gr"){
		ret_obj = data.frame(GZD = paste(zone,band,sep=""), GRIDREF = sprintf("%s%s%05.0f%05.0f",l_e,l_n,floor(east_100km),floor(north_100km)))
		if(length(good_inds) < length(zone)){
			ret_obj[-good_inds,] = c(NA,NA)
		}
	} else if(tolower(output_type) == "atomised"){
		ret_obj = data.frame(ZONE = zone, BAND = band, SQ100KM_ID = paste(l_e,l_n, sep=""), EASTING = round(east_100km,no_dec), NORTHING = round(north_100km, no_dec))
		if(length(good_inds) < length(zone)){
			ret_obj$SQ100KM_ID[-good_inds] = NA
		}
	} else {
		stop("Unknown output type: valid options are 'full_gr', 'split_gr', or 'atomised'")
	}
	
	# Return output object
	return(ret_obj)
		
}