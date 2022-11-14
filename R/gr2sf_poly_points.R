#' Convert grid references to spatial simple features polygon object
#' @description Takes one or more grid references, and any associated data or
#'   attributes, and converts them to a simple features polygon object.
#'
#' @param gridref A character vector containing the grid references to be
#'   converted.
#' @param gr_atts A data.frame of associated attribute information or data to be
#'   attached to the simple features polygons. The rows of data in the
#'   data.frame must be correspond to the grid references given in
#'   \code{gridref}, e.g. the 1st row in \code{gr_atts} are the data for the 1st
#'   grid ref in \code{gridref}, the 2nd row the data for the 2nd grid ref and
#'   so on. The default \code{NULL} will result in a single attribute GRIDREF,
#'   containing the grid reference string, being associated with each polygon.
#' @param sep_proj_layers A logical variable determining what happens when
#'   \code{gridref} contains grid references from different coordinate reference
#'   systems (CRS) and no output CRS is specified (when \code{crs = NULL}). see
#'   \code{details} for more information.
#' @param crs A valid coordiante reference system reference that will be
#'   correctly interpreted by \code{\link[sf]{st_crs}}.
#'
#' @return This function will return either a single simple features polygon
#'   object or a list of simple features polygon objects. The output will depend
#'   on the grid reference type(s) of the grid references supplied to
#'   \code{gridref}, whether a Coordinate Reference system is supplied to
#'   \code{crs}, and or whether \code{sep_proj_layers} is \code{TRUE} or
#'   code{FALSE}
#'
#' @details Each grid reference in \code{gridref} will be converted to a polygon
#'   in the simple features object. This means that if \code{gridref} contains
#'   duplicated grid references multiple overlapping polygons will be present in
#'   the final simple features object. This may be desirable if the attribute
#'   information attached to each instance of the same grid references is
#'   different, but may also be undesirable in some situations. If you want a
#'   single polygon for each grid reference then you will need to process your
#'   grid reference and attribute data first to ensure there is only a single
#'   entry for each gird reference.
#'
#'   When \code{sep_project_layers = FALSE} a single simple feature polygon
#'   object will be returned using either the CRS for the grid reference system
#'   most frequently represented in \code{gridref} or the CRS specified by
#'   \code{crs} when \code{crs} is not \code{NULL}. When
#'   \code{sep_project_layers = TRUE} a list of simple features will be returned
#'   with each element containing a simple feature polygon containing all grid
#'   references in \code{gridref} coming a single grid ref system (e.g. OSGB,
#'   OSI, UTM30), and using the corresponding crs, with the number of elements
#'   in the list depending on the number of grid references types present in
#'   \code{gridref}.
#' @export
#' @author Colin Harrower
#' @seealso \code{\link{gr2sf_points}}, \code{\link{gr2sp_poly}}
#'
#' @examples
#'
#' ## Example converting a a series of grid references to a simple features polygon object
#'  # Create a vector of grid references
#'    gridrefs <- c("SP49","WV57","H40","NN29","NT20","TL97")
#'
#'  # Create a data.frame of associated data
#'    gr_attributes = data.frame(site = paste(site,1:length(gridrefs)), VC = c(55,113,230,97,72,26))
#'
#'  # convert grid refs to simple features object
#'    gr_sf = gr2sf_poly(gridrefs, gr_atts = gr_attributes)
#'    
gr2sf_poly = function(gridref, gr_atts = NULL, sep_proj_layers = FALSE, crs = NULL){
  require(sf)
  
  # If gr_atts is supplied then check that the length/nrows matches gridref
  if(!is.null(gr_atts)){
    # if gr_atts is a vector then convert to a 1 column data.frame
    if(is.vector(gr_atts)){
      gr_atts = data.frame(gr_atts)
    }
    # Check the lengths
    if(length(gridref) != nrow(gr_atts)){
      stop("Supplied attributes info (gr_atts) does not match the length of the grid ref vector")
    }
  }
  
  # Standrdise gridrefs and check that all gridrefs are valid
    temp = fmt_gridref(gridref)
  # find any non-valid grid refs
    bad_gr = which(!is.na(gridref) & is.na(temp))
    if(length(bad_gr) > 0){
      # if non-valid found then warn
      warning(length(bad_gr),"non-valid grid refs found and excluded")
    }
    gridref = temp
  
  # Determine number of unique gridrefs
  gr_len = length(gridref)
  
  # Determine the system/country from which the grid ref originates
  gr_proj = gr_det_country(gridref)
  if(any(!gr_proj[!is.na(gridref)] %in% c("OSGB","OSNI","UTM30"))){
    stop("gridref vector contains values that are not a recognised grid reference format")
  }
  
  # Process grid refs and create determine corner coordinates in native eastings & northings 
  # Determine number of grid ref types in dataset
  gr_tp = unique(na.omit(gr_proj))
  
  # Determine which gr_tp has the most records
  if(is.null(crs) & length(gr_tp) > 1 & !sep_proj_layers){
    cnt_tp = tapply(gr_proj, gr_proj, length)
    max_tp = names(cnt_tp)[which.max(cnt_tp)]
    if(max_tp == "OSGB"){
      crs = "EPSG:27700"
    } else if(max_tp == "OSNI"){
      crs = "EPSG:29902"
    } else if(max_tp == "UTM30"){
      crs = "EPSG:32630"
    } else {
      # If grid ref type with most records is unrecognised (or NA) then set to OSGB
      crs = "EPSG:27700"
    }
    rm(cnt_tp, max_tp)
  }	
  
  # Create a object to store the output values
  sf_lt = vector("list",length(gr_tp))
  
  # Loop through grid ref types in dataset and create spatial objects
  for(i in 1:length(gr_tp)){	
    inds = which(gr_proj == gr_tp[i])
    # Determine length of gridref vector
    gr_len = length(inds)
    
    # Determine grid ref precision
    gr_prec = det_gr_precision(gridref[inds])
    
    # Setup dataframe to hold data
    #gr_poly = matrix(nrow = gr_len*5,ncol=2,byrow=TRUE, dimnames = )
    gr_poly = data.frame(EASTING = rep(NA,gr_len*5), NORTHING = rep(NA,gr_len*5))
    
    # Determine origin of square and asign one value to every 5th row
    gr_poly[seq(1,by=5, length.out=gr_len),] = gr_let2num(gridref[inds])
    
    # Determine other three corners and assign to every fith row in data.frame (i.e. 2nd, 7th for TL corner, 3rd, 8th for TR, etc)
    # Top Left
    gr_poly[seq(2,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(0,gr_len),gr_prec)
    # Top Right
    gr_poly[seq(3,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(gr_prec,gr_prec)
    # Bottom Right
    gr_poly[seq(4,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(gr_prec, rep(0,gr_len))
    # Finally back to bottom left
    gr_poly[seq(5,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),]
    
    
    # set relevant CRS for current grid ref type
    if(gr_tp[i] == "OSGB"){
      tp_crs = "EPSG:27700"
    } else if(gr_tp[i] == "OSNI"){
      tp_crs = "EPSG:29902"
    } else if(gr_tp[i] == "UTM30"){
      tp_crs = "EPSG:32630"
    } else {
      stop("gridref vector contains values that are not a recognised grid reference format (see help for details)")
    }
    
    # Determine row inds for each polygon
    poly_no = rep(1:gr_len, each = 5)
    # Convert this matrix into a list of individual matrices (one for each gridref)
    pol_lt = lapply(by(gr_poly,poly_no,list),as.matrix)
    # Create a sf geomemtry column with polygon simple features and relevenant crs
    sp_pols = lapply(pol_lt,function(x){sf::st_polygon(list(x))})
    sp_sfc = sf::st_sfc(sp_pols,crs = tp_crs)
    rm(gr_poly, poly_no, pol_lt, sp_pols)
    # Now bind with attribute data and create full simple feature object
      # Create data.frame to associate with features
      sp_df = data.frame(GRIDREF = gridref[inds])
      if(!is.null(gr_atts)){
        sp_df[,names(gr_atts)] = gr_atts[inds,]  # TODO: Need to check that length of atts and gridrefs match at start of script
      }
    row.names(sp_df) = NULL
    sp_sf = sf::st_sf(sp_df,geometry = sp_sfc)
    rm(sp_df)	
    # Test whether CRS argument is not NULL (i.e. it was supplied or either sep_proj_layers = FALSE or there is only 1 grid ref type present)		
    if(!is.null(crs)){
      # Test whether CRS of current gr_tp matches output CRS if not will need to reproject it
      if(sf::st_crs(sp_sf) != sf::st_crs(crs)){
        sp_sf = sf::st_transform(sp_sf,crs = crs)
      }
    }
    
    # Add to temporary list object created to hold output for each gr_tp
    sf_lt[[i]] = sp_sf
  }
  
  # If returning seperate layers for each CRS then return sf_lt otherwise collapse the list
  if(sep_proj_layers){
    out_obj = sf_lt
  } else {
    out_obj = do.call("rbind",sf_lt)
  }
  
  # Return output object
  return(out_obj)
}