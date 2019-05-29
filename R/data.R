#' @name UK_lowres
#' @title A dataset containing the coastline of the UK, Ireland and the Channel 
#'   Islands for uses as a background to species maps
#' @docType data
#' @usage UK_lowres
#' @author Colin Harrower
#'   
#' @description A dataset containing the coastline of the UK, Ireland and the 
#'   Channel Islands as a series of multipolygon features to be used as the base
#'   of maps produced by \code{BRCmap}. The coodrinates system used in the 
#'   dataset is the EPSG:27700 - OSGB 1936 / British National Grid - Projected 
#'   Coodinate References System (CRS), therefore the Irish data and Channel 
#'   Islands data have been reprojected. The data in this dataset, along with 
#'   all other coastline datasets included with BRCmap (see links below), are 
#'   derived from the \code{UK_VC} dataset where different datasets were derived
#'   by merging and/or simplifying the features to create the each dataset. This
#'   dataset is version of the \code{UK_VC} datasets where the features have 
#'   been simplified using GIS tools before merging the features till there is a
#'   single feature for each region (Great Britain, Ireland, Isle of Man, 
#'   Channel Islands) in the dataset. The \code{UK_VC} dataset used to produce 
#'   the other datasets included with this package was created using a couple of
#'   different open source datasets (see source section below)
#'   
#'   
#' @format A \code{SpatialPolygonsDataFrame} object containing of the polygons 
#'   required to plot the coastline of the UK, Ireland and Channel Islands. The 
#'   attribute data table associated with the polygons has the following fields 
#'   which can be used to plot subsets of the whole. \describe{ \item{REGION}{A 
#'   field containing a factor stating the region to which each feature is 
#'   attributed with four possible values; Great Britain, Ireland, Isle of Man, 
#'   and Channel Islands} \item{ID}{A field contain numerical IDs corresponding 
#'   to the four regions} }
#' @seealso \code{\link{UK_VC}}, \code{\link{UK_VC_lowres}}
#' @source
#'   \url{https://nbn.org.uk/tools-and-resources/nbn-toolbox/watsonian-vice-county-boundaries/}
#'    for Great Britain Watsonian Vice County Boundaries, 
#'   \url{https://github.com/SK53/Irish-Vice-Counties} for Irish Watsonian Vice
#'   County Boundaries, 
#'   \url{https://www.openstreetmap.org/export#map=10/49.3980/-2.2179} for 
#'   Channel Islands boundaries
#'   
#'   
#'   
"UK_lowres"