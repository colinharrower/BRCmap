neigh_smooth = function(gridref, smooth_radius = 1, square_list = NULL, sq_dimen = 10000, output_type = "neigh_only"){
	# NOTE at present gridrefs assumed to be 10km gridrefs however could adapt it to work at other grid sizes
	# If squares_list is null then return any square that can be validly converted to a gridreference in the same projection
	
	# Valid options for output type are "neigh_only" or "both"
	if(sq_dimen != 10000){
		stop("ERROR: At present function will only work for 10km grid references")
	}

	# Determine unique gridrefs
		data_gr = data.frame(GRIDREF = unique(gridref), stringsAsFactors = FALSE)

	# determine eastings and northings (and projection)
		data_gr[, c("EASTING","NORTHING", "PROJECTION")] = gr_let2num(data_gr$GRIDREF, centre = FALSE, return_projection = TRUE)

	# Filter list of all squares to remove those that are in gridref
	if(!is.null(square_list)){
		square_list = square_list[!square_list %in% data_gr$GRIDREF]
	}

	# For each gridref work out neighbours
		# Work out deviations from current easting & northing to calculate neighbours
		# Number of neighbours in radius
			no_neigh = (((smooth_radius*2)+1)^2)-1
		# Deviations in one axis round squres
			neigh_devs = (-smooth_radius:smooth_radius)*sq_dimen
	# All conversions factors required to get neighbours (number of rows is number of cells in neighbourhood including focal square
		neigh_conv = data.frame(EAST = rep(neigh_devs, each = (smooth_radius*2)+1 ), NORTH = rep(neigh_devs, times = (smooth_radius*2)+1))
	# Remove focal square from neigh_conv (where east and north deviations are 0 (also update row number)
		neigh_conv = neigh_conv[!(neigh_conv$EAST == 0 & neigh_conv$NORTH == 0),]
		rownames(neigh_conv) = NULL
	# Now extend neigh_conv to be same dimenstions as data_gr (so can add them together!)
		neigh_conv = data.frame(EAST = rep(neigh_conv$EAST, nrow(data_gr)), NORTH = rep(neigh_conv$NORTH, nrow(data_gr)))


	# Setup dataframe to hold data
	sq_neighbours = data.frame(
		SQUARE = rep(data_gr$GRIDREF, each = no_neigh), 
		EASTING = rep(data_gr$EASTING, each = no_neigh), 
		NORTHING = rep(data_gr$NORTHING, each = no_neigh),
		PROJECTION = rep(data_gr$PROJECTION, each = no_neigh),
		NEIGH_EASTING = NA,
		NEIGH_NORTHING = NA
	)

	# Calculate neighbour easting & northings
		sq_neighbours[,c("NEIGH_EASTING","NEIGH_NORTHING")] = sq_neighbours[,c("EASTING","NORTHING")] + neigh_conv

	# Remove any eastings/northings that are negative
		sq_neighbours = sq_neighbours[!sq_neighbours$NEIGH_EASTING < 0 | sq_neighbours$NEIGH_NORTH < 0,]

	# Get rid of duplicates and also information on focal cell as we only want neighbouring cells not which cell they are a neighbour too
		sq_neighbours = unique(sq_neighbours[,c("NEIGH_EASTING","NEIGH_NORTHING", "PROJECTION")])

	# Determine gridrefs from these eastings and northings
		sq_neighbours[,"NEIGH_GRIDREF"] = gr_num2let(sq_neighbours$NEIGH_EASTING, sq_neighbours$NEIGH_NORTHING, OSgrid = sq_neighbours$PROJECTION, keep_precision = FALSE)
		
		# Look for any neighbours where the returned gridref is NA (i.e. eastings & northings could not be changed to a valid gr) and remove
		rm_inds = which(is.na(sq_neighbours$NEIGH_GRIDREF))
		if(length(rm_inds) > 0){
			sq_neighbours = sq_neighbours[-rm_inds,]
		}	

	# From this only keep those in the full square list (if square_list supplied)
	if(!is.null(square_list)){
		sq_neighbours = sq_neighbours[sq_neighbours$NEIGH_GRIDREF %in% square_list,]
	}

	# Return gridrefshead
	if(tolower(output_type) == "neigh_only"){
		out_obj = sq_neighbours$NEIGH_GRIDREF
	} else if(tolower(output_type) == "both") {
		# Combined original provided gridrefs with filled neighbouring gridrefs
		out_obj = data.frame(GRIDREF = c(gridref, sq_neighbours$NEIGH_GRIDREF), OBSERVED = c(rep(1, length(gridref)), rep(0, length(sq_neighbours$NEIGH_GRIDREF))), stringsAsFactors = FALSE)
	} else {
		out_obj = sq_neighbours$NEIGH_GRIDREF
		cat("WARNING: output_value (\"", output_type, "\") not recognised, permitted values are \"neigh_only\" or \"both\".\nNOTE: Only neighbouring gridrefs have been returned\n", sep="")
	}

	# Return output
		return(out_obj)
}
