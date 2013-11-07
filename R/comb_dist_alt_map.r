# Simply 
percentile_plot = function(x, percentile = seq(0,100,by=10), col = "red", bar_width = 10, show_lengend =  FALSE, ...){
	# Determine probabilities needed for quantile to determine values
		upp_per  = 0 + (percentile/2)
		low_per = 100 - (percentile/2)
	# If upp_per == low_per for any pair then remove
		rm_inds = which(upp_per == low_per)
		if(length(rm_inds) > 0){
			upp_per = upp_per[-rm_inds]
			low_per = low_per[-rm_inds]
		}
	# Combine vectors and convert from percent to proportion
		full_prob = sort(c(upp_per, low_per))/100
	# Get percentiles
		percentiles = quantile(x, probs = full_prob, na.rm = TRUE)
		n_per = length(percentiles)
	# Get colour spectrum
		band_cols = colorRampPalette(c("white",col))(n_per/2)
	# Setup initial plot
		plot(x = rep(1,length(x)), y = x, xlim = c(1,bar_width), type="n", xaxt = "n", xlab = NA, ylab = NA, bty = "n", ...)
	# Add first rectangle for all data (the only one with an outline)
	# Loop through other percentiles and add to plot
	for(i in 2:(n_per/2)){
		rect(1,percentiles[i],bar_width,percentiles[n_per-(i-1)], col=band_cols[i], border = NA)
	}
	# Add final all data band (so that outline border goes over bands (fill is transparent so can see other bands underneath)
	rect(1,percentiles[1],bar_width,percentiles[n_per-(1-1)])
	# Add legend if requested
	if(show_legend = TRUE){
	
	}
}

alt_map = function(gridref, sq_alts){
	
}