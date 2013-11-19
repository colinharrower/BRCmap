
# Simply 
percentile_plot = function(x, percentile = seq(10,100,by=10), col = "red", bar_width = 10, show_legend =  FALSE, ...){
	# Determine probabilities needed for quantile to determine values
		low_per  = (100-percentile)/2
		upp_per = 100 - low_per
	# Combine vectors and convert from percent to proportion
		full_prob = sort(c(upp_per, low_per))/100
	# Get percentiles
		percentiles = quantile(x, probs = full_prob, na.rm = TRUE)
		n_per = length(percentiles)
	# Get colour spectrum
		band_cols = colorRampPalette(c("white",col))(n_per/2)
	# Setup initial plot
		# If legend to be show need to extend axis to make room for legend
		if(show_legend){
			legend_width = bar_width
			xlim = c(1,bar_width + legend_width)
		} else {
			xlim = c(1,bar_width)
		}
		plot(x = rep(1,length(x)), y = x, xlim = xlim, type="n", xaxt = "n", xlab = NA, ylab = NA, bty = "n", ...)
	# Add first rectangle for all data (the only one with an outline)
	# Loop through other percentiles and add to plot
	for(i in 2:(n_per/2)){
		rect(1,percentiles[i],bar_width,percentiles[n_per-(i-1)], col=band_cols[i], border = NA)
	}
	# Add final all data band (so that outline border goes over bands (fill is transparent so can see other bands underneath)
	rect(1,percentiles[1],bar_width,percentiles[n_per-(1-1)])
	# Add legend if requested
	if(show_legend){
		leg_val = rev(paste(percentile,"%", sep=""))
		#par(family = "mono")
		legend("right",legend = format(leg_val, justify = "right"), fill = band_cols, border = c("black",rep(NA,length(band_cols)-1)), bty = "n")
	}
}

alt_map = function(gridref, sq_alts){
	
}