\name{CI_outline}
\alias{CI_outline}
\docType{data}
\title{
The outline of the Channel Islands
}
\description{
A dataset containing the outline of the Channel Islands for plotting on maps. The outline is stored as a series of grid references that when plotted in order create the outline.
}
\usage{data(CI_outline)}
\format{
  The dataset is a character vector containg the grid references of points along the outline that when plotted using polygon will form the outline of the Channel Islands. The vector includes "" values that are used to seperate the distinct polygons that comprising the Channel Islands outline.
}
\details{
In general this dataset will be used through the function \code{add_channel_isles} which add the Channel Islands to the current plot. Note that this dataset will most likely be completely replaced at some point by the version of the Channel Islands included in the UK shapefile supplied as part of the package, however at present having difficulty repositioning shape files so this dataset is needed to create Channel Islands Insert box. 
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
\code{\link{add_channel_isles}}
}
\examples{
## Load in data and then produce uk map with Channel Islands insert box
  # Load in data
    data(CI_outline)
    data(UK)
  # Plot Britain and Ireland outlines (exc. Channel Islands)
    plot_GIS(UK[c("britain","ireland")])
  # Add insert box with Channel Islands
    add_channel_isles(CI_outline)

}
\keyword{datasets}
