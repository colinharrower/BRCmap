\name{gr2sp_points}
\alias{gr2sp_points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert UK Grid References to SpatialPointsDataFrame object(s)
}
\description{
This function will take a vector of UK Grid References and convert this grid references into 1 or more \code{SpatialPointsDateFrame} objects where grid reference is represented by a point located at the centre of the grid squares. Associated attribute information can also be supplied alongside the grid referencess and this will be incorported into attribute table of the \code{SpatialPointsDateFrame}.
}
\usage{
gr2sp_points(gridref, gr_atts = NULL, sep_proj_layers = FALSE, out_proj4 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridref}{
  A character vector containing valid British Grid references (see details for more information)
  }
  \item{gr_atts}{
  A vector or data.frame containing attribute information associated with each grid references
  }
  \item{sep_proj_layers}{
  A logical value determining whether the value returned will be a single \code{SpatialPointsDataFrame} or a list containing multiple \code{SpatialPointsDataFrame} objects (see details for more information). The default \code{sep_proj_layers = FALSE} will return a single \code{SpatialPointsDataFrame} reprojecting the data where necesary based on the value in \code{out_proj4}
  }
  \item{out_proj4}{
   A character string containing the proj.4 string of the Coordinate Reference System (CRS) in which the ouptut \code{SpatialPointsDataFrame} will be returned reprojected the \code{SpatialPointsDataFrame} as necessary. The default \code{out_proj4 = NULL} will do 1 of two things depending on the value supplied to \code{sep_proj_layers}. In the first case it will reproject grid references to the most common CRS used in grid references supplied via \code{gridref} (where \code{sep_proj_layers = FALSE}) or alternatively return all grid references in their own native CRS using a seperate \code{SpatialPolygonsDataFrame} for each type of grid reference present (where \code{sep_proj_layers = TRUE}). Values supplied need to be a recognised PROJ.4 string see \code{\link{CRS}} function documentation in \code{sp} pacakge for more details.
  }
}
\details{
This fuction takes a vector of grid references and creates 1 or more \code{SpatialPointsDataFrame} objects, where each grid reference is represented by a point located at the centre of the grid square. The number of \code{SpatialPointsDataFrame} objects produced will depend upon the number of different grid reference types represented in the grid reference vector passed to \code{gridref} along with the values supplied to the function arguments \code{sep_proj_layers} and \code{out_proj4}. Where the \code{gridref} vector contains more than one type of grid reference (e.g. OSGB, OSNI, UTM30) the options are to reproject all grid references to use the same coodrinate reference system or to produce a seperate \code{SpatialPointsDataFrame} for each grid reference type.

Attribute information can be added to the data table contained within the \code{SpatialPointsDataFrame(s)} by passing a vector or data.frame containing corresponding atribute information to the \code{gr_atts} argument. If passing a vector then it needs to be of the same length and ordered so each values lines up with the  corresponding entry in \code{gridref}. If passing a \code{data.frame} then the number of rows must match the length of \code{gridref} with rows being ordered to match the corresponding entry in \code{gridref}.
}
\value{
The function will either return a \code{SpatialPointsDataFrame} or a list containing multiple \code{SpatialPointsDataFrame} depending on the values given to \code{sep_proj_layers} and \code{out_proj4} in the function arguments. See details section above for more information.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Harrower
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{CRS}}, \code{\link{SpatialPointsDataFrame}}
}
\examples{
## Example of using gr2sp_poly function to convert grid references to SpatialPolygonDataFrame objects and then add this polygons to a plot

  # Create a vector of grid references to be plotted
    gridrefs <- c("SP49","WV57","H40","NN29","NT20","TL97")
    
  # Convert to SpatialPolygonDF
    # NOTE that by default this will reproject Irish & Channel Island grid references to OSGB projection (as this is the most frequent grid ref type in supplied grid refs)
    gr_sp = gr2sp_poly(gridrefs)
    
  # Plot UK outline
    plot_GIS(UK, new.window = FALSE)
    
  # Add points to UK outline
    plot(gr_sp, add = TRUE, col="red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
