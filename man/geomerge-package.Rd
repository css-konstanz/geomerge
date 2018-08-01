\name{geomerge-package}
\alias{geomerge-package}
\docType{package}
\title{
geomerge: Geospatial Data Integration
}
\description{
\code{geomerge} is a framework for geospatial data integration that merges raster, spatial polygon, and (dynamic) spatial points data into a spatial (panel) data frame at any geographical resolution.
}
\details{
The \code{geomerge} function conducts a series of spatial joins for Geographic Information Systems (GIS) data. It integrates three of R's most commonly used GIS data classes  - polygons, points and rasters. With flexible options for assignment rules and including the calculation of spatial and temporal lags, \code{geomerge} returns a time series \code{SpatialPolygonsDataFrame} that users may import into any predictive statistical analysis. 
}
\note{
The spatial resolution of the input datasets and scope of the area covered by the integration routine will influence the runtime of \code{\link{geomerge}}. Depending on the inputs, integration may therefore require some time.
}
\author{
Karsten Donnay and Andrew M. Linke
}
\references{
Andrew M. Linke, Karsten Donnay. (2017). "Scale Variability Misclassification: The Impact of Spatial Resolution on Effect Estimates in the Geographic Analysis of Foreign Aid and Conflict." Paper presented at the \emph{International Studies Association Annual Meeting}, February 22-25 2017, Baltimore.
}
\seealso{
\code{\link{geomerge}}, \code{\link{geomerge.merge}},\code{\link{geomerge.neighbor}}, \code{\link{geomerge.assign}},\code{\link{generateGrid}}
}