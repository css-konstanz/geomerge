\name{geomerge.assign}
\alias{geomerge.assign}
\title{
Implements different assignment rules using SQL [Auxiliary Function]
}
\description{
Implements assignment of polygon values to the \code{target} frame using different assignment rules. For efficient performance implemented using SQL.
}
\usage{
geomerge.assign(polygon_input,target,assignment,population.data,optional.inputs)
}
\arguments{
  \item{polygon_input}{
  input \code{SpatialPolygonsDataFrame} parsed from \code{geomerge} main function.
}
 \item{target}{  \code{SpatialPolygonsDataFrame} representing desired units of analysis. See Details of \code{\link{geomerge}}.}
  \item{assignment}{  identification of either population- or area-weighting assignment rules when handling \code{SpatialPolygonsDataFrame} joins to \code{target}. Default = \kbd{"max(area)"}. See Details in \code{\link{geomerge}}.
}
 \item{population.data}{  specifies data used for weighting if a population-based \code{assignment} rule is selected. See Details in \code{\link{geomerge}}.}
 \item{optional.inputs}{
  Any optional inputs compatible with the \code{extract} function in \pkg{raster}.
}
}
\details{
For details on different input parameters, please refer to the detailed documentation in \code{\link{geomerge}}.
}
\value{
Returns an object of class \code{data.frame} that contains the column from \code{input}, after proper assignment, that is to be added to \code{target@data}.
}
\references{
Andrew M. Linke, Karsten Donnay. (2017). "Scale Variability Misclassification: The Impact of Spatial Resolution on Effect Estimates in the Geographic Analysis of Foreign Aid and Conflict." Paper presented at the \emph{International Studies Association Annual Meeting}, February 22-25 2017, Baltimore.
}
\author{
  Karsten Donnay and Andrew M. Linke.
}
\seealso{
\code{\link{geomerge-package}}, \code{\link{geomerge}}, \code{\link{generateGrid}}
}