\name{geomerge.assign}
\alias{geomerge.assign}
\title{
Implements different assignment rules using SQL [Auxiliary Function]
}
\description{
Implements assignment of polygon values to the \code{target} frame using different assignment rules. For efficient performance implemented using SQL.
}
\usage{
geomerge.assign(polygon_input,target,assignment)
}
\arguments{
  \item{polygon_input}{
  input \code{SpatialPolygonsDataFrame} parsed from \code{geomerge} main function.
}
 \item{target}{
  \item{assignment}{
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