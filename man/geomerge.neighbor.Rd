\name{geomerge.neighbor}
\alias{geomerge.neighbor}
\title{
Returns first and second order spatial neighbors [Auxiliary Function]
}
\description{
Auxiliary function that uses functionality from \pkg{spdep} to retrieve first and second order neighbor weights.
}
\usage{
geomerge.neighbor(polygon_input)
}
\arguments{
  \item{polygon_input}{
  a \code{SpatialPolygonsDataFrame}.
}
}
\details{
The function serves as a wrapper for the \code{poly2nb}, \code{nblag} and \code{nb2listw} functions from \pkg{spdep} and returns first and second order neighbor weights using \code{zero.policy = TRUE}.
}
\value{
Returns a list of lists of neighbor weights named "wts1" and "wts2".
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