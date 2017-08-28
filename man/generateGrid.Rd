\name{generateGrid}
\alias{generateGrid}
\title{
Generates a grid in a given local CRS that is, by default, returned as SpatialPolygonsDataFrame in WGS84.
}
\description{
Implementation of a simple grid generation function that can be used to produce target SpatialPolygonDataFrames to be used in \code{geomerge}.
}
\usage{
generateGrid(extent, size, local.CRS, makeWGS84 = TRUE, silent = FALSE)
}
\arguments{
  \item{extent}{
  \code{SpatialPolygonsDataFrame} that defines the (minimum) extent of the grid to be generated.
}
 \item{size}{  size of the grid cells in m.}
  \item{local.CRS}{  definition of the local (projected) CRS the grid is spanned in. Has to be class \kbd{"CRS"}.
}
  \item{makeWGS84}{  Boolean switch indicating whether or not the grid is returned in WGS84. Default = \kbd{TRUE}.
}
  \item{silent}{  Boolean switch to suppress any (non-critical) warnings and messages. Default = \kbd{FALSE}.
}
}
\value{
Returns an object of \code{SpatialPolygonsDataFrame} that spans the grid with spatial resolution given by \kbd{size}.
}
\references{
Andrew M. Linke, Karsten Donnay. (2017). "Scale Variability Misclassification: The Impact of Spatial Resolution on Effect Estimates in the Geographic Analysis of Foreign Aid and Conflict." Paper presented at the \emph{International Studies Association Annual Meeting}, February 22-25 2017, Baltimore.
}
\author{
  Karsten Donnay and Andrew M. Linke.
}
\seealso{
\code{\link{geomerge-package}}, \code{\link{geomerge}}
}
\examples{
require(sp)
data(geomerge)

# Generate grid with 10 km cell size in local CRS for Nigeria
counties.grid <- generateGrid(counties,10000,local.CRS=CRS("+init=epsg:26391"),
                              silent=TRUE)
}