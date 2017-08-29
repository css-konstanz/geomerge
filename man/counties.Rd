\name{counties}
\docType{data}
\alias{counties}
\title{Polygon dataset to illustrate the functionality of geomerge}
\description{
Nigeria administrative units (ADM1) dataset used as example for the target \code{SpatialPolygonsDataFrame} data are merged. The dataset is available at http://www.arcgis.com/home/item.html?id=0e58995046b74254911c1dc0eb756fa4.
}
\details{
Note that the polygons in \code{counties} have been simplified to reduce the size of the \code{SpatialPolygonsDataFrame} used as integration target for easier illustration. This applies, in particular, to the Niger Delta region of Nigeria.
}
\usage{data(geomerge)}
\format{A \code{SpatialPolygonsDataFrame} containing observation and that data is merged to using \code{geomerge}.}
\source{http://www.arcgis.com/home/item.html?id=0e58995046b74254911c1dc0eb756fa4
}
\references{
Andrew M. Linke, Karsten Donnay. (2017). "Scale Variability Misclassification: The Impact of Spatial Resolution on Effect Estimates in the Geographic Analysis of Foreign Aid and Conflict." Paper presented at the \emph{International Studies Association Annual Meeting}, February 22-25 2017, Baltimore.
}
\author{
  Karsten Donnay and Andrew M. Linke
}