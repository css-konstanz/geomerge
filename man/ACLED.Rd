\name{ACLED}
\docType{data}
\alias{ACLED}
\title{Point dataset to illustrate the functionality of geomerge}
\description{
\emph{ACLED} conflict events for Nigeria in 2011 used as example for a \code{SpatialPointsDataFrame} available from http://www.acleddata.com/data. The dataset contains timestamped and geo-coded information on individual conflict events.
}
\details{
The original \emph{ACLED} "EVENT_DATE" column has been relabeled as "timestamp" in accordance with \code{geomerge} conventions.
}
\usage{data(geomerge)}
\format{A \code{SpatialPointsDataFrame} containing observations.}
\source{http://www.acleddata.com/data

Citation:
Clionadh Raleigh, Andrew Linke, Havard Hegre and Joakim Karlsen. (2010). "Introducing ACLED-Armed Conflict Location and Event Data." \emph{Journal of Peace Research} 47(5): 651-660.
}
\references{
Andrew M. Linke, Karsten Donnay. (2017). "Scale Variability Misclassification: The Impact of Spatial Resolution on Effect Estimates in the Geographic Analysis of Foreign Aid and Conflict." Paper presented at the \emph{International Studies Association Annual Meeting}, February 22-25 2017, Baltimore.
}
\author{
  Karsten Donnay and Andrew M. Linke
}