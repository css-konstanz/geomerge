\name{AidData}
\docType{data}
\alias{AidData}
\title{Point dataset to illustrate the functionality of geomerge}
\description{
\emph{AidData} aid project locations for projects in Nigeria with start date in 2011 used as example for a \code{SpatialPointsDataFrame}. The dataset is available from http://aiddata.org. The dataset contains timestamped and geo-coded information on individual aid projects.
}
\details{
The original \emph{AidData} "start_date" column has been relabeled as "timestamp" in accordance with \code{geomerge} conventions.
}
\usage{data(geomerge)}
\format{A \code{SpatialPointsDataFrame} containing observations.}
\source{http://aiddata.org

Citation:
Michael J. Tierney, Daniel L. Nielson, Darren G. Hawkins, J. Timmons Roberts, Michael G. Findley, Ryan M. Powers, Bradley Parks, Sven E. Wilson, and Robert L. Hicks. (2011). "More Dollars than Sense: Refining Our Knowledge of Development Finance Using AidData." \emph{World Development} 39(11): 1891-1906.
}
\references{
Andrew M. Linke, Karsten Donnay. (2017). "Scale Variability Misclassification: The Impact of Spatial Resolution on Effect Estimates in the Geographic Analysis of Foreign Aid and Conflict." Paper presented at the \emph{International Studies Association Annual Meeting}, February 22-25 2017, Baltimore.
}
\author{
  Karsten Donnay and Andrew M. Linke
}