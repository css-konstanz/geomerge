\name{summary.geomerge}
\alias{summary.geomerge}
\title{
Summary function for objects of class 'geomerge'.
}
\description{
Overloads the default \code{summary} for objects of class 'geomerge'.
}
\usage{
\method{summary}{geomerge}(object, ...)
}
\arguments{
  \item{object}{
  object of class \code{\link{geomerge}}.
}
  \item{\dots}{
  further arguments passed to or from other methods.
}
}
\value{
Returns a number of summary statistics describing the results of the \code{\link{geomerge}} integration, including how many variables were integrated, which of those are numerical vs. non numerical and whether spatially and/or temporally lagged values are available.
}
\references{
Andrew M. Linke, Karsten Donnay. (2017). "Scale Variability Misclassification: The Impact of Spatial Resolution on Effect Estimates in the Geographic Analysis of Foreign Aid and Conflict." Paper presented at the \emph{International Studies Association Annual Meeting}, February 22-25 2017, Baltimore.
}
\author{
  Karsten Donnay and Andrew M. Linke.
}
\seealso{
  \code{\link{geomerge}}
}