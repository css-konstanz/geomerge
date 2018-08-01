\name{plot.geomerge}
\alias{plot.geomerge}
\title{
Plot function for objects of class 'geomerge'.
}
\description{
Overloads the default \code{plot} for objects of class 'geomerge'.
}
\note{
\code{plot} for objects of class 'geomerge' relies in many core aspects of its functionality on \pkg{ggplot2}. If the target \code{SpatialPolygonsDataFrame} is very large it may reach or exceed the limits of what the plotting functionality from \pkg{ggplot2} can handle and \code{plot} may be very slow or even stall.
}
\usage{
\method{plot}{geomerge}(x, ...)
}
\arguments{
  \item{x}{
  object of class \code{\link{geomerge}}.
}
  \item{\dots}{
  further optional arguments.
}
}
\details{
Returns a series of maps that visualizes numeric variables produced by \code{geomerge}. It returns a map for each unique numeric variable including first order spatially and temporally lagged values if \code{spat.lag=TRUE} and \code{time.lag=TRUE} when running \code{geomerge}. For spatial panels, the function by default returns values for the last period.

Five optional arguments that are specific to this plotting function can be provided. The first is \kbd{period}, a numeric input that allows to specify a specific period to be plotted. \kbd{inputs} must be a sequence of character strings specifying select variables to be plotted only. These have to have been merged (with the same name) in \code{geomerge}. \kbd{time.lag} and \kbd{spat.lag} override the boolean values parsed automatically from the result of \code{geomerge}. They are mainly meant to switch off plotting of spatial and temporal lags as they are ignored if these lags were not generated in the first place. The last argument is \kbd{ncol}, a numeric input, which allows to specify the width of the panel of plotted maps. By default, always 2 maps are shown side-by-side.

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