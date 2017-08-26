\name{geomerge.merge}
\alias{geomerge.merge}
\title{
Performing dataset merger [Auxiliary Function]
}
\description{
Auxiliary function that performs the actual integration of the \code{target} frame with specified input data. The routine proceeds on dataset at a time.
}
\usage{
geomerge.merge(data,data.name,target,standard.CRS,outdata,wghts,
		time, time.lag,spat.lag,zonal.fun,assignment,
		point.agg, t_unit,silent,optional.inputs)
}
\arguments{
  \item{data}{
  input dataset. See Details in \code{\link{geomerge}}.
}
  \item{data.name}{
  name of input dataset
}
  \item{target}{
  \code{SpatialPolygonsDataFrame} representing desired units of analysis. See Details in \code{\link{geomerge}}.
}
  \item{standard.CRS}{
  Defines the CRS used. Default used in \code{geomerge} is WGS84.
}
  \item{outdata}{
  \code{data.frame} containing integrated data relative to the \code{SpatialPolygonsDataFrame} \code{target}.
}
 \item{wghts}{
  spatial weights calculated by \code{link{geomerge.neighbor}}. 
}
\item{time}{  specification of temporal window for temporal binning of point data by \code{c(start_date, end_date, interval_length)}. Default = \kbd{NA}. See Details in \code{\link{geomerge}}.}
    \item{time.lag}{
  Boolean indicating whether or not first and second order temporal lag values of all variables are returned. Default = \kbd{TRUE}.
}
  \item{spat.lag}{
  Boolean indicating whether or not first and second order spatial lag values of all variables are returned. Default = \kbd{TRUE}.
}
  \item{zonal.fun}{
  object of class \kbd{function} applied to values of \code{RasterLayer} or \code{SpatialLinesDataFrame} when generating zonal statistics for each target polygon. Default = \kbd{sum}. See Details in \code{\link{geomerge}}.
}
  \item{assignment}{
  identification of either population- or area-weighting assignment rules when handling \code{SpatialPolygonsDataFrame} joins to \code{target}. Default = \kbd{"area.assign"}. See Details in \code{\link{geomerge}}.
}
  \item{point.agg}{
  specification of aggregation format for data of type \code{SpatialPointsDataFrame}. Default = \kbd{"cnt"}. See Details in \code{\link{geomerge}}.
}
  \item{t_unit}{
  temporal unit used for dynamic point aggregation. Default = \kbd{"days"}.
}
 \item{silent}{
  Boolean switch to suppress any (non-critical) warnings and messages. Default = \kbd{FALSE}.
}
 \item{optional.inputs}{
  Any optional inputs compatible with the \code{extract} function in \pkg{raster}.
}
}
 
\details{
For details on different input parameters, please refer to the detailed documentation in \code{\link{geomerge}}. 
}

\value{
  Returns an object of class \code{data.frame} that contains all information from merger to \code{target} to be added to \code{target@data} in the main \code{geomerge} function. The documentation in \code{\link{geomerge}} provides a detailed overview over the columns returned and their naming conventions
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