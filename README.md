# geomerge: Geospatial Data Integration

`geomerge` is a flexible framework for geospatial data integration that merges raster, spatial polygon, and (dynamic) spatial points data into a spatial (panel) data frame at any geographical resolution.

# Installation
[![CRAN](https://www.r-pkg.org/badges/version/geomerge)](https://cran.r-project.org/package=geomerge)
![Downloads](https://cranlogs.r-pkg.org/badges/geomerge)

The package can be installed through the CRAN repository.

```R
install.packages("geomerge")
```

Or the development version from Github

```R
# install.packages("devtools")
devtools::install_github("css-konstanz/geomerge")
```
# Usage

The following simple illustrations use a number of datasets for Nigeria in 2011 that are included with the package. A more extensive user guide will be added shortly.

```R
data(geomerge)

# 1) Simple static integration of polygon data
output <- geomerge(geoEPR,target=counties,silent=TRUE)
summary (output)

# 2) Static integration for point, polygon, raster data
ACLED.events <- ACLED[,names(ACLED)%in%c('EVENT_TYPE')]
AidData.projects <- AidData[,names(AidData)%in%c('project_id')]
output <- geomerge(ACLED.events,AidData.projects,geoEPR,gpw,na.rm=TRUE,target=counties)
summary(output)
plot(output)

# 3) Dynamic point data integration for numeric variables
ACLED.fatalities <- ACLED[,names(ACLED)%in%c('timestamp','FATALITIES')]
AidData.commitment <- AidData[,names(AidData)%in%c('timestamp','commitme_1')]
output <- geomerge(ACLED.fatalities,AidData.commitment,geoEPR,target=counties,time=c("2011-01-01", "2011-12-31","1"),t_unit='months',point.agg='sum')
summary(output)
plot(output)

# 4) Population weighted assignment
output <- geomerge(geoEPR,target=counties,assignment='max(pop)',population.data = gpw)
summary(output)
plot(output)

```

## Meta
- Please [report any issues or bugs](https://github.com/css-konstanz/geomerge/issues).
- License:  LGPL-3
- Get citation information for `geomerge` in R using `citation(package = 'geomerge')`
- CRAN: https://cran.r-project.org/package=geomerge
