# MAIN function
# - takes variable number of dataset inputs
# - need to specify target frame (CRS defined by )
# - various optional arguments
#   'time' default is NA, sets startdata, enddate and resolution of aggregation for SpatialPointsData in days c("startdate","resolution in days")
geomerge <- function(...,target=NULL,time = NA,time.lag = TRUE, spat.lag = TRUE, zonal.fun = sum,assignment = 'max(area)',population.data = NA, point.agg = 'cnt', t_unit = 'days',silent=FALSE){
  
  all.inputs <- as.list(substitute(deparse(...)))[-1]
  if (length(all.inputs)>0){
    data.input <- sapply(1:length(all.inputs),function(x) class(all.inputs[[x]])=="name")
    inputs <- all.inputs[data.input]
  }else{
    stop('no inputs provided')
  }
  if (length(inputs)>0){
    data.names <- sapply(1:length(inputs),function(x) as.character(inputs[[x]]))
    inputs <- sapply(1:length(inputs),function(x) eval(inputs[[x]]))
    optional.inputs <- all.inputs[!data.input]
    if (length(optional.inputs>0)){
      optional.inputs <- sapply(1:length(optional.inputs),function(x) eval(optional.inputs[[x]]))
    }
  }else{
    stop('no dataset inputs provided')
  }
  
  if (silent){
    cat <-function(...){}
  }
  
  cat(' geomerge: Geospatial data integration.\n Karsten Donnay and Andrew Linke, 2017\n\n ATTENTION: Depending on the resolution and number of datasets, the merger may take some time!\n\n\n ')
  call <- match.call()
  if (!silent){
    print(call)
  }
  
  # INPUT CHECKS
  missing_arguments <- c()
  terminate <- FALSE
  
  # CHECK if target is missing
  if (is.null(target)){
    stop('\n required input target is missing')
  }else{
    # CHECK if target is projected, otherwise look for local CRS, abort if not given
    standard.CRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    if (standard.CRS@projargs != proj4string(target)){
      cat('\n NOTE: target not in global datum WGS84. Changing projection automatically but this may take a while... ')
      target <- spTransform(target,standard.CRS)
      cat(' Done.\n')
    }
  }
  # CHECK other input data format
  points.inputs <- 1
  agg.checked <- FALSE
  for (iter in 1:length(inputs)){
    agg.check <- point.agg
    num_points <- sum(sapply(1:length(inputs),function(x) class(inputs[[x]])=="SpatialPointsDataFrame"))
    if (class(inputs[[iter]])=="SpatialPointsDataFrame" & length(point.agg)>1 & !agg.checked){
      if (length(point.agg)!=num_points){
        missing_arguments <- append(missing_arguments,paste0('\n point.agg must be specified globally or as a vector of characters whose length matches the number of "SpatialPointsDataFrame" input arguments'))
        terminate <-TRUE
        agg.checked <- TRUE
      }else{
        agg.check <- point.agg[points.inputs]
        points.inputs <- points.inputs + 1
      }
    }
    # suppress warnings in later checks
    if (agg.checked){
      agg.check <- point.agg[1]
    }
    if (!(class(inputs[[iter]])=='SpatialPolygonsDataFrame' | class(inputs[[iter]])=='SpatialPointsDataFrame' | class(inputs[[iter]])=='RasterLayer')){
      if (class(inputs[[iter]])=='RasterStack'){
        missing_arguments <- append(missing_arguments,paste0('\n input ',data.names[iter],' is RasterStack, please input layers as separate RasterLayer'))
      }else{
        missing_arguments <- append(missing_arguments,paste0('\n input ',data.names[iter],' is not SpatialPolgyonsDataFrame, SpatialPointsDataFrame, or RasterLayer'))
      }
      terminate <-TRUE
    }
    if (class(inputs[[iter]])=='SpatialPolygonsDataFrame'){
      if(ncol(inputs[[iter]])!=1){
        missing_arguments <- append(missing_arguments,paste0('\n SpatialPolgyonsDataFrame ',data.names[iter],' must contain exactly one column with the variable of interest'))
        terminate <-TRUE
      }
      if (assignment%in%c('max(pop)','min(pop)','weighted(pop)') & class(population.data)!='RasterLayer'){
        missing_arguments <- append(missing_arguments,paste0('\n SpatialPolgyonsDataFrame ',data.names[iter],' is used with population-based assignment but population.data RasterLayer is missing or mis-specified'))
        terminate <-TRUE
      }
      if (assignment%in%c('weighted(area)','weighted(pop)') & !is.numeric(inputs[[iter]]@data[,1])){
        missing_arguments <- append(missing_arguments,paste0('\n not possible to use SpatialPolgyonsDataFrame ',data.names[iter],' with weighted assignment because variable is non-numeric'))
        terminate <-TRUE
      }
    }
    if (class(inputs[[iter]])=='SpatialPointsDataFrame'){
      if (length(names(inputs[[iter]])[names(inputs[[iter]])!='timestamp'])!=1){
        missing_arguments <- append(missing_arguments,paste0('\n SpatialPointsDataFrame ',data.names[iter],' must contain exactly one column with the variable of interest (and a "timestamp" column, if applicable)'))
        terminate <-TRUE
      }
      if (!(agg.check%in%c('sum','cnt'))){
        missing_arguments <- append(missing_arguments,paste0('\n argument point.agg must be either "cnt" or "sum"'))
        terminate <-TRUE
      }
      if (agg.check=='sum' & !is.numeric(inputs[[iter]]@data[,!(names(inputs[[iter]])%in%'timestamp')])){
        missing_arguments <- append(missing_arguments,paste0('\n SpatialPointsDataFrame ',data.names[iter],' must contain a numeric variable if using option point.agg="sum"'))
        terminate <-TRUE
      }
    }
    # CHECK that CRS of data is the same as target, if not transform data
    if (standard.CRS@projargs != proj4string(inputs[[iter]])){
      cat(paste0('\n NOTE: ',data.names[iter],' not in global datum WGS84. Changing projection but this may take a while... '))
      if (class(inputs[[iter]])=='RasterLayer'){
        inputs[[iter]] <- projectRaster(inputs[[iter]], crs=standard.CRS)
      }else{
        inputs[[iter]] <- spTransform(inputs[[iter]],standard.CRS)
      }
      cat('Done.\n')
    }
    if ((class(inputs[[iter]])=='SpatialPolygonsDataFrame' | class(inputs[[iter]])=='RasterLayer') & extent(inputs[[iter]]) < extent(target)){
      cat(paste0('\n NOTE: The extent of input ',data.names[iter],' is smaller than that of target. This might lead to NA values.\n'))
    }
  }
  # CHECK target format
  if (!class(target)=='SpatialPolygonsDataFrame'){
    missing_arguments <- append(missing_arguments,'\n target is not SpatialPolygonsDataFrame')
    terminate <-TRUE
  }
  # CHECK if static data or formats for non-static data are specified
  if (any(is.na(time))){
    cat('\n Running geomerge in static mode.')
  }else{
    cat('\n Running geomerge in dynamic mode.')
    timecheck <- lapply(1:2,function(x) try(strptime(time[x], format= "%Y-%m-%d")))
    if (length(time)!=3 | any(class(timecheck[[1]]) %in% c("try-error")) | any(is.na(timecheck[[1]])) | any(class(timecheck[[2]]) %in% c("try-error")) | any(is.na(timecheck[[2]])) | !is.character(time[3])){
      missing_arguments <- append(missing_arguments,'\n time input format must be c("start.date","end.date","time.step") and formatted as "YYYY-MM-DD" or "YYYY-MM-DD hh:mm:ss"')
      terminate <-TRUE
    }else if(time[1]==time[2]){
      missing_arguments <- append(missing_arguments,'\n start and end date in time are identical')
      terminate <-TRUE
    }
    classcheck <- sapply(1:length(inputs),function(x) class(inputs[[x]]))
    if (!any(classcheck %in% c("SpatialPointsDataFrame"))){
      missing_arguments <- append(missing_arguments,'\n Running geomerge in dynamic mode but no SpatialPointsDataFrame provided')
      terminate <-TRUE
    }else{
      timestamp <- FALSE
      for (iter in 1:length(inputs)){
        if (class(inputs[[iter]])=='SpatialPointsDataFrame' & any(names(inputs[[iter]]@data)%in%c('timestamp'))){
          timestamp <- TRUE
          datecheck <- try(strptime(inputs[[iter]]@data$timestamp, format= "%Y-%m-%d"))
          if (any(class(datecheck) %in% c("try-error")) || any(is.na(datecheck))){
            missing_arguments <- append(missing_arguments,'\n  timestamp (date format must be "YYYY-MM-DD" or "YYYY-MM-DD hh:mm:ss")')
            terminate <- TRUE
          }
        }
      }
      if (!timestamp){
        missing_arguments <- append(missing_arguments,'\n Running geomerge in dynamic mode but no "timestamp" in any SpatialPointsDataFrame provided')
        terminate <-TRUE
      }
    }
  }
  # CHECK zonal function input
  if (!class(zonal.fun)%in%c('function','methods')){
    missing_arguments <- append(missing_arguments,'\n zonal.fun mis-specified, has to be of class "function" or "methods"')
    terminate <-TRUE
  }
  # CHECK assignment format
  if (!(assignment%in%c('max(pop)','min(pop)','max(area)','min(area)','weighted(pop)','weighted(area)'))){
    missing_arguments <- append(missing_arguments,'\n assignment must be "max(area)", "min(area)", "weighted(area)", "max(pop)", "min(pop)" or "weighted(pop)"')
    terminate <-TRUE
  }
  # CHECK that population.data is projected to correct CRS
  if (class(population.data)=='RasterLayer'){
    if (standard.CRS@projargs != proj4string(population.data)){
      cat('\n NOTE: population.data not in global datum WGS84. Changing projection automatically but this may take a while...')
      population.data <- projectRaster(population.data, crs=standard.CRS)
      cat(' Done.')
    }
  }
  # CHECK t_unit format
  if (!(t_unit == 'secs'| t_unit=='mins' | t_unit=='hours' | t_unit=='days' | t_unit=='months' | t_unit=='years')){
    missing_arguments <- append(missing_arguments,'\n t_unit must be "secs", "mins", "hours", "days", "months or "years"')
    terminate <-TRUE
  }
  # ABORT if any check fails
  if (terminate){
    stop(missing_arguments)
  }# end if
  
  # EXECUTE otherwise
  if (!terminate){
    # CALCULATE first and second order neighborhood weights
    wghts <- geomerge.neighbor(target)
    
    # CALCULATE local areas for normalization and area weighted calculations
    outdata <- data.frame(FID=sapply(target@polygons, FUN=function(x) {slot(x, 'ID')}))
    outdata <- cbind(outdata,target@data)
    outdata$area <- sapply(target@polygons, function(x) sum(sapply(1:length(x@Polygons),function(y) areaPolygon(x@Polygons[[y]]@coords)))/1e6)
    
    # CLEAR data in target, add only ID column
    target@data <- data.frame(list(FID = sapply(target@polygons, function(x) {slot(x, 'ID')})))
    
    # MERGE datasets subsequently
    points.inputs <- 1
    for (iter in 1:length(inputs)){
      data <- inputs[[iter]]
      data.name = data.names[iter]
      if(class(data)=="SpatialPointsDataFrame" & length(point.agg)>1){
        paste.agg <- point.agg[points.inputs]
        points.inputs <- points.inputs + 1
      }else{
        paste.agg <- point.agg
      }
      cat(paste0('\n Dataset',iter,': ',data.name,' (',as.character(class(data)),')'))
      outdata <- geomerge.merge(data,data.name,target,standard.CRS,outdata,wghts,time,time.lag,spat.lag,zonal.fun,assignment,population.data,paste.agg,t_unit,silent,optional.inputs)
      cat(paste0(' \n Dataset ',data.name,' successfully merged to target.'))
    }
    
    # OUTPUT SpatialPolygonsDataFrame
    target@data <- outdata
    
    # PREPARE output
    params=list(time = time,time.lag = time.lag, spat.lag = spat.lag, zonal.fun = zonal.fun,
                assignment = assignment,population.data = population.data, point.agg = point.agg,
                t_unit = t_unit, silent=silent)
    out = list(data = target,inputData = setNames(as.list(inputs), data.names), parameters = params)
    class(out) <- "geomerge"
    
    # RETURN output
    cat('\n Completed!')
    return(out)
  }
}