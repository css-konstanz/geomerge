print.geomerge <- function(x, ...){
  
  # RETRIEVE names and number of datasets
  inputs <- names(x$inputData)
  datsets <- length(inputs)
  
  # CHECK input consistency
  if (!('period' %in% names(x$data))){
    time.lag <- FALSE
  }
  
  # CHECK which variables are numeric and not
  num <- c()
  non.num <- c()
  for (inpt in 1:datsets){
    # DISTINGUISH by class of input data
    if (class(x$inputData[[inpt]])=='SpatialPointsDataFrame'){
      if (is.numeric(x$data@data[,paste0(inputs[inpt],'.',x$parameters$point.agg)])){
        num <- c(num,inputs[inpt])
      }else{
        non.num <- c(non.num,inputs[inpt])
      }
    }else if (class(x$inputData[[inpt]])=='SpatialPolygonsDataFrame'){
      if (is.numeric(x$data@data[,inputs[inpt]])){
        num <- c(num,inputs[inpt])
      }else{
        non.num <- c(non.num,inputs[inpt])
      }
    }else if (class(x$inputData[[inpt]])=='RasterLayer'){
      num <- c(num,inputs[inpt])
    }
  }
  
  main = paste0("geomerge completed: ",datsets," datasets successfully integrated -")
  if (length(x$parameters$time)>1){
    main <- paste0(main,' run in dynamic mode, spatial panel was generated.\n\n')
  }else{
    main <- paste0(main,' run in static mode.\n\n')
  }
  num.message = paste0('The following ',length(num),' numerical variable(s) are available:','\n ',paste(num,collapse=", "),'\n\n')
  non.num.message = paste0('The following ',length(non.num),' non numerical variable(s) are available:','\n ',paste(non.num,collapse=", "),'\n\n')
  if (length(num)>0 & length(non.num)>0){
    message <- paste0(main,num.message,non.num.message)
  }else if (length(num)>0 & length(non.num)==0){
    message <- paste0(main,num.message)
  }else if (length(num)==0 & length(non.num)>0){
    message <- paste0(main,non.num.message)
  }
  if (x$parameters$spat.lag){
    message <- paste0(message,'First and second order spatial lag values available.\n')
  }
  if (length(x$parameters$time)>1 & x$parameters$time.lag){
    message <- paste0(message,'First and second order temporal lag values available.\n')
  }
  cat(message)
}