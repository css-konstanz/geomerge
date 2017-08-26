summary.geomerge = function(object, ...){
  
  # RETRIEVE names and number of datasets
  inputs <- names(object$inputData)
  datsets <- length(inputs)
  
  # CHECK input consistency
  if (!('period' %in% names(object$data))){
    time.lag <- FALSE
    object$parameters$time.lag <- FALSE
  }
  
  # CHECK which variables are numeric and not
  num <- c()
  non.num <- c()
  for (inpt in 1:datsets){
    # DISTINGUISH by class of input data
    if (class(object$inputData[[inpt]])=='SpatialPointsDataFrame'){
      if (is.numeric(object$data@data[,paste0(inputs[inpt],'.',object$parameters$point.agg)])){
        num <- c(num,inputs[inpt])
      }else{
        non.num <- c(non.num,inputs[inpt])
      }
    }else if (class(object$inputData[[inpt]])=='SpatialPolygonsDataFrame'){
      if (is.numeric(object$data@data[,inputs[inpt]])){
        num <- c(num,inputs[inpt])
      }else{
        non.num <- c(non.num,inputs[inpt])
      }
    }else if (class(object$inputData[[inpt]])=='RasterLayer'){
      num <- c(num,inputs[inpt])
    }
  }
  
  main = paste0("geomerge completed: ",datsets," datasets successfully integrated -")
  if (length(object$parameters$time)>1){
    main <- paste0(main,' run in dynamic mode, spatial panel was generated.\n\n')
  }else{
    main <- paste0(main,' run in static mode.\n\n')
  }
  num.message = paste0('The following ',length(num),' numerical variable(s) are available:','\n ',paste(num,collapse=", "),'\n\n')
  non.num.message = paste0('The following ',length(non.num),' non numerical variable(s) are available:','\n ',paste(non.num,collapse=", ") ,'\n\n')
  if (length(num)>0 & length(non.num)>0){
    message <- paste0(main,num.message,non.num.message)
  }else if (length(num)>0 & length(non.num)==0){
    message <- paste0(main,num.message)
  }else if (length(num)==0 & length(non.num)>0){
    message <- paste0(main,non.num.message)
  }
  if (object$parameters$spat.lag){
    message <- paste0(message,'First and second order spatial lag values available.\n')
  }
  if (length(object$parameters$time)>1 & object$parameters$time.lag){
    message <- paste0(message,'First and second order temporal lag values available.\n')
  }
  cat(message)
  
  out_summary <- list(numerical=num,non.numerical=non.num,total.var=datsets,spatial.lags=object$parameters$spat.lag,temporal.lags=object$parameters$time.lag)
  
  invisible(out_summary)
}