#### function implementing different assignment rules using sql
# assumes that target has ID field (which it has)
# assumes that target has area (which is has)
geomerge.assign<- function(polygon_input,target,assignment){
  pi <- intersect(polygon_input,target) #much larger (N of rows) SPDF with each polygon (where overlap exists) 'cut' but holding target ID
  areas <- data.frame(area=sapply(pi@polygons, FUN=function(x) areaPolygon(x@Polygons[[1]]@coords)))
  row.names(areas) <- sapply(pi@polygons, function(x) slot(x, 'ID'))
  attArea <- spCbind(pi, areas)
  attAreadf <- attArea@data #df versions for sql below
  # allow for multiple inputs
  vals <- paste(names(polygon_input),collapse=", ")
  if (assignment == "weighted(area)"){
    vals <- paste(sapply(names(polygon_input), function(x) paste0('sum(area*',x,')/sum(area)')),collapse=", ")
    out <- sqldf(paste0('select ',vals,', FID from attAreadf group by FID'))
  }else if(assignment == "weighted(pop)"){
    vals <- paste(sapply(names(polygon_input), function(x) paste0('sum(pop*',x,')/sum(pop)')),collapse=", ")
    out <- sqldf(paste0('select ',vals,', FID from attAreadf group by FID'))
  }else{
    out <- sqldf(paste0('select ',vals,', FID, ',assignment,' from attAreadf group by FID'))
  }
  # SORT by polygon ID
  out <- out[order(as.numeric(as.character(out$FID))),]
  row.names(out) <- NULL
  
  # ADD NA's where polygons do not intersect
  if (nrow(out)<length(target)){
    for (iter in 1:length(names(polygon_input))){
      if (is.numeric(out[,iter])){
        out[as.numeric(target$FID[!(target$FID %in% out$FID)])+1,iter] <- NA
      }else{
        out[as.numeric(target$FID[!(target$FID %in% out$FID)])+1,iter] <- 'no intersection'
      }
    }
  }
  # eliminate other entries
  out <- data.frame(out[,1:length(names(polygon_input))])
  names(out) <- names(polygon_input)
  return(out)
}