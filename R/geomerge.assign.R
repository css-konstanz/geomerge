#### function implementing different assignment rules using sql
# assumes that target has ID field (which it has)
# assumes that target has area (which is has)
geomerge.assign<- function(polygon_input,target,assignment,population.data,optional.inputs){
  # much larger (N of rows) SPDF with each polygon (where overlap exists) 'cut' but holding target FID
  att <- intersect(polygon_input,target[,1])
  # GENERATE population zonal stats if population weighing is used
  if (assignment%in%c('max(pop)','min(pop)','weighted(pop)')){
    cat(paste0('\n Generating zonal statistics for population based assignment...'))
    if (extent(population.data) > 1.2*extent(target)){
      population.data <- crop(population.data, extent(target))
      population.data <- mask(population.data, target)
    }
    if (length(optional.inputs>0)){
      att$pop<-extract(population.data,att, fun = mean, optional.inputs)
    }else{
      att$pop<-extract(population.data,att, fun = mean, na.rm = TRUE)
    }
    cat(' Done.')
    # fixed column label for polygon value
    names(att)[1] <- 'value'
    # add FIDs with missing overlap
    missing.value <- unlist(lapply(0:(length(target)-1), function(x) if(!x%in%att$FID){x}))
    if (length(missing.value)>0){
      add.missing <- data.frame(value=rep(NA,each=length(missing.value)), FID=missing.value, pop=target$pop[missing.value+1])
      att@data <- rbind(att@data,add.missing)
    }
  }
  # add area columns if necessary for assignment
  if (assignment%in%c('weighted(area)','max(area)','min(area)')){
    areas <- data.frame(area=sapply(att@polygons, function(x) sum(sapply(1:length(x@Polygons),function(y) areaPolygon(x@Polygons[[y]]@coords)))/1e6))
    att@data <- cbind(att@data, areas)
    # fixed column label for polygon value
    names(att)[1] <- 'value'
    # add FIDs with missing overlap
    missing.value <- unlist(lapply(0:(length(target)-1), function(x) if(!x%in%att$FID){x}))
    if (length(missing.value)>0){
      add.missing <- data.frame(value=rep(NA,each=length(missing.value)), FID=missing.value, area=0)
      att@data <- rbind(att@data,add.missing)
    }
  }
  if (assignment == "weighted(area)"){
    out <- unlist(lapply(0:(length(target)-1), function(x) sum(att$area[att$FID==x]*att$value[att$FID==x])/sum(att$area[att$FID==x])))
  }else if (assignment == "weighted(pop)"){
    out <- unlist(lapply(0:(length(target)-1), function(x) sum(att$pop[att$FID==x]*att$value[att$FID==x])/sum(att$pop[att$FID==x])))
  }else if (assignment == "max(area)"){
    out <- unlist(lapply(0:(length(target)-1), function(x) subset(att$value,att$FID==x)[which.max(att$area[att$FID==x])]))
  }else if (assignment == "min(area)"){
    out <- unlist(lapply(0:(length(target)-1), function(x) subset(att$value,att$FID==x)[which.min(att$area[att$FID==x])]))
  }else if (assignment == "max(pop)"){
    out <- unlist(lapply(0:(length(target)-1), function(x) subset(att$value,att$FID==x)[which.max(att$pop[att$FID==x])]))
  }else if (assignment == "min(pop)"){
    out <- unlist(lapply(0:(length(target)-1), function(x) subset(att$value,att$FID==x)[which.min(att$pop[att$FID==x])]))
  }
  out<-data.frame(out)
  row.names(out)<-NULL
  
  # relabel output
  names(out) <- names(polygon_input)
  return(out)
}