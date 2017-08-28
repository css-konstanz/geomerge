# MERGE function
# - pairwise merger of dataset to target
# - pass on weights matrix (first and second order weights, list of neighbors)
# - pass on all options from mergeit
geomerge.merge <- function(data,data.name,target,standard.CRS,outdata,wghts,time,time.lag,spat.lag,zonal.fun,assignment,population.data,point.agg,t_unit,silent,optional.inputs){
  if (silent){
    cat <-function(...){}
  }
  
  # DISTINCTION by type of data
  # 1) Polygon data
  if (class(data)=='SpatialPolygonsDataFrame'){
    cat(paste0('\n Merging polygon data...'))
    # CALL geomerge.assign here (distinguishes by assignment)
    out.stats <- geomerge.assign(data,target,assignment,population.data,optional.inputs)
    if (spat.lag & is.numeric(data@data[,1])){
      out.stats <- cbind(out.stats,lag.listw(nb2listw(wghts$wts1$neighbours,style= "W",zero.policy=TRUE),out.stats[,1],zero.policy=TRUE),lag.listw(nb2listw(wghts$wts2$neighbours,style= "W",zero.policy=TRUE),out.stats[,1],zero.policy=TRUE))
      out.stats <- data.frame(out.stats)
      out.stats <- out.stats[rep(seq_len(nrow(out.stats)),nrow(outdata)/nrow(out.stats)),]
      row.names(out.stats) <- NULL
      outdata <- cbind(outdata,out.stats)
      names(outdata) <- c(names(outdata)[1:(length(names(outdata))-3)],data.name,paste0(data.name,'.1st'),paste0(data.name,'.2nd'))
    }else{
      out.stats <- data.frame(out.stats)
      out.stats <- out.stats[rep(seq_len(nrow(out.stats)),nrow(outdata)/nrow(out.stats)),]
      row.names(out.stats) <- NULL
      outdata <- cbind(outdata,out.stats)
      names(outdata) <- c(names(outdata)[1:(length(names(outdata))-1)],data.name)
      if(spat.lag){
        cat(paste0('\n NOTE: No spatial lags calculated for ',data.name,' since data is non-numeric.\n'))
      }
    }
    cat(' Done.')
  }
  
  # 2) Point data
  if (class(data)=='SpatialPointsDataFrame'){
    # STATIC point data aggregation
    if (any(is.na(time)) | !any(names(data)=='timestamp')){
      cat(paste0('\n Aggregating point data...'))
      data$ones<-1
      points.in.poly<-over(data,target) #tells which unit the points are within
      data$FID<-points.in.poly$FID #tacking to the violence file
      if (point.agg=="cnt"){
        id.count<-aggregate(data$ones, by = list(data$FID), FUN = sum)
        colnames(id.count)<-c("FID","cnt")
        out.stats <- data.frame(list(cnt=id.count$cnt[match(target@data$FID, id.count$FID)]))
        if (nrow(out.stats>0)){
          out.stats$cnt[is.na(out.stats$cnt)] <- 0
          if (spat.lag){
            out.stats$cnt.1st<-lag.listw(nb2listw(wghts$wts1$neighbours,style= "W",zero.policy=TRUE),out.stats$cnt,zero.policy=TRUE,NAOK=TRUE)
            out.stats$cnt.2nd<-lag.listw(nb2listw(wghts$wts2$neighbours,style= "W",zero.policy=TRUE),out.stats$cnt,zero.policy=TRUE,NAOK=TRUE)
            names(out.stats) <- c(paste0(data.name,'.cnt'),paste0(data.name,'.cnt.1st'),paste0(data.name,'.cnt.2nd'))
          }else{
            names(out.stats) <- c(paste0(data.name,'.cnt'))
          }
          out.stats <- out.stats[rep(seq_len(nrow(out.stats)),nrow(outdata)/nrow(out.stats)),]
          row.names(out.stats) <- NULL
        }
        outdata <- cbind(outdata,out.stats)
      }else{
        names(data)[!names(data)%in%c('timestamp','ones','FID')] <- 'entry'
        id.count<-aggregate(data$entry, by = list(data$FID), FUN = sum)
        colnames(id.count)<-c("FID","sum")
        out.stats <- data.frame(list(sum=id.count$sum[match(target@data$FID, id.count$FID)]))
        if (nrow(out.stats>0)){
          out.stats$sum[is.na(out.stats$sum)] <- 0
          if (spat.lag){
            out.stats$sum.1st<-lag.listw(nb2listw(wghts$wts1$neighbours,style= "W",zero.policy=TRUE),out.stats$sum,zero.policy=TRUE,NAOK=TRUE)
            out.stats$sum.2nd<-lag.listw(nb2listw(wghts$wts2$neighbours,style= "W",zero.policy=TRUE),out.stats$sum,zero.policy=TRUE,NAOK=TRUE)
            names(out.stats) <- c(paste0(data.name,'.sum'),paste0(data.name,'.sum.1st'),paste0(data.name,'.sum.2nd'))
          }else{
            names(out.stats) <- c(paste0(data.name,'.sum'))
          }
          out.stats <- out.stats[rep(seq_len(nrow(out.stats)),nrow(outdata)/nrow(out.stats)),]
          row.names(out.stats) <- NULL
        }
        outdata <- cbind(outdata,out.stats)
      }
      # UPDATE column labels
      if (!spat.lag){
        names(outdata) <- c(names(outdata)[1:(length(names(outdata))-1)],paste0(data.name,'.',point.agg))
      }else{
        names(outdata) <- c(names(outdata)[1:(length(names(outdata))-3)],paste0(data.name,'.',point.agg),paste0(data.name,'.',point.agg,'.1st'),paste0(data.name,'.',point.agg,'.2nd'))
      }
      cat (' Done.')
      
      # DYNAMIC point data aggregation
    }else{
      # PARSE specifications
      Length.period <- as.numeric(time[3]) # period length in t_unit
      current.date <- as_datetime(strptime(time[1],format= "%Y-%m-%d %H:%M:%S"),tz = "UTC")
      if (is.na(current.date)){
        current.date <- as_datetime(strptime(time[1],format= "%Y-%m-%d"),tz = "UTC")
      }
      end.date <- as_datetime(strptime(time[2],format= "%Y-%m-%d %H:%M:%S"), tz = "UTC")
      if (is.na(end.date)){
        end.date <- as_datetime(strptime(time[2],format= "%Y-%m-%d"), tz = "UTC")
      }
      tstamp <- strptime(data$timestamp,format= "%Y-%m-%d %H:%M:%S", tz = "UTC")
      if (any(is.na(tstamp))){
        tstamp <- strptime(data$timestamp,format= "%Y-%m-%d", tz = "UTC")
      }
      data$timestamp <- tstamp
      
      # CHECK if already panel structure or not
      panel.check <- TRUE
      if (!any(names(outdata)=='period')){
        panel.check <- FALSE
      }
      # BEST STRATEGY:
      # proceed period by period and repeat above process for each subsetting data according to period
      # - need start and enddate to ensure that panel is always generated for same time horizon
      # - cutoff for last period is the period after which current.data > end.date
      if (!spat.lag & !time.lag){
        point.stats <- data.frame(matrix(0,0,3))
        names(point.stats) <- c('period','enddate',point.agg)
      }else if(!spat.lag & time.lag){
        point.stats <- data.frame(matrix(0,0,5))
        names(point.stats) <- c('period','enddate',point.agg,paste0(point.agg,'.t_1'),paste0(point.agg,'.t_2'))
      }else if(spat.lag & !time.lag){
        point.stats <- data.frame(matrix(0,0,5))
        names(point.stats) <- c('period','enddate',point.agg,paste0(point.agg,'.1st'),paste0(point.agg,'.2nd'))
      }else{
        point.stats <- data.frame(matrix(0,0,11))
        names(point.stats) <- c('period','enddate',point.agg,paste0(point.agg,'.t_1'),paste0(point.agg,'.t_2'),paste0(point.agg,'.1st'),paste0(point.agg,'.1st.t_1'),paste0(point.agg,'.1st.t_2'),paste0(point.agg,'.2nd'),paste0(point.agg,'.2nd.t_1'),paste0(point.agg,'.2nd.t_2'))
      }
      
      # DYNAMIC aggregation of SpatialPointData counts per target polygon
      period.count <- 1
      while(current.date < end.date){
        cat(paste0('\n Aggregating point data for period ',period.count,'...'))
        # subset data by period
        next.date <- current.date
        if (t_unit == "secs"){
          next.date <- next.date + seconds(Length.period)
        }else if (t_unit == "mins"){
          next.date <- next.date + minutes(Length.period)
        }else if (t_unit == "hours"){
          next.date <- next.date + hours(Length.period)
        }else if (t_unit == "days" | t_unit == "months" | t_unit == "years"){
          check <- TRUE
          offset <- 0
          while(check){
            next.date <- next.date + days(offset)
            if (t_unit == "days"){
              next.date <- next.date + days(Length.period)
            }else if (t_unit == "months"){
              next.date <- next.date + months(Length.period) 
            }else{
              next.date <- next.date + years(Length.period)
            }
            if (!is.na(next.date)){
              check <- FALSE
            }else{
              next.date <- current.date
              offset <- offset + 1 
            }
          }
        }
        current.data <- data[data$timestamp>=current.date & data$timestamp<next.date,]
        if (nrow(current.data)>0){
          current.data$ones<-1
          points.in.poly<-over(current.data,target) #tells which unit the points are within
          current.data$FID<-points.in.poly$FID
          if (point.agg=="cnt"){
            id.count<-aggregate(current.data$ones, by = list(current.data$FID), FUN = sum)
            colnames(id.count)<-c("FID","cnt")
            out.stats <- data.frame(list(cnt = id.count$cnt[match(target$FID, id.count$FID)]))
            out.stats$cnt[is.na(out.stats$cnt)] <- 0
            out.stats$period <- period.count
            out.stats$enddate <- next.date
            out.stats <- out.stats[,c(2,3,1)]
            
            # ADD spatially lagged results
            if (spat.lag){
              out.stats$cnt.1st<-lag.listw(nb2listw(wghts$wts1$neighbours,style= "W",zero.policy=TRUE),out.stats$cnt,zero.policy=TRUE,NAOK=TRUE)
              out.stats$cnt.2nd<-lag.listw(nb2listw(wghts$wts2$neighbours,style= "W",zero.policy=TRUE),out.stats$cnt,zero.policy=TRUE,NAOK=TRUE)
            }
            
            # ADD time lagged results to out.stats
            if (time.lag){
              if (period.count==1){
                out.stats$cnt.t_1 <- NA
                out.stats$cnt.t_2 <- NA
                if(spat.lag){
                  out.stats$cnt.1st.t_1 <- NA
                  out.stats$cnt.1st.t_2 <- NA
                  out.stats$cnt.2nd.t_1 <- NA
                  out.stats$cnt.2nd.t_2 <- NA
                  # REORDER columns to fit point.stats column order
                  out.stats <- out.stats[,c(1,2,3,6,7,4,8,9,5,10,11)]
                }
              }else if(period.count==2){
                out.stats$cnt.t_1 <- point.stats$cnt[point.stats$period==1]
                out.stats$cnt.t_2 <- NA
                if (spat.lag){
                  out.stats$cnt.1st.t_1 <- point.stats$cnt.1st[point.stats$period==1]
                  out.stats$cnt.1st.t_2 <- NA
                  out.stats$cnt.2nd.t_1 <- point.stats$cnt.2nd[point.stats$period==1]
                  out.stats$cnt.2nd.t_2 <- NA
                  # REORDER columns to fit point.stats column order
                  out.stats <- out.stats[,c(1,2,3,6,7,4,8,9,5,10,11)]
                }
              }else{
                out.stats$cnt.t_1 <- point.stats$cnt[point.stats$period==period.count-1]
                out.stats$cnt.t_2 <- point.stats$cnt.t_1[point.stats$period==period.count-2]
                if (spat.lag){
                  out.stats$cnt.1st.t_1 <- point.stats$cnt.1st[point.stats$period==period.count-1]
                  out.stats$cnt.1st.t_2 <- point.stats$cnt.1st.t_1[point.stats$period==period.count-2]
                  out.stats$cnt.2nd.t_1 <- point.stats$cnt.2nd[point.stats$period==period.count-1]
                  out.stats$cnt.2nd.t_2 <- point.stats$cnt.2nd.t_1[point.stats$period==period.count-2]
                  # REORDER columns to fit point.stats column order
                  out.stats <- out.stats[,c(1,2,3,6,7,4,8,9,5,10,11)]
                }
              }
            }
          }else{
            names(current.data)[!names(current.data)%in%c('timestamp','ones','FID')] <- 'entry'
            id.count<-aggregate(current.data$entry, by = list(current.data$FID), FUN = sum)
            colnames(id.count)<-c("FID","sum")
            out.stats <- data.frame(list(sum = id.count$sum[match(target$FID, id.count$FID)]))
            out.stats$sum[is.na(out.stats$sum)] <- 0
            out.stats$period <- period.count
            out.stats$enddate <- next.date
            out.stats <- out.stats[,c(2,3,1)]
            
            # ADD spatially lagged results
            if (spat.lag){
              out.stats$sum.1st<-lag.listw(nb2listw(wghts$wts1$neighbours,style= "W",zero.policy=TRUE),out.stats$sum,zero.policy=TRUE,NAOK=TRUE)
              out.stats$sum.2nd<-lag.listw(nb2listw(wghts$wts2$neighbours,style= "W",zero.policy=TRUE),out.stats$sum,zero.policy=TRUE,NAOK=TRUE)
            }
            
            # ADD time lagged results to out.stats
            if (time.lag){
              if (period.count==1){
                out.stats$sum.t_1 <- NA
                out.stats$sum.t_2 <- NA
                if (spat.lag){
                  out.stats$sum.1st.t_1 <- NA
                  out.stats$sum.1st.t_2 <- NA
                  out.stats$sum.2nd.t_1 <- NA
                  out.stats$sum.2nd.t_2 <- NA
                  
                  # REORDER columns in first iteration to fit point.stats column order
                  out.stats <- out.stats[,c(1,2,3,6,7,4,8,9,5,10,11)]
                }
              }else if(period.count==2){
                out.stats$sum.t_1 <- point.stats$sum[point.stats$period==1]
                out.stats$sum.t_2 <- NA
                if (spat.lag){
                  out.stats$sum.1st.t_1 <- point.stats$sum.1st[point.stats$period==1]
                  out.stats$sum.1st.t_2 <- NA
                  out.stats$sum.2nd.t_1 <- point.stats$sum.2nd[point.stats$period==1]
                  out.stats$sum.2nd.t_2 <- NA
                }
              }else{
                out.stats$sum.t_1 <- point.stats$sum[point.stats$period==period.count-1]
                out.stats$sum.t_2 <- point.stats$sum.t_1[point.stats$period==period.count-2]
                if (spat.lag){
                  out.stats$sum.1st.t_1 <- point.stats$sum.1st[point.stats$period==period.count-1]
                  out.stats$sum.1st.t_2 <- point.stats$sum.1st.t_1[point.stats$period==period.count-2]
                  out.stats$sum.2nd.t_1 <- point.stats$sum.2nd[point.stats$period==period.count-1]
                  out.stats$sum.2nd.t_2 <- point.stats$sum.2nd.t_1[point.stats$period==period.count-2]
                }
              }
            }
          }
        }else{
          # GENERATE empty data frame if no data is there to merge
          if (!spat.lag & !time.lag){
            out.stats <- data.frame(matrix(0,0,3))
            names(out.stats) <- c('period','enddate',point.agg)
          }else if(!spat.lag & time.lag){
            out.stats <- data.frame(matrix(0,0,5))
            names(out.stats) <- c('period','enddate',point.agg,paste0(point.agg,'.t_1'),paste0(point.agg,'.t_2'))
          }else if(spat.lag & !time.lag){
            out.stats <- data.frame(matrix(0,0,5))
            names(out.stats) <- c('period','enddate',point.agg,paste0(point.agg,'.1st'),paste0(point.agg,'.2nd'))
          }else{
            out.stats <- data.frame(matrix(0,0,11))
            names(out.stats) <- c('period','enddate',point.agg,paste0(point.agg,'.t_1'),paste0(point.agg,'.t_2'),paste0(point.agg,'.1st'),paste0(point.agg,'.1st.t_1'),paste0(point.agg,'.1st.t_2'),paste0(point.agg,'.2nd'),paste0(point.agg,'.2nd.t_1'),paste0(point.agg,'.2nd.t_2'))
          }
        }
        
        # ADD data for the current period using rbind()
        point.stats <- rbind(point.stats,out.stats)
        
        # UPDATE period counter
        current.date <- next.date
        period.count <- period.count + 1
        cat (' Done.')
      }
      
      # UPDATE outdata once fully iterated
      if (panel.check){
        # exclude 'period' and 'enddate' columns here if they already exist
        point.stats <- point.stats[,3:ncol(point.stats)]
        outdata <- cbind(outdata,point.stats)
      }else{
        rep.outdata <- outdata[rep(seq_len(nrow(outdata)),nrow(point.stats)/nrow(outdata)),]
        row.names(rep.outdata) <- NULL
        outdata <- cbind(rep.outdata,point.stats)
        outdata <- outdata[,c('period','enddate',names(outdata)[!names(outdata)%in%c('period','enddate')])]
      }
      # UPDATE column labels
      if (!spat.lag & !time.lag){
        names(outdata) <- c(names(outdata)[1:(length(names(outdata))-1)],paste0(data.name,'.',point.agg))
      }else if(spat.lag & !time.lag){
        names(outdata) <- c(names(outdata)[1:(length(names(outdata))-3)],paste0(data.name,'.',point.agg),paste0(data.name,'.',point.agg,'.1st'),paste0(data.name,'.',point.agg,'.2nd'))
      }else if(!spat.lag & time.lag){
        names(outdata) <- c(names(outdata)[1:(length(names(outdata))-3)],paste0(data.name,'.',point.agg),paste0(data.name,'.',point.agg,'.t_1'),paste0(data.name,'.',point.agg,'.t_2'))
      }else{
        names(outdata) <- c(names(outdata)[1:(length(names(outdata))-9)],paste0(data.name,'.',point.agg),paste0(data.name,'.',point.agg,'.t_1'),paste0(data.name,'.',point.agg,'.t_2'),paste0(data.name,'.',point.agg,'.1st'),paste0(data.name,'.',point.agg,'.1st.t_1'),paste0(data.name,'.',point.agg,'.1st.t_2'),paste0(data.name,'.',point.agg,'.2nd'),paste0(data.name,'.',point.agg,'.2nd.t_1'),paste0(data.name,'.',point.agg,'.2nd.t_2'))
      }
    }
  }
  
  # 3) Raster data
  if (class(data)=='RasterLayer'){
    # REDUCE size of raster to target if larger
    if (extent(data) > 1.2*extent(target)){
      cat(paste0('\n Cropping RasterLayer...'))
      data <- crop(data, extent(target))
      data <- mask(data, target)
      cat(' Done.')
    }
    
    # EXTRACT data
    cat(paste0('\n Generating zonal statistics...'))
    if (length(optional.inputs>0)){
      zonal.stats <- extract(data,target, fun = zonal.fun, optional.inputs)
    }else{
      zonal.stats <- extract(data,target, fun = zonal.fun)
    }
    # span outstats here
    out.stats <- data.frame(list(Zonal=rep(zonal.stats,nrow(outdata)/nrow(zonal.stats))))
    out.stats$ZonalPerKm2 <- out.stats$Zonal/outdata$area
    if (spat.lag){
      options(warn=-1)
      out.stats$Zonal1st<-lag.listw(nb2listw(wghts$wts1$neighbours,style= "W",zero.policy=TRUE),out.stats$Zonal,zero.policy=TRUE,NAOK=TRUE)
      out.stats$Zonal2nd<-lag.listw(nb2listw(wghts$wts2$neighbours,style= "W",zero.policy=TRUE),out.stats$Zonal,zero.policy=TRUE,NAOK=TRUE)
      out.stats$ZonalPerKm1st<-lag.listw(nb2listw(wghts$wts1$neighbours,style= "W",zero.policy=TRUE),out.stats$ZonalPerKm2,zero.policy=TRUE,NAOK=TRUE)
      out.stats$ZonalPerKm2nd<-lag.listw(nb2listw(wghts$wts2$neighbours,style= "W",zero.policy=TRUE),out.stats$ZonalPerKm2,zero.policy=TRUE,NAOK=TRUE)
      options(warn=0)
      out.stats <- out.stats[,c(1,3,4,2,5,6)]
      names(out.stats) <- c(data.name,paste0(data.name,'.1st'),paste0(data.name,'.2nd'),paste0(data.name,'.PerKm2'),paste0(data.name,'.PerKm2.1st'),paste0(data.name,'.PerKm2.2nd'))
    }else{
      names(out.stats) <- c(data.name,paste0(data.name,'.PerKm2'))
    }
    outdata <- cbind(outdata,out.stats)
    cat(' Done.')
  }
  #row.names(outdata)<-NULL
  return(outdata)
}