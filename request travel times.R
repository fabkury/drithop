# drithop
# Fabricio Kury, MD, Raymonde Uy, MD, MBA, Raymond Sarmiento, MD, Paul Fontelo, MD, MPH
# ------
# request travel times.R
#
# Project start: Early 2015
# Coding start: August 6, 2015
# Core routines start: 16-01-28
# drithop2.R start: April 14, 2016
# 

setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')
source('Code/core.R')

App_id <- # HERE API APP ID
App_code <- # HERE API APP CODE

here_api_matrix_routing_limit <- 100

# TO DO: Remove the ".cit" from below.
matrix_resource_address <- 'http://matrix.route.cit.api.here.com/routing/7.2/calculatematrix.json?'
attributes_string <- '&summaryAttributes=traveltime,distance'
transportation_mode_string <- 'fastest;car;traffic:enabled'

departures <- list(
  '2009'=list(date='2009-04-08', time='18:00:00'),
  '2010'=list(date='2010-04-14', time='18:00:00'),
  '2011'=list(date='2011-04-13', time='18:00:00'),
  '2012'=list(date='2012-04-11', time='18:00:00'),
  '2013'=list(date='2013-04-10', time='18:00:00'),
  '2014'=list(date='2014-04-09', time='18:00:00'),
  '2015'=list(date='2015-04-08', time='18:00:00'))

requestHEREYearUSA <- function(year, hospital_filter, type = 'car') {
  if(type == 'car')
    try(createSQLTableRouting(year), silent = T)
  else
    try(createSQLTablePTRouting(year), silent = T)
  library(rgdal)
  library(plyr)
  library(RANN)
  library(xlsx)
  library(geosphere)
  message('Processing year ', year, '.')
  us.cp <- loadCentersOfPopulation(year)
  hosp <- loadHospitals(year, hospital_filter)[, c('ID', 'LAT', 'LON')] # Reduce to only the variables we need.
  hosp$ROW <- 1:nrow(hosp)
  
  for(st in USA.states$Abbreviation) {
    state_code <- subset(USA.states, Abbreviation==st)$Code
    state_name <- subset(USA.states, Abbreviation==st)$Name
    message('Processing state ', state_name, '.')
    map.cp.rds <- paste0(data_dir, 'USA/Shapefiles/', year, '/map.cp-', st, '-', hospital_filter, '-', year,'.rds')
    if(!file.exists(map.cp.rds)) {
      shapefile <- paste0('tl_', year, '_', state_code, '_bg', ifelse(year==2010, '10', ifelse(year==2009, '00', '')))
      shapefile_path <- paste0(data_dir, 'USA/Shapefiles/', year, '/', shapefile)
      if(!dir.exists(shapefile_path))
        next
      map <- readOGR(shapefile_path, shapefile)
      message('Map of file ', shapefile, ' loaded.')
      if(year==2010) {
        # In year 2010 the variable names are different.
        map@data$GEOID <- map@data$GEOID10
        map@data$GEOID10 <- NULL
        map@data$INTPTLAT <- map@data$INTPTLAT10
        map@data$INTPTLAT10 <- NULL
        map@data$INTPTLON <- map@data$INTPTLON10
        map@data$INTPTLON10 <- NULL
      }
      if(year==2009) {
        # In year 2009 the variable names are different.
        map@data$GEOID <- map@data$GEOID00
        map@data$GEOID00 <- NULL
        map@data$INTPTLAT <- map@data$INTPTLAT00
        map@data$INTPTLAT00 <- NULL
        map@data$INTPTLON <- map@data$INTPTLON00
        map@data$INTPTLON00 <- NULL
        map@data$GEOID <- map@data$BKGPIDFP00
        map@data$BKGPIDFP00 <- NULL
      }
      
      # Inpute the internal point as center of population for the missing cases.
      map.cp <- join(us.cp[, c('GEOID', 'LATITUDE', 'LONGITUDE')],
        map@data[, c('GEOID', 'INTPTLAT', 'INTPTLON')], by='GEOID', type='right')
      
      ## This line below is temporary while I haven't split the map making from the requesting of travel times.
      remove(map)
      gc()
      
      map.cp$INTPTLAT <- as.numeric(levels(map.cp$INTPTLAT)[map.cp$INTPTLAT])
      map.cp$INTPTLON <- as.numeric(levels(map.cp$INTPTLON)[map.cp$INTPTLON])
      message('Centers of population imputed with missing values in ',
        sum(is.na(map.cp$LATITUDE) | is.na(map.cp$LONGITUDE)), ' cases.')
      map.cp$LATITUDE[is.na(map.cp$LATITUDE)] <- map.cp$INTPTLAT[is.na(map.cp$LATITUDE)]
      map.cp$LONGITUDE[is.na(map.cp$LONGITUDE)] <- map.cp$INTPTLON[is.na(map.cp$LONGITUDE)]
      map.cp <- map.cp[, c('GEOID', 'LATITUDE', 'LONGITUDE')]
      
      message('Computing Haversine-distance nearest hospitals.')
      for(e in 1:nrow(map.cp)) {
        thosp <- cbind(hosp$ROW, distHaversine(data.frame(hosp$LON, hosp$LAT),
          data.frame(map.cp[e,'LONGITUDE'], map.cp[e,'LATITUDE'])))
        colnames(thosp) <- c('ROW', 'DIST')
        for(i in 1:5)
          map.cp[e,paste0('NH', i)] <- thosp[order(thosp[,'DIST']),][i,'ROW']
      }
      message('Haversine-distance nearest hospitals computed.')
      saveRDS(map.cp, map.cp.rds)
      message('Cached data frame map.cp at "', map.cp.rds, '".')
    }
    else {
      message('Read cached map.cp from "', map.cp.rds, '".')
      map.cp <- readRDS(map.cp.rds)
    }
#     nearest_hosp <- nn2(matrix(c(hosp$LAT,hosp$LON), nrow=length(hosp$LAT)),
#       matrix(c(map.cp$LATITUDE, map.cp$LONGITUDE), nrow=length(map.cp$LATITUDE)), k = 5)
#     message('Euclidean-distance nearest hospitlals calculated.')
    
    # Find the true nearest hospital via HERE API Matrix Routing and save into SQL.
    message('Processing routing.')
    if(type == 'car')
      processMatrixRouting('USA', year, map.cp, map.cp[, sapply(1:5, function(e) { paste0('NH', e) })], hosp)
    else
      processPublicTransportRouting('USA', year, map.cp, map.cp[, sapply(1:5, function(e) { paste0('NH', e) })], hosp)
    message(state_name, ' added to map.')
  }
}



requestHEREYearBRA <- function(year, hospital_filter, type = 'car') {
  if(type == 'car')
    try(createSQLTableRoutingBRA(year), silent = T)
  else
    try(createSQLTablePTRoutingBRA(year), silent = T)
  library(rgdal)
  library(plyr)
  library(RANN)
  library(xlsx)
  library(geosphere)
  message('Processing year ', year, ' in Brazil', ifelse(!is.null(hospital_filter),
    paste0(' under hospital filter "', hospital_filter, '".'), '.'))
  hosp <- dbNoTimeOutGetQuery('select ID, Latitude as LAT, Longitude as LON from BRA_hospitals;')
    #loadBRAHospitals(year, hospital_filter)[, c('ID', 'LAT', 'LON')] # Reduce to only the variables we need.
  hosp$ROW <- 1:nrow(hosp)
  
  for(st in BRA.states$Abbreviation) {
    state_code <- subset(BRA.states, Abbreviation==st)$Code
    state_name <- subset(BRA.states, Abbreviation==st)$Name
    message('Processing state ', state_name, '.')
    map.cp.rds <- paste0(data_dir, 'BRA/Shapefiles/map.cp-', st, '-', hospital_filter,'.rds')
    if(!file.exists(map.cp.rds)) {
      shapefile <- paste0(state_code, 'SEE250GC_SIR')
      shapefile_path <- paste0(data_dir, 'BRA/Shapefiles/', st)
      if(!dir.exists(shapefile_path))
        next
      map <- readOGR(shapefile_path, shapefile)
      message('Map of file ', shapefile, ' loaded.')
      
      map.cp <- coordinates(map)
      map.cp <- data.frame(GEOID=map@data$CD_GEOCODI, LATITUDE=map.cp[,2], LONGITUDE=map.cp[,1])
      ## This line below is temporary while I haven't split the map making from the requesting of travel times.
      remove(map)
      gc()

      message('Computing Haversine-distance nearest hospitals.')
      for(e in 1:nrow(map.cp)) {
        thosp <- cbind(hosp$ROW, distHaversine(data.frame(hosp$LON, hosp$LAT),
          data.frame(map.cp[e,'LONGITUDE'], map.cp[e,'LATITUDE'])))
        colnames(thosp) <- c('ROW', 'DIST')
        thosp <- thosp[order(thosp[,'DIST']),]
        for(i in 1:5)
          map.cp[e,paste0('NH', i)] <- thosp[i,'ROW']
      }
      message('Haversine-distance nearest hospitals computed.')
      saveRDS(map.cp, map.cp.rds)
      message('Cached data frame map.cp at "', map.cp.rds, '".')
    }
    else {
      message('Read cached map.cp from "', map.cp.rds, '".')
      map.cp <- readRDS(map.cp.rds)
    }
#     nearest_hosp <- nn2(matrix(c(hosp$LAT,hosp$LON), nrow=length(hosp$LAT)),
#       matrix(c(map.cp$LATITUDE, map.cp$LONGITUDE), nrow=length(map.cp$LATITUDE)), k = 5)
#     message('Euclidean-distance nearest hospitlals calculated.')
    
    # Find the true nearest hospital via HERE API Matrix Routing and save into SQL.
    message('Processing routing.')
    if(type == 'car')
      processMatrixRouting('BRA', year, map.cp, map.cp[, sapply(1:5, function(i) { paste0('NH', i) })], hosp)
    else
      processPublicTransportRouting('BRA', year, map.cp, map.cp[, sapply(1:5, function(i) { paste0('NH', i) })], hosp)
    message(state_name, ' added to map.')
  }
}



createSQLTableRouting <- function(year)
  dbNoTimeOutGetQuery('create table DH_USA_', year, ' (GEOID CHAR(12) NOT NULL, AHAID VARCHAR(20) NOT NULL, distance
  INT UNSIGNED, travelTime INT UNSIGNED, costFactor INT UNSIGNED, departure_date DATE NOT NULL, departure_time TIME
  NOT NULL)')

createSQLTablePTRouting <- function(year)
  dbNoTimeOutGetQuery('create table DH_USA_PT_', year, ' (GEOID CHAR(12) NOT NULL, AHAID VARCHAR(20) NOT NULL, distance
  INT UNSIGNED, travelTime INT UNSIGNED, flags VARCHAR(256), departure_date DATE NOT NULL, departure_time TIME
  NOT NULL)')

## TO DO: Normalize "GEOID" and "AHAID" to "GID" (Geographical ID) and "HID" (Hospital ID)
createSQLTableRoutingBRA <- function(year)
  dbNoTimeOutGetQuery('create table DH_BRA_', year, ' (GEOID CHAR(15) NOT NULL, AHAID VARCHAR(20) NOT NULL, distance
  INT UNSIGNED, travelTime INT UNSIGNED, costFactor INT UNSIGNED, departure_date DATE NOT NULL, departure_time TIME
  NOT NULL)')

createSQLTablePTRoutingBRA <- function(year)
  dbNoTimeOutGetQuery('create table DH_BRA_PT_', year, ' (GEOID CHAR(15) NOT NULL, AHAID VARCHAR(20) NOT NULL, distance
  INT UNSIGNED, travelTime INT UNSIGNED, flags VARCHAR(256), departure_date DATE NOT NULL, departure_time TIME
  NOT NULL)')


processMatrixRouting <- function(country, year, starts, destinations, hospitals) {
  library(dplyr)
  latLongtoChar <- function(latlong) paste0(latlong[1], ',', latlong[2])

  sql_table_name <- paste0('DH_', country, '_', year)
  departure <- departures[[as.character(year)]]
  parameters_string <- paste0('&mode=', transportation_mode_string, '&departure=',
    departure[['date']], 'T', departure[['time']])

  for(destination_column in 1:ncol(destinations)) {
    message('Destination column ', destination_column, ' of ', ncol(destinations), '.')
    resdf <- anti_join(data.frame(GEOID=as.character(starts$GEOID),
      AHAID=as.character(hospitals[destinations[,destination_column],'ID']),
      START=paste0(starts$LATITUDE,',',starts$LONGITUDE), stringsAsFactors = F),
      dbNoTimeOutGetQuery('select GEOID, AHAID from ', sql_table_name, ';'),
      by = c('GEOID', 'AHAID'))
    colnames(resdf) <- c('GEOID', 'ID', 'START')
    resdf <- join(resdf, hospitals, by='ID')
    message('Joins completed. Remaining rows to be requested: ', nrow(resdf), '.')
    
    for(unique_destination in unique(resdf[,'ID'])) {
      destination_coords <- latLongtoChar(resdf[resdf$ID == unique_destination,][1, c('LAT', 'LON')])
      destination_slices <- subset(resdf, ID == unique_destination)
      
      for(slice_chunk in make.chunks.by.size(1:nrow(destination_slices), here_api_matrix_routing_limit)) {
        slice <- destination_slices[slice_chunk,]
        rownames(slice) <- 0:(length(rownames(slice))-1) # This is just to facilitate debugging.
        start_string <- paste0('&start', 0:(nrow(slice)-1), '=', slice$START, collapse = '')
        matrix_string <- paste0(start_string, '&destination0=', destination_coords)
        request_string <- paste0(matrix_resource_address, 'app_id=', App_id, '&app_code=', App_code,
          attributes_string, parameters_string, matrix_string)
        
        for(query_attempts in 1:max_query_attempts) {
          res <- try(RJSONIO::fromJSON(RCurl::getURL(request_string)), silent=T)
          if(inherits(res, 'try-error')) {
            message('Failed call to Nokia HERE API.')
            if(query_attempts < max_query_attempts) {
              message('Will try again in 5 seconds.')
              Sys.sleep(5)
            }
            else
              message('Will stop trying.')
              # TO DO: Write all errors to file.
          }
          else
            break
        }
        
        if(inherits(res, 'try-error')) {
          # Unable to get data! Big error!
          # TO DO: Write all errors to file.
          message('## Critical error: unable to get data from HERE API for slice ', sn, '.')
          break
        }
        
        if(exists('type', where=res$response) && res$response$type == 'SystemError')
          stop('## Critical error: HERE API SystemError.')
        
        if(!exists('matrixEntry', where=res$response)) {
          # All routes failed. Produce an empty result set.
          # browser()
          res <- data.frame(GEOID = slice$GEOID, AHAID=hospitals[hospitals$ID==unique_destination, 'ID'][1],
            distance=NA, travelTime=NA, costFactor=NA)
        }
        else {
          # TO DO: Report errors.
          failed <- sapply(res$response$matrixEntry, exists, x = 'status')
          if(any(failed))
            for(i in 1:length(res$response$matrixEntry))
              if(failed[i]) {
                # Some of the routes failed.
                # Remove the 'status=failed' item from the list
                res$response$matrixEntry[[i]] <- res$response$matrixEntry[[i]][c('startIndex', 'destinationIndex')]
                # Add items that are missing because the API could not find a route from start to destination.
                res$response$matrixEntry[[i]]$summary <-
                  c(distance=as.numeric(NA), travelTime=as.numeric(NA), costFactor=as.numeric(NA))
              }
          
          # Return to results their names that get lost in unlist(). SQL query will need the names.
          res <- data.frame(matrix(unlist(res$response$matrixEntry), ncol=5, byrow = T, dimnames = list(
            0:(length(res$response$matrixEntry)-1), c('GEOID', 'AHAID', 'distance', 'travelTime', 'costFactor'))))
          res$GEOID <- slice$GEOID
          res$AHAID <- hospitals[hospitals$ID==unique_destination, 'ID'][[1]]
        }
        
        # Add information that is equal for all rows.
        res$departure_date <- departure[['date']]
        res$departure_time <- departure[['time']]
        
        # Upload results of the slice to SQL
        dbNoTimeOutGetQuery(gsub("'NA'", 'NULL', paste0('insert into ', sql_table_name,
          " (GEOID, AHAID, distance, travelTime, costFactor, departure_date, departure_time) values ('",
          paste(apply(res, 1, paste, collapse="', '"), collapse="'), ('"), "');"), fixed=T))
        # message('Summaries stored in DH_', year, '. Will now update DH[[\'', year, '\']].')
        # DH[[as.character(year)]] <<- rbind(DH[[as.character(year)]], res[, c('GEOID', 'AHAID')])
      }
    }
  }
}



processPublicTransportRouting <- function(country, year, starts, destinations, hospitals) {
  library(dplyr)
  library(plyr)
  latLongtoChar <- function(latlong) paste0(latlong[1], ',', latlong[2])

  public_transport_resource <- 'http://route.api.here.com/routing/7.2/calculateroute.json?'
  sql_table_name <- paste0('DH_', country, '_PT_', year)
  departure <- departures[[as.character(year)]]
  parameters_string <- paste0('&mode=fastest;publicTransport&combineChange=true&departure=',
    departure[['date']], 'T', departure[['time']])
  
  cl <- makeCluster(n_threads)
  registerDoParallel(cl)

  for(destination_column in 1:ncol(destinations)) {
    message('Destination column ', destination_column, ' of ', ncol(destinations), '.')
    resdf <- anti_join(data.frame(GEOID=as.character(starts$GEOID),
      AHAID=as.character(hospitals[destinations[,destination_column],'ID']),
      START=paste0(starts$LATITUDE,',',starts$LONGITUDE), stringsAsFactors = F),
      dbNoTimeOutGetQuery('select GEOID, AHAID from ', sql_table_name, ';'),
      by = c('GEOID', 'AHAID'))
    colnames(resdf) <- c('GEOID', 'ID', 'START')
    resdf <- join(resdf, hospitals, by='ID')
    message('Joins completed. Remaining rows to be requested: ', nrow(resdf), '.')
    if(nrow(resdf) == 0)
      next
    
    # for(chunk in make.n.chunks(1:nrow(resdf), p_length)) {
    foreach(chunk=make.n.chunks(1:nrow(resdf), p_length),
      .packages=c('rgeos', 'rgdal', 'plyr', 'RANN', 'maptools', 'RMySQL', 'reldist'),
      .export = c('connect.to.sql', 'dbNoTimeOutGetQuery', 'sql_host', 'sql_username', 'sql_password',
      'sql_port', 'sql_database', 'App_code', 'App_id', 'max_query_attempts')) %dopar% {
      connect.to.sql()
      resdf.chunk <- resdf[chunk,]
      resdf.chunk$destination_coords <- paste0(resdf.chunk$LAT, ',', resdf.chunk$LON)
      resdf.chunk$request_string <- paste0(public_transport_resource, 'app_id=', App_id, '&app_code=', App_code,
        parameters_string, '&waypoint0=', resdf.chunk$START, '&waypoint1=', resdf.chunk$destination_coords)
      
      for(r in 1:nrow(resdf.chunk)) {
        for(query_attempts in 1:max_query_attempts) {
          res <- try(RJSONIO::fromJSON(RCurl::getURL(resdf.chunk[r,'request_string'])), silent=T)
          if(inherits(res, 'try-error')) {
            message('Failed call to Nokia HERE API.')
            if(query_attempts < max_query_attempts) {
              message('Will try again in 3 seconds.')
              Sys.sleep(3)
            }
            else
              message('Will stop trying.')
              # TO DO: Write all errors to file.
          }
          else
            break
        }
        
        if(inherits(res, 'try-error')) {
          # Unable to get data! Big error!
          # TO DO: Write all errors to file.
          resdf.chunk[r, 'distance'] <- NA
          resdf.chunk[r, 'travelTime'] <- NA
          resdf.chunk[r, 'flags'] <- 'TRY ERROR'
          # stop('## Critical error: unable to get data from HERE API for resdf.chunk[', r, ',].')
        }
        else {
          if(!exists('response', where=res) || !exists('route', where=res$response)) {
            # Route failed. Produce an empty result set.
            resdf.chunk[r, 'distance'] <- NA
            resdf.chunk[r, 'travelTime'] <- NA
            if(exists('details', where=res))
              resdf.chunk[r, 'flags'] <- res$details
            else
              resdf.chunk[r, 'flags'] <- NA
          }
          else {
            resdf.chunk[r, 'distance'] <- res$response$route[[1]]$summary$distance
            resdf.chunk[r, 'travelTime'] <- res$response$route[[1]]$summary$travelTime
            resdf.chunk[r, 'flags'] <- paste0(res$response$route[[1]]$summary$flags, collapse = ' ')
          }
        }
          
        # Add information that is equal for all rows.
        resdf.chunk[r, 'departure_date'] <- departure[['date']]
        resdf.chunk[r, 'departure_time'] <- departure[['time']]
      }
      
      # Upload results to SQL
      dbNoTimeOutGetQuery(gsub("'NA'", 'NULL', paste0('insert into ', sql_table_name,
        " (GEOID, AHAID, distance, travelTime, flags, departure_date, departure_time) values ('",
        paste(apply(resdf.chunk[,c('GEOID', 'ID', 'distance', 'travelTime', 'flags', 'departure_date',
        'departure_time')], 1, paste, collapse="', '"), collapse="'), ('"), "');"), fixed=T))
      message(nrow(resdf.chunk), ' public transport summaries stored in DH_', country, '_PT_', year, '.')
      # DH[[as.character(year)]] <<- rbind(DH[[as.character(year)]], res[, c('GEOID', 'AHAID')])
    }
    message(nrow(resdf), ' public transport summaries stored in DH_', country, '_PT_', year, '.')
  }
  
  stopCluster(cl)
  remove(cl)
}


