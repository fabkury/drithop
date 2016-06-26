# drithop
# Fabricio Kury, MD, Raymonde Uy, MD, MBA, Raymond Sarmiento, MD, Paul Fontelo, MD, MPH
# ------
# make maps.R
#
# Project start: Early 2015
# Coding start: August 6, 2015
# Core routines start: 16-01-28
# drithop2.R start: April 14, 2016
# 

is_master <- T
setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')
source('Code/core.R')

makeMapYear <- function(year, hospital_filter, acs_tables) {
  library(rgeos)
  library(rgdal)
  library(plyr)
  library(RANN)
  library(maptools)
  library(RMySQL)
  
  if(exists('USA.nat.map'))
    remove('USA.nat.map')
  hosp <- loadHospitals(year,hospital_filter)[, c('ID', 'LAT', 'LON')] # Reduce to only the variables we need.
  for(st in USA.states$Abbreviation) {
    state_code <- subset(USA.states, Abbreviation==st)$Code
    state_name <- subset(USA.states, Abbreviation==st)$Name
    shapefile <- paste0('tl_', year, '_', state_code, '_bg', ifelse(year==2010, '10', ifelse(year==2009, '00', '')))
    shapefile_path <- paste0(data_dir, 'USA/Shapefiles/', year, '/', shapefile)
    if(!dir.exists(shapefile_path))
      next
    message('Processing state ', state_name, '.')
    map <- readOGR(shapefile_path, shapefile)
    message('Map of file ', shapefile, ' loaded.')
    if(year==2010) {
      # In year 2010 the variable names are different.
      map@data$GEOID <- map@data$GEOID10
      map@data$GEOID10 <- NULL
    }
    if(year==2009) {
      # In year 2009 the variable names are different.
      map@data$GEOID <- map@data$BKGPIDFP00
      map@data$BKGPIDFP00 <- NULL
    }
    
    joinACSTable <- function(table_id)
      join(map@data, dbNoTimeOutGetQuery('select * from DH_', table_id, '_', year, " where GEOID in ('",
        paste0(as.character(map@data$GEOID), collapse="', '"), "');"), by='GEOID')
    
    message('Fetching stored data from the Ammerican Community Survey.')
    for(table_id in acs_tables) {
      map@data <- joinACSTable(table_id)
      message('Table ', table_id, ' joined to map data.')
    }
    
    map@data <- join(map@data, dbNoTimeOutGetQuery("select GEOID, min(travelTime) as minTravelTime
      from DH_USA_", year, " where AHAID in ('",
      paste0(as.character(hosp$ID), collapse="', '"), "')
      group by GEOID;"), by='GEOID')
    message('Travel time to nearest hospital under filter ', hospital_filter, ' joined to map data.')

    map <- spChFIDs(map, as.character(map@data$GEOID))
    message('Polygon IDs assigned.')

    # Fill in blank travel times with one second more than the worst travel time of the map, that is, of the state.
    map@data$minTravelTime[is.na(map@data$minTravelTime)] <- max(map@data$minTravelTime, na.rm = T)+1
    
    # Compute the quantiles by population size.
    quants <- sapply(1:24/24, wtd.quantile, x=map@data$minTravelTime, weight=map@data$B01003_001e)
    # Record the quantiles in the data frame
    for(i in 24:1)
      map@data$QTRAVELTIME[map@data$minTravelTime <= quants[[i]]] <- i
    
    # Write the shapefile.
    ogr_path <- paste0(output_dir, 'USA/Maps/', hospital_filter, '/', year, '/', st)
    ogr_name <- paste0(st, '-', year, '-', hospital_filter)
    message('Writing OGR ', ogr_name, '.')
    if(dir.exists(ogr_path))
      unlink(ogr_path, recursive = T)
    if(!dir.exists(ogr_path))
      dir.create(ogr_path, recursive = T)
    colnames(map@data) <- sub('_', '', colnames(map@data))
    writeOGR(map, ogr_path, ogr_name, "ESRI Shapefile")
    message(ogr_name,' written successfuly.')
    
    USA.nat.map <- if(exists('USA.nat.map'))
      spRbind(USA.nat.map, map)
    else
      map
    message('State map bound to national map.')
    
    # Let's try to help the R engine by calling garbage collection after we remove the big map object.
    remove(map)
    gc()
  }
  
  ogr_path <- paste0(output_dir, 'USA/Maps/', hospital_filter, '/', year, '/USA')
  ogr_name <- paste0('_', year, '-', hospital_filter)
  if(dir.exists(ogr_path))
    unlink(ogr_path, recursive = T)
  if(!dir.exists(ogr_path))
    dir.create(ogr_path, recursive = T)
  message('Writing OGR ', ogr_name, ' to "', ogr_path, '".')
  writeOGR(USA.nat.map, ogr_path, ogr_name, "ESRI Shapefile")
  message(ogr_name, ' written successfuly.')
  USA.nat.map
}


makeMapYearBRA <- function(year, hospital_filter) {
  library(rgeos)
  library(rgdal)
  library(plyr)
  library(RANN)
  library(maptools)
  library(RMySQL)
  
  if(exists('BRA.nat.map'))
    remove('BRA.nat.map')
  hosp <- dbNoTimeOutGetQuery('select ID, Latitude as LAT, Longitude as LON from BRA_hospitals;')
  
  for(st in BRA.states$Abbreviation) {
    state_code <- subset(BRA.states, Abbreviation==st)$Code
    state_name <- subset(BRA.states, Abbreviation==st)$Name
    
    shapefile <- paste0(state_code, 'SEE250GC_SIR')
    shapefile_path <- paste0(data_dir, 'BRA/Shapefiles/', st)
    if(!dir.exists(shapefile_path))
      next
    map <- readOGR(shapefile_path, shapefile)
    message('Map of file ', shapefile, ' loaded.')
#     
#     joinACSTable <- function(table_id)
#       join(map@data, dbNoTimeOutGetQuery('select * from DH_', table_id, '_', year, " where GEOID in ('",
#         paste0(as.character(map@data$GEOID), collapse="', '"), "');"), by='GEOID')
#     
#     message('Fetching stored data from the Ammerican Community Survey.')
#     for(table_id in acs_tables) {
#       map@data <- joinACSTable(table_id)
#       message('Table ', table_id, ' joined to map data.')
#     }
    
    map@data$GEOID <- map@data$CD_GEOCODI
    map@data <- join(map@data, dbNoTimeOutGetQuery("select GEOID, min(travelTime) as minTravelTime
      from DH_BRA_", year, " where AHAID in ('",
      paste0(as.character(hosp$ID), collapse="', '"), "')
      group by GEOID;"), by='GEOID')
    message('Travel time to nearest hospital under filter ', hospital_filter, ' joined to map data.')

    map <- spChFIDs(map, as.character(map@data$GEOID))
    message('Polygon IDs assigned.')

    # Fill in blank travel times with one second more than the worst travel time of the map, that is, of the state.
    map@data$minTravelTime[is.na(map@data$minTravelTime)] <- max(map@data$minTravelTime, na.rm = T)+1
    
    # Compute the quantiles by population size.
    quants <- sapply(1:24/24, wtd.quantile, x=map@data$minTravelTime, weight=map@data$B01003_001e)
    # Record the quantiles in the data frame
    for(i in 24:1)
      map@data$QTRAVELTIME[map@data$minTravelTime <= quants[[i]]] <- i
    
    map@data$GEOID <- NULL
    
    # Write the shapefile.
    ogr_path <- paste0(output_dir, 'BRA/Maps/', hospital_filter, '/', year, '/', st)
    ogr_name <- paste0(st, '-', year, '-', hospital_filter)
    message('Writing OGR ', ogr_name, '.')
    if(dir.exists(ogr_path))
      unlink(ogr_path, recursive = T)
    if(!dir.exists(ogr_path))
      dir.create(ogr_path, recursive = T)
    colnames(map@data) <- sub('_', '', colnames(map@data))
    writeOGR(map, ogr_path, ogr_name, "ESRI Shapefile")
    message(ogr_name,' written successfuly.')
    
    BRA.nat.map <- if(exists('BRA.nat.map'))
      spRbind(BRA.nat.map, map)
    else
      map
    message('State map bound to national map.')
    
    # Let's try to help the R engine by calling garbage collection after we remove the big map object.
    remove(map)
    gc()
  }
  
  ogr_path <- paste0(output_dir, 'BRA/Maps/', hospital_filter, '/', year, '/BRA')
  ogr_name <- paste0('_', year, '-', hospital_filter)
  if(dir.exists(ogr_path))
    unlink(ogr_path, recursive = T)
  if(!dir.exists(ogr_path))
    dir.create(ogr_path, recursive = T)
  message('Writing OGR ', ogr_name, ' to "', ogr_path, '".')
  writeOGR(BRA.nat.map, ogr_path, ogr_name, "ESRI Shapefile")
  message(ogr_name, ' written successfuly.')
  BRA.nat.map
}

