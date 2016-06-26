# drithop
# Fabricio Kury, MD, Raymonde Uy, MD, MBA, Raymond Sarmiento, MD, Paul Fontelo, MD, MPH
# ------
# load census data.R
#
# Project start: Early 2015
# Coding start: August 6, 2015
# Core routines start: 16-01-28
# drithop2.R start: April 14, 2016
# 

setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')
is_master <- T
source('Code/core.R')
# census_api_key <- '2167fc3860ab9ec62ae41f243de5917214eaa27b'
# api.key.install(census_api_key)

# if(no_cores > length(year_list))
  # no_cores <- length(year_list)
# message('Launching parallel processing over ', no_cores, ' threads.')
# message('Attention: progress messages will not be visible in this screen.')
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
connect.to.sql()
# foreach(y=1:length(year_list), .packages=c('rgeos', 'rgdal', 'plyr', 'RANN', 'maptools', 'RMySQL')) %dopar% {
  # connect.to.sql()
for(y in 1:length(year_list)) {
  library(rgeos); library(rgdal); library(plyr); library(RANN); library(maptools)
  year <- year_list[[y]]
  message('###### Processing year ', year, '.')
  us.cp <- loadCentersOfPopulation(year)
  hosp <- loadHospitals(year)
  # Normalize variable names.
  if(year %in% c(2011, 2015))
    hosp$ID <- hosp$AHA.ID  
  if(year %in% c(2012, 2013, 2014))
    hosp$LON <- hosp$LONG
  if(year == 2015) {
    hosp$LAT <- hosp$Latitude
    hosp$LON <- hosp$Longitude
  }
  
  # Reduce to only the variables we need.
  hosp <- hosp[, c('ID', 'LAT', 'LON')]
    
  rb.map.l <- list()
  for(st in USA.states$Abbreviation) {
    state_code <- subset(USA.states, Abbreviation==st)$FIPS.Code
    state_name <- subset(USA.states, Abbreviation==st)$Name
    shapefile <- paste0('tl_', year, '_', state_code, '_bg', ifelse(year==2010, '10', ''))
    shapefile_path <- paste0(data_dir, 'USA/Shapefiles/', shapefile)
    
    message('Processing state ', state_name, '.')
    if(!file.exists(paste0(shapefile_path)))
      next
    map <- readOGR(shapefile_path, shapefile)
    message('Map of file ', shapefile, ' loaded.')
    if(year==2010) {
      # In year 2010 the variable names are different.
      map@data$GEOID <- map@data$GEOID10
      map@data$INTPTLAT <- map@data$INTPTLAT10
      map@data$INTPTLON <- map@data$INTPTLON10
    }

    # Inpute the internal point as center of population for the missing cases.
    map.cp <- join(us.cp[, c('GEOID', 'LATITUDE', 'LONGITUDE')],
      map@data[, c('GEOID', 'INTPTLAT', 'INTPTLON')], by='GEOID', type='right')
    map.cp$INTPTLAT <- as.numeric(levels(map.cp$INTPTLAT)[map.cp$INTPTLAT])
    map.cp$INTPTLON <- as.numeric(levels(map.cp$INTPTLON)[map.cp$INTPTLON])
    map.cp$LATITUDE[is.na(map.cp$LATITUDE)] <- map.cp$INTPTLAT[is.na(map.cp$LATITUDE)]
    map.cp$LONGITUDE[is.na(map.cp$LONGITUDE)] <- map.cp$INTPTLON[is.na(map.cp$LONGITUDE)]
    message('Centers of population imputed with missing values in ',
      sum(is.na(map.cp$LATITUDE) | is.na(map.cp$LONGITUDE)), ' cases.')
    map.cp <- map.cp[, c('GEOID', 'LATITUDE', 'LONGITUDE')]
    
    nearest_hosp <- nn2(matrix(c(hosp$LAT,hosp$LON), nrow=length(hosp$LAT)),
      matrix(c(map.cp$LATITUDE, map.cp$LONGITUDE), nrow=length(map.cp$LATITUDE)), k = 5)
    message('Linear distance nearest neighbors calculated.')
    
    # Find the true nearest hospital via HERE API Matrix Routing
    travel_times <- processMatrixRouting(year, reference_departure_time, map.cp, nearest_hosp$nn.idx, hosp)
    
    message(state_name, ' added to map.')
    remove(map)
    gc()
  }
}
stopCluster(cl)

message('Script execution completed.')
