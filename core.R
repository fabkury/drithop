# drithop
# Fabricio Kury, MD, Raymonde Uy, MD, MBA, Raymond Sarmiento, MD, Paul Fontelo, MD, MPH
# ------
# core.R
#
# Project start: Early 2015
# Coding start: August 6, 2015
# Core routines start: 16-01-28
# drithop2.R start: April 14, 2016
# 

setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')

#
## Globals
curtime <- function() format(Sys.time(), "%Y_%m_%d %H_%M")
message('Execution started at ', curtime(), '.')
library(RMySQL)
library(plyr)
sql_port <- 3306
sql_database <- # SQL DATABASE
sql_host <- # SQL SERVER
sql_username <- # USERNAME
sql_password <- # PASSWORD
if(!exists('DH_USA', envir = .GlobalEnv))
  assign('DH_USA', list(), envir = .GlobalEnv)

acs_year_list <- 2009:2014

data_dir <- "./Data/"
output_dir <- "./Output/"
log_dir <- paste0(output_dir, 'Logs/')
rds_dir <- paste0(data_dir, 'rds/')
acs_base_dir <- paste0(data_dir, 'USA/ACS/')
max_query_attempts <- 3
sql_upload_chunk_size <- 5000

USA.states <- read.csv(paste0(data_dir, 'USA/USA States.csv'), colClasses = 'character')
BRA.states <- read.csv(paste0(data_dir, 'BRA/BRA States.csv'), colClasses = 'character')
acs_tables <- c('B01002I', 'B01003', 'B02001', 'C17002', 'B15002')

# Prepare ground for logging.
dir.create(log_dir, showWarnings = F, recursive = T)

states_to_process <- c(
  'AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS',
  'KY', 'LA', 'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV',
  'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY')

loadHospitalsOneYear <- function(year, ahaid_filter) {
  hosp <- read.csv(paste0(data_dir, 'USA/AHA/AHA HD ', year, '.csv'), header = T)
  
  # Normalize variable names that sometimes differ between years.
  if(year %in% c(2011, 2015)) {
    hosp$LAT <- hosp$Latitude
    hosp$LON <- hosp$Longitude
    hosp$Latitude <- NULL
    hosp$Longitude <- NULL
    hosp$ID <- hosp$AHA.ID
    hosp$AHA.ID <- NULL
  }
  
  if(year %in% c(2009, 2010, 2012, 2013, 2014)) {
    hosp$LON <- hosp$LONG
    hosp$LONG <- NULL
  }

  if(year < 2015) {
    vc1 <- read.csv(paste0(data_dir, 'USA/AHA/', year, ifelse(year==2013,
      '/AnnualSurvey/Comma/', '/COMMA/'), ifelse(year %in% c(2009, 2012), paste0('as', substr(as.character(year), 3, 4),
      'svc1'), paste0('AS', substr(as.character(year), 3, 4), 'SVC1')), '.csv'))
#     vc1 <- subset(vc1, select = c('ID', 'EMDEPHOS', 'EMDEPSYS', 'EMDEPVEN')) #'EMDEPNET', 
    vc1$ID <- as.character(vc1$ID)

    vc2 <- read.csv(paste0(data_dir, 'USA/AHA/', year, ifelse(year==2013,
      '/AnnualSurvey/Comma/', '/COMMA/'), ifelse(year %in% c(2009, 2012), paste0('as', substr(as.character(year), 3, 4),
      'svc2'), paste0('AS', substr(as.character(year), 3, 4), 'SVC2')), '.csv'))
    vc2$ID <- as.character(vc2$ID)
    
    hosp <- join(hosp, vc1, by='ID', type = 'left')
    hosp <- join(hosp, vc2, by='ID', type = 'left')

    hosp$EMDEPHOS <- as.logical(hosp$EMDEPHOS)
    hosp$EMDEPSYS <- as.logical(hosp$EMDEPSYS)
#   hosp$EMDEPNET <- as.logical(hosp$EMDEPNET)
    hosp$EMDEPVEN <- as.logical(hosp$EMDEPVEN)
    hosp$TRAUMHOS <- as.logical(hosp$TRAUMHOS)
  }
  
  if(year == 2015) {
    hosp$EMDEPHOS <- hosp$Emergency.Department=='Y'
    hosp$Emergency.Department <- NULL
    hosp$EMDEPSYS <- NA
    hosp$EMDEPVEN <- NA
    
    hosp$TRAUMHOS <- hosp$Certified.trauma.center=='Y'
    hosp$Certified.trauma.center <- NULL
  }
  
  if(is.factor(hosp$LAT))
    hosp$LAT <- as.numeric(levels(hosp$LAT)[hosp$LAT])
  if(is.factor(hosp$LON))
    hosp$LON <- as.numeric(levels(hosp$LON)[hosp$LON])
  
  # Normalize more variable names.
  if(year %in% c(2011, 2015)) {
    hosp$CHC <- hosp$Community.hospital.designation=='Community'
    hosp[hosp$CHC, 'CHC'] <- '1' # Normalize with the rest of the years.
    hosp$Community.hospital.designation <- NULL
  }
  
  # Add state abbreviations in year 2011
  if(year == 2011) {
    fillin_mstate <- USA.states[,c('Abbreviation', 'Name')]
    fillin_mstate$Name <- tolower(fillin_mstate$Name)
    colnames(fillin_mstate) <- c('MSTATE', 'MSTATE_FORJOIN')
    hosp$MSTATE_FORJOIN <- tolower(hosp$State..physical.)
    hosp <- join(hosp, fillin_mstate, by = 'MSTATE_FORJOIN')
    hosp$MSTATE_FORJOIN <- NULL
  }
  
  # subset(hosp, MSTATE %in% states_to_process)
  hosp
}


loadHospitals <- function(year, ahaid_filter, update_sql_table = F) {
  hosp <- suppressWarnings(loadHospitalsOneYear(year, ahaid_filter))
  
  # Try to fill in missing latitude and longitude with other year's data.
  for(other_year in 2015:2009) {
    if(other_year == year)
      next
    if(sum(is.na(hosp$LAT) | sum(is.na(hosp$LON))) == 0)
      break
    next_y_hosp <- suppressWarnings(loadHospitalsOneYear(other_year, ahaid_filter))
    next_y_hosp <- next_y_hosp[next_y_hosp$ID %in% hosp$ID,]
    fillin_latlong <- next_y_hosp[,c('ID', 'LAT', 'LON')]
    colnames(fillin_latlong) <- c('ID', 'LAT2', 'LON2')
    hosp <- join(hosp, fillin_latlong, by = 'ID')
    missing_latlon <- (is.na(hosp$LAT) | is.na(hosp$LON))
    hosp[missing_latlon, 'LAT'] <- hosp[missing_latlon, 'LAT2']
    hosp[missing_latlon, 'LON'] <- hosp[missing_latlon, 'LON2']
    hosp$LAT2 <- NULL
    hosp$LON2 <- NULL
  }
  
  # Subset hospitals
  if(ahaid_filter == 'emergency')
    hosp <- subset(hosp, EMDEPHOS) # EMDEPNET | EMDEPSYS | EMDEPVEN

  if(ahaid_filter == 'trauma')
    hosp <- subset(hosp, TRAUMHOS)

  if(ahaid_filter == 'community')
    hosp <- subset(hosp, CHC == '1')
  
  if(update_sql_table) {
    table_name <- paste0('DH_H', year, '_', ahaid_filter)
    try(dbNoTimeOutGetQuery('drop table ', table_name, ';'), silent = T)
    dbNoTimeOutGetQuery('create table ', table_name, ' (AHAID VARCHAR(20))')
    for(slice in make.chunks.by.size(1:nrow(hosp), sql_upload_chunk_size))
      dbNoTimeOutGetQuery('insert into ', table_name, " values ('", paste(hosp[slice,]$ID, collapse="'), ('"), "');")
    dbNoTimeOutGetQuery('create unique index AHAID on ', table_name, ' (AHAID);')
  }
  
  hosp
}


loadCentersOfPopulation <- function(year) {
  vintage <- ifelse(year < 2010, 2000, 2010)
  vintage_rds <- paste0(data_dir, 'USA/Centers of population/us.cp', vintage, '.rds')
  if(file.exists(vintage_rds))
    us.cp <- readRDS(vintage_rds)
  else {
    library(stringr)
    make.GEOID <- function(geoid.info) {
      geoid.info <- sub(' *', '', geoid.info[1:4])
      paste0(str_pad(geoid.info[1], 2, pad = '0'), str_pad(geoid.info[2], 3, pad = '0'),
        str_pad(geoid.info[3], 6, pad = '0'), geoid.info[4])
    }
    
    us.cp <- read.csv(paste0(data_dir, 'USA/Centers of population/', if(vintage == 2000)
      'blkgrp_pop_centroid_withname.txt' else 'CenPop2010_Mean_BG.txt'))
    us.cp$GEOID <- apply(us.cp, 1, make.GEOID)
    if(vintage == 2000) {
      us.cp$LATITUDE <- us.cp$cntrlatc
      us.cp$cntrlatc <- NULL
      us.cp$LONGITUDE <- us.cp$cntrlonc
      us.cp$cntrlonc <- NULL
    }
    # Fix some rare problems with the data. 
    us.cp$LATITUDE[(us.cp$LATITUDE == '+.') | (us.cp$LATITUDE == '-.')] <- NA
    us.cp$LONGITUDE[(us.cp$LONGITUDE == '+.') | (us.cp$LONGITUDE == '-.')] <- NA
    if(is.factor(us.cp$LATITUDE))
      us.cp$LATITUDE <- as.numeric(levels(us.cp$LATITUDE)[us.cp$LATITUDE])
    if(is.factor(us.cp$LONGITUDE))
      us.cp$LONGITUDE <- as.numeric(levels(us.cp$LONGITUDE)[us.cp$LONGITUDE])
    saveRDS(us.cp, vintage_rds)
  }
  message('Centers of population from vintage ', vintage, ' loaded.') 
  us.cp
}


library(doParallel)
max_cores <- detectCores()
no_cores <- max_cores
if(no_cores > 4)
  no_cores <- no_cores-1


connect.to.sql <- function() {
  if(!exists('sql_conn', where = .GlobalEnv))
    assign('sql_conn', dbConnect(RMySQL::MySQL(), host = sql_host, username = sql_username,
      password = sql_password, port = sql_port, dbname = sql_database), envir = .GlobalEnv)
}


# make.chunks.by.size: Thanks to Ferdinand Kraft at StackOverflow
# http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
make.chunks.by.size <- function(x, size)
  split(x,sort(rep(1:(trunc(length(x)/size)+1),size))[1:length(x)])

make.n.chunks <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))

dbNoTimeOutGetQuery <- function(...) {
  query <- paste0(...)
  res <- try(dbGetQuery(sql_conn, query), silent=T)
  if(inherits(res, 'try-error')) {
    if(grepl('MySQL server has gone away', res)) {
      remove('sql_conn', envir = .GlobalEnv)
      connect.to.sql()
      return(dbGetQuery(get('sql_conn', envir = .GlobalEnv), query))
    }
    else
      stop(res)
  }
  res
}
