# drithop
# Fabricio Kury, MD, Raymonde Uy, MD, MBA, Raymond Sarmiento, MD, Paul Fontelo, MD, MPH
# ------
# download data.R
#
# Project start: Early 2015
# Coding start: August 6, 2015
# Core routines start: 16-01-28
# drithop2.R start: April 14, 2016
# drithop.R start: April 30, 2016 1:42 PM
#

setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')
source('Code/core.R')

source('Code/download data.R') 

source('Code/request travel times.R')

source('Code/make maps.R')

acs_year_list <- 2010#, 2014)#:2009

hospital_filters <- c('community', 'trauma')#, 'emergency')
max_threads <- 7
p_length <- ifelse(max_threads > 0,
  min(min(no_cores, length(acs_year_list)), max_threads),
  min(no_cores, length(acs_year_list)))
# connect.to.sql()
for(hospital_filter in hospital_filters) {
  message('Making maps of filter "', hospital_filter, '".')
  message('Launching parallel processing over ', p_length, ' threads.')
  message('Attention: progress messages will not be visible in this screen.')
  # cl <- makeCluster(p_length)
  # registerDoParallel(cl)
#   foreach(y=1:length(acs_year_list),
#     .packages=c('rgeos', 'rgdal', 'plyr', 'RANN', 'maptools', 'RMySQL', 'reldist')) %do% {
#     if(exists('sql_conn', where = .GlobalEnv))
#       remove('sql_conn', envir = .GlobalEnv)
#     connect.to.sql()
#     repeat {
#       tryres <- try(requestHEREYearUSA(acs_year_list[[y]], hospital_filter, 'publicTransport'))
#       if(!inherits(tryres, 'try-error'))
#         break
#     }
    # makeMapYear(acs_year_list[[y]], hospital_filter, acs_tables)
    # makeMapYearBRA(acs_year_list[[y]], hospital_filter)
  # }
  # stopCluster(cl)
  # remove(cl)
  
  n_threads <- no_cores*3
  p_length <- n_threads*2
  for(y in acs_year_list) {
#     repeat {
#       connect.to.sql()
#       tryres <- try(requestHEREYearBRA(y, hospital_filter, 'publicTransport'))
#       if(!inherits(tryres, 'try-error'))
#         break
#     }
    
    repeat {
      connect.to.sql()
      tryres <- try(requestHEREYearUSA(y, hospital_filter, 'publicTransport'))
      if(!inherits(tryres, 'try-error'))
        break
      Sys.sleep(30)
    }
  }
}
message('Execution completed at ', curtime(), '.')
