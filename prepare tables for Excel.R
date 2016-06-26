
setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')
source('Code/core.R')
library(RMySQL)
library(data.table)
library(plyr)
thresholds <- c(0,2.5,5,7.5,10,15,30,60,120)
years <- c(2010, 2014)

makeExcelPopTable <- function(year, minutes=0) {
  if(!exists('B01003'))
    B01003 <<- list()
  if(!exists(as.character(year), where=B01003))
    B01003[[as.character(year)]] <<- dbNoTimeOutGetQuery('select GEOID, B01003_001e from DH_B01003_', year, ';')
  
  if(!exists('BG'))
    BG <<- list()
  if(!exists(as.character(year), where=BG))
    BG[[as.character(year)]] <<- dbNoTimeOutGetQuery('select GEOID, STATE_NAME from DH_BG_', year, ';')
  
  if(!exists('DH'))
    DH <<- list()
  if(!exists(as.character(year), where=DH))
    DH[[as.character(year)]] <<- dbNoTimeOutGetQuery('select GEOID, min(travelTime) as minTravelTime from DH_USA_',
      year, ' group by GEOID;')
  
  tDH <- if(minutes==0)
    DH[[as.character(year)]]
  else
    subset(DH[[as.character(year)]], !is.na(minTravelTime) & (minTravelTime < 60*minutes))
  
  ddply(join(join(B01003[[as.character(year)]], BG[[as.character(year)]], by = 'GEOID', type ='inner'),
    tDH, by='GEOID', type='inner'), c("STATE_NAME"), function(df) sum(df$B01003_001e))
}

connect.to.sql()
if(!exists('res'))
  res <- list()
for(year in years) {
  res[[as.character(year)]] <- do.call(cbind, lapply(thresholds, makeExcelPopTable, year = year))
  res[[as.character(year)]] <- cbind(res[[as.character(year)]][, 1],
    res[[as.character(year)]][, !grepl('STATE', colnames(res[[as.character(year)]]), fixed=T)])
  colnames(res[[as.character(year)]]) <- c('STATE_NAME', thresholds)
  res[[as.character(year)]][,3:10] <- res[[as.character(year)]][,3:10]*100/res[[as.character(year)]][,2]
}

res_diff <- cbind(res[[as.character(years[[2]])]][, 1:2],
  res[[as.character(years[[2]])]][,-1:-2]-res[[as.character(years[[1]])]][,-1:-2])
write.table(res_diff[,-2], "clipboard", sep="\t", row.names=FALSE)
dbDisconnect(sql_conn)
message('Done.')

da <- list()
da$`2014` <- join(join(B01003$`2014`, BG$`2014`, by = 'GEOID', type ='inner'),
  DH$`2014`, by='GEOID', type='inner')
da$`2013` <- join(join(B01003$`2013`, BG$`2013`, by = 'GEOID', type ='inner'),
  DH$`2013`, by='GEOID', type='inner')
da$`2012` <- join(join(B01003$`2012`, BG$`2012`, by = 'GEOID', type ='inner'),
  DH$`2012`, by='GEOID', type='inner')
da$`2011` <- join(join(B01003$`2011`, BG$`2011`, by = 'GEOID', type ='inner'),
  DH$`2011`, by='GEOID', type='inner')
da$`2010` <- join(join(B01003$`2010`, BG$`2010`, by = 'GEOID', type ='inner'),
  DH$`2010`, by='GEOID', type='inner')
da$`2009` <- join(join(B01003$`2009`, BG$`2009`, by = 'GEOID', type ='inner'),
  DH$`2009`, by='GEOID', type='inner')

for(y in 2009:2014)
  print(wtd.quantile(da[[as.character(y)]]$minTravelTime, q=0.99, na.rm = T, da[[as.character(y)]]$B01003_001e))
