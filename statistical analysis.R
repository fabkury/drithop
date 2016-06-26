# drithop
# Fabricio Kury, MD, Raymonde Uy, MD, MBA, Raymond Sarmiento, MD, Paul Fontelo, MD, MPH
# ------
# statistical analysis.R
#
# Project start: Early 2015
# Coding start: August 6, 2015
# Core routines start: 16-01-28
# drithop2.R start: April 14, 2016
# statistical analysis.R start: May 17, 2016
# 

setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')
source('Code/core.R')
library(foreign)
library(ggplot2)
library(stringr)
library(matrixStats)

hard_limit_travel_time <- 2*60 # Two hours

loadBRAData <- function(year = 2010, ignore.rds = F, dbf = NULL) {
  message('Loading data from Brazil for statistical analysis.')
  rds <- paste0(paste0(output_dir, 'BRA/Maps/community/', year, '/_', year, '-community.rds'))
  if(!ignore.rds && file.exists(rds)) {
    message('Reading cached data from ', rds, '.')
    return(readRDS(rds))
  }
  
  if(is.null(dbf)) {
    dbf <- read.dbf(paste0(output_dir, 'BRA/Maps/community/', year, '/_', year, '-community.dbf'))
    dbf$minTravelTime <- dbf$mnTrvlT
    dbf$mnTrvlT <- NULL
    dbf$minTravelTime <- dbf$minTravelTime/60 # Convert seconds to minutes
    message('Loaded national data frame with travel times.')
  }

  # Read the Census data
  loadOneFile <- function(filename, variables, prefix) {
    message('Loading file ', filename, '.')
    census_data <- data.frame(matrix(0, ncol = length(variables)+1, nrow = 0))
    for(st in BRA.states$Abbreviation) {
      if(st == 'SP') {
        # Sao Paulo is an exception because its data is officially split into two parts.
        census_data <- rbind(census_data, read.csv2(paste0(data_dir, 'BRA/Census/', st,
          '1/Base informaçoes setores2010 universo ', st, '1/CSV/', filename, '_', st, '1.csv'), na.strings = 'X')[,
          c('Cod_setor', paste0('V', str_pad(variables, 3, pad = "0")))])
        census_data <- rbind(census_data, read.csv2(paste0(data_dir, 'BRA/Census/', st,
          '2/Base informaçoes setores2010 universo ', st, '2/CSV/', filename, '_', st, '2.csv'), na.strings = 'X')[,
          c('Cod_setor', paste0('V', str_pad(variables, 3, pad = "0")))])
      }
      else {
        census_data <- rbind(census_data, read.csv2(paste0(data_dir, 'BRA/Census/', st,
          '/Base informaçoes setores2010 universo ', st, '/CSV/', filename, '_', st, '.csv'), na.strings = 'X')[,
          c('Cod_setor', paste0('V', str_pad(variables, 3, pad = "0")))])
      }
      message('Loaded state ', BRA.states[BRA.states$Abbreviation == st, 'Name'], '.')
    }
    
    colnames(census_data) <- c('CDGEOCODI', paste0(prefix, 'V', str_pad(variables, 3, pad = "0")))
    census_data
  }
  
  message('Loading counts of population count and race counts.')
  dbf <- join(dbf, loadOneFile('Pessoa03', 1:6, 'P3'), type = 'left', by = 'CDGEOCODI')
  message('Loading counts of population by income level.')
  dbf <- join(dbf, loadOneFile('DomicilioRenda', 1:14, 'DR'), type = 'left', by = 'CDGEOCODI')
  message('Census data joined to national data frame.')
  saveRDS(dbf, rds)  
  message('Data cached to ', rds, '.')
  dbf
}


addACSTable <- function(dbf, table_id, year)
  join(dbf, dbNoTimeOutGetQuery('select * from DH_', table_id, '_', year, ";"), by='GEOID', type = 'left')


loadUSAData <- function(year = 2010, ignore.rds = F, dbf = NULL) {
  message('Loading data from USA for statistical analysis.')
  rds <- paste0(paste0(output_dir, 'USA/Maps/community/', year, '/USA/_', year, '-community.rds'))
  if(!ignore.rds && file.exists(rds)) {
    message('Reading cached data from ', rds, '.')
    return(readRDS(rds))
  }
  if(is.null(dbf)) {
    dbf <- read.dbf(paste0(output_dir, 'USA/Maps/community/', year, '/USA/_', year, '-community.dbf'))
    message('Loaded national data frame with travel times.')
    dbf$minTravelTime <- dbf$mnTrT
    dbf$mnTrT <- NULL
    dbf$minTravelTime <- dbf$minTravelTime/60
  }
  saveRDS(dbf, rds)
  message('Data cached to ', rds, '.')
  dbf
}


loadUSAStateDBF <- function(st, year= 2010, hospital_filter = 'community')
  read.dbf(paste0(output_dir, 'USA/Maps/', hospital_filter, '/', year, '/', st, '/',
    st, '-', year, '-', hospital_filter, '.dbf'))


regressionRaceUSA <- function(st, year= 2010, hospital_filter = 'community') {
  dbf <- loadUSAStateDBF(st, hospital_filter, year)
  glm(data = dbf, family = Gamma(link = log),
    mnTrT ~ B02001002e + B02001003e + B02001004e + B02001005e + B02001006e + B02001007e)
}


regressionRaceBRA <- function(dbf, vars=c(2,3,6,4,5), max.traveltime = 120) {
  fml <- as.formula(paste('minTravelTime ~ ',
    paste(paste0('P3V', str_pad(vars, 3, pad = "0")), collapse=" + ")))
  glm(data = dbf[(dbf$minTravelTime>0) & (dbf$minTravelTime<max.traveltime),], family = Gamma(link = log), fml)
}


theme_drithop <- theme(
  panel.background = element_rect(fill="white"),
  #axis.ticks = element_line(colour=NA),
  panel.grid = element_line(colour="black"),
  #axis.text.y = element_text(colour=NA),
  axis.text = element_text(colour="#666666", size=32),
  # axis.text.y = element_text(colour="black", size=18),
  panel.grid.major.y = element_line(colour="black"),
  panel.grid.minor.y = element_line(colour="gray"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  axis.title=element_text(size=32),
  title = element_text(colour="black", size=32)
  # text = element_text(size=16, family="Humor Sans")
)


plotBRAHistogram <- function(dbf) {
  ggplot(dbf[dbf$minTravelTime<hard_limit_travel_time,],
    aes(dbf$minTravelTime[dbf$minTravelTime<hard_limit_travel_time],
    weight = dbf$BV002[dbf$minTravelTime<hard_limit_travel_time])) +
    geom_histogram(color = '#009900', fill = '#009900', breaks=1:hard_limit_travel_time) +
    xlab("Minutes") + ylab("Count of population") +
    ggtitle("USA & Brazil: Travel time to nearest community hospital") +
    scale_x_continuous(breaks=seq(0, 180, 15)) +
    ylim(0, 2*10^7) + 
    theme_drithop
}


plotUSAHistogram <- function(dbf) {
  ggplot(dbf[dbf$minTravelTime<hard_limit_travel_time,],
    aes(dbf$minTravelTime[dbf$minTravelTime<hard_limit_travel_time],
    weight = dbf$B01003001e[dbf$minTravelTime<hard_limit_travel_time])) +
    geom_histogram(color = '#990000', fill = '#990000', breaks=1:hard_limit_travel_time) +
    xlab("Minutes") + ylab("Count of population") +
    ggtitle("USA & Brazil: Travel time to nearest community hospital") +
    scale_x_continuous(breaks=seq(0, 180, 15)) +
    ylim(0, 2*10^7) + 
    theme_drithop
}


computeUSAStatistics <- function(dbf) {
#   onehour_pop <- sum(usa.dbf$B01003001e[usa.dbf$minTravelTime > 60], na.rm = T)
#   total_pop <- sum(usa.dbf$B01003001e, na.rm = T)
  for(state_fips in unique(dbf$STATE)) {
    state_name <- subset(USA.states, Code==state_fips)$Name
    m <- glm(data = dbf[dbf$STATE==state_fips,], family = Gamma(link = log),
      minTravelTime ~ B02001002e + B02001003e + B02001004e + B02001005e + B02001006e + B02001007e)
    
    st.res <- data.frame(State = state_name, matrix(exp(coefficients(m)), nrow = 1))
    if(!exists('retval'))
      retval <- st.res
    else
      retval <- rbind(retval, st.res)
  }
  colnames(retval) <- c('State', 'Intercept', 'White', 'Black', 'Native American', 'Asian', 'Pacific Islander', 'Other')
  retval$Intercept <- NULL
  retval
}


computeBRAStatistics <- function(dbf) {
  onehour_pop <- sum(dbf$BV002[dbf$minTravelTime > 60], na.rm = T)
  total_pop <- sum(dbf$BV002, na.rm = T)
}


computeUSARaceProportions <- function(time_threshold, dbf=usa.dbf) {
  selection <- (usa.dbf$minTravelTime < time_threshold) & (usa.dbf$minTravelTime > (time_threshold-1))
  totpop <- sum(usa.dbf$B02001001e[selection], na.rm = T)
  data.frame(
    # Total=100*sum(usa.dbf$B02001001e[usa.dbf$minTravelTime < time_threshold], na.rm = T)/sum(usa.dbf$B02001001e, na.rm = T),
    White=100*sum(usa.dbf$B02001002e[selection], na.rm = T)/totpop,
    Black=100*sum(usa.dbf$B02001003e[selection], na.rm = T)/totpop,
    Native.American=100*sum(usa.dbf$B02001004e[selection], na.rm = T)/totpop,
    Asian=100*sum(usa.dbf$B02001005e[selection], na.rm = T)/totpop,
    Pacific.Islander=100*sum(usa.dbf$B02001006e[selection], na.rm = T)/totpop,
    Other=100*sum(usa.dbf$B02001007e[selection], na.rm = T)/totpop
    )
}


computeBRARaceProportions <- function(time_threshold, dbf=bra.dbf) {
  selection <- (dbf$minTravelTime < time_threshold) & (dbf$minTravelTime > (time_threshold-1))
  totpop <- sum(dbf$P3V001[selection], na.rm = T)
  data.frame(
    # Total=100*sum(dbf$P3V001[dbf$minTravelTime < time_threshold], na.rm = T)/sum(dbf$P3V001, na.rm = T),
    White=100*sum(dbf$P3V002[selection], na.rm = T)/totpop,
    Black=100*sum(dbf$P3V003[selection], na.rm = T)/totpop,
    Native=100*sum(dbf$P3V006[selection], na.rm = T)/totpop,
    Asian=100*sum(dbf$P3V004[selection], na.rm = T)/totpop,
    # Pacific.Islander=sum(dbf$B02001006e[selection], na.rm = T)/totpop,
    Pardo=100*sum(dbf$P3V005[selection], na.rm = T)/totpop)
}


computeBRAIncomeProportions <- function(time_threshold, dbf=bra.dbf) {
  selection <- (dbf$minTravelTime < time_threshold) & (dbf$minTravelTime > (time_threshold-1))
  tot <- sum(dbf$DRV001[selection]+dbf$DRV005[selection]+dbf$DRV006[selection]+dbf$DRV007[selection]+
    dbf$DRV008[selection]+dbf$DRV009[selection]+dbf$DRV010[selection]+dbf$DRV011[selection]+dbf$DRV012[selection]+
    dbf$DRV013[selection]+dbf$DRV014[selection], na.rm = T)
  data.frame(
    #Total=sum(dbf$DRV001[dbf$minTravelTime < time_threshold], na.rm = T)/sum(dbf$DRV001, na.rm = T),
    G.0=100*sum(dbf$DRV014[selection], na.rm = T)/tot,
    G.LT1_8=100*sum(dbf$DRV005[selection], na.rm = T)/tot,
    G.1_8.1_4=100*sum(dbf$DRV006[selection], na.rm = T)/tot,
    G.1_4.1_2=100*sum(dbf$DRV007[selection], na.rm = T)/tot,
    G.1_2.1=100*sum(dbf$DRV008[selection], na.rm = T)/tot,
    G.1_2=100*sum(dbf$DRV009[selection], na.rm = T)/tot,
    G.2_3=100*sum(dbf$DRV010[selection], na.rm = T)/tot,
    G.3_5=100*sum(dbf$DRV011[selection], na.rm = T)/tot,
    G.GT5=100*((sum(dbf$DRV012[selection], na.rm = T)+sum(dbf$DRV013[selection], na.rm = T)))/tot
    #G.GT10=sum(dbf$DRV013[selection], na.rm = T)/tot
    )
}



computeUSAIncomeProportions <- function(time_threshold, dbf=usa.dbf) {
  selection <- (usa.dbf$minTravelTime < time_threshold) & (usa.dbf$minTravelTime > (time_threshold-1))
  totpop <- sum(usa.dbf$C17002001e[selection], na.rm = T)
  data.frame(
    #Total=sum(usa.dbf$C17002001e[usa.dbf$minTravelTime < time_threshold], na.rm = T)/sum(usa.dbf$C17002001e, na.rm = T),
    `< .5 PL`=100*sum(usa.dbf$C17002002e[selection], na.rm = T)/totpop,
    `.5 to .99 PL`=100*sum(usa.dbf$C17002003e[selection], na.rm = T)/totpop,
    `1.00 to 1.25 PL`=100*sum(usa.dbf$C17002004e[selection], na.rm = T)/totpop,
    `1.25 to 1.49 PL`=100*sum(usa.dbf$C17002005e[selection], na.rm = T)/totpop,
    `1.50 to 1.84 PL`=100*sum(usa.dbf$C17002006e[selection], na.rm = T)/totpop,
    `1.85 to 1.99 PL`=100*sum(usa.dbf$C17002007e[selection], na.rm = T)/totpop,
    `> 2 PL`=100*sum(usa.dbf$C17002008e[selection], na.rm = T)/totpop
    )
}


computeUSAIncomeProportionsPT <- function(time_threshold, dbf=usa.pt.dbf) {
  if(is.na(time_threshold))
    selection <- is.na(dbf$minTravelTimePT) | (dbf$minTravelTimePT > 60)
  else
    selection <- !is.na(dbf$minTravelTimePT) &
      (dbf$minTravelTimePT < time_threshold) &
      (dbf$minTravelTimePT > (time_threshold-1))
  totpop <- sum(dbf$C17002001e[selection], na.rm = T)
  data.frame(
    #Total=sum(dbf$C17002001e[dbf$minTravelTimePT < time_threshold], na.rm = T)/sum(dbf$C17002001e, na.rm = T),
    `< .5 PL`=100*sum(dbf$C17002002e[selection], na.rm = T)/totpop,
    `.5 to .99 PL`=100*sum(dbf$C17002003e[selection], na.rm = T)/totpop,
    `1.00 to 1.25 PL`=100*sum(dbf$C17002004e[selection], na.rm = T)/totpop,
    `1.25 to 1.49 PL`=100*sum(dbf$C17002005e[selection], na.rm = T)/totpop,
    `1.50 to 1.84 PL`=100*sum(dbf$C17002006e[selection], na.rm = T)/totpop,
    `1.85 to 1.99 PL`=100*sum(dbf$C17002007e[selection], na.rm = T)/totpop,
    `> 2 PL`=100*sum(dbf$C17002008e[selection], na.rm = T)/totpop)
}


computeUSARacePT <- function(time_threshold, dbf=usa.pt.dbf) {
    if(is.na(time_threshold))
    selection <- is.na(dbf$minTravelTimePT) | (dbf$minTravelTimePT > 60)
  else
    selection <- !is.na(dbf$minTravelTimePT) &
      (dbf$minTravelTimePT < time_threshold) &
      (dbf$minTravelTimePT > (time_threshold-1))
  totpop <- sum(dbf$B02001001e[selection], na.rm = T)
  data.frame(
    # Total=100*sum(dbf$B02001001e[dbf$minTravelTime < time_threshold], na.rm = T)/sum(dbf$B02001001e, na.rm = T),
    White=100*sum(dbf$B02001002e[selection], na.rm = T)/totpop,
    Black=100*sum(dbf$B02001003e[selection], na.rm = T)/totpop,
    Native.American=100*sum(dbf$B02001004e[selection], na.rm = T)/totpop,
    Asian=100*sum(dbf$B02001005e[selection], na.rm = T)/totpop,
    Pacific.Islander=100*sum(dbf$B02001006e[selection], na.rm = T)/totpop,
    Other=100*sum(dbf$B02001007e[selection], na.rm = T)/totpop)
}

computeUSAEmergency <- function(time_threshold, dbf=e14) {
  if(is.na(time_threshold))
    selection <- is.na(dbf$meanTravelTime) | (dbf$meanTravelTime > 60)
  else
    selection <- !is.na(dbf$meanTravelTime) &
      (dbf$meanTravelTime < time_threshold) &
      (dbf$meanTravelTime > (time_threshold-1))
  totpop <- sum(dbf$sumPop[selection], na.rm = T)
  data.frame(
    Population=sum(dbf$sumPop[selection], na.rm = T),
    Travel_Time=sum(dbf$meanTravelTime[selection], na.rm = T))
}


plotAgainstTravelTime <- function(fun, graph_title, legend_title, group_names, breaks=c(1:60, NA)) {
  dat <- rbind.fill(lapply(breaks, fun))
  dat.stack <- stack(dat)
  dat.stack$x <- rep(seq_len(nrow(dat)), ncol(dat))
  dat.stack$ind <- factor(dat.stack$ind, levels = colnames(dat))
  ggplot(dat.stack, aes(x=x, y=values, group=ind, color = ind)) +
    geom_line(size=2.5) +
    theme_drithop +
    scale_x_continuous(breaks = c(seq(0, 60, 5), NA), labels = c(as.character(seq(0, 60, 5)), NA)) +
    labs(title=graph_title, x='Minimum travel time to nearest hospital (minutes)', y='% of population') +
    guides(color=guide_legend(title=legend_title, keyheight=2)) +
    theme(legend.text=element_text(size=24)) +
    scale_color_discrete(labels=group_names)
}

# usa.dbf <- loadUSAData()
# bra.dbf <- loadBRAData()
# 
# plotAgainstTravelTime(computeUSAIncomeProportions,
#   'USA: Geographical access to hospitals\naccording to poverty level', 'Poverty level',
#   c('< .5 PL', '.5 to .99 PL', '1.00 to 1.25 PL', '1.25 to 1.49 PL', '1.50 to 1.84 PL', '1.85 to 1.99 PL', '> 2 PL'))
# 
# plotAgainstTravelTime(computeBRAIncomeProportions,
#   'Brazil: Geographical access to hospitals\naccording to income per capita', 'Income group',
#   c('Zero', '< 1/8 min. wage', '1/8 to 1/4 min. wage',
#  '1/4 to 1/2 min. wage', '1/2 to 1 min. wage', '1 to 2 min. wage',
#   '2 to 3 min. wage', '3 to 5 min. wage', '> 5 min. wage'))

# plotAgainstTravelTime(computeUSARaceProportions,
#   'USA: Geographical access to\nhospitals according to race', 'Race',
#   c('Caucasian', 'African Am.', 'Native Am.', 'Asian', 'Pacific Isl.', 'Other'))
# 
# plotAgainstTravelTime(computeBRARaceProportions,
#   'Brazil: Geographical access to\nhospitals according to race', 'Race',
#   c('White', 'Black', 'Native', 'Asian', 'Pardo'))


loadUSAPT <- function() {
  dbNoTimeOutGetQuery("select a.GEOID, b.minTravelTime
    from (select distinct GEOID from DH_USA_PT_2010) a
    left join
    (select GEOID, min(travelTime) as minTravelTime
    from DH_USA_PT_2010
      where travelTime is not null
    group by GEOID) b
    on a.GEOID = b.GEOID;")
}


loadDHUSA <- function() {
  dbNoTimeOutGetQuery("select a.GEOID, b.minTravelTime
    from (select distinct GEOID from DH_USA_2010) a
    left join
    (select GEOID, min(travelTime) as minTravelTime
      from DH_USA_2010
      where travelTime is not null
      group by GEOID) b
    on a.GEOID = b.GEOID;")
}


loadPTState <- function(st) {
  fips <- subset(USA.states, Abbreviation == st)$Code
  dbNoTimeOutGetQuery("select a.GEOID, b.minTravelTime
    from (select distinct GEOID
    from DH_USA_PT_2010
    where GEOID LIKE '", fips, "%') a
    left join
    (select GEOID, min(travelTime) as minTravelTime
    from DH_USA_PT_2010
    where GEOID LIKE '", fips, "%'
      and travelTime is not null
    group by GEOID) b
    on a.GEOID = b.GEOID;")
}

loadPTStateBRA <- function(st) {
  cd <- subset(BRA.states, Abbreviation == st)$Code
  dbNoTimeOutGetQuery("select a.GEOID, b.minTravelTime
    from (select distinct GEOID
    from DH_BRA_PT_2010
    where GEOID LIKE '", cd, "%') a
    left join
    (select GEOID, min(travelTime) as minTravelTime
    from DH_BRA_PT_2010
    where GEOID LIKE '", cd, "%'
      and travelTime is not null
    group by GEOID) b
    on a.GEOID = b.GEOID;")
}


# loadPTStateBRA <- function() {
#   dbNoTimeOutGetQuery("select a.GEOID, b.minTravelTime
#     from (select distinct GEOID
#     from DH_BRA_2010) a
#     left join
#     (select GEOID, min(travelTime) as minTravelTime
#     from DH_BRA_2010
#       and travelTime is not null
#     group by GEOID) b
#     on a.GEOID = b.GEOID;")
# }


loadBRAPT <- function() {
  dbNoTimeOutGetQuery("select a.GEOID, b.minTravelTime
    from (select distinct GEOID from DH_BRA_PT_2010) a
    left join
    (select GEOID, min(travelTime) as minTravelTime
      from DH_BRA_PT_2010
      where travelTime is not null
      group by GEOID) b
    on a.GEOID = b.GEOID;")
}


printPTState <- function(st) {
  st.data <- loadPTState(st)
  st.data <- join(st.data, usa.pt.dbf[, c('GEOID', 'B01003001e')], by='GEOID')
  st.median <- round(weightedMedian(st.data$minTravelTimePT, st.data$B01003001e, na.rm = T)/60, 2)
  st.unknown <- round(sum(st.data$B01003001e[is.na(st.data$minTravelTime)], na.rm = T)*100/
    sum(st.data$B01003001e, na.rm = T), 2)
  data.frame(State=subset(USA.states, Abbreviation==st)$Name, Median=st.median, Unrouted=st.unknown)
}


printPTStateBRA <- function(st) {
  browser()
  st.data <- loadPTStateBRA(st)
  st.data <- join(st.data, bra.pt.dbf[, c('GEOID', 'P3V001')], by='GEOID')
  st.median <- round(weightedMedian(st.data$minTravelTimePT, st.data$P3V001, na.rm = T)/60, 2)
  st.unknown <- round(sum(st.data$P3V001[is.na(st.data$minTravelTimePT)], na.rm = T)*100/
    sum(st.data$P3V001, na.rm = T), 2)
  data.frame(State=subset(USA.states, Abbreviation==st)$Name, Median=st.median, Unrouted=st.unknown)
}

# usa.pt.dbf <- rbind.fill(lapply(states_to_process, loadPTState))
# usa.pt.dbf$minTravelTimePT <- usa.pt.dbf$minTravelTime
# usa.pt.dbf$minTravelTime <- NULL
# usa.pt.dbf$minTravelTimePT <- usa.pt.dbf$minTravelTimePT/60
# usa.pt.dbf <- join(usa.pt.dbf, usa.dbf, by='GEOID', type = 'left')
# 
# plotAgainstTravelTime(computeUSAIncomeProportionsPT,
#   'USA: Public transport access to hospitals\naccording to poverty level', 'Poverty level',
#   c('< .5 PL', '.5 to .99 PL', '1.00 to 1.25 PL', '1.25 to 1.49 PL', '1.50 to 1.84 PL', '1.85 to 1.99 PL', '> 2 PL'))
# 
# plotAgainstTravelTime(computeUSARacePT,
#   'USA: Public transportation access to\nhospitals according to race', 'Race',
#   c('Caucasian', 'African Am.', 'Native Am.', 'Asian', 'Pacific Isl.', 'Other'))
# 
# bra.pt.dbf <- rbind.fill(lapply(BRA.states$Abbreviation, loadPTStateBRA))
# bra.pt.dbf$minTravelTimePT <- bra.pt.dbf$minTravelTime
# bra.pt.dbf$minTravelTime <- NULL
# bra.pt.dbf$minTravelTimePT <- bra.pt.dbf$minTravelTimePT/60
# bra.dbf <- loadBRAData()
# bra.dbf$GEOID <- bra.dbf$CDGEOCODI
# bra.pt.dbf <- join(bra.pt.dbf, bra.dbf, by='GEOID', type = 'left')
# remove(bra.dbf)


computeBRAIncomeProportionsPT <- function(time_threshold, dbf=bra.pt.dbf) {
  if(is.na(time_threshold))
    selection <- is.na(dbf$minTravelTimePT) | (dbf$minTravelTimePT > 60)
  else
    selection <- !is.na(dbf$minTravelTimePT) & (dbf$minTravelTimePT < time_threshold) &
    (dbf$minTravelTimePT > (time_threshold-1))
  tot <- sum(dbf$DRV001[selection]+dbf$DRV005[selection]+dbf$DRV006[selection]+dbf$DRV007[selection]+
    dbf$DRV008[selection]+dbf$DRV009[selection]+dbf$DRV010[selection]+dbf$DRV011[selection]+dbf$DRV012[selection]+
    dbf$DRV013[selection]+dbf$DRV014[selection], na.rm = T)
  data.frame(
    #Total=sum(dbf$DRV001[dbf$minTravelTime < time_threshold], na.rm = T)/sum(dbf$DRV001, na.rm = T),
    G.0=100*sum(dbf$DRV014[selection], na.rm = T)/tot,
    G.LT1_8=100*sum(dbf$DRV005[selection], na.rm = T)/tot,
    G.1_8.1_4=100*sum(dbf$DRV006[selection], na.rm = T)/tot,
    G.1_4.1_2=100*sum(dbf$DRV007[selection], na.rm = T)/tot,
    G.1_2.1=100*sum(dbf$DRV008[selection], na.rm = T)/tot,
    G.1_2=100*sum(dbf$DRV009[selection], na.rm = T)/tot,
    G.2_3=100*sum(dbf$DRV010[selection], na.rm = T)/tot,
    G.3_5=100*sum(dbf$DRV011[selection], na.rm = T)/tot,
    G.GT5=100*((sum(dbf$DRV012[selection], na.rm = T)+sum(dbf$DRV013[selection], na.rm = T)))/tot
    #G.GT10=sum(dbf$DRV013[selection], na.rm = T)/tot
    )
}


computeBRARacePT <- function(time_threshold, dbf=bra.pt.dbf) {
  if(is.na(time_threshold))
    selection <- is.na(dbf$minTravelTimePT) | (dbf$minTravelTimePT > 60)
  else
    selection <- !is.na(dbf$minTravelTimePT) &
      (dbf$minTravelTimePT < time_threshold) &
      (dbf$minTravelTimePT > (time_threshold-1))
  totpop <- sum(dbf$P3V001[selection], na.rm = T)
  data.frame(
    # Total=100*sum(dbf$P3V001[dbf$minTravelTime < time_threshold], na.rm = T)/sum(dbf$P3V001, na.rm = T),
    White=100*sum(dbf$P3V002[selection], na.rm = T)/totpop,
    Black=100*sum(dbf$P3V003[selection], na.rm = T)/totpop,
    Native=100*sum(dbf$P3V006[selection], na.rm = T)/totpop,
    Asian=100*sum(dbf$P3V004[selection], na.rm = T)/totpop,
    # Pacific.Islander=sum(dbf$B02001006e[selection], na.rm = T)/totpop,
    Pardo=100*sum(dbf$P3V005[selection], na.rm = T)/totpop)
}


# plotAgainstTravelTime(computeBRAIncomeProportionsPT,
#   'Brazil: Public transport access to hospitals\naccording to income per capita', 'Income group',
#   c('Zero', '< 1/8 min. wage', '1/8 to 1/4 min. wage',
#   '1/4 to 1/2 min. wage', '1/2 to 1 min. wage', '1 to 2 min. wage',
#   '2 to 3 min. wage', '3 to 5 min. wage', '> 5 min. wage'))
# 
# plotAgainstTravelTime(computeBRARacePT,
#   'Brazil: Public transport access to\nhospitals according to race', 'Race',
#   c('White', 'Black', 'Native', 'Asian', 'Pardo'))

# write.table(data.frame(Name=USA.states$Name, Code=USA.states$Code, medianTravelTime=sapply(USA.states$Code, function(e) { medianTravelTime=weightedMedian(usa.dbf[usa.dbf$state_fips==e, 'minTravelTime'], w=usa.dbf[usa.dbf$state_fips==e, 'b02001_001e'], na.rm = T)})), "clipboard", sep="\t", row.names=FALSE)
