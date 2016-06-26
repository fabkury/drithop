# drithop
# Fabricio Kury, MD, Raymonde Uy, MD, MBA, Raymond Sarmiento, MD, Paul Fontelo, MD, MPH
# ------
# download data.R
#
# Project start: Early 2015
# Coding start: August 6, 2015
# Core routines start: 16-01-28
# drithop2.R start: April 14, 2016
# 

setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')
source('Code/core.R')

substrRight <- function(x, n)
  substr(x, nchar(x)-n+1, nchar(x))

loadACSBlockGroupsIntoSQL <- function(year, st) {
  ## Read the geography file. Subset to only census block groups.
  acs_dir <- paste0(acs_base_dir, year, '/')
  geo_table <- rename(read.xlsx2(paste0(acs_dir, 'Geography/', tolower(st), '.xls'), sheetIndex = 1,
    stringsAsFactors = F, colClasses = c('character', 'numeric', 'character', 'character')),
    c('GEOGRAPHY.NAME' = 'GEOGRAPHY_NAME')) # Make the name not contain a '.'.
  if(year == 2012 && ncol(geo_table) > 4)
    # In year 2012 there is an extraneous last column devoid of content.
    geo_table <- geo_table[,-ncol(geo_table)]
  
  # Extract only block groups from the geography file. Use a double check: length of the GEOID and the mention of
  # "Block Group" in its GEOGRAPHY NAME column.
  # Reference for understanding Block Group GEOIDs: https://www.census.gov/geo/reference/geoidentifiers.html
  geo_table <- subset(geo_table, (nchar(GEOID)==19) & grepl('Block Group', GEOGRAPHY_NAME, fixed = T))
  geo_table_ldply <- ldply(strsplit(geo_table$GEOGRAPHY_NAME, ', ', fixed = T))[,-1:-2]
  geo_table$COUNTY_NAME <-  gsub("'", "\\'", geo_table_ldply[,1], fixed = T)
  geo_table$STATE_NAME <-geo_table_ldply[,2]
  geo_table$GEOGRAPHY_NAME <- NULL
  geo_table$LOGRECNO <- NULL
  geo_table$GEOID <- substrRight(geo_table$GEOID, 12)
  geo_table$STATE_FIPS <- substr(geo_table$GEOID, 1, 2)
  geo_table$COUNTY <- substr(geo_table$GEOID, 3, 5)
  geo_table$CENSUS_TRACT <- substr(geo_table$GEOID, 6, 11)
  geo_table$BLOCK_GROUP <- substrRight(geo_table$GEOID, 1)
  
  ## Create SQL table if needed, but fail silently in case it already exists.
  table_name <- paste0('DH_BG_', year)
  try(dbNoTimeOutGetQuery('create table ', table_name, ' (STATE CHAR(2) NOT NULL, GEOID CHAR(12) NOT NULL, ',
    'COUNTY_NAME VARCHAR(192), STATE_NAME VARCHAR(32), STATE_FIPS CHAR(2) NOT NULL, COUNTY CHAR(3) NOT NULL, ', 
    'CENSUS_TRACT CHAR(6) NOT NULL, BLOCK_GROUP CHAR(1) NOT NULL);'), silent = T)

  ## Upload data to SQL.
  for(slice in make.chunks.by.size(1:nrow(geo_table), sql_upload_chunk_size))
    dbNoTimeOutGetQuery(gsub("'NA'", 'NULL', paste0('insert into ', table_name, " values ('",
      paste(apply(geo_table[slice,], 1, paste, collapse="', '"), collapse="'), ('"), "');"), fixed=T))
  
  state_name <- as.character(subset(USA.states, Abbreviation==st)$Name)
  message('Block groups of state ', state_name, ' in year ', year, ' loaded into SQL.')
}


loadACSTableIntoSQL <- function(year, table_id, st) {
  ## Read the geography file. Subset to only census block groups.
  acs_dir <- paste0(acs_base_dir, year, '/')
  geo_table <- read.xlsx2(paste0(acs_dir, 'Geography/', tolower(st), '.xls'), sheetIndex = 1,
                          stringsAsFactors = F)
  # Extract only block groups from the geography file. Use a double check: length of the GEOID and the
  #  mention of "Block Group" in its GEOGRAPHY NAME column.
  geo_table <- subset(geo_table, (nchar(GEOID)==19) & grepl('Block Group', GEOGRAPHY.NAME, fixed = T),
    select = c(LOGRECNO, GEOID))
  geo_table$GEOID <- substrRight(geo_table$GEOID, 12)
  geo_table$LOGRECNO <- as.numeric(geo_table$LOGRECNO)
  
  ## Read the sequence table template.
  if(!exists('lookup_table'))
    lookup_table <<- list()
  if(!exists(as.character(year), where=lookup_table))
    lookup_table[[as.character(year)]] <<- read.csv(paste0(acs_dir, 'ACS_5yr_Seq_Table_Number_Lookup.txt'))
  
  # In 2011 the Sequence Lookup File has a different column name.
  seq_number <- subset(lookup_table[[as.character(year)]],
    grepl(table_id, Table.ID))[[ifelse(year==2011, 'seq', 'Sequence.Number')]][[1]]
  seq_template <- t(read.xlsx(paste0(acs_dir, 'Templates/Seq', seq_number, '.xls'), sheetIndex = 1, header = F))
  colnames(seq_template) <- c('Variable.ID', 'Variable.Name')
  variable_classes <- c(rep.int("NULL", 5), 'numeric',
    ifelse(grepl(table_id, seq_template[-1:-6,1], fixed = T), 'numeric', 'NULL'))
  variables <- grep(table_id, seq_template[,1], fixed = T, value = T)
  # Select all variables of the specified table, and the locator number. 
  # seq_template <- rbind(seq_template[6,], seq_template[grepl(table_id, seq_template[,1], fixed = T),])
  
  
  ## Load the table. Loop for the e=Estimate and m=Margin of Error parts.
  full_seq_data <- lapply(c('e','m'), function(prefix) {
      seq_filepath <- paste0(acs_dir, 'Data/', st, '/', prefix, year, '5',
        tolower(st), sprintf('%04d', seq_number), '000.txt')
      seq_data <- read.csv(seq_filepath, header = F, quote='', na.string = '.', colClasses = variable_classes)
      # Notice that we subset the columns of seq_data by the rows of seq_template. This is possible because we
      # transposed the seq_template when after calling read.xlsx(). We also suffix variables with e=estimate or
      # m=margin of error.
      colnames(seq_data) <- c('LOGRECNO', paste0(variables, prefix))
      seq_data
    })
  # The line below could probably be cbind() instead of join(). But join() is safe while cbind() relies on assumption.
  full_seq_data <- join(full_seq_data[[1]], full_seq_data[[2]], by = 'LOGRECNO')
  
  
  ## Join (and thereby also subset) with geography file.
  full_seq_data <- join(geo_table, full_seq_data, by = 'LOGRECNO')
  full_seq_data$LOGRECNO <- NULL
  
  
  ## Create SQL table for ACS table containing variables of interest if needed, but fail silently in case the table
  # already exists.
  table_name <- paste0('DH_', table_id, '_', year)
  try(dbNoTimeOutGetQuery('create table ', table_name, ' (GEOID CHAR(12) NOT NULL, ',
    paste(colnames(full_seq_data)[-1], collapse = ' FLOAT UNSIGNED, '), ' FLOAT UNSIGNED);'), silent = T)

  
  ## Upload to SQL.
  for(slice in make.chunks.by.size(1:nrow(full_seq_data), sql_upload_chunk_size))
    dbNoTimeOutGetQuery(gsub("'NA'", 'NULL', paste0('insert into ', table_name, ' (',
      paste(colnames(full_seq_data), collapse = ', '), ") values ('",
      paste(apply(full_seq_data[slice,], 1, paste, collapse="', '"), collapse="'), ('"), "');"), fixed=T))
  
  state_name <- as.character(subset(USA.states, Abbreviation==st)$Name)
  message('Table ', table_id, ' of year ', year, ' loaded for ', state_name, '.')
  
  # Return nothing so that use of in this function inside *apply() functions will not waste memory.
  return(NULL)
}



downloadACS5yrSeqByTable <- function(year, table_id, st) {
  ## Warning: This function does not assert the integrity of the downloaded files. If the download is interrupted and
  # then resumed, the partially downloaded files will be considered complete and therefore left in its incomplete, 
  # corrupt state. This function assumes a stable internet connection from start to end of the download, which is over
  # 5 GB big.
  
  .simpleCap <- function(x) {
    # Capitalizes the first letter of each word.
    s <- strsplit(x, " ")[[1]]
    paste0(toupper(substring(s, 1, 1)), substring(s, 2), collapse = " ")
  }

  state_name <- as.character(subset(USA.states, Abbreviation==st)$Name)
  message('Downloading year ', year, ' of table ', table_id, ' for state ', state_name, '.')
  
  acs_addr <- paste0('http://www2.census.gov/programs-surveys/acs/summary_file/', year, '/')
  acs_dir <- paste0(acs_base_dir, year, '/')
  if(!dir.exists(acs_dir))
    dir.create(acs_dir, recursive = T)
  
  if(!exists('lookup_table'))
    lookup_table <<- list()
  if(!exists(as.character(year), where=lookup_table)) {
    ## Download the Sequence Table Lookup File.
    seq_lookup_file <- paste0(acs_dir, 'ACS_5yr_Seq_Table_Number_Lookup.txt')
    if(!file.exists(seq_lookup_file)) {
      seq_lookup_addr <- paste0(acs_addr, ifelse(year < 2013,
        'documentation/5_year/user_tools/Sequence_Number_and_Table_Number_Lookup.txt',
        'documentation/user_tools/ACS_5yr_Seq_Table_Number_Lookup.txt'))
      if(download.file(seq_lookup_addr, seq_lookup_file, quiet = T) != 0) {
        message('### Error downloading Sequence Lookup Table for year ', year , '.')
        return(NULL)
      }
      message('Downloaded ', seq_lookup_addr, '.')
    }
    lookup_table[[as.character(year)]] <<- read.csv(seq_lookup_file)
  }
  
  if(!(table_id %in% lookup_table[[as.character(year)]]$Table.ID)) {
    message('### Unable to find table ', table_id, ' in ACS year ', year, '.')
    return(NA)
  }
  
  # In 2011 the Sequence Lookup File has a different column name.
  seq_number <- subset(lookup_table[[as.character(year)]],
    grepl(table_id, Table.ID))[[ifelse(year==2011, 'seq', 'Sequence.Number')]][[1]]
  
  ## If the desider sequence file does not exist, download the ZIP file containing all of them.
  seq_template_file <- paste0(acs_dir, 'Templates/Seq', seq_number, '.xls')
  if(!file.exists(seq_template_file)) {
    template_dir <- paste0(acs_dir, 'Templates')
    if(!dir.exists(template_dir))
      dir.create(template_dir, recursive = T)
    # In 2010 there is no _ between "Summary" and "FileTemplates."
    template_zip <- paste0(year, '_5yr_Summary', ifelse(year==2010, '', '_'), 'FileTemplates.zip')
    template_filepath <- paste0(template_dir, '/', template_zip)
    if(!file.exists(template_filepath)) {
      template_adr <- paste0(acs_addr, 'data/',template_zip)
      download.file(template_adr, template_filepath, quiet = T)
      message('Downloaded ', template_adr, '.')
      unzip(template_filepath, exdir = template_dir)
      file.remove(template_filepath)
    }
    
    # In years 2014, the sequence files are inside a "seq" folder. Let's get rid of it.
    if(year == 2014) {
      file_list <- list.files(paste0(template_dir, '/seq'))
      file.rename(paste0(template_dir, '/seq/', file_list), paste0(template_dir, '/', file_list))
      unlink(paste0(template_dir, '/seq'), recursive = T)
    }
    
    # 2011 the sequence numbers are zero-padded. Let's undo this.
    if(year == 2011) {
      library(stringr)
      template_file_list <- grep('Seq', list.files(template_dir), value = T, fixed = T)
      new_file_list <- paste0('Seq', as.numeric(str_match(template_file_list, 'Seq(\\d*)\\.xls')[,2]), '.xls')
      file.rename(paste0(template_dir, '/', template_file_list), paste0(template_dir, '/', new_file_list))
    }
  }
  
  ## Download geography file.
  geography_dir <- paste0(acs_dir, 'Geography')
  geography_file <- paste0(geography_dir, '/', tolower(st), '.xls')
  if(!file.exists(geography_file)) {
    if(!dir.exists(geography_dir))
      dir.create(geography_dir, recursive = T)
    geography_addr <- paste0('http://www2.census.gov/programs-surveys/acs/summary_file/', year, '/documentation/',
      ifelse(year < 2013, '5_year/geography/', ifelse(year < 2014, 'geography/5_year_Geo/', 'geography/5yr_year_geo/')),
      tolower(st), '.xls')
    if(download.file(geography_addr, geography_file, quiet = T, mode = 'wb') != 0) {
      message('### Error downloading geography file for ', state_name, ' in year ', year , '.')
      return(NULL)
    }
    message('Downloaded ', geography_addr, '.')
  }
  
  ## Download block group data files.
  seq_dir <- paste0(acs_dir, 'Data/', st)
  if(!dir.exists(seq_dir))
    dir.create(seq_dir, recursive = T)
  seq_file <- paste0(year, '5', tolower(st), sprintf('%04d', seq_number), '000.zip')
  seq_filepath <- paste0(seq_dir, '/', seq_file)
  # In 2009 'District of Columbia' is written 'DistrictofColumbia' instead of 'DistrictOfColumbia'.
  seq_addr <- paste0(acs_addr, 'data/5_year_seq_by_state/', ifelse(year==2009 && st == 'DC', 'DistrictofColumbia',
    gsub(' ', '', .simpleCap(state_name))), '/Tracts_Block_Groups_Only/', seq_file)
  download.file(seq_addr, seq_filepath, quiet = T)
  message('Downloaded ', seq_addr, '.')
  unzip(seq_filepath, exdir = seq_dir)
  file.remove(seq_filepath)
  
  # Return nothing so that use of in this function inside *apply() functions will not waste memory.
  return(NULL)
}



downloadUSAStateShapefiles <- function(st, year) {
  state_code <- subset(USA.states, Abbreviation==st)$FIPS.Code
  state_name <- as.character(subset(USA.states, Abbreviation==st)$Name)
  
  shapefile <- paste0('tl_', year, '_', state_code, '_bg', ifelse(year==2009, '00', ifelse(year==2010, '10', '')))
  remote_file <- paste0('ftp://ftp2.census.gov/geo/tiger/TIGER', year, '/', ifelse(year==2009,
    paste0(state_code, '_', gsub(' ', '_', toupper(ifelse((state_code==78) & (year==2009),
      'Virgin Islands of The United States', state_name))), '/'),
    paste0('BG/', ifelse(year==2010, '2010/', ''))), shapefile, '.zip')
  local_file_dir <- paste0(data_dir, 'USA/Shapefiles')
  local_file_path <- paste0(local_file_dir, '/', shapefile)
  
  message('Downloading Block Group shapefile for ', state_name, ' to ', local_file_path,
    '.\nAttention: the file might be tens of megabytes big and the download take a bit long.')
  if(!dir.exists(local_file_path))
    dir.create(local_file_path, showWarnings = F, recursive = T)
  download.file(remote_file, paste0(local_file_path, '.zip'), quiet = T)
  
  message('Unzipping file ', shapefile, '.zip to ', local_file_dir, '.')
  unzip(paste0(local_file_path, '.zip'), exdir=local_file_path)
  
  message('Removing downloaded file ', shapefile, '.zip.')
  file.remove(paste0(local_file_path, '.zip'))
}


downloadBRAStateShapefiles <- function(st) {
  state <- subset(BRA.States, Abbreviation==st)
  remote_file <- paste0('ftp://geoftp.ibge.gov.br/malhas_digitais/censo_2010/setores_censitarios/', tolower(st), '/',
    tolower(st), '_setores_censitarios.zip')
  
  # There is an inconsistency for the file of state Goiânia (GO). We need to fix it manually.
  if(st=='GO')
    remote_file <- sub('setores_censitarios.zip', 'setores%20_censitarios.zip', remote_file)
  
  'ftp://geoftp.ibge.gov.br/malhas_digitais/censo_2010/setores_censitarios/go/go_setores%20_censitarios.zip'
  local_file_dir <- paste0(data_dir, 'BRA/Shapefiles/SC/', st)
  local_file <- paste0(st, '_SC.zip')
  local_file_path <- paste0(local_file_dir, '/', local_file)
  
  message('Downloading Setores Censitarios (SC) shapefile for ', state$Name, ' to ', local_file,
    '.\nAttention: the file might be big and the progress bar is unreliable.')
  dir.create(local_file_dir, showWarnings = F, recursive = T) # Guarantee directory existence
  download.file(remote_file, local_file_path, quiet = T)
  
  message('Unzipping file ', local_file, ' to ', local_file_dir, '.')
  unzip(local_file_path, exdir=local_file_dir)
  
  message('Removing downloaded file ', local_file, '.')
  file.remove(local_file_path)
}



downloadAllNeededShapefiles <- function() {
  message('Downloading block group shapefiles for ', nrow(USA.states), ' U.S. states across ', 
    length(acs_year_list), ' years from ftp://ftp2.census.gov/geo/tiger/.')
  for(year in acs_year_list)
    for(st in USA.states$Abbreviation) {
      attempts <- 0
      while(attempts < 4) {
        res <- try(downloadUSAStateShapefiles(st, year), silent=F)
        if(inherits(res, 'try-error')) {
          message('Download failed. Will try again in 15 seconds.')
          attempts <- attempts + 1
          Sys.sleep(15)
        }
        else
          break
      }
    }
  message('Download complete.')
}



downloadNeededACSTables <- function(acs_tables = acs_tables) {
  message('Downloading ', length(acs_tables), ' tables of the American Community Survey for ', 
    length(acs_year_list), ' years from http://www2.census.gov/programs-surveys/acs/summary_file/.')
  for(year in acs_year_list)
    for(acs_table in acs_tables)
      for(st in states_to_process)#USA.states$Abbreviation)
        downloadACS5yrSeqByTable(year, acs_table, st)
  remove(lookup_table, envir = .GlobalEnv)
  message('Download complete.')
}



loadNeededACSTablesIntoSQL <- function(acs_tables = acs_tables) {
  library(xlsx)
  library(plyr)
  library(RMySQL)
  connect.to.sql()
  message('Inserting ', length(acs_tables), ' tables of the American Community Survey for ', 
    length(acs_year_list), ' years into the SQL server.')
  for(year in acs_year_list)
    for(acs_table in acs_tables)
      for(st in states_to_process)#USA.states$Abbreviation)
        loadACSTableIntoSQL(year, acs_table, st)
  remove(lookup_table, envir = .GlobalEnv)
  dbDisconnect(sql_conn)
  message('Loading into SQL complete.')
}



loadNeededBlockGroupsIntoSQL <- function() {
  library(xlsx)
  library(plyr)
  library(RMySQL)
  connect.to.sql()
  message('Inserting Block Groups geographical data for ', nrow(USA.states), ' states along ',
    length(acs_year_list), ' years into the SQL server.')
  for(year in acs_year_list)
    for(st in USA.states$Abbreviation)
      loadACSBlockGroupsIntoSQL(year, st)
  dbDisconnect(sql_conn)
  message('Loading into SQL complete.')
}


#
## Script execution starts here.
# downloadNeededShapefiles()
# downloadNeededACSTables()
# loadNeededACSTablesIntoSQL()
# loadNeededBlockGroupsIntoSQL()
