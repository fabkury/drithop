## Scrape the juridical nature of the CNES.
# 13/05/2016 15:44 
# fabricio.kury at nih.gov

library(RCurl)
library(stringr)
library(doParallel)
library(foreach)

setwd('C:/Users/kuryfs/Documents/NLM/Projects/AHA/drithop')

threads_per_core <- 3
make.n.chunks <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))
curtime <- function() format(Sys.time(), "%Y_%m_%d %H_%M")
exec_start_time <- curtime()
if(!dir.exists(paste0('Output/', exec_start_time, '/Logs')))
  dir.create(paste0('Output/', exec_start_time, '/Logs'), recursive = T)

n_threads <- detectCores()*threads_per_core
cl <- makeCluster(n_threads)
registerDoParallel(cl)

BRA.hosp <- read.csv('Data\\BRA Hospital CNES with geo.csv')
hosp_chunks <- make.n.chunks(1:nrow(BRA.hosp), n_threads)
no_chunks <- length(hosp_chunks)

message('Parallel execution started at ', curtime(), '.')
message('Warning: progress messages will not be visible here.')
foreach(c = 1:length(hosp_chunks), .packages=c('RCurl', 'stringr')) %dopar% {
  exec_label <- paste0(exec_start_time, ' (', c, ' of ', no_chunks, ')')
  res_file <- paste0('Output/', exec_start_time, '/', exec_label, ' RES.csv')
  add.To.RES <- function(CO_UNIDADE, NAT_JUR)
    cat(paste0(CO_UNIDADE, ',', NAT_JUR), file=res_file, sep="\n", append=TRUE)
  add.To.RES('"CO_UNIDADE"', '"NAT_JUR"') # Add the column names to the CSV file.
  
  hosp_chunk <- BRA.hosp[hosp_chunks[[c]],]
  for(i in 1:nrow(hosp_chunk))
    add.To.RES(hosp_chunk[[i, 'CO_UNIDADE']], str_match(getURL(paste0('http://cnes2.datasus.gov.br/',
      'Mod_Bas_Caracterizacao.asp?VCo_Unidade=', as.character(hosp_chunk[i, 'CO_UNIDADE']))),
      paste0('<td colspan=3 align=center><font size=1 face=Verdana,arial color="#ffcc99">', 
        '(\\d*)', '<font color="#0099cc"></B></font></td>'))[2])
}
message('Parallel execution completed at ', curtime(), '.')


