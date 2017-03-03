
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(XLConnect)
})

for(i in 1:12){
  if(i < 10){
    download.file(
      paste0('http://www.miamidade.gov/elections/STATS/2016/2016-0',i,'-voter-registration-statistics-districts.xls'),
      destfile = paste0('MD_file_',i)
    )
  } else {
    download.file(
      paste0('http://www.miamidade.gov/elections/STATS/2016/2016-',i,'-voter-registration-statistics-districts.xls'),
      destfile = paste0('MD_file_',i)
    )
  }
  cat("downloaded file # ", i)
}

temp <- list.files("~/Desktop/MD_files/")
setwd("~/Desktop/MD_files/")

myfiles <- lapply(temp, function(i){
  readWorksheetFromFile(i, sheet=1) %>% 
    select(Col5, Col2, Col9, Col12, Col14, Col17) %>% 
    filter(Col2 != 'Time', Col2 != 'CloseDate') %>% 
    as.data.frame()
})

final_MD_reg_stats <- myfiles %>% 
  map(function(i){
    names(i) <- c("district", "demographic", "total_rvs", "dems", 'reps', 'npa')
    
    try(for(x in seq_along(i$district)){
      if(is.na(i$district[[x]])){
        i$district[[x]] <- i$district[[x - 1]]
      }
    }
    )
    
    i <- i %>%
      filter(demographic != 'District')
    
    df_list <- i %>%
      mutate(
        dems = as.numeric(gsub(",", "", as.character(dems))),
        reps = as.numeric(gsub(",", "", as.character(reps))),
        npa = as.numeric(gsub(",", "", as.character(npa))),
        total_rvs = as.numeric(gsub(",", "", as.character(total_rvs)))
      ) %>%
      split(.$district) %>%
      map(
        function(m){
          m %>%
            mutate(
              percent_dems = dems / total_rvs,
              percent_reps = reps / total_rvs,
              percent_npa  = npa / total_rvs
            )
          
        }
      )
    
    df_list
    
  })


final_MD_reg_stats[[12]]$`100th House District` #Voter Reg stats for December at the 100th House Districts

readr::write_rds(final_MD_reg_stats, '~/Desktop/MD_files/final_MD_reg_stats.rds')
 




