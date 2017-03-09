
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(XLConnect)
  library(ggplot2)
})

for(i in 1:12){
  if(i < 10){
    download.file(
      paste0('http://www.miamidade.gov/elections/STATS/2016/2016-0',i,'-voter-registration-statistics-districts.xls'),
      destfile = paste0('MD_file_2016_',i)
    )
  } else {
    download.file(
      paste0('http://www.miamidade.gov/elections/STATS/2016/2016-',i,'-voter-registration-statistics-districts.xls'),
      destfile = paste0('MD_file_2016_',i)
    )
  }
  cat("downloaded file # ", i)
}

### Links for 2014 data ####
# looks like in July the switched to the new link version
for(i in 1:12){
  if(i >=7){
    if(i < 10){
      download.file(
        paste0('http://www.miamidade.gov/elections/STATS/2014/2014-0',i,'-voter-registration-statistics-districts.xls'),
        destfile = paste0('MD_file_2014_',i)
      )
    } else {
      download.file(
        paste0('http://www.miamidade.gov/elections/STATS/2014/2014-',i,'-voter-registration-statistics-districts.xls'),
        destfile = paste0('MD_file_2014_',i)
      )
    }
  } else {
    download.file(
      paste0('https://www.miamidade.gov/elections/STATS/2014/', tolower(month.abb)[i], '14dist.xls'),
      destfile = paste0('MD_file_2014_',i)
    )
  }
  cat("downloaded file # ", i, "\n")
}  


### Save it all ####
temp <- list.files("~/Desktop/MD_files/")
setwd("~/Desktop/MD_files/")

### Mutates ####
myfiles <- lapply(temp, function(i){
  year <- unique(na.omit(as.numeric(unlist(strsplit(unlist(i), "[^0-9]+")))))[1]
  month_num <- unique(na.omit(as.numeric(unlist(strsplit(unlist(i), "[^0-9]+")))))[2]
  readWorksheetFromFile(i, sheet=1) %>% 
    select(Col5, Col2, Col9, Col12, Col14, Col17) %>% 
    filter(Col2 != 'Time', Col2 != 'CloseDate') %>% 
    mutate(
      month = month.name[month_num],
      year = year
    ) %>% 
    as.data.frame()
})

final_MD_reg_stats <- myfiles %>% 
  map(function(i){
    names(i) <- c("district", 
                  "demographic", 
                  "total_rvs", 
                  "dems", 
                  'reps', 
                  'npa', 
                  'month', 
                  'year')
    
    for(x in seq_along(i$district)){
      if(is.na(i$district[[x]])){
        i$district[[x]] <- i$district[[x - 1]]
      }
    }
    
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

# readr::write_rds(final_MD_reg_stats, '~/Desktop/MD_files/final_MD_reg_stats.rds')
df_list <- readr::read_rds("~/Desktop/MD_files/final_MD_reg_stats.rds")

# df_list[[1]]$`116th House District` %>%
#   filter(demographic == '**TOTAL**')

#Jose F Diaz District
totals <- lapply(1:length(df_list), function(i){
  df_list[[i]]$`116th House District`
}) %>%
  bind_rows() %>% 
  tbl_df() %>% 
  mutate(
    date = lubridate::ymd(paste0(year, month, " 01"))
  )

#Anitere's District - Holy shit.  There more Dems in her District now
totals <- lapply(1:length(df_list), function(i){
  df_list[[i]]$`37th Senatorial District`
}) %>%
  bind_rows() %>% 
  tbl_df() %>% 
  mutate(
    date = lubridate::ymd(paste0(year, month, " 01"))
  )

totals_final <- totals[order(totals$date),, drop = F] %>% 
  select(date, percent_dems, percent_reps, percent_npa, demographic) %>% 
  reshape2::melt(., id = c("date", "demographic"))
# 
# test <- ggplot(totals_final, aes(x=date, y=value, color = variable)) +
#   theme_bw() + facet_grid(variable ~ demographic) + geom_line()
# test

test2 <- ggplot(totals_final, aes(x=date, y=value, color = variable)) +
  theme_bw() + facet_grid(. ~ demographic) + geom_line()
test2


### Just look at total Hispanic/age/etc. ###
hispanic_trends <- lapply(1:length(df_list), function(i){
  df_list[[i]]$`37th Senatorial District` %>% 
    filter(grepl("AGE", demographic))
}) %>%
  bind_rows() %>% 
  tbl_df() %>% 
  mutate(
    date = lubridate::ymd(paste0(year, month, " 01"))
  )

totals_final <- hispanic_trends[order(hispanic_trends$date),, drop = F] %>% 
  select(date, percent_dems, percent_reps, percent_npa, demographic) %>% 
  reshape2::melt(., id = c("date", "demographic"))

test <- ggplot(totals_final, aes(x=date, y=value, color = variable)) +
  theme_bw() + geom_line() + facet_grid(. ~ demographic)
test 










