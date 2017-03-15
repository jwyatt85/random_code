
### Load Packages ####
suppressPackageStartupMessages({
  suppressWarnings({
    library(dplyr)
    library(purrr)
    library(XLConnect)
    library(ggplot2)
  })
})

### Loop download ####
setwd("~/Desktop/MD_files/")
for(i in 1:12){
  if(i < 10){
    download.file(
      paste0('http://www.miamidade.gov/elections/STATS/2017/2017-0',i,'-voter-registration-statistics-districts.xls'),
      destfile = paste0('MD_file_2017_',i)
    )
  } else {
    download.file(
      paste0('http://www.miamidade.gov/elections/STATS/2016/2016-',i,'-voter-registration-statistics-districts.xls'),
      destfile = paste0('MD_file_2017_',i)
    )
  }
  cat("downloaded file # ", i)
}

### Links for 2014 data ####
# looks like in July the switched to the new link version
# for(i in 1:12){
#   if(i >=7){
#     if(i < 10){
#       download.file(
#         paste0('http://www.miamidade.gov/elections/STATS/2014/2014-0',i,'-voter-registration-statistics-districts.xls'),
#         destfile = paste0('MD_file_2014_',i)
#       )
#     } else {
#       download.file(
#         paste0('http://www.miamidade.gov/elections/STATS/2014/2014-',i,'-voter-registration-statistics-districts.xls'),
#         destfile = paste0('MD_file_2014_',i)
#       )
#     }
#   } else {
#     download.file(
#       paste0('https://www.miamidade.gov/elections/STATS/2014/', tolower(month.abb)[i], '14dist.xls'),
#       destfile = paste0('MD_file_2014_',i)
#     )
#   }
#   cat("downloaded file # ", i, "\n")
# }  


### Data Munging and Saving ####
temp <- list.files("~/Desktop/MD_files/")
setwd("~/Desktop/MD_files/")

myfiles <- lapply(temp, function(i){
  year <- unique(na.omit(as.numeric(unlist(strsplit(unlist(i), "[^0-9]+")))))[1]
  month_num <- unique(na.omit(as.numeric(unlist(strsplit(unlist(i), "[^0-9]+")))))[2]
  XLConnect::readWorksheetFromFile(i, sheet=1) %>% 
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

readr::write_rds(final_MD_reg_stats, '~/Desktop/MD_files/final_MD_reg_stats.rds')

### Analysis ####
df_list <- readr::read_rds("~/Desktop/MD_files/final_MD_reg_stats.rds")

#Jose F Diaz
totals <- lapply(1:length(df_list), function(i){
  df_list[[i]]$`116th House District`
}) %>%
  bind_rows() %>% 
  tbl_df() %>% 
  mutate(
    date = lubridate::ymd(paste0(year, month, " 01"))
  )

totals_final <- totals[order(totals$date),, drop = F] %>% 
  select(date, percent_dems, percent_reps, percent_npa, demographic) %>% 
  reshape2::melt(., id = c("date", "demographic"))

names(totals_final) <- c("year", "demographic", "party", "percent")

test2 <- ggplot(totals_final, aes(x=year, y=percent, color = party)) +
  theme(strip.text.x = element_text(size = 10, colour = "#990000", angle = 90), 
        axis.text.x = element_text(angle=90, hjust=1, size = 8)) + 
  facet_grid(. ~ demographic) + geom_line() + 
  ggtitle(paste0("Percent Registration by Party from 2014 - 2017: ", unique(totals$district), " - Diaz")) + 
  xlab("Year") + ylab("Percent of Registered Voters")
test2

#Frank Artiles
totals <- lapply(1:length(df_list), function(i){
  df_list[[i]]$`40th Senatorial District`
}) %>%
  bind_rows() %>% 
  tbl_df() %>% 
  mutate(
    date = lubridate::ymd(paste0(year, month, " 01"))
  )

totals_final <- totals[order(totals$date),, drop = F] %>% 
  select(date, percent_dems, percent_reps, percent_npa, demographic) %>% 
  reshape2::melt(., id = c("date", "demographic"))

names(totals_final) <- c("year", "demographic", "party", "percent")

test2 <- ggplot(totals_final, aes(x=year, y=percent, color = party)) +
  theme(strip.text.x = element_text(size = 10, colour = "#990000", angle = 90), 
  axis.text.x = element_text(angle=90, size = 7, hjust = 1)) + 
  facet_grid(. ~ demographic) + geom_line() + 
  ggtitle(paste0("Percent Registration by Party from 2014 - 2017: ", unique(totals$district), " - Artiles")) + 
  xlab("Year") + ylab("Percent of Registered Voters")
test2


#Anitere Flores
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

names(totals_final) <- c("year", "demographic", "party", "percent")

test2 <- ggplot(totals_final, aes(x=year, y=percent, color = party)) +
  theme(strip.text.x = element_text(size = 10, colour = "#990000", angle = 90), 
        axis.text.x = element_text(angle=90, hjust=1, size = 8)) + 
  facet_grid(. ~ demographic) + geom_line() + ggtitle(paste0("Percent Registration by Party from 2014 - 2017: ", unique(totals$district), " - Flores")) + 
  xlab("Year") + ylab("Percent of Registered Voters")
test2













