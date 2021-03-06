
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

suppressPackageStartupMessages({
  suppressWarnings({
    library(dplyr)
    library(purrr)
    library(XLConnect)
    library(ggplot2)
  })
})

df_list <- readr::read_rds("~/Desktop/MD_files/final_MD_reg_stats.rds")

percent_population <- function(district){
  
  filter_df <- lapply(
    1:length(df_list), 
    function(i){
      df_list[[i]][[district]]
    }) %>%
    bind_rows() %>% 
    tbl_df() %>% 
    filter(year == "2017") %>% 
    filter(month == min(month)) %>% 
    filter(grepl("TOTAL", demographic))
  
  use_number <- as.numeric(filter_df$total_rvs)
  
  return_df <- lapply(
    1:length(df_list), 
    function(i){
      df_list[[i]][[district]]
    }) %>%
    bind_rows() %>% 
    tbl_df() %>% 
    filter(year == "2017") %>% 
    filter(month == min(month)) %>% 
    mutate(percent_total_population = total_rvs / use_number) %>% 
    mutate(new_demographic = paste0(demographic, " - ", round(percent_total_population, 4)*100, "% of pop")) %>% 
    select(demographic, new_demographic)
  
  return(return_df)
}

plot_district <- function(district, year = "all"){
  
  if(year == "all"){
    totals <- lapply(
      1:length(df_list), 
      function(i){
        df_list[[i]][[district]]
      }) %>%
      bind_rows() %>% 
      tbl_df() %>% 
      mutate(
        date = lubridate::ymd(paste0(year, month, " 01"))
      ) %>% 
      left_join(.,percent_population(unique(.$district)), by = "demographic")
    
    totals_final <- totals[order(totals$date),, drop = F] %>% 
      select(date, percent_dems, percent_reps, percent_npa, new_demographic) %>% 
      reshape2::melt(., id = c("date", "new_demographic"))
    
    names(totals_final) <- c("year", "demographic", "party", "percent")
    
    export_plot <- ggplot(totals_final, aes(x=year, y=percent, color = party)) +
      theme(strip.text.x = element_text(size = 10, colour = "#990000", angle = 90), 
            axis.text.x = element_text(angle=90, size = 6)) + 
      facet_grid(. ~ demographic) + geom_line() + 
      ggtitle(paste0("Percent Registration by Party from 2014 - 2017: ", unique(totals$district))) + 
      xlab("Year") + ylab("Percent of Registered Voters")
  } else {
    
    totals <- lapply(1:length(df_list), function(i){
      df_list[[i]][[district]]
    }) %>%
      bind_rows() %>% 
      tbl_df() %>% 
      mutate(
        date = lubridate::ymd(paste0(year, month, " 01"))
      ) %>% 
      left_join(.,percent_population(unique(.$district)), by = "demographic")
    
    totals_final <- totals[order(totals$date),, drop = F] %>% 
      select(date, percent_dems, percent_reps, percent_npa, new_demographic) %>% 
      reshape2::melt(., id = c("date", "new_demographic")) %>% 
      filter(date >= lubridate::ymd(paste0(year,"01", " 01")))
    
    names(totals_final) <- c("year", "demographic", "party", "percent")
    
    export_plot <- ggplot(totals_final, aes(x=year, y=percent, color = party)) +
      theme(strip.text.x = element_text(size = 10, colour = "#990000", angle = 90), 
            axis.text.x = element_text(angle=90, size = 6)) + 
      facet_grid(. ~ demographic) + geom_line() + 
      ggtitle(paste0("Percent Registration by Party from 2014 - 2017: ", unique(totals$district))) + 
      xlab("Year") + ylab("Percent of Registered Voters")
    
  }
  
  return(export_plot)
}

district <- "40th Senatorial District" # Frank Artiles
district <- "116th House District"     # Jose F Diaz
district <- "39th Senatorial District" # Anitere Flores
district <- "37th Senatorial District" # Jose Javier Rodriguez
district <- "38th Senatorial District" # Campbell
district <- "36th Senatorial District" # Rene Garcia

plot_district(district, year = 2017)
plot_district(district, year = "all")


