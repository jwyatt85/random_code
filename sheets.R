
# alias check_clients='function _blah(){
# cd ~/Desktop;
# Rscript ~/Desktop/sheets.R $1;
# };_blah'

args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library("dplyr")
  library("googlesheets")
  suppressWarnings(library("knitr"))
})

check_clients <- function(call){
  
  internet_access <- try(is.character(RCurl::getURL("www.google.com"))) == TRUE
  cat("\014") # this code clears the terminal so you don't see the error if no internet
  
  if(internet_access){ #If internet download latest sheets and save, then load
    gap <- suppressMessages(googlesheets::gs_title("Client_list"))
    clients1 <- suppressMessages(googlesheets::gs_read(gap, ws=1))
    clients2 <- suppressMessages(googlesheets::gs_read(gap, ws=2))
    clients3 <- suppressMessages(googlesheets::gs_read(gap, ws=3))
    download_time <- Sys.time()
    
    client_data <- list(clients1, clients2, clients3, download_time)
    readr::write_rds(client_data, "~/Desktop/client_data.rds")
    
  } else {
    latest_client_data <- readr::read_rds("~/Desktop/client_data.rds")
    clients1 <- latest_client_data[[1]]
    clients2 <- latest_client_data[[2]]
    clients3 <- latest_client_data[[3]]
    download_time <- latest_client_data[[4]]
    
    cat("\n")
    print(paste0("Internet access not detected.  Loading most recent client information download: ", download_time))
    cat("\n")
  }
  
  if(call == "all") {
    clients1 <-clients1 %>% 
      mutate(
        last_contact = as.Date(last_contact, "%m/%d/%Y"),
        urgent = ifelse(Sys.Date() - last_contact >= 20, 1, urgent)
      ) %>% 
      select(
        client, notes, last_contact, urgent
      ) %>% 
      arrange(urgent, desc(last_contact))
    
    clients2 <- clients2 %>% 
      arrange(desc(poll))
    
    clients3 <- clients3 %>% 
      mutate(
        `due date` =  as.Date(`due date`, "%m/%d/%Y")
      ) %>% 
      filter(!is.na(active)) %>% 
      arrange(`due date`)
    
    print(kable(clients1)) #output to terminal
    print(kable(clients2)) #output to terminal
    print(kable(clients3))
    cat("\n\n\n\n")
    
  } else if(call == "edit") {
    
    if(!internet_access){
      stop("can't edit link to Sheets - you have no internet access")
    }
    
    browseURL("https://docs.google.com/spreadsheets/d/1W3SnvZqTlZ1etcfbVpigca1IROFknm3gyoSxGuDmyXM/edit#gid=0", 
              browser = getOption("browser"),
              encodeIfNeeded = FALSE)
    
  } else if(call == 'todo') {
    clients3 <- clients3 %>% 
      mutate(
        `due date` =  as.Date(`due date`, "%m/%d/%Y")
      ) %>% 
      filter(!is.na(active)) %>% 
      arrange(`due date`)
    
    print(kable(clients3))
    cat("\n\n\n")
    
  } else if(call == "projects") {
    clients2 <- clients2 %>% 
      arrange(desc(poll))
    
    totals <- clients2 %>% 
      group_by(client) %>% 
      summarize(totalQs_used_on_all_polls = sum(Qs_used)) %>% 
      arrange(totalQs_used_on_all_polls)
    
    print(kable(totals))
    print(kable(clients2))
    cat("\n\n\n")
    
  } else {
    stop("call me please")
  }
}

check_clients(args)


