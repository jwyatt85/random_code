
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
  
  call_temp <- call[1]
  poll_number <- call[2]
  client <- call[3]
  tab_type <- call[4]
  tab_narrow <- call[5]
  
  call <- call_temp
  
  if(call == "man"){
    cat("\n\n\n all      = get all information (notes, polls, todos) \n",
      "todo     = list of todos and relevant due dates for tasks \n",
      "edit     = link to Sheets to edit the information \n", 
      "projects = polls clients have been on and total Qs used \n",
      "tabs     = pulls most recent tabs of prefered client on prefered poll \n\n ar1: pollnumber; \n arg2:client; \n arg3: tab type (crosstab or topline)); \n arg4: optional keyword to narrow search \n\n", 
      "search   = arg1: looks up folders/poll where that client is at \n",
      "man      = get the manual that you're reading now \n\n\n")
    stop("Terminated check_clients")
  }
  
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
    
    # mc_data <- latest_client_data[[5]]
    
    clients1 <- latest_client_data[[1]]
    clients2 <- latest_client_data[[2]]
    clients3 <- latest_client_data[[3]]
    download_time <- latest_client_data[[4]]
    
    cat("\n")
    cat("Internet access not detected.  Loading most recent client information, download: ", download_time, "\n")
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
    
    print(knitr::kable(clients1)) #output to terminal
    print(knitr::kable(clients2)) #output to terminal
    print(knitr::kable(clients3))
    cat("\n\n\n")
    
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
    
    print(knitr::kable(clients3))
    cat("\n\n\n")
    
  } else if(call == "projects") {
    clients2 <- clients2 %>% 
      arrange(desc(poll))
    
    totals <- clients2 %>% 
      group_by(client) %>% 
      summarize(totalQs_used_on_all_polls = sum(Qs_used)) %>% 
      arrange(totalQs_used_on_all_polls)
    
    print(knitr::kable(totals))
    print(knitr::kable(clients2))
    cat("\n\n\n")
    
  } else if(call == "tabs") {
    
    clients2 <- clients2 %>% 
      arrange(desc(poll))
    
    print(knitr::kable(clients2))
    cat("\n")
    
    poll_number <- as.character(poll_number)
    client <- as.character(client)
    
    files <- sort(list.files(paste0("~/Dropbox/tmc/polls/", poll_number, "/Writeup")), decreasing = T)
    narrow <- files[grep(client, files)]
    narrow <- narrow[grep(poll_number, narrow)]
    narrow_print <- narrow
    
    narrow <- narrow[grep(tab_type, narrow)]
    narrow <- sort(narrow, decreasing = T)
    
  
    cat("All files with poll and client: \n")
    print(knitr::kable(data.frame(files = narrow_print)))
    cat("\n")
    
    if(!is.na(tab_narrow)){
      narrow <- narrow[grep(tab_narrow, narrow)]
      open_this_file <- narrow[1]
    } else {
      open_this_file <- narrow[1]
    }
    
    cat("Opening the following file: ", open_this_file, "\n")
    try(system(paste0("open ", paste0("~/Dropbox/tmc/polls/", poll_number, "/Writeup/"), open_this_file)))
    
  } else if(call == "search") {
    client <- tolower(poll_number)
    files <- sort(list.files("~/Dropbox/tmc/polls"), decreasing = T)
    files_filtered <- as.character(suppressWarnings(na.omit(as.numeric(files))))

    location_hits <- list()
    
    for(i in seq_along(files_filtered)){
      
      poll <- files_filtered[i]
      location <- paste0("~/Dropbox/tmc/polls/", poll, "/Writeup")
      poll_files <- list.files(location)
      
      are_there <- grepl(client, tolower(poll_files))
      
      if(any(are_there)){
        location_hits <- c(location_hits, poll)
      } else {
        location_hits <- location_hits
      }
    }
    
    if(length(location_hits) > 0){
      
    location_hits <- unlist(location_hits)
    final_location_hits <- data.frame(location = cbind(location_hits))
    
    cat("\n\n That client is located in the following project folders: \n")
    print(knitr::kable(final_location_hits))
    cat("\n\n")
    } else {
      cat("\n Can't find that client \n")
    }
    
  } else {
    stop("call me please")
  }
}

check_clients(args)


