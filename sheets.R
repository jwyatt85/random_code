
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
  
  gap <- suppressMessages(googlesheets::gs_title("Client_list"))
  clients1 <- suppressMessages(googlesheets::gs_read(gap, ws=1))
  clients2 <- suppressMessages(googlesheets::gs_read(gap, ws=2))
  clients3 <- suppressMessages(googlesheets::gs_read(gap, ws=3))
  
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
    
    } else {
    stop("call me please")
  }
}

check_clients(args)


