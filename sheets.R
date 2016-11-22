
# alias check_clients='function _blah(){
# cd ~/Desktop;
# Rscript ~/Desktop/sheets.R $1;
# };_blah'

args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library("dplyr")
  library("googlesheets")
  library("knitr")
})

check_clients <- function(call){
  
  gap <- suppressMessages(googlesheets::gs_title("Client_list"))
  clients1 <- suppressMessages(googlesheets::gs_read(gap, ws=1))
  clients2 <- suppressMessages(googlesheets::gs_read(gap, ws=2))
  
  if(call == "all"){
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
    
    print(kable(clients1)) #output to terminal
    print(kable(clients2))  #output to terminal
   
  } else
    print("give me a call")
}

check_clients(args)



