
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
   clients <-  clients1 %>% 
      mutate(
        last_contact = as.Date(last_contact, "%m/%d/%Y")
      ) %>% 
      arrange(reminder, last_contact)
   
   print(kable(clients)) #output to terminal
   print(kable(clients2))  #output to terminal
   
  } else
    print("give me a call")
}

check_clients(args)

args = "all"


