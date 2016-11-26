
args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(xlsx)
})

email_links <- function(args){
  
  data_location <- args[1]
  

data3 <- read.xlsx(paste0("~/Desktop/", data_location, ".xlsx"), sheetName = "Downloads - Raw Data") %>% 
  mutate(
    URL = tolower(as.character(URL))
  )

# unique(data3$Program)
vars <- data3$URL
NROW(vars)

names <- lapply(vars, 
         function(i){
           link <- i
           if(grepl("/mplc/", link)) {
             program <- "Market Innovation Center"
           } else if(grepl("/itsc/|health-care-it-advisor|itsc|health-care-it", link)){
             program <- "Health Care IT Advisor"
           } else if(grepl("/hcab/|-hcab-|heatlh-care-advisory-board|hcab", link)){
             program <- "Health Care Advisory Board"
           } else if(grepl("/hric/", link)){
             program <- "HR Advancement Center"
           } else if(grepl("/or/|oncology", link)){
             program <-  "Oncology Roundtable"
           } else if(grepl("/nec/|nursing-executive-center|nec", link)){
             program <- "Nursing Executive Center"
           } else if(grepl("/ipp/|ipp", link)){
             program <- "Imaging Performance Partnership"
           } else {
             program <- NA
           }
           program
         })

final_names <- unlist(names)
data3$Predicted_Program_Name <- final_names

readr::write_csv(data3, "~/Desktop/links_predicted_FINAL.csv")
}

email_links(args)


 


