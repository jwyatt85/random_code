
# Code to run: Rscript command_line_convertCSV.R "~/Desktop/test1.xlsx"

args <- commandArgs(trailingOnly = TRUE)

convertCSV <- function(path){
  
   if(any(c("xlsx", "xls") %in% tools::file_ext(path))){
    path <- args[1]
    data <- xlsx::read.xlsx(path, sheetName = "Sheet1")
  
    x <- unlist(strsplit(path, "/"))
    name_locattion <- length(x)
    name <- paste0(strsplit(x[name_locattion], "[.]")[[1]][1], ".csv")
    
    x <- paste0(x[-name_locattion], "/")
    
    new_path <- paste0(paste(x, sep="", collapse=""),name)
    readr::write_csv(data, new_path)
    
  } else {
    print("file has to be of xlsx or xls format")
    stop()
  }
}

convertCSV(args)
