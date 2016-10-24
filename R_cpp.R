
#### Load sources ####
library(Rcpp)
library(microbenchmark)
library(rbenchmark)
library(dplyr)

# options(expressions=100000)

# url <- "https://raw.githubusercontent.com/jwyatt85/random_code/master/sources/export.cpp"
# destfile <- paste0(tempdir(), "/mysource.cpp")
# 
# download.file(url, destfile, mode="w")
# sourceCpp(destfile)

sourceCpp("~/Documents/git_repos/random_code/sources/export.cpp")

#### Benchmarking county check 0 ####
my_list <- readr::read_rds("~/Desktop/county_margins.rds")
my_list <- my_list[1:10]
my_list <- my_list[1]

final <- cpp_county_check(my_list) 

r_county_check <- function(x){
  for(i in 1:length(x)){
    # print(i)
    for(z in 1:length(x[[i]])){
      for(y in 1:NROW(x[[i]][[z]])){
        x[[i]][[z]][y,]$Freq
        if(x[[i]][[z]][y,]$Freq == 0){
          x[[i]][[z]][y,]$Freq <- .0001
        }
      }
    }
  }
}

county_mrp_bench <- microbenchmark(
  r_county_check(my_list), 
  cpp_county_check(my_list)
)
ggplot2::autoplot(county_mrp_bench)
#### Benchmarking Data frame ####
a <- replicate(1000, 1:100, simplify=FALSE)

microbenchmark(
  as.data.frame(a), 
  as_data_frame(a)
)
#returns dataframes1
m <- as.data.frame(a)
m2 <- as_data_frame(a)
#### Pointer testing ####
t_pointers()





