
#### Load sources ####
library(Rcpp)
library(microbenchmark)
library(rbenchmark)
library(dplyr)

# url <- "https://raw.githubusercontent.com/jwyatt85/random_code/master/sources/export.cpp"
# destfile <- paste0(tempdir(), "/mysource.cpp")
# 
# download.file(url, destfile, mode="w")
# sourceCpp(destfile)
# 
sourceCpp("~/Documents/git_repos/random_code/sources/export.cpp")

runthis()

my_list <- readr::read_rds("~/Desktop/county_margins.rds")


my_list <- my_list[[1]]
lapply(my_list, function(i){
  check_county(i)
})

final <- lapply(my_list, function(i){
  lapply(i, function(x){
    check_county(x)
  })
})






### Benchmarking ####
a <- replicate(1000, 1:100, simplify=FALSE)

microbenchmark(
  as.data.frame(a), 
  as_data_frame(a)
)
#returns dataframes1
m <- as.data.frame(a)
m2 <- as_data_frame(a)



