
#### Load sources ####
library(Rcpp)
library(microbenchmark)
library(rbenchmark)

url <- "https://raw.githubusercontent.com/jwyatt85/random_code/master/sources/export.cpp"
destfile <- paste0(tempdir(), "/mysource.cpp")

download.file(url, destfile, mode="w")
sourceCpp(destfile)

### Benchmarking ####
a <- replicate(1000, 1:100, simplify=FALSE)

microbenchmark(
  as.data.frame(a), 
  as_data_frame(a)
)

return_size(a)
