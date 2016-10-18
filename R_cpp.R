
#### Load sources ####
library(Rcpp)
library(microbenchmark)
library(rbenchmark)

url <- "https://raw.githubusercontent.com/jwyatt85/random_code/master/sources/export.cpp"
destfile <- paste0(tempdir(), "/mysource.cpp")
download.file(url, destfile, mode="wb")
sourceCpp(destfile)

### Benchmarking ####
microbenchmark(
  mean(c(2,3,4,5,6)),
  james(2)
)

a <- replicate(250, 1:100, simplify=FALSE)

res <- benchmark(as.data.frame(a), 
                 CheapDataFrameBuilder(a), 
                 order="relative", replications=500)

microbenchmark(
  as.data.frame(a), 
  CheapDataFrameBuilder(a)
)

a[1]
