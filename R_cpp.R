
#### Load sources ####
library(Rcpp)
library(microbenchmark)
library(rbenchmark)


# url <- "https://raw.githubusercontent.com/jwyatt85/random_code/master/sources/export.cpp"
# destfile <- paste0(tempdir(), "/mysource.cpp")
# 
# download.file(url, destfile, mode="w")
# sourceCpp(destfile)
# 
# sourceCpp("~/Documents/git_repos/random_code/sources/export.cpp")

### Benchmarking ####
a <- replicate(1000, 1:100, simplify=FALSE)

microbenchmark(
  as.data.frame(a), 
  as_data_frame(a)
)


x = list(test = c(1,2,3,4), test2= c(2,3,3,2))

x <- return_names(x)
x

y <- c(1,2,3,4)
calculateSum(y)

runthis()

