

library(Rcpp)
library(microbenchmark)

cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
            return sum;
            }')

add(1,2,3)

sourceCpp("~/Desktop/learning_cpp_r/export.cpp")

convolveCpp(3,10)

microbenchmark(
  mean(c(2,3,4,5,6)),
  james(2)
)
