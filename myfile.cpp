
#include <Rcpp.h> 
#include <iostream>
#include <iomanip>
#include <vector>
#include <math.h>
#include <time.h>
#include <R.h>
#include <Rmath.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector convolveCpp(NumericVector a, NumericVector b) {
  int na = a.size(), nb = b.size();
  int nab = na + nb - 1;
  NumericVector xab(nab);
  for (int i = 0; i < na; i++)
    for (int j = 0; j < nb; j++)
      xab[i + j] += a[i] * b[j];
  return xab;
}

// [[Rcpp::export]]
int james(int x) {
  if(x > 5) {
    x = x* 5;
    return(std::cout << "The Value is: " << x);
  }
  else {
    x = x + 5;
    return(x);
  }
}


// [[Rcpp::export]]
List as_data_frame(List a) {
  List returned_frame = clone(a);
  GenericVector sample_row = returned_frame(0);
  
  StringVector row_names(sample_row.length());
  for (int i = 0; i < sample_row.length(); i++) {
    char name[5];
    sprintf(&(name[0]), "%d", i);
    row_names(i) = name;
  }
  returned_frame.attr("row.names") = row_names;
  
  StringVector col_names(returned_frame.length());
  for (int j = 0; j < returned_frame.length(); ++j) {
    char name[6];
    sprintf(&(name[0]), "X.%d", j);
    col_names(j) = name;
  }
  returned_frame.attr("names") = col_names;
  returned_frame.attr("class") = "data.frame";
  
  return returned_frame;
}