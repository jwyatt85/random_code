
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

// [[Rcpp::export]]
RObject return_names(List my_vec) {
  List test = clone(my_vec);
  CharacterVector i = my_vec.names();
  
  for(int m = 0; m < i.size(); m++)
  {
    i[m] = "Test1";
  }
  
  test.names() = i;

  return test;
}

