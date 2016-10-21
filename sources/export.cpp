
#include <Rcpp.h> 
#include <iostream>
#include <iomanip>
#include <vector>
#include <math.h>
#include <time.h>
#include <R.h>
#include <Rmath.h>
#include <iostream>
#include <list>

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::DataFrame as_data_frame(List a) {
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
RObject runthis(){

  Rcpp::List x = Rcpp::List::create(
    NumericVector::create(4,2,5), 
    NumericVector::create(2,3,5,5,5,5),
    NumericVector::create(1,4,6,5,4,5,5,5,5,5),
    CharacterVector::create("eke", "woo")
  );
  
  int my_it;
  
  Rcpp::NumericVector check_vec;
  typedef Rcpp::List::iterator list_it;
  typedef Rcpp::NumericVector::iterator num_it;
  
  for(list_it m = x.begin(); m != x.end(); ++m){
    check_vec = *m;
    for(num_it yay = check_vec.begin(); yay != check_vec.end(); ++yay){
      if(*yay == 5){
        *yay = 9;
        my_it = *yay;
        // Rcout << " The number now is: " << my_it;
      }
    }
  }
  return x;
}
  




