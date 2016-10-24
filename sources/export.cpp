
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
    StringVector::create("male", "female", "1", "2"),
    NumericVector::create(2,3,5,5,5,5),
    NumericVector::create(1,4,6,5,4,5,5,5,5,5)
  );
  
  StringVector my_it;
  
  Rcpp::StringVector check_vec;
  typedef Rcpp::List::iterator list_it;
  typedef Rcpp::StringVector::iterator num_it;
  
  for(list_it m = x.begin(); m != x.end(); ++m){
    check_vec = *m;
    for(num_it yay = check_vec.begin(); yay != check_vec.end(); ++yay){
        my_it = *yay;
      if(*yay == "male"){
        *yay = "change me";
      }
    }
  }
  return x;
}

// [[Rcpp::export]]
RObject cpp_county_check(List x){
  
  Rcpp::DataFrame df_vec;
  Rcpp::List beginner_list;
  Rcpp::List beginner_list2;
  Rcpp::NumericVector this_vec;
  
  typedef Rcpp::List::iterator list_it;
  typedef Rcpp::List::iterator list_it2;
  typedef Rcpp::List::iterator list_it3;
  typedef Rcpp::DataFrame::iterator num_it;
  
  for(list_it m1 = x.begin(); m1 != x.end(); ++m1){
    beginner_list = *m1;
    for(list_it2 m2 = beginner_list.begin(); m2 != beginner_list.end(); ++m2){
      beginner_list2 = *m2;
      for(list_it3 m3 = beginner_list2.begin(); m3 != beginner_list2.end(); ++m3){
        df_vec = *m3;
        for(num_it vector = df_vec.begin(); vector != df_vec.end(); ++vector){
          this_vec = *vector;
          for(int looper = 0; looper != this_vec.size(); ++looper)
            if(this_vec(looper) == 0){
              this_vec(looper) = .0001;
            }
            *vector = this_vec;
        }
      }
    }
  }
  return x;
}
