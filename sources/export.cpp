
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
            if(this_vec[looper] == 0){
              this_vec[looper] = .0001;
            }
            *vector = this_vec;
        }
      }
    }
  }
  return x;
}

// [[Rcpp::export]]
RObject t_pointers(){
  int y = 6;
  int *ip;
  
  ip = &y;
  
  time_t now = time(0);
  char* dt = ctime(&now);
  
  Rcout << " the address of y = " << ip << std::endl;
  Rcout << "The value of y stored in ip = " << *ip << std::endl; 
  Rcout << " Time is now: " << dt;

  
  typedef struct
  {
    char m[10];
    char m2[10];
  } Books;
  
  Books Book1;
  strcpy(Book1.m, "hey-o");
  strcpy(Book1.m2, "test2");
  
  Rcout << Book1.m << std::endl; 
  
  IntegerVector x = IntegerVector::create( 0, 1, NA_INTEGER, 3 );
  IntegerVector test1 = seq_len(5);
  
  bool res = all(is_na(x));
  
  if(res){
    Rcout << "yay it's not defined";
  }
  
  for(int i = 0; i != test1.size(); i++){
    Rcout << test1[i];
  }
  
  test1.names() = CharacterVector::create("one" ,"two", "three", "four", "five");
    
  return(test1);
}


