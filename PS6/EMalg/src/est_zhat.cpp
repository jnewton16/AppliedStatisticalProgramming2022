#include <Rcpp.h>
using namespace Rcpp;

//' Solve equation 2 of the EM algorithm
//'
//' @param y, a vector of forecasts
//' @param ftk, a vector of the forecasts
//' @export
// [[Rcpp::export]]


NumericVector est_zhat(NumericVector y, NumericVector ftk) {
  int n = y.size();
  int m = ftk.size();
  NumericVector weights(m, 1/m);
  NumericMatrix zhat(n,m);
  for(int i=0, i<n, i++){
    NumericVector num_vec(m);
    for(int j=0, j<m, j++){
      num_vec[j] = dnorm(y,ftk,1);
    }
    zhat(i,) = num_vec/sum(num_vec);
  }
  return zhat
}


/*** R
est_zhat(y=c(.2,0.5),ftk=seq(0,1,0.1))
*/

library(Rcpp)
cppFunction(
  'int add(int x, int y, int z){
    int sum = x + y + z;
    return sum;
  }'
)
add(10,20,3)

