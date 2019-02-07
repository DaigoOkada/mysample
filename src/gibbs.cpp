#include <Rcpp.h>
using namespace Rcpp;

// use the code of http://abrahamcow.hatenablog.com/entry/2017/09/16/040128
// [[Rcpp::export]]
List GibbsNormal(NumericVector x, double init,
                 double mu0, double sigma20, double a, double b,
                 int iter) {
  NumericVector mu(iter);
  NumericVector sigma2(iter);
  int n = x.length();
  double xbar = mean(x);
  sigma2[0] = init;
  double B;
  B = 1/sigma20+n/sigma2[0];
  mu[0] = R::rnorm(((1/sigma20)*mu0+n*(1/sigma2[0])*xbar)/B,1/sqrt(B));
  double ra1;
  double sh1 = a+n/2;
  for(int i=1; i<iter; i++){
    B = 1/sigma20+n/sigma2[i-1];
    mu[i] = R::rnorm(((1/sigma20)*mu0+n*(1/sigma2[i-1])*xbar)/B,1/sqrt(B));
    ra1 = 0;
    for(int j=0; j<n ;j++){
      ra1 = ra1+pow(x[j]-mu[i],2);
    }
    ra1 = ra1/2 + b;
    sigma2[i] = 1/R::rgamma(sh1,1/ra1);
  }
  return List::create(Named("mean") = mu , _["variance"]=sigma2);
}
