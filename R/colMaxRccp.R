#' Columns Max
#'
#' Caculates columns Max
#' @return Columns max.
#' @export

colMaxRcpp <- cxxfunction(signature(X_="numeric"), plugin="Rcpp",
                          body='
                          Rcpp::NumericMatrix X(X_);
                          int n = X.ncol();
                          Rcpp::NumericVector V(n);
                          for (int i=0; i<n; i++) {
                          Rcpp::NumericVector W = X.column(i);
                          V[i] = *std::max_element(W.begin(), W.end());  // from the STL
                          }
                          return(V);
                          ')
