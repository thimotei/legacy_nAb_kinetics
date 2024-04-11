#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame convert_log_scale_inverse_cpp(DataFrame dt_in,
                                        CharacterVector vars_to_transform) {
  // Make a clone of the data.frame to avoid modifying the original
  DataFrame dt_clone = clone(dt_in);
  
  // Loop over all the variables to transform
  for (int i = 0; i < vars_to_transform.size(); i++) {
    String var = vars_to_transform[i];
    
    // Retrieve the column as a NumericVector
    NumericVector col = dt_clone[var];
    
    // Apply the inverse transformation
    for (int j = 0; j < col.size(); j++) {
      col[j] = (col[j] <= 1) ? 5 * pow(2, col[j]) : 5 * pow(2, col[j] + 1);
    }
    
    // Replace the column in the cloned data.frame
    dt_clone[var] = col;
  }
  
  return dt_clone;
}
