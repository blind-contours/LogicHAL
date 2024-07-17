#include <Rcpp.h>
#include <algorithm>
#include <iomanip> // Include for setting precision

using namespace Rcpp;

// Function to compute quantiles
NumericVector compute_quantiles(NumericVector x, int num_knots) {
  NumericVector quantiles(num_knots);
  NumericVector sorted_x = clone(x).sort();
  int n = sorted_x.size();

  for (int k = 0; k < num_knots; k++) {
    double pos = (double)(k + 1) / (num_knots + 1) * (n - 1);
    int lower = floor(pos);
    int upper = ceil(pos);
    double weight = pos - lower;
    quantiles[k] = sorted_x[lower] * (1.0 - weight) + sorted_x[upper] * weight;
  }

  return quantiles;
}

// [[Rcpp::export]]
DataFrame create_basis_functions(DataFrame data, CharacterVector columns, int num_knots = 0, int precision = 2) {
  int n = data.nrows();
  int p = columns.size();
  List result;

  for (int j = 0; j < p; j++) {
    NumericVector col = data[std::string(columns[j])];
    NumericVector unique_values = unique(col).sort();

    if (num_knots > 0) {
      unique_values = compute_quantiles(col, num_knots);
    }

    for (int i = 0; i < unique_values.size(); i++) {
      double s = unique_values[i];
      LogicalVector indicator(n);
      for (int k = 0; k < n; k++) {
        indicator[k] = col[k] < s;
      }
      std::ostringstream col_name;
      col_name << std::fixed << std::setprecision(precision) << std::string(columns[j]) << "_lt_" << s;
      result[col_name.str()] = indicator;
    }
  }

  return DataFrame(result);
}
