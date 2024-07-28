#include <Rcpp.h>
using namespace Rcpp;

// Function to compute F1 score
double f1_score_pair(IntegerVector true_labels, LogicalVector predicted_labels) {
  int tp = 0, fp = 0, fn = 0;
  int n = true_labels.size();

  for (int i = 0; i < n; ++i) {
    if (predicted_labels[i]) {
      if (true_labels[i] == 1) tp++;
      else fp++;
    } else {
      if (true_labels[i] == 1) fn++;
    }
  }

  double precision = (tp + fp == 0) ? 0 : (double)tp / (tp + fp);
  double recall = (tp + fn == 0) ? 0 : (double)tp / (tp + fn);

  if (precision + recall == 0) {
    return 0;
  } else {
    return 2 * (precision * recall) / (precision + recall);
  }
}

// [[Rcpp::export]]
Rcpp::List computePairwiseLogicInteractions(DataFrame data, List pairs, IntegerVector outcome) {
  int n = data.nrows();
  int m = pairs.size();

  Rcpp::NumericVector F1And(m, 0.0);
  Rcpp::NumericVector F1Or(m, 0.0);

  Rcpp::CharacterVector RuleAnd(m);
  Rcpp::CharacterVector RuleOr(m);

  for (int idx = 0; idx < m; ++idx) {
    CharacterVector pair = pairs[idx];
    std::string col1_name = Rcpp::as<std::string>(pair[0]);
    std::string col2_name = Rcpp::as<std::string>(pair[1]);

    Rcpp::IntegerVector col1 = data[col1_name];
    Rcpp::IntegerVector col2 = data[col2_name];

    LogicalVector ruleAnd(n);
    LogicalVector ruleOr(n);

    for (int k = 0; k < n; ++k) {
      ruleAnd[k] = col1[k] & col2[k];
      ruleOr[k] = col1[k] | col2[k];
    }

    double f1_and = f1_score_pair(outcome, ruleAnd);
    double f1_or = f1_score_pair(outcome, ruleOr);

    F1And[idx] = f1_and;
    F1Or[idx] = f1_or;

    RuleAnd[idx] = "(" + col1_name + " & " + col2_name + ")";
    RuleOr[idx] = "(" + col1_name + " | " + col2_name + ")";
  }

  return Rcpp::List::create(
    Rcpp::Named("F1And") = F1And,
    Rcpp::Named("F1Or") = F1Or,
    Rcpp::Named("RuleAnd") = RuleAnd,
    Rcpp::Named("RuleOr") = RuleOr
  );
}
