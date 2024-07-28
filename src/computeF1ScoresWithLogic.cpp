#include <Rcpp.h>
using namespace Rcpp;

// Function to compute F1 score
double f1_score(IntegerVector true_labels, LogicalVector predicted_labels) {
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
Rcpp::List computeF1ScoresWithLogic(DataFrame data, CharacterVector columns, IntegerVector outcome, LogicalVector previous_rule, std::string previous_rule_name) {
  int n = data.nrows();
  int m = columns.size();
  std::vector<std::string> colnames = Rcpp::as<std::vector<std::string>>(columns);

  Rcpp::NumericVector F1_Individual(m, 0.0);
  Rcpp::NumericVector F1_indiv_ruleAnd(m, 0.0);
  Rcpp::NumericVector F1_indiv_ruleOr(m, 0.0);

  Rcpp::CharacterVector RuleIndividual(m);
  Rcpp::CharacterVector Rule_indiv_And(m);
  Rcpp::CharacterVector Rule_indiv_Or(m);

  Rcpp::NumericVector F1AndAnd(m * (m - 1) / 2, 0.0);
  Rcpp::NumericVector F1AndOr(m * (m - 1) / 2, 0.0);
  Rcpp::NumericVector F1OrAnd(m * (m - 1) / 2, 0.0);
  Rcpp::NumericVector F1OrOr(m * (m - 1) / 2, 0.0);

  Rcpp::CharacterVector RuleAndAnd(m * (m - 1) / 2);
  Rcpp::CharacterVector RuleAndOr(m * (m - 1) / 2);
  Rcpp::CharacterVector RuleOrAnd(m * (m - 1) / 2);
  Rcpp::CharacterVector RuleOrOr(m * (m - 1) / 2);

  int idx = 0;
  for (int i = 0; i < m; ++i) {
    Rcpp::IntegerVector col1 = data[colnames[i]];
    LogicalVector indiv_ruleAnd(n);
    LogicalVector indiv_ruleOr(n);

    LogicalVector ruleAndAnd(n);
    LogicalVector ruleOrAnd(n);
    LogicalVector ruleAndOr(n);
    LogicalVector ruleOrOr(n);
    LogicalVector indiv_rule(n);

    for (int k = 0; k < n; ++k) {
      indiv_ruleAnd[k] = col1[k] & previous_rule[k];
      indiv_ruleOr[k] = col1[k] | previous_rule[k];
    }

    F1_indiv_ruleAnd[i] = f1_score(outcome, indiv_ruleAnd);
    F1_indiv_ruleOr[i] = f1_score(outcome, indiv_ruleOr);

    Rule_indiv_And[i] = "(" + colnames[i] + " == 1 & " + previous_rule_name + ")";
    Rule_indiv_Or[i] = "(" + colnames[i] + " == 1 | " + previous_rule_name + ")";

    for (int j = i + 1; j < m; ++j) {
      Rcpp::IntegerVector col2 = data[colnames[j]];

      for (int k = 0; k < n; ++k) {
        ruleAndAnd[k] = (col1[k] & col2[k]) & previous_rule[k];
        ruleOrOr[k] = (col1[k] | col2[k]) | previous_rule[k];
        ruleAndOr[k] = (col1[k] & col2[k]) | previous_rule[k];
        ruleOrAnd[k] = (col1[k] | col2[k]) & previous_rule[k];
      }

      double f1_and_and = f1_score(outcome, ruleAndAnd);
      double f1_and_or = f1_score(outcome, ruleAndOr);
      double f1_or_and = f1_score(outcome, ruleOrAnd);
      double f1_or_or = f1_score(outcome, ruleOrOr);

      F1AndAnd[idx] = f1_and_and;
      F1AndOr[idx] = f1_and_or;
      F1OrAnd[idx] = f1_or_and;
      F1OrOr[idx] = f1_or_or;

      RuleAndAnd[idx] = "((" + colnames[i] + " == 1 & " + colnames[j] + " == 1) & " + previous_rule_name + ")";
      RuleAndOr[idx] = "((" + colnames[i] + " == 1 & " + colnames[j] + " == 1) | " + previous_rule_name + ")";
      RuleOrAnd[idx] = "((" + colnames[i] + " == 1 | " + colnames[j] + " == 1) & " + previous_rule_name + ")";
      RuleOrOr[idx] = "((" + colnames[i] + " == 1 | " + colnames[j] + " == 1) | " + previous_rule_name + ")";

      idx++;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("F1_Individual") = F1_Individual,
    Rcpp::Named("F1_indiv_ruleAnd") = F1_indiv_ruleAnd,
    Rcpp::Named("F1_indiv_ruleOr") = F1_indiv_ruleOr,
    Rcpp::Named("F1AndAnd") = F1AndAnd,
    Rcpp::Named("F1AndOr") = F1AndOr,
    Rcpp::Named("F1OrAnd") = F1OrAnd,
    Rcpp::Named("F1OrOr") = F1OrOr,
    Rcpp::Named("RuleIndividual") = RuleIndividual,
    Rcpp::Named("Rule_indiv_And") = Rule_indiv_And,
    Rcpp::Named("Rule_indiv_Or") = Rule_indiv_Or,
    Rcpp::Named("RuleAndAnd") = RuleAndAnd,
    Rcpp::Named("RuleAndOr") = RuleAndOr,
    Rcpp::Named("RuleOrAnd") = RuleOrAnd,
    Rcpp::Named("RuleOrOr") = RuleOrOr
  );
}
