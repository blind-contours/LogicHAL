#include <Rcpp.h>
using namespace Rcpp;

// Function to compute mean difference score
double mean_difference_score(NumericVector true_labels, LogicalVector predicted_labels) {
  NumericVector group_1 = true_labels[predicted_labels];
  NumericVector group_0 = true_labels[!predicted_labels];

  if (group_1.size() == 0 || group_0.size() == 0) {
    return NA_REAL;
  }

  double mean_1 = mean(group_1);
  double mean_0 = mean(group_0);

  return std::abs(mean_1 - mean_0);
}

// [[Rcpp::export]]
Rcpp::List computeMeanDifferenceScoresWithLogic(DataFrame data, CharacterVector columns, NumericVector outcome, Nullable<LogicalVector> previous_rule = R_NilValue, std::string previous_rule_name = "") {
  int n = data.nrows();
  int m = columns.size();
  std::vector<std::string> colnames = Rcpp::as<std::vector<std::string>>(columns);

  Rcpp::NumericVector MeanDiff_Individual(m, NA_REAL);
  Rcpp::NumericVector MeanDiff_indiv_ruleAnd(m, NA_REAL);
  Rcpp::NumericVector MeanDiff_indiv_ruleOr(m, NA_REAL);

  Rcpp::CharacterVector RuleIndividual(m);
  Rcpp::CharacterVector Rule_indiv_And(m);
  Rcpp::CharacterVector Rule_indiv_Or(m);

  Rcpp::NumericVector MeanDiffAnd(m * (m - 1) / 2, NA_REAL);
  Rcpp::NumericVector MeanDiffOr(m * (m - 1) / 2, NA_REAL);

  Rcpp::CharacterVector RuleAnd(m * (m - 1) / 2);
  Rcpp::CharacterVector RuleOr(m * (m - 1) / 2);

  Rcpp::NumericVector MeanDiffAndAnd(m * (m - 1) / 2, NA_REAL);
  Rcpp::NumericVector MeanDiffAndOr(m * (m - 1) / 2, NA_REAL);
  Rcpp::NumericVector MeanDiffOrAnd(m * (m - 1) / 2, NA_REAL);
  Rcpp::NumericVector MeanDiffOrOr(m * (m - 1) / 2, NA_REAL);

  Rcpp::CharacterVector RuleAndAnd(m * (m - 1) / 2);
  Rcpp::CharacterVector RuleAndOr(m * (m - 1) / 2);
  Rcpp::CharacterVector RuleOrAnd(m * (m - 1) / 2);
  Rcpp::CharacterVector RuleOrOr(m * (m - 1) / 2);

  LogicalVector prev_rule(n, true);
  if (previous_rule.isNotNull()) {
    prev_rule = previous_rule.get();
  }

  int idx = 0;
  for (int i = 0; i < m; ++i) {
    Rcpp::IntegerVector col1 = data[colnames[i]];
    LogicalVector indiv_ruleAnd(n);
    LogicalVector indiv_ruleOr(n);

    LogicalVector ruleAnd(n);
    LogicalVector ruleOr(n);

    LogicalVector ruleAndAnd(n);
    LogicalVector ruleOrAnd(n);
    LogicalVector ruleAndOr(n);
    LogicalVector ruleOrOr(n);
    LogicalVector indiv_rule(n);

    for (int k = 0; k < n; ++k) {
      if (previous_rule.isNotNull()) {
        indiv_ruleAnd[k] = col1[k] & prev_rule[k];
        indiv_ruleOr[k] = col1[k] | prev_rule[k];
      } else {
        indiv_rule[k] = col1[k];
      }
    }

    if (previous_rule.isNotNull()) {
      MeanDiff_indiv_ruleAnd[i] = mean_difference_score(outcome, indiv_ruleAnd);
      MeanDiff_indiv_ruleOr[i] = mean_difference_score(outcome, indiv_ruleOr);

      Rule_indiv_And[i] = "(" + colnames[i] + " == 1) & (" + previous_rule_name + ")";
      Rule_indiv_Or[i] = "(" + colnames[i] + " == 1) | (" + previous_rule_name + ")";
    } else {
      MeanDiff_Individual[i] = mean_difference_score(outcome, indiv_rule);
      RuleIndividual[i] = colnames[i] + " == 1";
    }

    for (int j = i + 1; j < m; ++j) {
      Rcpp::IntegerVector col2 = data[colnames[j]];

      for (int k = 0; k < n; ++k) {
        if (previous_rule.isNotNull()) {
          ruleAndAnd[k] = (col1[k] & col2[k]) & prev_rule[k];
          ruleOrOr[k] = (col1[k] | col2[k]) | prev_rule[k];
          ruleAndOr[k] = (col1[k] & col2[k]) | prev_rule[k];
          ruleOrAnd[k] = (col1[k] | col2[k]) & prev_rule[k];
        } else {
          ruleAnd[k] = col1[k] & col2[k];
          ruleOr[k] = col1[k] | col2[k];
        }
      }

      double mean_diff_and = mean_difference_score(outcome, ruleAnd);
      double mean_diff_or = mean_difference_score(outcome, ruleOr);

      double mean_diff_and_and = mean_difference_score(outcome, ruleAndAnd);
      double mean_diff_and_or = mean_difference_score(outcome, ruleAndOr);
      double mean_diff_or_and = mean_difference_score(outcome, ruleOrAnd);
      double mean_diff_or_or = mean_difference_score(outcome, ruleOrOr);

      MeanDiffAnd[idx] = mean_diff_and;
      MeanDiffOr[idx] = mean_diff_or;

      MeanDiffAndAnd[idx] = mean_diff_and_and;
      MeanDiffAndOr[idx] = mean_diff_and_or;
      MeanDiffOrAnd[idx] = mean_diff_or_and;
      MeanDiffOrOr[idx] = mean_diff_or_or;

      if (previous_rule.isNotNull()) {
        RuleAndAnd[idx] = "(" + colnames[i] + " == 1) & (" + colnames[j] + " == 1) & (" + previous_rule_name + ")";
        RuleAndOr[idx] = "(" + colnames[i] + " == 1) | (" + colnames[j] + " == 1) & (" + previous_rule_name + ")";
        RuleOrAnd[idx] = "(" + colnames[i] + " == 1) & (" + colnames[j] + " == 1) | (" + previous_rule_name + ")";
        RuleOrOr[idx] = "(" + colnames[i] + " == 1) | (" + colnames[j] + " == 1) | (" + previous_rule_name + ")";
      } else {
        RuleAnd[idx] = "(" + colnames[i] + " == 1) & (" + colnames[j] + " == 1)";
        RuleOr[idx] = "(" + colnames[i] + " == 1) | (" + colnames[j] + " == 1)";
      }

      idx++;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("MeanDiff_Individual") = MeanDiff_Individual,
    Rcpp::Named("MeanDiff_indiv_ruleAnd") = MeanDiff_indiv_ruleAnd,
    Rcpp::Named("MeanDiff_indiv_ruleOr") = MeanDiff_indiv_ruleOr,
    Rcpp::Named("MeanDiffAnd") = MeanDiffAnd,
    Rcpp::Named("MeanDiffOr") = MeanDiffOr,
    Rcpp::Named("MeanDiffAndAnd") = MeanDiffAndAnd,
    Rcpp::Named("MeanDiffAndOr") = MeanDiffAndOr,
    Rcpp::Named("MeanDiffOrAnd") = MeanDiffOrAnd,
    Rcpp::Named("MeanDiffOrOr") = MeanDiffOrOr,
    Rcpp::Named("RuleIndividual") = RuleIndividual,
    Rcpp::Named("Rule_indiv_And") = Rule_indiv_And,
    Rcpp::Named("Rule_indiv_Or") = Rule_indiv_Or,
    Rcpp::Named("RuleAnd") = RuleAnd,
    Rcpp::Named("RuleOr") = RuleOr,
    Rcpp::Named("RuleAndAnd") = RuleAndAnd,
    Rcpp::Named("RuleAndOr") = RuleAndOr,
    Rcpp::Named("RuleOrAnd") = RuleOrAnd,
    Rcpp::Named("RuleOrOr") = RuleOrOr
  );
}
