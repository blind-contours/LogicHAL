#include <Rcpp.h>
using namespace Rcpp;

// Function to compute F1 score
double f1_score_triple(IntegerVector true_labels, LogicalVector predicted_labels) {
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
Rcpp::List computeThreeWayLogicInteractions(DataFrame data, List triples, IntegerVector outcome) {
  int n = data.nrows();
  int m = triples.size();

  Rcpp::NumericVector F1_AndAndAnd(m, 0.0);
  Rcpp::NumericVector F1_AndAndOr(m, 0.0);
  Rcpp::NumericVector F1_AndOr(m, 0.0);
  Rcpp::NumericVector F1_OrAnd(m, 0.0);
  Rcpp::NumericVector F1_OrOr(m, 0.0);
  Rcpp::NumericVector F1_AndOrAnd(m, 0.0);
  Rcpp::NumericVector F1_OrAndOr(m, 0.0);
  Rcpp::NumericVector F1_OrOrAnd(m, 0.0);

  Rcpp::CharacterVector Rule_AndAndAnd(m);
  Rcpp::CharacterVector Rule_AndAndOr(m);
  Rcpp::CharacterVector Rule_AndOr(m);
  Rcpp::CharacterVector Rule_OrAnd(m);
  Rcpp::CharacterVector Rule_OrOr(m);
  Rcpp::CharacterVector Rule_AndOrAnd(m);
  Rcpp::CharacterVector Rule_OrAndOr(m);
  Rcpp::CharacterVector Rule_OrOrAnd(m);

  int idx = 0;
  for (int t = 0; t < m; ++t) {
    CharacterVector triple = triples[t];
    std::string col1_name = Rcpp::as<std::string>(triple[0]);
    std::string col2_name = Rcpp::as<std::string>(triple[1]);
    std::string col3_name = Rcpp::as<std::string>(triple[2]);

    Rcpp::IntegerVector col1 = data[col1_name];
    Rcpp::IntegerVector col2 = data[col2_name];
    Rcpp::IntegerVector col3 = data[col3_name];

    LogicalVector rule_AndAndAnd(n), rule_AndAndOr(n), rule_AndOr(n), rule_OrAnd(n), rule_OrOr(n), rule_AndOrAnd(n), rule_OrAndOr(n), rule_OrOrAnd(n);

    for (int l = 0; l < n; ++l) {
      rule_AndAndAnd[l] = (col1[l] & col2[l]) & col3[l]; // (A AND B) AND C
      rule_AndAndOr[l] = (col1[l] & col2[l]) | col3[l]; // (A AND B) OR C
      rule_AndOr[l] = col1[l] & (col2[l] | col3[l]); // A AND (B OR C)
      rule_OrAnd[l] = col1[l] | (col2[l] & col3[l]); // A OR (B AND C)
      rule_OrOr[l] = (col1[l] | col2[l]) | col3[l]; // (A OR B) OR C
      rule_AndOrAnd[l] = (col1[l] & (col2[l] | col3[l])); // A AND (B OR C)
      rule_OrAndOr[l] = col1[l] | (col2[l] | col3[l]); // A OR (B OR C)
      rule_OrOrAnd[l] = (col1[l] | col2[l]) & col3[l]; // (A OR B) AND C
    }

    F1_AndAndAnd[idx] = f1_score_triple(outcome, rule_AndAndAnd);
    Rule_AndAndAnd[idx] = "(" + col1_name + " & " + col2_name + ") & " + col3_name;

    F1_AndAndOr[idx] = f1_score_triple(outcome, rule_AndAndOr);
    Rule_AndAndOr[idx] = "(" + col1_name + " & " + col2_name + ") | " + col3_name;

    F1_AndOr[idx] = f1_score_triple(outcome, rule_AndOr);
    Rule_AndOr[idx] = col1_name + " & (" + col2_name + " | " + col3_name + ")";

    F1_OrAnd[idx] = f1_score_triple(outcome, rule_OrAnd);
    Rule_OrAnd[idx] = col1_name + " | (" + col2_name + " & " + col3_name + ")";

    F1_OrOr[idx] = f1_score_triple(outcome, rule_OrOr);
    Rule_OrOr[idx] = "(" + col1_name + " | " + col2_name + ") | " + col3_name;

    F1_AndOrAnd[idx] = f1_score_triple(outcome, rule_AndOrAnd);
    Rule_AndOrAnd[idx] = col1_name + " & (" + col2_name + " | " + col3_name + ")";

    F1_OrAndOr[idx] = f1_score_triple(outcome, rule_OrAndOr);
    Rule_OrAndOr[idx] = col1_name + " | (" + col2_name + " | " + col3_name + ")";

    F1_OrOrAnd[idx] = f1_score_triple(outcome, rule_OrOrAnd);
    Rule_OrOrAnd[idx] = "(" + col1_name + " | " + col2_name + ") & " + col3_name;

    idx++;
  }

  return Rcpp::List::create(
    Rcpp::Named("F1_AndAndAnd") = F1_AndAndAnd,
    Rcpp::Named("F1_AndAndOr") = F1_AndAndOr,
    Rcpp::Named("F1_AndOr") = F1_AndOr,
    Rcpp::Named("F1_OrAnd") = F1_OrAnd,
    Rcpp::Named("F1_OrOr") = F1_OrOr,
    Rcpp::Named("F1_AndOrAnd") = F1_AndOrAnd,
    Rcpp::Named("F1_OrAndOr") = F1_OrAndOr,
    Rcpp::Named("F1_OrOrAnd") = F1_OrOrAnd,
    Rcpp::Named("Rule_AndAndAnd") = Rule_AndAndAnd,
    Rcpp::Named("Rule_AndAndOr") = Rule_AndAndOr,
    Rcpp::Named("Rule_AndOr") = Rule_AndOr,
    Rcpp::Named("Rule_OrAnd") = Rule_OrAnd,
    Rcpp::Named("Rule_OrOr") = Rule_OrOr,
    Rcpp::Named("Rule_AndOrAnd") = Rule_AndOrAnd,
    Rcpp::Named("Rule_OrAndOr") = Rule_OrAndOr,
    Rcpp::Named("Rule_OrOrAnd") = Rule_OrOrAnd
  );
}
