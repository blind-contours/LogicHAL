#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate the F1 score
double f1_score_merge(IntegerVector true_labels, LogicalVector predicted_labels) {
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
DataFrame compute_merge_trees(CharacterVector rules, IntegerVector true_labels, List data) {
  int n = rules.size();
  int m = true_labels.size();

  std::vector<std::string> new_rules;
  std::vector<double> f1_scores;

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      if (i != j) {
        // AND combination
        LogicalVector and_result(m);
        LogicalVector rule_i = data[i];
        LogicalVector rule_j = data[j];
        for (int k = 0; k < m; ++k) {
          and_result[k] = rule_i[k] && rule_j[k];
        }
        new_rules.push_back("(" + std::string(rules[i]) + " & " + std::string(rules[j]) + ")");
        f1_scores.push_back(f1_score_merge(true_labels, and_result));

        // OR combination
        LogicalVector or_result(m);
        for (int k = 0; k < m; ++k) {
          or_result[k] = rule_i[k] || rule_j[k];
        }
        new_rules.push_back("(" + std::string(rules[i]) + " | " + std::string(rules[j]) + ")");
        f1_scores.push_back(f1_score_merge(true_labels, or_result));
      }
    }
  }

  return DataFrame::create(Named("Rule") = new_rules,
                           Named("F1_Score") = f1_scores);
}
