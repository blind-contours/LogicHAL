
# LogicHAL

<!-- badges: start -->
<!-- badges: end -->

The goal of LogicHAL is to create logic tree interactions in the highly
adaptive lasso. Logic trees are constructed as boolean statements, such
as var 1 & var 2, or, var 1 \| var 2, where the pipe is the OR operator
and & is the AND operator. Tools like logic regression have built these
trees for binary data. We extend this to 0-order splines so that rules
like age \< 50 \| BMI \> 20 may be a rule used in a logic tree, for
example the rule may be (age \< 50 \| BMI \> 20) & pre_existing
condition == TRUE.

This package uses a recursive building strategy of the boolean rules to
explore the logic space. For binary outcomes we use the F1 score to
determine how well a rule classifies the outcome, for a continuous
outcome we use mean differences between rule == 0 and rule == 1, that is
finding rules that have the largest mean difference in outcome.

In each case, we start with the pairwise rule such as var 1 & var 2, or,
var 1 \| var 2, that maximize the F1 or mean differences score in the
outcome. This defines the logic root. This logic root defines the
pairwise logic statement that best explains the outcome. We then can
build on these two logic roots. On each, we can add an & statement, or,
\| statement with one variable added, such as (var 1 & var 2) \| var 3 -
if we were to add an \| statement to the base & logic root, or, (var 1
\| var 2) & var 3 - if we were to add an & statement to the base \|
logic root.

We can also add a new pair of variables such as (var 4 & var 5), or,
(var 4 \| var 5) can be added to one of the logic roots using an &, or,
\|, logic statement. So we could add an &(&) connector which is adding
two variables connected by an & to the logic root with and &, likewise
&(\|) is adding two variables connected with an \| statement to the
logic root with an &. So we have four options &(&), &(\|), \|(&),
\|(\|) - when adding pairs of variables.

So on the logic root we can create six new logic rules. For example, to
the logic root (var 4 & var 5) we could have ((var 4 & var 5) \| var 6)
where var 6 is now the variable that when added leads to a higher F1 or
mean difference score compared to (var 4 & var 5) and is the maximum
score of all the other three way interaction variables using this rule,
meaning the score for ((var 4 & var 5) \| var 6) is higher than ((var 4
& var 5) \| var 8) etc. The same is true for all the other connectors
such as ((var 4 & var 5) \| (var 6 & var 12)), here we add (var 6 & var
12) which is the maximizer down this path. That is, from our logic root
(var 4 & var 5), we add this to all other (var i & var j) pairs
connected with and \| statement and select the one that has the highest
outcome score.

This process is recursive. We start with the pairwise &, \|,
interactions, then we add all 6 connector options, select the logic rule
down each path that maximizes the score for the outcome, then pass this
rule back in and add all six options again. We do this until max depth
is met down each logic path. We keep track of which logic rule across
all paths results in the best outcome score and is least complex (for
interpretability) and return this rule as the tree that, given this
greedy logic building search, results in the best outcome score with
least complexity.

The user specifies the max number of trees, this logic tree process is
done up to max trees. At iteration one, we find the tree that best
classifies the outcome given this search strategy, then at iteration 2,
we remove the variables used in the first tree in the second search and
buld another tree, etc, until max trees are finished.

Using these logic trees, these then become the variables in a lasso
regression. The output is the lasso model with the logic trees used in
this model.

## Installation

You can install the development version of LogicHAL like so:

``` r
remotes::install_github("blind-contours/LogicHAL@main")
```

## Binary Outcome Example

Consider the following minimal example in using LogicHAL to generate
predictions and Logic Trees:

``` r
library(LogicHAL)
#> Warning: replacing previous import 'dplyr::union' by 'igraph::union' when
#> loading 'LogicHAL'
#> Warning: replacing previous import 'dplyr::as_data_frame' by
#> 'igraph::as_data_frame' when loading 'LogicHAL'
#> Warning: replacing previous import 'dplyr::groups' by 'igraph::groups' when
#> loading 'LogicHAL'

# Generate example data
set.seed(123)

# Number of observations
n <- 2000

# Number of features
p <- 100

# Create a binary matrix with some correlation
X <- matrix(rbinom(n * p, 1, 0.1), n, p)
# Create column names
colnames(X) <- paste0("var", 1:p)

# Create a binary outcome based on a rule (var1 OR var8)
Y <- as.numeric((X[, 1] | X[, 8]) & (X[, 34]))

# Combine into a data frame
df <- data.frame(X, outcome = Y)

# Run LogicHAL
result <- LogicHAL(data = df, outcome = "outcome", columns = colnames(X), max_trees = 2, max_depth = 3, family = "binomial")
#> Warning: from glmnet C++ code (error code -2); Convergence for 2th lambda value
#> not reached after maxit=100000 iterations; solutions for larger lambdas
#> returned

# Print final model and trees used
coef(result$model)
#> 3 x 1 sparse Matrix of class "dgCMatrix"
#>                                                              s1
#> (Intercept)                                           -4.716860
#> (var34 == 1 & (var1 == 1 | var8 == 1))                 4.666672
#> ((var10 == 1 | var34 == 1) & (var1 == 1 | var8 == 1))  .
```

``` r
# Number of observations
n <- 2000

# Number of features
p <- 100

# Create a binary matrix with some correlation
X <- matrix(rbinom(n * p, 1, 0.1), n, p)
# Create column names
colnames(X) <- paste0("var", 1:p)

# Create a continuous outcome based on a logic statement with added noise
Y <- 30 * (X[, 1] | X[, 8]) + 20 * (X[, 34]) + rnorm(n, mean = 0, sd = 0.1)

# Combine into a data frame
df <- data.frame(X, outcome = Y)

# Run LogicHAL
result <- LogicHAL(data = df, outcome = "outcome", columns = colnames(X), max_trees = 2, max_depth = 3, family = "gaussian")

# Print final model and trees used
coef(result$model)
#> 3 x 1 sparse Matrix of class "dgCMatrix"
#>                                                                                 s1
#> (Intercept)                                                               7.479121
#> (var34 == 1) & ((var1 == 1) | (var8 == 1))                               31.174558
#> (var34 == 1) & ((var1 == 1) | (var2 == 1) & ((var1 == 1) | (var8 == 1)))  .
```

``` r
# Number of observations
n <- 2000

# Create binary indicators
binary_indicators <- matrix(rbinom(n * 20, 1, 0.5), n, 20)
colnames(binary_indicators) <- paste0("bin", 1:20)

# Create continuous variables
continuous_variables <- matrix(runif(n * 5, min = 0, max = 100), n, 5)
colnames(continuous_variables) <- paste0("cont", 1:5)

# Combine binary and continuous variables into a data frame
df <- data.frame(binary_indicators, continuous_variables)

columns <- colnames(df)

# Create a continuous outcome based on a logic statement with added noise
# Example logic: outcome is high if bin1 == 1 and cont1 < 50
df$outcome <- 30 * (df$bin1 == 1 & df$cont1 < 50) + rnorm(n, mean = 0, sd = 1)

# Define columns to be used in the model

# Run LogicHAL
result <- LogicHAL(data = df, outcome = "outcome", columns = columns, max_trees = 2, max_depth = 3, family = "gaussian")

# Print final model and trees used
coef(result$model)
#> 3 x 1 sparse Matrix of class "dgCMatrix"
#>                                                                                                            s1
#> (Intercept)                                                                                          1.669449
#> (bin1 == 1) & (cont1_lt_44.74 == 1)                                                                 25.408117
#> (bin1 == 1) & ((bin1 == 1) & (cont4_lt_9.16 == 1) | ((cont1_lt_8.68 == 1) | (cont1_lt_44.74 == 1)))  .
```
