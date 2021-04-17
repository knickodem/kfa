
# kfa: K-Fold Cross Validation For Factor Analysis

**kfa** provides utilities for examining the dimensionality of a set of
variables to foster scale and instrument development. Harnessing a
k-fold cross validation approach, **kfa** helps researchers compare
possible factor structures and identify which structures are plausible
and replicable across samples.

## Installation

``` r
install.packages("remotes")
remotes::install_github("knickodem/kfa")
library(kfa)
```

## Workflow

There are two primary functions are `kfa()` and `kfa_report()`. When the
set of potential variables and (optionally) the maximum number of
factors, *m*, are supplied to `kfa()`, the function:

  - (if requested) conducts a power analysis to determine the number of
    folds, *k*, on which to split the data
  - creates *k* folds (i.e. training and testing samples).

Then for each fold:

  - calculates sample statistics (e.g., correlation matrix, thresholds
    \[if necessary\]) from training sample.
  - runs 2:*m* factor exploratory factor analysis (EFA) models using the
    sample statistics, applies rotation (if specified), and extracts the
    factor structure for a confirmatory factor analysis (CFA). The
    structure for a 1-factor CFA is also defined.
  - runs the 1:*m* factor CFA models on the testing sample.

The factor analyses are run using the `lavaan` package with many of the
`lavaan` estimation and missing data options available for use in
`kfa()`. `kfa()` returns a list of lists with *k* outer elements for
each fold and *m* inner elements, each containing a `lavaan` object. To
expedite running *k* x *m* x 2 (EFA and CFA) models, the function
utilizes the `parallel` and `foreach` packages for parallel processing.

``` r
library(kfa)
data("example")

set.seed(1161) # set seed to obtain the same folds for replication
mods <- kfa(variables = example,
                  k = NULL) # prompts power analysis to determine number of folds
```

`kfa_report()` then aggregates the model fit, parameter estimates, and
model-based reliability across folds for each factor structure extracted
in `kfa()`. The results are then organized and exported via `rmarkdown`.

``` r
# Run report
kfa_report(mods, file.name = "example_kfa_report",
           report.format = "html_document",
           report.title = "K-fold Factor Analysis - Example")
```

## Under Development and Consideration

  - **Item Stability Index** - Akin to using Rubin’s rules to pool
    parameter estimates over multiple imputations, future versions will
    summarize the stability of the loading for each item across folds
    for a given factor structure.

  - **Overfitting Index** - In order to understand the replicability of
    a factor structure across folds, we are considering approaches to
    measure and summarize the degree to which a model is overfitting the
    data within a fold.

  - **Clustered Data** - The package does not currently account for
    clustered data. Future versions will utilize the cluster argument
    from `lavaan` to estimate cluster robust standard errors when
    calculating the correlation matrix for the factor analyses. We are
    also considering how to account for nesting structures in the
    creation of the folds, which are currently created assuming a simple
    random sample. If so, we will also incorporate cluster adjustments
    for the power analysis determining the value of *k*.
