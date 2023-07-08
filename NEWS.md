# kfa 0.2.2

*New features and adjustments:
  * loading flag based on absolute value of loading
  * The degrees of freedom calculation in `find_k` is now based on a CFA model rather than EFA
  * Can toggle plots off when generating report - temporarily changed default to off to limit strong dependency with archived package

# kfa 0.2.1

*New features and adjustments:
  * `data` argument added and `variable` argument re-defined as specifying column names in `data`
  * The `sampling.weights` argument is passed appropriately to `lavaan` and incorporated into the analysis
  * Changed name of `threshold` argument to the more specific name `min.loading`
  * Adjusted order of `custom.cfa` models in flag table to match the order in the model fit table of the report

# kfa 0.2.0

* New features and major adjustments:
  * Users can keep cross-loading variables by setting `simple = FALSE` and specifying a `threshold`
  * Default test statistic and fit indices switched from naive to scaled versions (mean and variance adjusted)
  * In `agg_model_fit`, changed default index to `"all"`
  * `model_structure`, `get_std_loadings`, and `index_available` are exported for use
  * Tables sorted based on order of variables and models provided by user rather than alphabetical
* Bugs and background
  * Deleted Rd files for non-exported functions - silly me :)
  * Fixed rotation error bug in `run_efa`
  * path disassociated with report.title
  * Plot edges are set to black

# kfa 0.1.0

* Initial CRAN submission
