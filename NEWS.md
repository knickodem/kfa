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
