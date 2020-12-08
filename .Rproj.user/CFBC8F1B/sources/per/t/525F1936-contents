#### kfa package tests ####

## Import data
student <- read.csv("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/EFA&CFA/scaledim/student_survey-latest.csv")
# extract items
items <- student[ ,grepl("^a11", names(student))]


# ---- Using correlation matrix in lavaan -----------------
test <- as.data.frame(lapply(items, as.ordered))

microbenchmark::microbenchmark(
  ## polychoric correlation matrix
  prelim = lavaan::lavCor(object = items,
                          ordered = names(items),
                          estimator = "DWLS",  # must specify estimator, otherwise defaults to none
                          missing = "pairwise",
                          output = "fit",
                          cor.smooth = FALSE), # could include a cluster =  argument
  poly = polycor::hetcor(data = as.data.frame(lapply(items,as.ordered)),
                         use = "pairwise.complete.obs",
                         std.err = FALSE), # could include a cluster =  argument
  times = 20)

prelim <- lavaan::lavCor(object = items,
                        ordered = names(items),
                        estimator = "DWLS",  # must specify estimator, otherwise defaults to none
                        missing = "pairwise",
                        meanstructure = TRUE,
                        output = "fit",
                        cor.smooth = FALSE)


start <- lavaan::lavInspect(prelim, "start") #
itrtns <- lavaan::lavInspect(prelim, "iterations")

nobs <- lavaan::lavInspect(prelim, "nobs")
wlsv <- lavaan::lavInspect(prelim, "wls.v")
nacov <- lavaan::lavInspect(prelim, "gamma")
cormat <- lavaan::lavInspect(prelim, "sampstat")$cov # same as lavaan::lavInspect(prelim, "cor.ov")
means <- lavaan::lavInspect(prelim, "sampstat")$mean
th <- lavaan::lavInspect(prelim, "sampstat")$th # same values as lavaan::lavInspect(prelim, "est")$tau
attr(th, "th.idx") <- lavaan::lavInspect(prelim, "th.idx")


## fit efa with sample statistics
matonly <- lavaan::cfa(model = st.mod,
                       sample.cov = cormat,
                       sample.nobs = nobs,
                       sample.mean = means,
                       sample.th = th,
                       WLS.V = wlsv,
                       NACOV = nacov,
                       estimator = "DWLS",
                       parameterization = "delta",
                       se = "none")

## fit with full dataset
dat <- lavaan::cfa(model = st.mod,
                  data = items,
                  estimator = "DWLS",
                  missing = "pairwise",
                  ordered = names(items),
                  parameterization = "delta",
                  se = "none")

## Do the matrix/sample stats and data method produce the same results
all.equal(lavaan::parameterestimates(matonly, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE),
          lavaan::parameterestimates(dat, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE))

# check <- dplyr::full_join(lavaan::parameterestimates(matonly, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE),
#           lavaan::parameterestimates(dat, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE),
#           by = c("lhs", "op", "rhs", "label"), suffix = c(".mat", ".dat"))



#### Comparing time difference when cor matrix is provided or not ####
# expr       min       lq     mean   median       uq      max neval
# mat  7.232165 17.84711 17.84960 18.65208 19.65406 29.44586    20
# dat 13.979109 32.33991 32.04155 34.46950 36.70216 51.01137    20

inputtype <- microbenchmark::microbenchmark(
  mat = lavaan::cfa(model = st.mod,
                    sample.cov = cormat,
                    sample.nobs = nobs,
                    sample.mean = means,
                    sample.th = th,
                    WLS.V = wlsv,
                    NACOV = nacov,
                    estimator = "DWLS",
                    parameterization = "delta",
                    se = "none",
                    test = "none"),
  dat = lavaan::cfa(model = st.mod,
                    data = items,
                    estimator = "DWLS",
                    missing = "pairwise",
                    ordered = names(items),
                    parameterization = "delta",
                    se = "none",
                    test = "none"),
  times = 20

)

######################################


# ---- Comparing semTools to direct use in lavaan ---------------------

## Notes:
# - direct approach uses the model produced by semTools
# - parameterization = "delta" produces 0s for all loadings for semTools call
#   - https://groups.google.com/g/lavaan/c/ujkHmCVirEY/m/-LGut4ewAwAJ
#   - delta works when lavaan called directly
# - data argument is specified even when sample.cov and sample.nobs are provided
#   otherwise get error (see above)

## semTools with previously calculated polychoric correlation matrix
st.mat = semTools::efaUnrotate(data = items,
                               nf = 3,
                               start = FALSE,
                               sample.cov = cormat,
                               sample.nobs = nobs,
                               ordered = names(items),
                               parameterization = "theta",
                               se = "none",
                               test = "none")
st.mod <- lavaan::lavInspect(st.mat, "call")$model

## semTools with only raw data input
st.dat = semTools::efaUnrotate(data = items,
                               nf = 3,
                               start = FALSE,
                               ordered = names(items),
                               parameterization = "theta",
                               se = "none",
                               test = "none")



## direct with polychoric correlation
lv.mat <- lavaan::cfa(model = st.mod,
                     data = items,
                     sample.cov = cormat,
                     sample.nobs = nobs,
                     ordered = names(items),
                     parameterization = "theta",
                     se = "none",
                     test = "none")

## direct with only raw data input
lv.dat <- lavaan::cfa(model = st.mod,
                     data = items,
                     ordered = names(items),
                     parameterization = "theta",
                     se = "none",
                     test = "none")

## comparing model features and results
get_std_loadings(st.mat) # in k_efa.R
get_std_loadings(st.dat)
get_std_loadings(lv.mat)
get_std_loadings(lv.dat)

lapply(list(st.mat, st.dat, lv.mat, lv.dat),
       function(x) lavaan::lavInspect(x, "categorical"))
lapply(list(st.mat, st.dat, lv.mat, lv.dat),
       function(x) lavaan::lavInspect(x, "parameterization"))
lapply(list(st.mat, st.dat, lv.mat, lv.dat),
       function(x) lavaan::lavInspect(x, "iterations")) # delta takes fewer
lapply(list(st.mat, st.dat, lv.mat, lv.dat),
       function(x) lavaan::lavInspect(x, "options")$missing)
lapply(list(st.mat, st.dat, lv.mat, lv.dat),
       function(x) lavaan::lavInspect(x, "nobs"))
# listwise deletion used even when the pairwise nobs was specified
lapply(list(st.mat, st.dat, lv.mat, lv.dat),
       function(x) lavaan::lavInspect(x, "est"))

lapply(list(st.mat, st.dat, lv.mat, lv.dat), lavaan::parTable)

# Are all specified options the same
all.equal(lavaan::lavInspect(lv.mat, "options"), lavaan::lavInspect(lv.dat, "options"))
all.equal(lavaan::lavInspect(lv.mat, "options"), lavaan::lavInspect(st.mat, "options"))

# Threshold estimates
all.equal(lavaan::lavInspect(lv.mat, "est")$tau, lavaan::lavInspect(lv.dat, "est")$tau) # yes
all.equal(lavaan::lavInspect(lv.mat, "est")$tau, lavaan::lavInspect(st.mat, "est")$tau) # yes
all.equal(lavaan::lavInspect(lv.mat, "sampstat")$th, th) # yes


# ----- Comparing model defined by semTools to model defined by regsem ------------
## Model from regsem
rs.mod <- regsem::efaModel(3, names(items))
rs <- lavaan::cfa(model = rs.mod,
                  sample.cov = cormat,
                  sample.nobs = nobs,
                  sample.mean = means,
                  sample.th = th,
                  WLS.V = wlsv,
                  NACOV = nacov,
                  estimator = "DWLS",
                  parameterization = "delta",
                  se = "none",
                  test = "none")

## Do the regsem and semTools syntax produce the same results
all.equal(get_std_loadings(matonly), get_std_loadings(rs)) # nope; loadings are the same on each factor for rs
lapply(list(matonly, rs),function(x) lavaan::lavInspect(x, "iterations"))

pfa <- psych::fa(r = cormat, nfactors = 3, n.obs = nobs, rotate = "none", covar = FALSE, fm = "wls")

## rs.mod is substantially fastor
# expr      min       lq      mean   median       uq       max neval
# rs 1.017891 1.060982  1.426087 1.121872  1.39415  2.671816    20
# st 7.143214 7.405637 10.612499 7.824584 16.39990 18.224163    20

rs.v.st <- microbenchmark::microbenchmark(
  rs = lavaan::cfa(model = rs.mod,
                   sample.cov = cormat,
                   sample.nobs = nobs,
                   sample.mean = means,
                   sample.th = th,
                   WLS.V = wlsv,
                   NACOV = nacov,
                   estimator = "DWLS",
                   parameterization = "delta",
                   se = "none",
                   test = "none"),
  st = lavaan::cfa(model = st.mod,
                   sample.cov = cormat,
                   sample.nobs = nobs,
                   sample.mean = means,
                   sample.th = th,
                   WLS.V = wlsv,
                   NACOV = nacov,
                   estimator = "DWLS",
                   parameterization = "delta",
                   se = "none",
                   test = "none"),

  times = 20)


# Questions:
# - Does regsem produce same loadings as semtools? No. Do either match Mplus?
#   - If yes, what is the speed difference? regsem is considerably faster
# - What exactly do I need to return from the lavCor call? TBD


## lawley-rao syntax - not done yet
write_efa <- function(nf, itemnames){
  factors <- paste0("f", 1:nf)
  loadings <- paste(itemnames, collapse = " + ")
  syntax <- paste(lapply(factors, function(x) paste0(x, " =~ ", loadings)), collapse = "\n")
  syntax

}

lr <- write_efa(3, names(items))
lr.dat <- lavaan::lavaan(model = test,
                           data = items,
                           auto.efa = TRUE,
                           ordered = names(items),
                           estimator = "DWLS",
                           missing = "pairwise",
                           parameterization = "theta",
                           orthogonal = TRUE,
                           se = "none")

lavaan::lavInspect(test.dat, "partable")


# write_efa <- function(nf, varNames){
#
# syntax <- character(0)
# for (i in seq_along(varNames)) {
#   syntax <- c(syntax, paste0("fac_", i, " =~ ", paste(varNames[i:length(varNames)], collapse = " + ")))
#   if (i == nf) break
# }
# syntax
# }




## set seed to get the same folds
set.seed(936639)

## defaults
# rotation = "oblimin"
# k = NULL
# rmsea0 = .05
# rmseaA = .08
# m = floor(ncol(items) / 4)
# threshold = NA # might not make threshold an argument
# ordered = FALSE
# missing = "listwise"

tictoc::tic()
thesyntax <- kfold_fa(items = items, extract.method = "lrt",
                      rotation = "oblimin", k = NULL,
                      ordered = TRUE, missing = "pairwise")

tictoc::toc()



#### part of kfold_fa ####

if(ordered == TRUE){
  ordered <- names(items)
} else if(ordered == FALSE){
  ordered <- NULL
}

if(is.null(k)){

  ## determine number of folds based on power analysis
  k <- findk(p = items, m = m, rmsea0 = rmsea0, rmseaA = rmseaA)
}

trainfolds <- caret::createFolds(y = 1:nrow(items),
                                 k = k, list = TRUE,
                                 returnTrain = TRUE)

######

## Testing standalone efa function
efatest <- run_efa(items = items[trainfolds[[1]], ],
                         rotation = "oblimin", m = 5,
                         simple = TRUE, threshold = NA,
                         ordered = TRUE, missing = "pairwise")
