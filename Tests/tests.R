###################################
#                                 #
#        kfa package tests        #
#                                 #
###################################

## Import data
shortfile <- c("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/EFA&CFA/")


student <- read.csv(paste0(shortfile, "scaledim/student_survey-latest.csv"))
teacher <- read.csv(paste0(shortfile, "scaledim/teacher_survey-latest.csv"))
principal <- read.csv(paste0(shortfile, "scaledim/principal_survey-latest.csv"))
coach <- read.csv(paste0(shortfile, "scaledim/coach_survey-latest.csv"))

## item map
itemmaps <- lapply(c("Coach_survey", "Teacher Survey",
                     "Principal Survey", "Student Survey"), function(x){
                   readxl::read_excel(paste0(shortfile, "Item construct map_2020_10_25.xlsx"),
                                      sheet = x)})
names(itemmaps) <- c("coach", "teacher", "principal", "student")

## items for analysis
studentdf <- student[ ,names(student) %in% na.omit(itemmaps$student$`Variable Name`)]
teacherdf <- teacher[ ,names(teacher) %in% na.omit(itemmaps$teacher$`Variable Name`)]
principaldf <- principal[ ,names(principal) %in% na.omit(itemmaps$principal$`Variable Name`)]
coachdf <- coach[ ,names(coach) %in% na.omit(itemmaps$coach$`Variable Name`)]
studentdf2 <- studentdf[!(names(studentdf) %in% c("a1118x", "a1120x", "a1121x"))]



# ---- test full kfold_fa function ----------------

## set seed to get the same folds
set.seed(936639)
tictoc::tic()
kstudent <- kfa(variables = studentdf,
                  k = NULL,
                  m = 5,
                  rotation = "oblimin",
                  ordered = TRUE,
                  estimator = "DWLS",
                  missing = "pairwise")
tictoc::toc() # ~ 100 seconds

# Run report
kfa_report(kstudent, report.title = "K-fold Factor Analysis - Lebenon Students",
           file.name = "kfa_students")


#### Model Fit ####
k <- 10
kfits <- vector("list", length = k)
for(f in 1:k){

  fits <- lapply(kstudent[[f]], function(x) {
    lavaan::fitmeasures(x, c("chisq", "df", "cfi",
                             "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
  })

  fits.df <- cbind(data.frame(factors = as.character(1:length(fits))),
                   as.data.frame(Reduce(rbind, fits)))
  row.names(fits.df) <- NULL

  kfits[[f]] <- fits.df
}

bdf <- as.data.frame(Reduce(rbind, kfits))
aggfit <- data.frame(factors = 1:4,
                       mean = tapply(bdf$rmsea, bdf$factors, mean),
                       min = tapply(bdf$rmsea, bdf$factors, min),
                       max = tapply(bdf$rmsea, bdf$factors, max))

lowmod <- lapply(kfits, function(x) x[x$rmsea == min(x$rmsea),])
lowmod.df <- as.data.frame(Reduce(rbind,lowmod))
lowmod <- as.data.frame(table(lowmod.df$factors))
names(lowmod) <- c("factors", "low_in_fold")


merge(x = aggfit, y = lowmod, by = "factors", all.x = TRUE)


#### Model Structure ####
# All items are included in the 1 factor model; only differences in loadings will occur across folds
structures <- vector("list", length = k)
for(s in 1:k){
  structures[[s]] <- efa_cfa_syntax(lavaan::lavInspect(kstudent[[1]][[2]], "est")$lambda)
}
structuredf <- cbind(data.frame(fold = 1:10),
                     as.data.frame(Reduce(rbind, structures)))

unique.structures <- unique(structuredf$V1)

test <- lapply(unique.structures, function(x) structuredf[structuredf$V1 == x,]$fold)

alllambda <- vector("list", 4)
for(m in 1:4){
  lambdas <- data.frame()
  for(s in 1:k){
    loads <- subset(lavaan::standardizedSolution(kstudent[[s]][[m]], "std.lv",
                                                 se = FALSE, zstat = FALSE,
                                                 pvalue = FALSE, ci = FALSE),
                    op == "=~")
    # loads$factors <- m
    # loads$fold <- s
    lambdas <- rbind(lambdas, loads)
  }

  agglambdas <- data.frame(variable = vn,
                           mean = tapply(lambdas$est.std, lambdas$rhs, mean),
                           min = tapply(lambdas$est.std, lambdas$rhs, min),
                           max = tapply(lambdas$est.std, lambdas$rhs, max))
  row.names(agglambdas) <- NULL
  alllambda[[m]] <- agglambdas
}

# could do this within structure so the output of the inner loop is a df and output of outer loop is list of dfs

alllvcor <- vector("list", 3)
for(m in 2:4){
  cor.lv <- vector("list", k)
  for(s in 1:k){
    cor.lv[[s]] <- lavaan::lavInspect(kstudent[[s]][[m]], "cor.lv")
  }

  alllvcor[[m]] <- Reduce(`+`, cor.lv) / length(cor.lv)

}

cortest <- vector("list", k)
for(s in 1:k){
  cortest[[s]] <- lavaan::lavInspect(kstudent[[s]][[2]], "cor.lv")
}



for(i in 1){
  lapply(kstudent, function(x){
    semPlot::semPaths(x[[i]], what = "std", whatLabels = "no",
                      intercepts = FALSE, residuals = FALSE,
                      thresholds = FALSE, reorder = FALSE)
    })
}

vn <- dimnames(lavaan::lavInspect(kstudent[[1]][[1]], "sampstat")$cov)[[1]]
semPlot::semPaths(kstudent[[1]][[3]], what = "std", whatLabels = "no",
                  intercepts = FALSE, residuals = FALSE,
                  thresholds = FALSE, reorder = FALSE,
                  manifests = vn)

flextest <- kfa::flextab_format(kfits[[1]])

dimnames(lavaan::lavInspect(kstudent[[1]][[1]], "sampstat")$cov)[[1]]

lavaan::summary(kstudent[[1]][[1]], fit.measures = TRUE)

tictoc::tic()
kteacher <- kfold_fa(variables = teacherdf,
                     k = NULL,
                     m = 10,
                     rotation = "oblimin",
                     ordered = TRUE,
                     estimator = "DWLS",
                     missing = "pairwise")
tictoc::toc() #

# tictoc::tic()
# kprincipal <- kfold_fa(variables = principaldf,
#                      k = NULL,
#                      m = 5,
#                      rotation = "oblimin",
#                      ordered = TRUE,
#                      estimator = "DWLS",
#                      missing = "pairwise")
# tictoc::toc() #

tictoc::tic()
kcoach <- kfold_fa(variables = coachdf,
                     k = NULL,
                     m = 10,
                     rotation = "oblimin",
                     ordered = TRUE,
                     estimator = "DWLS",
                     missing = "pairwise")
tictoc::toc() #

##############################################

# ----  Power analysis ---------------

df <- ((62 - 5)^2 - (62 + 5)) / 2
semTools::findRMSEAsamplesize(rmsea0 = .05, rmseaA = .08, df = df)

findRMSEAsamplesizenested(rmsea0A = .05, rmsea0B = NULL, rmsea1A,
                          rmsea1B = NULL, dfA, dfB, power = 0.8, alpha = 0.05, group = 1)

# ---- Using correlation matrix in lavaan -----------------
tictoc::tic()
kefa <- k_efa(variables = items,
              m = 5,
              rotation = "oblimin",
              ordered = names(items),
              estimator = "DWLS",
              missing = "pairwise")
tictoc::toc() # ~10 sec

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
                         std.err = FALSE),
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




efa.mod <- write_efa(3, names(items))

modtest <- lavaan::cfa(model = efa.mod,
                       sample.cov = cormat,
                       sample.nobs = nobs,
                       sample.mean = means,
                       sample.th = th,
                       WLS.V = wlsv,
                       NACOV = nacov,
                       std.lv = TRUE,
                       orthogonal = TRUE,
                       estimator = "DWLS",
                       parameterization = "delta",
                       se = "none")


lavaan::summary(modtest, standardized = TRUE)
modtestload <- lavaan::lavInspect(modtest, "est")$lambda


fatest <- psych::fa(cormat, 3, n.obs = nobs, rotate = "none")


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
