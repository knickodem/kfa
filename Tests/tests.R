###################################
#                                 #
#        kfa package tests        #
#                                 #
###################################

#### Using Dorothy Data ####
schclimate <- haven::read_sav("C:/Users/kylenick/OneDrive - University of North Carolina at Chapel Hill/Dorothy//00 School Climate W1-W4 cleaned/SPSS/sources_school_climate_w1_2021-1-27.sav")
names(schclimate) <- gsub("_W1", "", names(schclimate))

## Possible Scales
StudentInterveneVars <- paste("STUDENTS_INTERVENE", 1:7, sep = "_")
StaffInterveneVars <- paste("STAFF_INTERVENE", 1:7, sep = "_")
YouInterveneVars <- paste("YOU_INTERVENE", 1:7, sep = "_")
SchCom2BullyVars <- paste("SCHOOL_COMMITMENT_BULLYING", 1:6, sep = "_")
SchCom2SHVars <- paste("SCHOOL_COMMITMENT_SEX_HARR", 1:6, sep = "_")
SchCom2MHVars <- paste("SCHOOL_COMMITMENT_MT_HEALTH", 1:6, sep = "_")
SchClimateVars <- paste("POSITIVE_PERCEP_SCHOOL_CLIMATE", 1:11, sep = "_")
SchAggressionVars <- paste("AGGRESSION_PROBLEM_AT_SCHOOL", 1:7, sep = "_")

SchoolScales <- list(StudentInterveneVars, StaffInterveneVars, YouInterveneVars,
                     SchCom2BullyVars, SchCom2SHVars, SchCom2MHVars,
                     SchAggressionVars, SchClimateVars)
names(SchoolScales) <- c("Student_Intervene", "Staff_Intervene", "You_Intervene",
                         "School_Commitment_to_Bullying", "School_Commitment_to_SH",
                         "School_Commitment_to_Mental_Health", "School_Aggression_Problems",
                         "Positive_School_Climate")


set.seed(210106)
tictoc::tic()
kexp2 <- kfa(variables = exposurew2,
             k = NULL,
             m = 4,
             rotation = "oblimin",
             ordered = TRUE,
             estimator = "DWLS",
             missing = "pairwise")
tictoc::toc() # ~ 22 seconds

# Run report
kfa_report(kexp2, report.title = "Exposure Wave 2: K-fold Factor Analysis",
           file.name = "kfa_exposure_W2")

#############################################

#### Import data ####
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

set.seed(936639)
tictoc::tic()
kstudent.par <- temp_kfa(variables = studentdf,
                k = NULL,
                m = 5,
                parallel = FALSE,
                rotation = "oblimin",
                ordered = TRUE,
                estimator = "DWLS",
                missing = "pairwise")
tictoc::toc() # ~ 50 seconds

# Run report
kfa_report(kstudent, file.name = "kfa_students",
           report.format = "html_document",
           report.title = "K-fold Factor Analysis - Lebenon Students")

k <- 5
m <- 5
rotation = "oblimin"
ordered = TRUE
estimator = "DWLS"
missing = "pairwise"
testfolds <- caret::createFolds(y = 1:nrow(studentdf),
                                k = k, list = TRUE,
                                returnTrain = FALSE)

testefa <- mclapply(1:k, function(fold){
  k_efa(training = studentdf[!c(row.names(studentdf) %in% testfolds[[fold]]), ],
        m = 5,
        rotation = rotation,
        ordered = ordered,
        estimator = estimator,
        missing = missing)
})

traincfa <- mclapply(1:k, function(fold){
  k_cfa(efa = testefa[[fold]],
        testset = studentdf[testfolds[[fold]], ],
        ordered = ordered,
        estimator = estimator,
        missing = missing)
})





#### Plotting ####
palette.colors(n = 10, "Set 1")
cut <- .3
graphics::layout
vnames <- dimnames(lavaan::lavInspect(kstudent[[1]][[1]], "sampstat")$cov)[[1]]
plot.settings <- list(what = "std", whatLabels = "no", layout = "tree",
                      intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
                      cut = cut, posCol = c("#BF0000","#000000"), fade = FALSE,
                      # edge.color = , # could create custom function to utilize this argument
                      weighted = TRUE, negDashed = TRUE, esize = 5,
                      manifests = vnames, reorder = FALSE)
kstructures <- model_structure(kstudent)
l <- length(kstructures[[1]][[1]]$folds)
plotmat <- matrix(1:l, nrow = 2)
graphics::layout(plotmat)
for(f in 1:l){
do.call(semPlot::semPaths, args = c(list(object = kstudent[[f]][[1]],
                                         color = list(lat = palette.colors(n = 1 + 1, palette = "Okabe-Ito",
                                                                           recycle = TRUE)[-1]),
                                         title = FALSE),
                                    plot.settings))
title(paste("Fold:", f), adj = 0, line = 0)
}



curiousplot <- semPlot::semPaths(kstudent[[1]][[3]], what = "std", whatLabels = "no",
                                 layout = "tree", negDashed = TRUE,
                  intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
                  color = list(lat = palette.colors(n = 4, palette = "Okabe-Ito",
                                                    recycle = TRUE)[-1]),
                  cut = .3, posCol = c("#BF0000","#000000"), fade = FALSE,
                  # edge.color = , # could create custom function to utilize this argument
                  weighted = TRUE, esize = 5,
                  manifests = vn, reorder = FALSE)

plot.settings <- list(what = "std", whatLabels = "no",
                      layout = "tree", negDashed = TRUE,
                      intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
                      color = list(lat = palette.colors(n = 4, palette = "Okabe-Ito",
                                                        recycle = TRUE)[-1]),
                      cut = .3, posCol = c("#BF0000","#000000"), fade = FALSE,
                      # edge.color = , # could create custom function to utilize this argument
                      weighted = TRUE, esize = 5,
                      manifests = vn, reorder = FALSE)

do.call(semPlot::semPaths, args = c(list(object = kstudent[[1]][[3]]), plot.settings))

## Not sure how to arrange plots into a grid yet
# arranging packages (cowplot, gridExtra) use grob objects rather than qgraph objects
# cowplot::as_grob nor qgraph::as.ggraph currently do the conversion
qgraph::as.ggraph(curiousplot)






tictoc::tic()
set.seed(231220)
kteacher <- kfa(variables = teacherdf,
                k = 5,
                m = 10,
                rotation = "oblimin",
                ordered = TRUE,
                estimator = "DWLS",
                missing = "pairwise")
tictoc::toc() #

# Run report
kfa_report(kteachers, report.title = "K-fold Factor Analysis - Lebenon Teachers",
           file.name = "kfa_teachers")

# tictoc::tic()
# kprincipal <- kfa(variables = principaldf,
#                      k = NULL,
#                      m = 5,
#                      rotation = "oblimin",
#                      ordered = TRUE,
#                      estimator = "DWLS",
#                      missing = "pairwise")
# tictoc::toc() #

tictoc::tic()
kcoach <- kfa(variables = coachdf,
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
