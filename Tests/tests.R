###################################
#                                 #
#        kfa package tests        #
#                                 #
###################################

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


# ---- test full kfold_fa function ----------------

## custom factor structure (just an example, not based on theory)
custom2 <- paste0("f1 =~ ", paste(names(studentdf)[1:10], collapse = " + "),
                 "\nf2 =~ ",paste(names(studentdf)[11:21], collapse = " + "))
custom3 <- paste0("f1 =~ ", paste(names(studentdf)[1:19], collapse = " + "),
                  "\nf2 =~ ",paste(names(studentdf)[20], collapse = " + "),
                  "\nf3 =~ ",paste(names(studentdf)[21], collapse = " + "))

## set seed to get the same folds
set.seed(936639)
tictoc::tic()
kstudent <- kfa(variables = studentdf,
                k = NULL,
                m = 5,
                custom.cfas = list(`Custom 2f` = custom2, Break = custom3),
                rotation = "oblimin",
                ordered = TRUE,
                estimator = "DWLS",
                missing = "pairwise")
tictoc::toc() # ~ 60 seconds

kstructures <- model_structure(kstudent, which = "cfa")

library(tidySEM)
tidySEM::graph_sem(kstudent$cfas[[2]]$`2-factor`)


k <- length(kstudent$cfas)
m <- max(unlist(lapply(kstudent$cfas, length)))


kfit <- k_model_fit(kstudent)
mfit <- k_model_fit(kstudent, by.fold = FALSE)
agg_model_fit(kfit)
appendix <- get_appendix(mfit)
index = c("chisq", "cfi", "rmsea")

model.names <- names(kstudent$cfas[[1]])

length(index)*length(model.names) > 16
modsplit <- floor(16/length(index))
colsplit <- 1+length(index)*modsplit
appx1 <- appx[1:colsplit]
appx2 <- appx[c(1, (colsplit+1):length(appx))]

appendix1.map <- data.frame(col_keys = names(appx1),
                           top = c("fold", rep(model.names[1:modsplit], each = length(index))),
                           bottom = c("fold", rep(index, times = modsplit)))

appendix2.map <- data.frame(col_keys = names(appx2),
                            top = c("fold", rep(model.names[(modsplit+1):length(model.names)], each = length(index))),
                            bottom = c("fold", rep(index, times = length(model.names) - modsplit)))


appendix1.flex <- flextable::flextable(appx1)
appendix1.flex <- flextable::colformat_double(appendix1.flex, j = -c(1), digits = 2)
appendix1.flex <- two_level_flex(appendix1.flex, mapping = appendix1.map, vert.cols = c("fold"), border = officer::fp_border(width = 2))



tictoc::tic()
ld <- agg_loadings(kstudent)
tictoc::toc() #.16

tictoc::tic()
cr <- agg_cors(kstudent)
tictoc::toc() #.01

tictoc::tic()
rl <- agg_rels(kstudent)
tictoc::toc() #74.9

tictoc::tic()
str(model_flags(kstudent, cr, rl, ld))
tictoc::toc() #.11


# Run report
kfa_report(kstudent, file.name = "kfa_students_custom",
           report.format = "html_document",
           report.title = "K-fold Factor Analysis - Lebenon Students")




# -----  README Test --------
library(lavaan)
data("HolzingerSwineford1939")


set.seed(1161)
tictoc::tic()
kstudent <- kfa(variables = HolzingerSwineford1939[7:15],
                k = NULL,
                m = 4,
                rotation = "oblimin")
tictoc::toc()

findk(HolzingerSwineford1939[7:15], 1)

#########################################

k <- 5
m <- 5
rotation = "oblimin"
ordered = TRUE
estimator = "DWLS"
missing = "pairwise"
testfolds <- caret::createFolds(y = 1:nrow(studentdf),
                                k = k, list = TRUE,
                                returnTrain = FALSE)

testefa <- lapply(1:k, function(fold){
  k_efa(variables = studentdf[!c(row.names(studentdf) %in% testfolds[[fold]]), ],
        m = 5,
        rotation = rotation,
        ordered = ordered,
        estimator = estimator,
        missing = missing)
})


set.seed(936639)
testefa <- kfa(variables = studentdf,
                k = NULL,
                m = 5,
               efa.only = TRUE,
                rotation = "oblimin",
                ordered = TRUE,
                estimator = "DWLS",
                missing = "pairwise")

efa.structures <- model_structure(testefa, which = "efa") # present in appendix

# collect most common structure for each factor model to use in CFAs
cfa.structures <- vector("list", length = 5)
for(n in 1:m){
  if(length(all.structures[[n]]) == 1){
    cfa.structures[[n]] <- all.structures[[n]][[1]]$structure
  } else {
    # number of folds each structure was found; keep most common structure
    num.folds <- unlist(lapply(all.structures[[n]], function(x) length(x$folds)))
    cfa.structures[[n]] <- all.structures[[n]][[which(num.folds == max(num.folds))]]$structure
  }
}

names(cfa.structures) <- as.character(1:m)




k <- length(testefa)
m <- max(unlist(lapply(testefa, length)))



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






