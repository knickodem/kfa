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
custom3 <- paste0("f1 =~ ", paste(names(studentdf)[1:2], collapse = " + "),
                  "\nf2 =~ ",paste(names(studentdf)[3:4], collapse = " + "),
                  "\nf3 =~ ",paste(names(studentdf)[5:6], collapse = " + "),
                  "\nf4 =~ ",paste(names(studentdf)[7:8], collapse = " + "),
                  "\nf5 =~ ",paste(names(studentdf)[9:10], collapse = " + "),
                  "\nf6 =~ ",paste(names(studentdf)[11:12], collapse = " + "),
                  "\nf7 =~ ",paste(names(studentdf)[13:21], collapse = " + "),
                  "\na1101x ~~ ", paste(names(studentdf)[2:21], collapse = " + "),
                  "\na1102x ~~ ", paste(names(studentdf)[3:21], collapse = " + "))

# When custom model does not contain all items, the following error is currently produced:
# Error in { : task 1 failed - "invalid 'times' argument"
# set.seed(936639) # set seed to reproduce same folds


tictoc::tic()

kstudent <- kfa(variables = studentdf,
                k = NULL,
                m = 5,
                seed = 936639,
               custom.cfas = list(`Custom 2f` = custom2, Break = custom3),
                rotation = "oblimin",
                ordered = TRUE,
                estimator = "DWLS",
                missing = "pairwise")
tictoc::toc()# ~ 60 seconds

lavaan::lavInspect(kstudent$cfas[[1]]$`1-factor`, "options")$estimator
lavaan::lavInspect(kstudent$cfas[[1]]$`1-factor`, "options")$missing

kfit <- k_model_fit(kstudent)
mfit <- k_model_fit(kstudent, by.fold = FALSE)

# kfa.time <- tictoc::tic.log()

agg_model_fit(kfit)
appendix <- get_appendix(mfit)

strux <- model_structure(kstudent, which = "cfa")

tictoc::tic()
loads <- agg_loadings(kstudent)
tictoc::toc() #.16

tictoc::tic()
cors <- agg_cors(kstudent)
tictoc::toc() #.04

tictoc::tic()
rels <- agg_rels(kstudent)
tictoc::toc() #74.9

tictoc::tic()
model_flags(kstudent, cors, rels, loads)
tictoc::toc() #.11


# Run report
kfa_report(kstudent,
           file.name = "kfa_students_test",
           report.title = "K-fold Factor Analysis - Lebanon Students")

# report.format = "html_document",


# library(tidySEM)
tidySEM::graph_sem(kstudent$cfas[[2]]$Break)


#####################################################


# -----  README Test --------
# library(lavaan)
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


# --------- Power Plots -------------------

# semTools::findRMSEApower ...
# calculates non-centrality parameter for null and alternative RMSEA: ((n - 1) * df * rmsea^2)
# then the critical value of the chisquare distribution for a given alpha, df, and ncp via qchisq()
# then calculates probability of critical value or higher for chisquare distribution for rmseaA with a given alpha, df, and ncp via pchisq()
# df is a product of p and m. n only influences the ncp. Thus, p and m will have a bigger influence on power
ncp0 <- ((10 - 1) * df * .05^2)
qchisq(.05, df, ncp = ncp0, lower.tail = FALSE)

## vary power by number of items (p), number of factors (m), and sample size (n)
grid <- expand.grid(p = seq(5, 50, 5), m = seq(1, 6, 1), n = seq(10, 1000, 50))
grid <- dplyr::mutate(.data = grid,
                      df = ((p - m)^2 - (p + m)) / 2,
                      power = semTools::findRMSEApower(rmsea0 = .05, rmseaA = .08, alpha = .05, df = df, n = n))
grid <- grid[!is.na(grid$power),] # remove rows with negative degrees of freedom

# library(ggplot2)
# library(magrittr)
grid %>%
  dplyr::filter(p < 45 & n <= 500) %>%
  dplyr::mutate(m = factor(m)) %>%
  ggplot(., aes(x = n, y = power, group = m, colour = m)) +
  geom_line(size = 1.5) +
  labs(colour = "factors") +
  theme_bw(base_size = 18) +
  facet_wrap(~p, nrow = 2)
# facet_grid(p ~ m)

s.grid <- expand.grid(p = seq(5, 50, 5), m = seq(1, 6, 1))
s.grid$df <- ((s.grid$p - s.grid$m)^2 - (s.grid$p + s.grid$m)) / 2
s.grid <- s.grid[s.grid$df > 0,] # remove rows with non-positive degrees of freedom
s.grid <- dplyr::rowwise(s.grid) %>%
  dplyr::mutate(n = semTools::findRMSEAsamplesize(rmsea0 = .05, rmseaA = .08, df = df, power = .80, alpha = .05))


s.grid %>%
  dplyr::filter(n <= 2000) %>%
  dplyr::mutate(m = factor(m)) %>%
  ggplot(., aes(x = p, y = n, group = m, colour = m)) +
  geom_line(size = 1.5) +
  scale_y_continuous(breaks = seq(0, 1500, 100)) +
  scale_x_continuous(name = "# of variables", breaks = seq(5, 50, 5)) +
  labs(colour = "factors") +
  theme_bw(base_size = 18)
# facet_wrap(~p, nrow = 2)


p <- 30
m <- 6
df <- ((p - m)^2 - (p + m)) / 2
semTools::findRMSEAsamplesize(rmsea0 = .05, rmseaA = .08, df = df, power = .80, alpha = .05)


######################################################




variables <- studentdf
k <- 5
m <- 5
rotation = "oblimin"
ordered = TRUE
estimator = "DWLS"
missing = "pairwise"
custom.cfas <- list(custom2, custom3)

lavaan.args <- c(list(rotation = rotation, ordered = ordered,
                      estimator = estimator, missing = missing)) #, list(...)


testfolds <- caret::createFolds(y = 1:nrow(variables),
                                k = k, list = TRUE,
                                returnTrain = FALSE)

testefa <- lapply(1:k, function(fold){
  do.call(k_efa, args = c(list(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
                               m = m),
                          lavaan.args))
  # k_efa(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
  #       m = m,
  #       rotation = rotation,
  #       ordered = ordered,
  #       estimator = estimator,
  #       missing = missing)
})

# formatting for use in model_structure
temp <- list(cfas = NULL,
             efas = testefa)

# identifying unique structure
efa.structures <- model_structure(temp, which = "efa")
# names(efa.structures) <- paste0(1:m, "-factor")



# # collect most common structure for each factor model to use in CFAs
# mode.structure <- vector("list", length = m)
# for(n in 1:m){
#   if(length(efa.structures[[n]]) == 1){
#     mode.structure[[n]] <- efa.structures[[n]][[1]]$structure
#   } else {
#     # number of folds each structure was found; keep most common structure
#     num.folds <- unlist(lapply(efa.structures[[n]], function(x) length(x$folds)))
#     ties <- which(num.folds == max(num.folds))
#     ties <- ifelse(length(ties) == 1, ties, ties[[1]]) # in case of ties, use first structure
#     mode.structure[[n]] <- efa.structures[[n]][[ties]]$structure
#   }
# }
# names(mode.structure) <- paste0(1:m, "-factor")

mode.structure <- rep(list(rep(list(""), m)), k) # creates list of lists framework needed for running cfas
for(n in 1:m){  # replaces "" in the sublists with most common (mode) structure when appropriate given the values in x$folds
  if(length(efa.structures[[n]]) == 1){ # if there is only 1 structure of a given model
    for(i in efa.structures[[n]][[1]]$folds){
      mode.structure[[i]][[n]] <- efa.structures[[n]][[1]]$structure
    }
  } else {
    # number of folds each structure was found; keep most common structure
    num.folds <- unlist(lapply(efa.structures[[n]], function(x) length(x$folds)))
    ties <- which(num.folds == max(num.folds))
    ties <- ifelse(length(ties) == 1, ties, ties[[1]]) # in case of ties, use first structure
    for(i in efa.structures[[n]][[ties]]$folds){
      mode.structure[[i]][[n]] <- efa.structures[[n]][[ties]]$structure
    }
  }
}

## add names to models for each fold
mode.structure <- lapply(mode.structure, function(x) {
  names(x) <- paste0(1:m, "-factor")
  return(x)
})



if(!is.null(custom.cfas)){
  if(class(custom.cfas) != "list"){ # converting single object to named list
    custom.name <- deparse(substitute(custom.cfas))
    custom.cfas <- list(custom.cfas)
    names(custom.cfas) <- custom.name
  } else if(is.null(names(custom.cfas))){ # (if necessary) adding names to list
    names(custom.cfas) <- paste("custom", LETTERS[1:length(custom.cfas)])
  }
  # add custom models to set of models for each fold
  cfa.syntax <- lapply(mode.structure, function(x) c(x, custom.cfas))
} else {
  cfa.syntax <- mode.structure
}


## Run CFAs
cfas <- vector("list", length = k)
for(fold in 1:k) {

  # do.call(k_cfa, args = c(list(syntax = cfa.syntax[[fold]],
  #                              variables = variables[testfolds[[fold]], ]),
  #                         lavaan.args))
  cfas[[fold]] <- k_cfa(syntax = cfa.syntax[[fold]],
        variables = variables[testfolds[[fold]], ],
        ordered = ordered,
        estimator = estimator,
        missing = missing)

}

## calculate and extract sample statistics for test sample
sampstats <- lavaan::lavCor(object = variables[testfolds[[3]], ],
                            ordered = ordered,
                            estimator = estimator,
                            missing = missing,
                            output = "fit",
                            cor.smooth = FALSE)

sample.nobs <- lavaan::lavInspect(sampstats, "nobs")
sample.cov <- lavaan::lavInspect(sampstats, "sampstat")$cov
sample.mean <- lavaan::lavInspect(sampstats, "sampstat")$mean
sample.th <- lavaan::lavInspect(sampstats, "sampstat")$th
attr(sample.th, "th.idx") <- lavaan::lavInspect(sampstats, "th.idx")
WLS.V <- lavaan::lavInspect(sampstats, "wls.v")
NACOV <- lavaan::lavInspect(sampstats, "gamma")


## run CFAs
syntax <- cfa.syntax[[3]]
cfa <- vector(mode = "list", length = length(syntax))

for(c in 1:length(syntax)){

  if(nchar(syntax[[c]]) > 0){

    fit <- lavaan::cfa(model = syntax[[c]],
                       sample.cov = sample.cov,
                       sample.nobs = sample.nobs,
                       sample.mean = sample.mean,
                       sample.th = sample.th,
                       WLS.V = WLS.V,
                       NACOV = NACOV,
                       estimator = estimator,
                       parameterization = "delta")

    cfa[[c]] <- fit

  } else {

    cfa[[c]] <- NULL
  }

}

# If largest model is NULL, cfa will be one element shorter than names(syntax)
if(nchar(syntax[[length(syntax)]]) > 0){
  names(cfa) <- names(syntax)
} else{
  names(cfa) <- names(syntax)[-length(syntax)]
}

cfa <- cfa[lengths(cfa) != 0] # dropping NULL elements




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
tictoc::toc() # ~

# Run report
kfa_report(kteachers, file.name = "kfa_teachers",
           report.format = "html_document",
           report.title = "K-fold Factor Analysis - Lebenon Teachers")

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




#### lavaan may have an unofficial mechanism for running efas ####
mod <- paste(paste(paste0('efa("efa")*f', 1:(nf-1), " +"), collapse = "\n"),
             paste0('efa("efa")*f', nf, " =~ ", paste(names(variables), collapse = " + ")))

test <- lavaan::cfa(model = mod,
                    data = variables,
                    estimator = "WLSMV",
                    ordered = TRUE)
# errors out: ‘H’ is not a slot in class “lavModel”


temp <- run_efa(studentdf,
                m = 2,
                ordered = TRUE,
                missing = "pairwise")

## calculate and extract sample statistics
sampstats <- lavaan::lavCor(object = studentdf,
                            ordered = names(studentdf),
                            estimator = "DWLS",
                            missing = 'pairwise',
                            output = "fit",
                            cor.smooth = FALSE)

sample.nobs <- lavaan::lavInspect(sampstats, "nobs")
sample.cov <- lavaan::lavInspect(sampstats, "sampstat")$cov
sample.mean <- lavaan::lavInspect(sampstats, "sampstat")$mean
sample.th <- lavaan::lavInspect(sampstats, "sampstat")$th
attr(sample.th, "th.idx") <- lavaan::lavInspect(sampstats, "th.idx")
WLS.V <- lavaan::lavInspect(sampstats, "wls.v")
NACOV <- lavaan::lavInspect(sampstats, "gamma")

# returns a data frame and summary information
nfactors <- parameters::n_factors(x = variables,
                                  type = "FA",
                                  rotation = rotation,
                                  package = c("psych", "nFactors"),
                                  cor = sample.cov,
                                  safe = TRUE)


## Running EFAs, comparing models, converting structure to CFA syntax
# results objects
efa.loadings <- vector(mode = "list", length = m)
mod.compare <- data.frame()


## write efa syntax
efa.mod <- write_efa(nf = 2, vnames = names(studentdf))

unrotated <- lavaan::cfa(model = efa.mod,
                         sample.cov = sample.cov,
                         sample.nobs = sample.nobs,
                         sample.mean = sample.mean,
                         sample.th = sample.th,
                         WLS.V = WLS.V,
                         NACOV = NACOV,
                         std.lv = TRUE,
                         orthogonal = TRUE,
                         estimator = "DWLS",
                         missing = "pairwise",
                         parameterization = "delta",
                         se = "none",
                         test = "none")

loading <- GPArotation::GPFoblq(lavaan::lavInspect(unrotated, "est")$lambda, method = "oblimin")$loadings

row.names(loading) <- paste0("v", 1:21)

round(loading, 3)
short <- round(loading[c(1,5,16:21),], 3)
row.names(short) <- paste0("v", 1:8)

