###################################
#                                 #
#        kfa package tests        #
#                                 #
###################################


shortfile <- c("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/EFA&CFA/")

# -------   Full test on Peru data    --------------------
library(dplyr)
library(kfa)
peru.orig <- read.delim(paste0(shortfile, "scaledim/combined_p1p3_cr_emo-nightly.tsv"))

peru <- peru.orig %>%
  filter(rowMeans(is.na(select(., starts_with("aemo")))) < 1) %>% # non-response across all items
  select(cid_1, starts_with("aemo"))

lapply(names(peru[-1]), function(x) table(peru[[x]]))

tictoc::tic()

peru.mods <- kfa(variables = peru[-1],
                k = 5,
                m = 3,
                seed = 165,
                # custom.cfas = list(`Custom 2f` = custom2, Break = custom3),
                rotation = "oblimin",
                ordered = TRUE,
                estimator = "DWLS",
                missing = "listwise")
tictoc::toc()

tictoc::tic()
kfa_report(peru.mods,
           file.name = "aemo_report",
           report.format = "html_document",
           report.title = "Caregiver Emotional Awareness and Expression",
           path = shortfile)
tictoc::toc()

########################################################################

# -------   Full test on Lebanon data    --------------------
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

# Run report
kfa_report(kstudent,
           file.name = "kfa_students_test",
           report.title = "K-fold Factor Analysis - Lebanon Students")

# report.format = "html_document",


##################################################################


# ------ Running tests on kfa_report -------------------------

models <- peru.mods
index = c("chisq", "cfi", "rmsea")
load.flag = .30
cor.flag = .90
rel.flag = .60
digits = 2

cfas <- models$cfas
## analysis summary info
k <- length(cfas) # number of folds
m <- max(unlist(lapply(cfas, length))) # number of models per fold (includes both efa AND custom structures); m == length(mnames)
mnames <- models$model.names # model names
fac.allow <- length(models$efa.structures)
fac.max <- max(as.numeric(substring(mnames[grepl("-factor", mnames)], 1, 1)))  # kfa naming convention "#-factor"; custom functions are assumed to have different convention
vnames <- dimnames(lavaan::lavInspect(cfas[[1]][[1]], "sampstat")$cov)[[1]] # variable names
nvars <- length(vnames)
nobs <- sum(unlist(lapply(cfas, function(x) lavaan::lavInspect(x[[1]], "nobs"))))
opts <- lavaan::lavInspect(cfas[[1]][[1]], "options") # estimation options; assumed to be the same for all models

#### Model Fit ####
## summarizing fit statistics by fold
kfits <- k_model_fit(models, index = index, by.fold = TRUE) # dataframe for each fold
fit.table <- agg_model_fit(kfits, index = index, digits = 2)

## creating appendix -  folds x model table of fit statistics
mfits <- k_model_fit(models, index = index, by.fold = FALSE)
appendix <- get_appendix(mfits, index = index)

#### Parameters ####
## model structures
kstructures <- model_structure(models, which = "cfa")

## loadings
klambdas <- agg_loadings(models, flag = load.flag, digits = digits)

## factor correlations
kcorrs <- agg_cors(models, flag = cor.flag)

## score reliabilities
krels <- agg_rels(models, flag = rel.flag, digits = digits)

## flagged problems
flagged <- model_flags(models, kstructures, klambdas, kcorrs, krels)


cfas <- example$cfas
mnames <- unique(unlist(lapply(cfas, names))) # model names
flag <- .90

############################################################

# ------ Running tests on kfa -------------------------


variables <- as.data.frame(sim.lav)
k <- 4
m <- 5
rotation = "oblimin"
ordered = FALSE
estimator = "ML"
missing = "listwise"
# custom.cfas <- list(custom2, custom3)

##########################################################################3



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


##############################################
