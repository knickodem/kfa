# ###################################
# #                                 #
# #        kfa package tests        #
# #                                 #
# ###################################
#
#
shortfile <- c("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/EFA&CFA/")
#
# # -------   Emotional Processes data    --------------------
# library(kfa)
# aemo.orig <- read.delim(paste0(shortfile, "scaledim/combined_p1p3_cr_emo-nightly.tsv"))
# aemo <- aemo.orig[, grep("aemo", names(aemo.orig))]
# aemo <- aemo[apply(is.na(aemo), 1, mean) < 1, ]
#
# lapply(names(aemo[-1]), function(x) table(aemo[[x]]))
#
#
# # run kfa
# aemo.mods <- kfa(variables = aemo,
#                 k = 4,
#                 m = 3,
#                 seed = 165,
#                 # custom.cfas = list(`Custom 2f` = custom2),
#                 rotation = "oblimin",
#                 ordered = TRUE,
#                 estimator = "DWLS",
#                 missing = "listwise")
#
#
# # generate reports
# kfa_report(aemo.mods,
#            file.name = "aemo_report",
#            report.format = "html_document",
#            report.title = "Child Emotion Processes - Caregiver Report",
#            path = shortfile)
#
#
# ########################################################################
#
# # -------   Full test on Lebanon data    --------------------
student <- read.csv(paste0(shortfile, "scaledim/student_survey-latest.csv"))
teacher <- read.csv(paste0(shortfile, "scaledim/teacher_survey-latest.csv"))
principal <- read.csv(paste0(shortfile, "scaledim/principal_survey-latest.csv"))
coach <- read.csv(paste0(shortfile, "scaledim/coach_survey-latest.csv"))

## item map
itemmaps <- lapply(c("Coach_survey", "Teacher Survey",
                     "Principal Survey", "Student Survey"), function(x){
                   readxl::read_excel(paste0(shortfile, "Documentation/Item construct map_2020_10_25.xlsx"),
                                      sheet = x)})
names(itemmaps) <- c("coach", "teacher", "principal", "student")

## items for analysis
studentdf <- student[ ,names(student) %in% na.omit(itemmaps$student$`Variable Name`)]
teacherdf <- teacher[ ,names(teacher) %in% na.omit(itemmaps$teacher$`Variable Name`)]
principaldf <- principal[ ,names(principal) %in% na.omit(itemmaps$principal$`Variable Name`)]
coachdf <- coach[ ,names(coach) %in% na.omit(itemmaps$coach$`Variable Name`)]




fa.s <- lavaan::lavCor(studentdf,
                       # ordered = names(studentdf),
                       std.ov = TRUE,
                       estimator = "ML",
                       missing = "listwise",
                       output = "cor",
                       cor.smooth = FALSE)
ss.m <- lavaan::lavInspect(fa.m, "sampstat")

fa.wd <- lavaan::lavCor(studentdf,
                        ordered = names(studentdf)[[1]],
                        estimator = "DWLS",
                        missing = "listwise",
                        output = "fit",
                        cor.smooth = FALSE)
ss.wd <- lavaan::lavInspect(fa.wd, "sampstat")
identical(ss.wd$cov, ss.wn$cov)
lavaan::lavInspect(fa.m, "options")$estimator


sample.nobs <- lavaan::lavInspect(fa.s, "nobs")
sample.cov <- lavaan::lavInspect(fa.s, "sampstat")$cov
sample.th <- lavaan::lavInspect(fa.s, "sampstat")$th
attr(sample.th, "th.idx") <- lavaan::lavInspect(fa.s, "th.idx")
WLS.V <- lavaan::lavInspect(fa.s, "wls.v")
NACOV <- lavaan::lavInspect(fa.s, "gamma")

check <- lavaan::lavInspect(fa.s, "sampstat")$cov
lavaan::lavInspect(fa.m, "options")$std.ov

## write efa syntax
efa.mod <- write_efa(nf = 3, vnames = names(studentdf))

efa.s <- lavaan::cfa(model = efa.mod,
                     sample.cov = fa.s,
                     sample.nobs = sample.nobs,
                     sample.th = sample.th,
                     # WLS.V = WLS.V,
                     # NACOV = NACOV,
                     std.lv = TRUE,
                     orthogonal = TRUE,
                     estimator = "ML",
                     missing = "listwise",
                     parameterization = "delta",
                     se = "none",
                     test = "none")

lavaan::summary(efa.m.orig, standardized = TRUE) # ML with WLS.V and NACOV; un = std.lv != std.all
lavaan::summary(efa.m.no, standardized = TRUE)
all.equal(lavaan::standardizedsolution(efa.m.orig, type = "std.lv"),
          lavaan::standardizedsolution(efa.m.no, type = "std.lv")) # using WLS.V and NACOV does not matter with ML and cov matrix
lavaan::summary(efa.s, standardized = TRUE) # ML with WLS.V and NACOV; un = std.lv = std.all
all.equal(lavaan::standardizedsolution(efa.m.orig, type = "std.all"),
          lavaan::standardizedsolution(efa.s, type = "std.all"))
all.equal(round(kfa:::get_std_loadings(efa.m.orig, type = "std.all"),4), # std.all = cor
          round(kfa:::get_std_loadings(efa.s, type = "std.all"), 4))



check.m <- run_efa(studentdf)
check.w <- run_efa(studentdf, ordered = TRUE)

lavaan::lavInspect(check.w$efas[[2]], "options")$estimator


round(kfa:::get_std_loadings(check.m$efas[[2]]), 3)


#
#
#
# ## custom factor structure (just an example, not based on theory)
# custom2 <- paste0("f1 =~ ", paste(names(studentdf)[1:10], collapse = " + "),
#                  "\nf2 =~ ",paste(names(studentdf)[11:21], collapse = " + "))
# custom3 <- paste0("f1 =~ ", paste(names(studentdf)[1:2], collapse = " + "),
#                   "\nf2 =~ ",paste(names(studentdf)[3:4], collapse = " + "),
#                   "\nf3 =~ ",paste(names(studentdf)[5:6], collapse = " + "),
#                   "\nf4 =~ ",paste(names(studentdf)[7:8], collapse = " + "),
#                   "\nf5 =~ ",paste(names(studentdf)[9:10], collapse = " + "),
#                   "\nf6 =~ ",paste(names(studentdf)[11:12], collapse = " + "),
#                   "\nf7 =~ ",paste(names(studentdf)[13:21], collapse = " + "),
#                   "\na1101x ~~ ", paste(names(studentdf)[2:21], collapse = " + "),
#                   "\na1102x ~~ ", paste(names(studentdf)[3:21], collapse = " + "))
#
# # When custom model does not contain all items, the following error is currently produced:
# # Error in { : task 1 failed - "invalid 'times' argument"
# # set.seed(936639) # set seed to reproduce same folds
#
#
# tictoc::tic()
#
# kstudent <- kfa(variables = studentdf,
#                 k = NULL,
#                 m = 5,
#                 seed = 936639,
#                 custom.cfas = list(`Custom 2f` = custom2, Break = custom3),
#                 rotation = "oblimin",
#                 ordered = TRUE,
#                 estimator = "DWLS",
#                 missing = "pairwise")
# tictoc::toc()# ~ 60 seconds
#
# # Run report
# kfa_report(kstudent,
#            file.name = "kfa_students_test",
#            report.title = "K-fold Factor Analysis - Lebanon Students")
#
# # report.format = "html_document",
#
#
# ##################################################################
#
#
# # ------ Running tests on kfa_report -------------------------
#
# models <- peru.mods
# index = c("chisq", "cfi", "rmsea")
# load.flag = .30
# cor.flag = .90
# rel.flag = .60
# digits = 2
#
# cfas <- models$cfas
# ## analysis summary info
# k <- length(cfas) # number of folds
# m <- max(unlist(lapply(cfas, length))) # number of models per fold (includes both efa AND custom structures); m == length(mnames)
# mnames <- models$model.names # model names
# fac.allow <- length(models$efa.structures)
# fac.max <- max(as.numeric(substring(mnames[grepl("-factor", mnames)], 1, 1)))  # kfa naming convention "#-factor"; custom functions are assumed to have different convention
# vnames <- dimnames(lavaan::lavInspect(cfas[[1]][[1]], "sampstat")$cov)[[1]] # variable names
# nvars <- length(vnames)
# nobs <- sum(unlist(lapply(cfas, function(x) lavaan::lavInspect(x[[1]], "nobs"))))
# opts <- lavaan::lavInspect(cfas[[1]][[1]], "options") # estimation options; assumed to be the same for all models
#
# #### Model Fit ####
# ## summarizing fit statistics by fold
# kfits <- k_model_fit(models, index = index, by.fold = TRUE) # dataframe for each fold
# fit.table <- agg_model_fit(kfits, index = index, digits = 2)
#
# ## creating appendix -  folds x model table of fit statistics
# mfits <- k_model_fit(models, index = index, by.fold = FALSE)
# appendix <- get_appendix(mfits, index = index)
#
# #### Parameters ####
# ## model structures
# kstructures <- model_structure(models, which = "cfa")
#
# ## loadings
# klambdas <- agg_loadings(models, flag = load.flag, digits = digits)
#
# ## factor correlations
# kcorrs <- agg_cors(models, flag = cor.flag)
#
# ## score reliabilities
# krels <- agg_rels(models, flag = rel.flag, digits = digits)
#
# ## flagged problems
# flagged <- model_flags(models, kstructures, klambdas, kcorrs, krels)
#
#
# cfas <- example$cfas
# mnames <- unique(unlist(lapply(cfas, names))) # model names
# flag <- .90
#
# ############################################################
#
#
#
#
#
#
# variables <- peru[-1]
# k <- 5
# m <- 3
# rotation = "oblimin"
# ordered = TRUE
# estimator = "DWLS"
# missing = "listwise"
# seed = 165
# # custom.cfas <- list(custom2, custom3)
#
#
# set.seed(seed)
#
# ## create folds
# # returns list of row numbers for each test fold (i.e., the cfa sample)
# # contents of y doesn't matter, just needs to be nrow(variables) in length
# testfolds <- caret::createFolds(y = 1:nrow(variables),
#                                 k = k, list = TRUE,
#                                 returnTrain = FALSE)
#
#
# # variables <- peru[-1]
# # check <- vector("list", length = k)
# # for(fold in 1:k){
# #   check[[fold]] <- run_efa(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
# #                            m = 3, single.item = "none", ordered = TRUE, rotation = "oblimin",
# #                            estimator = "DWLS")
# # }
# # lapply(check, "[[", "syntax")
#
# breakpoint <- vector("list", length = k)
# for (fold in 1:k){
# ## calculate and extract sample statistics
# sampstats <- lavaan::lavCor(object = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
#                             ordered = ordered,
#                             estimator = estimator,
#                             missing = missing,
#                             output = "fit",
#                             cor.smooth = FALSE)
#
# sample.nobs <- lavaan::lavInspect(sampstats, "nobs")
# sample.cov <- lavaan::lavInspect(sampstats, "sampstat")$cov
# sample.mean <- lavaan::lavInspect(sampstats, "sampstat")$mean
# sample.th <- lavaan::lavInspect(sampstats, "sampstat")$th
# attr(sample.th, "th.idx") <- lavaan::lavInspect(sampstats, "th.idx")
# WLS.V <- lavaan::lavInspect(sampstats, "wls.v")
# NACOV <- lavaan::lavInspect(sampstats, "gamma")
#
# ## Running EFAs (no need to run 1-factor b/c we already know the structure)
# efa.loadings <- vector(mode = "list", length = m)
#
# for(nf in 2:m){
#
#   ## write efa syntax
#   efa.mod <- write_efa(nf = nf, vnames = names(variables))
#
#   unrotated <- lavaan::cfa(model = efa.mod,
#                            sample.cov = sample.cov,
#                            sample.nobs = sample.nobs,
#                            sample.mean = sample.mean,
#                            sample.th = sample.th,
#                            WLS.V = WLS.V,
#                            NACOV = NACOV,
#                            std.lv = TRUE,
#                            orthogonal = TRUE,
#                            estimator = estimator,
#                            missing = missing,
#                            parameterization = "delta",
#                            se = "none",
#                            test = "none")
#
#   # list of unrotated factor loadings
#   efa.loadings[[nf]] <- lavaan::lavInspect(unrotated, "est")$lambda
# }
#
# ## if chosen, applying rotation to standardized factor loadings for models where m > 1
# # oblique rotations
# if(rotation %in% c("oblimin", "oblimax", "quartimin",
#                    "targetQ", "pstQ", "simplimax",
#                    "bentlerQ", "geominQ", "cfQ",
#                    "infomaxQ", "bifactorQ")){
#
#   loadings <- lapply(efa.loadings[-1], function(x){
#     GPArotation::GPFoblq(x, method = rotation)$loadings}
#   )
#
#   # loadings <- lapply(efa.loadings[-1], function(x){
#   #   tryCatch(expr = {GPArotation::GPForth(x, method = rotation)$loadings},
#   #            error = {return(x)})
#   # })
#
#
#   # orthogonal rotations
# } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
#                           "bentlerT", "tandemI", "tandemII",
#                           "geominT", "cfT", "infomaxT",
#                           "mccammon", "bifactorT")){
#
#   loadings <- lapply(efa.loadings[-1], function(x){
#     tryCatch(expr = {GPArotation::GPForth(x, method = rotation)$loadings},
#              error = {return(x)}
#     )
#   })
#
# } else {
#   loadings <- efa.loadings[-1]
#   message("Reporting unrotated factor loadings")
# }
#
# # converting efa results to cfa syntax
# cfa.syntax <- lapply(loadings, function(x){
#   efa_cfa_syntax(loadings = x,
#                  simple = TRUE,
#                  threshold = NA,
#                  single.item = "none")
# })
#
# ## adding the 1-factor model as first element in cfa syntax list
# onefac <- paste0("f1 =~ ", paste(names(variables), collapse = " + "))
# cfa.syntax <- c(list(onefac), cfa.syntax)
# breakpoint2[[fold]] <- list(syntax = cfa.syntax,
#                            loadings = loadings)
# }
#
# # ------ Running tests on kfa -------------------------
#
#   variables <- as.data.frame(variables)
#
#   # The ordered = TRUE functionality in lavaan is not currently equivalent to listing
#   # all items, so need to do it manually since I want this functionality for our users
#   if(is.character(ordered)){
#     if(is.null(estimator)){
#       estimator <- "DWLS"
#     }
#   }
#   if(ordered == TRUE){
#     ordered <- names(variables)
#     if(is.null(estimator)){
#       estimator <- "DWLS"
#     }
#   } else if(ordered == FALSE){
#     ordered <- NULL
#     if(is.null(estimator)){
#       estimator <- "ML"
#     }
#   }
#
#
#   set.seed(seed)
#
#   ## create folds
#   # returns list of row numbers for each test fold (i.e., the cfa sample)
#   # contents of y doesn't matter, just needs to be nrow(variables) in length
#   testfolds <- caret::createFolds(y = 1:nrow(variables),
#                                   k = k, list = TRUE,
#                                   returnTrain = FALSE)
#
#   test <- k_efa(variables = variables[!c(row.names(variables) %in% testfolds[[1]]), ],
#         m = m,
#         rotation = rotation,
#         ordered = ordered,
#         estimator = estimator,
#         missing = missing)
#
#       ## run EFAs and return lavaan syntax for CFAs
#       efa <- for(fold in 1:k){
#
#         k_efa(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
#               m = m,
#               rotation = rotation,
#               ordered = ordered,
#               estimator = estimator,
#               missing = missing)
#
#         ## do.call version throws error for some reason ( Error in { : task 1 failed - "$ operator is invalid for atomic vectors" )
#         # do.call(k_efa, args = c(list(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
#         #                              m = m),
#         #                         lavaan.args))
#       }
#
#
#
#
#
#
#       ## identifying unique structures
#       # formatting for use in model_structure
#       temp <- list(cfas = NULL,
#                    efas = efa)
#       efa.structures <- model_structure(temp, which = "efa")
#       names(efa.structures) <- paste0(1:m, "-factor")
#
#       ## collect most common (mode) structure for each factor model to use in CFAs
#       mode.structure <- rep(list(rep(list(""), m)), k) # creates list of lists framework needed for running cfas
#       for(n in 1:m){  # replaces "" in the sublists with mode structure when appropriate given the values in x$folds
#         if(length(efa.structures[[n]]) == 1){ # if there is only 1 structure of a given model
#           for(i in efa.structures[[n]][[1]]$folds){
#             mode.structure[[i]][[n]] <- efa.structures[[n]][[1]]$structure
#           }
#         } else {
#           # number of folds each structure was found; keep most common structure
#           num.folds <- unlist(lapply(efa.structures[[n]], function(x) length(x$folds)))
#           ties <- which(num.folds == max(num.folds))
#           ties <- ifelse(length(ties) == 1, ties, ties[[1]]) # in case of ties, use first structure
#           for(i in efa.structures[[n]][[ties]]$folds){
#             mode.structure[[i]][[n]] <- efa.structures[[n]][[ties]]$structure
#           }
#         }
#       }
#
#       ## add names to models for each fold
#       mode.structure <- lapply(mode.structure, function(x) {
#         names(x) <- paste0(1:m, "-factor")
#         return(x)
#       })
#
#       ## add custom models (if present)
#       if(!is.null(custom.cfas)){
#         if(class(custom.cfas) != "list"){ # converting single object to named list
#           custom.name <- deparse(substitute(custom.cfas))
#           custom.cfas <- list(custom.cfas)
#           names(custom.cfas) <- custom.name
#         } else if(is.null(names(custom.cfas))){ # (if necessary) adding names to list
#           names(custom.cfas) <- paste("custom", LETTERS[1:length(custom.cfas)])
#         }
#         # add custom models to set of models for each fold
#         cfa.syntax <- lapply(mode.structure, function(x) c(x, custom.cfas))
#       } else {
#         cfa.syntax <- mode.structure
#       }
#
#       ## run CFAs
#       cfas <- foreach::foreach(fold = 1:k) %dopar% {
#
#         k_cfa(syntax = cfa.syntax[[fold]],
#               variables = variables[testfolds[[fold]], ],
#               ordered = ordered,
#               estimator = estimator,
#               missing = missing,
#               ...)
#
#         # do.call(k_cfa, args = c(list(syntax = cfa.syntax[[fold]],
#         #                              variables = variables[testfolds[[fold]], ]),
#         #                         lavaan.args))
#       }
#
#       ## gathering model names
#       mnames <- names(cfa.syntax[[1]])
#       empty <- vector("list", k)
#       for(f in 1:k){
#         empty[[f]] <- unlist(lapply(cfa.syntax[[f]], function(x) x == ""))
#       }
#
#       drop <- colSums(Reduce("rbind", empty)) == k
#       mnames <- mnames[!drop]
#
#       ## organizing output
#       output <- list(cfas = cfas,
#                      cfa.syntax = cfa.syntax,
#                      model.names = mnames,
#                      efa.structures = efa.structures)
#
# ##########################################################################3
#
#
#
# #### Plotting ####
# palette.colors(n = 10, "Set 1")
# cut <- .3
# graphics::layout
# vnames <- dimnames(lavaan::lavInspect(kstudent[[1]][[1]], "sampstat")$cov)[[1]]
# plot.settings <- list(what = "std", whatLabels = "no", layout = "tree",
#                       intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
#                       cut = cut, posCol = c("#BF0000","#000000"), fade = FALSE,
#                       # edge.color = , # could create custom function to utilize this argument
#                       weighted = TRUE, negDashed = TRUE, esize = 5,
#                       manifests = vnames, reorder = FALSE)
# kstructures <- model_structure(kstudent)
# l <- length(kstructures[[1]][[1]]$folds)
# plotmat <- matrix(1:l, nrow = 2)
# graphics::layout(plotmat)
# for(f in 1:l){
# do.call(semPlot::semPaths, args = c(list(object = kstudent[[f]][[1]],
#                                          color = list(lat = palette.colors(n = 1 + 1, palette = "Okabe-Ito",
#                                                                            recycle = TRUE)[-1]),
#                                          title = FALSE),
#                                     plot.settings))
# title(paste("Fold:", f), adj = 0, line = 0)
# }
#
#
#
# curiousplot <- semPlot::semPaths(kstudent[[1]][[3]], what = "std", whatLabels = "no",
#                                  layout = "tree", negDashed = TRUE,
#                   intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
#                   color = list(lat = palette.colors(n = 4, palette = "Okabe-Ito",
#                                                     recycle = TRUE)[-1]),
#                   cut = .3, posCol = c("#BF0000","#000000"), fade = FALSE,
#                   # edge.color = , # could create custom function to utilize this argument
#                   weighted = TRUE, esize = 5,
#                   manifests = vn, reorder = FALSE)
#
# plot.settings <- list(what = "std", whatLabels = "no",
#                       layout = "tree", negDashed = TRUE,
#                       intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
#                       color = list(lat = palette.colors(n = 4, palette = "Okabe-Ito",
#                                                         recycle = TRUE)[-1]),
#                       cut = .3, posCol = c("#BF0000","#000000"), fade = FALSE,
#                       # edge.color = , # could create custom function to utilize this argument
#                       weighted = TRUE, esize = 5,
#                       manifests = vn, reorder = FALSE)
#
# do.call(semPlot::semPaths, args = c(list(object = kstudent[[1]][[3]]), plot.settings))
#
# ## Not sure how to arrange plots into a grid yet
# # arranging packages (cowplot, gridExtra) use grob objects rather than qgraph objects
# # cowplot::as_grob nor qgraph::as.ggraph currently do the conversion
# qgraph::as.ggraph(curiousplot)
#
#
# ##############################################
