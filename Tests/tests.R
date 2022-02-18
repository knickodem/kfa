# ###################################
# #                                 #
# #        kfa package tests        #
# #                                 #
# ###################################
#
# # ap <- available.packages()
# # tools::package_dependencies("kfa", ap, reverse = FALSE, recursive = FALSE)
#
# shortfile <- c("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/EFA&CFA/")
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
# student <- read.csv(paste0(shortfile, "scaledim/student_survey-latest.csv"))
# teacher <- read.csv(paste0(shortfile, "scaledim/teacher_survey-latest.csv"))
# principal <- read.csv(paste0(shortfile, "scaledim/principal_survey-latest.csv"))
# coach <- read.csv(paste0(shortfile, "scaledim/coach_survey-latest.csv"))
#
# ## item map
# itemmaps <- lapply(c("Coach_survey", "Teacher Survey",
#                      "Principal Survey", "Student Survey"), function(x){
#                    readxl::read_excel(paste0(shortfile, "Documentation/Item construct map_2020_10_25.xlsx"),
#                                       sheet = x)})
# names(itemmaps) <- c("coach", "teacher", "principal", "student")
#
# ## items for analysis
# studentdf <- student[ ,names(student) %in% na.omit(itemmaps$student$`Variable Name`)]
# teacherdf <- teacher[ ,names(teacher) %in% na.omit(itemmaps$teacher$`Variable Name`)]
# principaldf <- principal[ ,names(principal) %in% na.omit(itemmaps$principal$`Variable Name`)]
# coachdf <- coach[ ,names(coach) %in% na.omit(itemmaps$coach$`Variable Name`)]
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
#
# student.mods <- kfa(variables = studentdf,
#                  k = 4,
#                  m = 3,
#                  seed = 165,
#                  custom.cfas = list(`Custom 2f` = custom2),
#                  rotation = "oblimin",
#                  ordered = TRUE)
#
#
# ##################################################################
#
# #  ------- Testing kfa in its entirity ----------------------
#
# ####   Set up through EFA  #####
#
# ## arguments
# variables <- studentdf
# k = 4
# m = 3
# custom.cfas = list(`A Mod` = custom2, `B Mod` = custom2)
# rotation = "oblimin"
# simple = TRUE
# threshold = NA
# ordered = names(variables)
# estimator = "WLSMV"
# missing = "listwise"
# seed = 165
#
# set.seed(seed)
#
# ## create folds
# if(k == 0){
#   testfolds <- list(Fold1 = 0)
#   k <- 1
# } else {
#
#   # returns list of row numbers for each test fold (i.e., the cfa sample)
#   # contents of y doesn't matter, just needs to be nrow(variables) in length
#   testfolds <- caret::createFolds(y = 1:nrow(variables),
#                                   k = k, list = TRUE,
#                                   returnTrain = FALSE)
# }
#
# efa <- lapply(1:k, function(fold){
#
#   kfa:::k_efa(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
#               m = m,
#               rotation = rotation,
#               simple = simple,
#               threshold = threshold,
#               ordered = ordered,
#               estimator = estimator,
#               missing = missing)
# })
#
#
# #### within k_efa - tests a single fold   ####
# ## calculate and extract sample statistics
# sampstats <- kfa:::sample_stats(variables = variables[!c(row.names(variables) %in% testfolds[[1]]), ],
#                           ordered = ordered,
#                           estimator = estimator,
#                           missing = missing)
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
#                            sample.cov = sampstats$cov,
#                            sample.nobs = sampstats$nobs,
#                            sample.th = sampstats$th,
#                            WLS.V = sampstats$wls.v,
#                            NACOV = sampstats$nacov,
#                            std.lv = TRUE,
#                            orthogonal = TRUE,
#                            estimator = estimator,
#                            missing = missing,
#                            parameterization = "delta",
#                            se = "none",
#                            test = "none")
#
#   # list of unrotated factor loadings
#   efa.loadings[[nf]] <- get_std_loadings(unrotated, type = "std.all") # LVs and OVs are standardized
#   #lavaan::lavInspect(unrotated, "est")$lambda # LVs are standardized, OVs are not
#
# }
#
# ## if chosen, applying rotation to standardized factor loadings for models where m > 1
# # oblique rotations
# if(rotation %in% c("oblimin", "oblimax", "quartimin",
#                    "targetQ", "pstQ", "simplimax",
#                    "bentlerQ", "geominQ", "cfQ",
#                    "infomaxQ", "bifactorQ")){
#
#   f <- function(x){
#     try <- tryCatch(expr = GPArotation::GPFoblq(x, method = rotation)$loadings,
#                     error = function(e) return(NA))
#     out <- if(is.logical(try)) x else try
#     return(out)
#   }
#   loadings <- lapply(efa.loadings[-1], f) # skip the blank element for m = 1
#
#
#   # orthogonal rotations
# } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
#                           "bentlerT", "tandemI", "tandemII",
#                           "geominT", "cfT", "infomaxT",
#                           "mccammon", "bifactorT")){
#
#   f <- function(x){
#     try <- tryCatch(expr = GPArotation::GPForth(x, method = rotation)$loadings,
#                     error = function(e) return(NA))
#     out <- if(is.logical(try)) x else try
#     return(out)
#   }
#   loadings <- lapply(efa.loadings[-1], f) # skip the blank element for m = 1
#
# } else {
#   loadings <- efa.loadings
#   message("Reporting unrotated factor loadings")
# }
#
# # converting efa results to cfa syntax
# cfa.syntax <- lapply(loadings, function(x){
#   efa_cfa_syntax(loadings = x,
#                  simple = simple,
#                  threshold = threshold,
#                  single.item = "none",
#                  identified = TRUE,
#                  constrain0 = TRUE)
# })
#
# ## adding the 1-factor model as first element in cfa syntax list
# onefac <- paste0("f1 =~ ", paste(names(variables), collapse = " + "))
# cfa.syntax <- c(list(onefac), cfa.syntax)
#
# ## once it all works, run k_efa in line ~205
#
# #############################################
#
# #####    Converting EFA to CFA section #####
# ## identifying unique structures
# efa.structures <- kfa:::model_structure_efa(efa)
# names(efa.structures) <- paste0(1:m, "-factor")
#
# ## collect most common (mode) structure for each factor model to use in CFAs
# mode.structure <- rep(list(rep(list(""), m)), k) # creates list of lists framework needed for running cfas
# for(n in 1:m){  # replaces "" in the sublists with mode structure when appropriate given the values in x$folds
#   if(length(efa.structures[[n]]) == 1){ # if there is only 1 structure of a given model
#     for(i in efa.structures[[n]][[1]]$folds){
#       mode.structure[[i]][[n]] <- efa.structures[[n]][[1]]$structure
#     }
#   } else {
#     # number of folds each structure was found; keep most common structure
#     num.folds <- unlist(lapply(efa.structures[[n]], function(x) length(x$folds)))
#     ties <- which(num.folds == max(num.folds))
#     ties <- ifelse(length(ties) == 1, ties, ties[[1]]) # in case of ties, use first structure
#     for(i in efa.structures[[n]][[ties]]$folds){
#       mode.structure[[i]][[n]] <- efa.structures[[n]][[ties]]$structure
#     }
#   }
# }
#
# ## add names to models for each fold
# mode.structure <- lapply(mode.structure, function(x) {
#   names(x) <- paste0(1:m, "-factor")
#   return(x)
# })
#
# ## add custom models (if present)
# if(!is.null(custom.cfas)){
#   if(class(custom.cfas) != "list"){ # converting single object to named list
#     custom.name <- deparse(substitute(custom.cfas))
#     custom.cfas <- list(custom.cfas)
#     names(custom.cfas) <- custom.name
#   } else if(is.null(names(custom.cfas))){ # (if necessary) adding names to list
#     names(custom.cfas) <- paste("custom", LETTERS[1:length(custom.cfas)])
#   }
#   # add custom models to set of models for each fold
#   cfa.syntax <- lapply(mode.structure, function(x) c(x, custom.cfas))
# } else {
#   cfa.syntax <- mode.structure
# }
#
# if(k == 1 & length(testfolds$Fold1) == 1){
#   testfolds$Fold1 <- 1:nrow(variables)
# }
#
# ########################################################
#
#
# ## run CFAs
# cfas <- lapply(1:k, function(fold){
#
#   kfa:::k_cfa(syntax = cfa.syntax[[fold]],
#         variables = variables[testfolds[[fold]], ],
#         ordered = ordered,
#         estimator = estimator,
#         missing = missing)
#   })
#
# #### within k_cfa - tests a single fold   ####
# syntax = cfa.syntax[[1]]
#
# ## calculate and extract sample statistics
# sampstats <- kfa:::sample_stats(variables = variables[!c(row.names(variables) %in% testfolds[[1]]), ],
#                                 ordered = ordered,
#                                 estimator = estimator,
#                                 missing = missing)
#
# ## run CFAs
# mods <- vector(mode = "list", length = length(syntax))
#
# for(c in 1:length(syntax)){
#
#   if(nchar(syntax[[c]]) > 0){
#
#     fit <- lavaan::cfa(model = syntax[[c]],
#                        sample.cov = sampstats$cov,
#                        sample.nobs = sampstats$nobs,
#                        sample.th = sampstats$th,
#                        WLS.V = sampstats$wls.v,
#                        NACOV = sampstats$nacov,
#                        estimator = estimator,
#                        missing = missing,
#                        parameterization = "delta")
#
#     mods[[c]] <- fit
#
#   } else {
#
#     mods[[c]] <- NULL
#   }
#
# }
#
# mods <- mods[lengths(mods) != 0] # dropping NULL elements
# names(mods) <- names(syntax)[which(nchar(syntax) > 0)] # adding names
#
#
# ## once it all works, run k_cfa in line ~250
# #########################################################
#
#
# ## gathering model names
# mnames <- names(cfa.syntax[[1]])
# empty <- vector("list", k)
# for(f in 1:k){
#   empty[[f]] <- unlist(lapply(cfa.syntax[[f]], function(x) x == ""))
# }
#
# if(k > 1){
#   drop <- colSums(Reduce("rbind", empty)) == k
# } else {
#   drop <- empty[[1]]
# }
# mnames <- mnames[!drop]
#
# ## organizing output
# output <- list(cfas = cfas,
#                cfa.syntax = cfa.syntax,
#                model.names = mnames,
#                efa.structures = efa.structures)
# class(output) <- "kfa"
#
#
# ##############################################################
#
#
# # ------ Testing kfa_report in its entirity -------------------------
#
# models <- cl.example # output
# index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled")
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
#
# #### Model Fit ####
# ## summarizing fit statistics by fold
# kfits <- k_model_fit(models, index = index, by.fold = TRUE) # dataframe for each fold
# fit.table <- agg_model_fit(kfits, index = "all", digits = 2)
#
# ## creating appendix -  folds x model table of fit statistics
# mfits <- k_model_fit(models, index = index, by.fold = FALSE)
# appendix <- kfa:::get_appendix(mfits, index = "all")
#
# #### Parameters ####
# ## model structures
# kstructures <- model_structure(models)
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
# flagged <- kfa:::model_flags(models, kstructures, klambdas, kcorrs, krels)
#
#
# ############################################################
#
#
# # ------- Testing agg_loadings ----------------------------
#
# models <- cl.example
# flag = .20
# digits = 2
#
# cfas <- models$cfas
# vnames <- dimnames(lavaan::lavInspect(cfas[[1]][[1]], "sampstat")$cov)[[1]] # variable names
# mnames <- models$model.names # model names
# m <- length(mnames) #max(unlist(lapply(cfas, length))) # maximum number of models per fold
# k <- length(cfas) # number of folds
#
# klambdas <- vector("list", m)  # factor loadings across folds
# names(klambdas) <- mnames
# lflag <- vector("integer", m)  # low loading flag
# names(lflag) <- mnames
# hflag <- vector("integer", m)  # heywood case flag
# names(hflag) <- mnames
# for(n in mnames){
#   # n <- "3-factor"
#   lambdas <- vector("list", k)
#   thetas <- vector("list", k)
#   load.flag <- vector("integer", k)
#   hey.flag <- vector("integer", k)
#   for(f in 1:k){
#     if(n %in% names(cfas[[f]])){
#       ## gather standardized loadings
#       l.temp <- lavaan::standardizedSolution(cfas[[f]][[n]], "std.all", se = FALSE)
#       lambdas[[f]] <- l.temp[l.temp$op == "=~",]
#
#       # flag for model summary table
#       load.flag[[f]] <- sum(lambdas[[f]]$est.std < flag)
#
#       ## gather residual variances
#       t.temp <- lavaan::parameterestimates(cfas[[f]][[n]], se = FALSE)
#       thetas[[f]] <- t.temp[t.temp$op == "~~" & t.temp$lhs %in% vnames & t.temp$lhs == t.temp$rhs,]
#
#       # flag for model summary table
#       hey.flag[[f]] <- sum(thetas[[f]]$est < 0)
#     }
#   }
#   lambdas <- do.call("rbind", lambdas)
#   thetas <- do.call("rbind", thetas)
#
#   # identifying cross-loading items
#   byfactor <- tapply(lambdas$est.std, INDEX = list(lambdas$lhs, lambdas$rhs), sum) # most any function will do; we do not use the value in the cells, only care if NA
#   cls <- names(which(apply(byfactor, 2, function(x) sum(!is.na(x)) > 1))) # cross-loading variables
#
#   if(length(cls) > 0){ # only search for cross-loading secondary factor if needed
#
#     # is.na(lambdas$est.std) <- !lambdas$est.std # converts 0 to NA; don't need to do this though b/c the variable would be constrained in all folds
#     lambdas$f.v <- paste(lambdas$lhs, lambdas$op, lambdas$rhs) # adds column of factor by variable strings
#
#     fv.means <- tapply(lambdas$est.std, lambdas$f.v, mean) # mean loading for each factor-variable pair
#     ord <- lapply(cls, function(x) sort(fv.means[grepl(x, names(fv.means))], decreasing = TRUE)) # ordering means
#     p <- unlist(lapply(ord, function(x) names(x)[[1]])) # extracting primary factor
#     s <- unlist(lapply(ord, function(x) names(x)[[2]])) # extracting secondary factor
#     # need both p and s in case there are tertiary cross-loadings, which we will not report
#
#     primary <- lambdas[!lambdas$rhs %in% cls | lambdas$f.v %in% p,]
#     secondary <- lambdas[lambdas$f.v %in% s,]
#
#
#     primary.df <- data.frame(mean = tapply(primary$est.std, primary$rhs, mean),
#                              range = paste(format(round(tapply(primary$est.std, primary$rhs, min), digits = digits), nsmall = digits), "-",
#                                            format(round(tapply(primary$est.std, primary$rhs, max), digits = digits), nsmall = digits)),
#                              `loading flag` = tapply(primary$est.std, primary$rhs, function(x) sum(x < flag)),
#                              `heywood flag` = tapply(thetas$est, thetas$rhs, function(x) sum(x < 0)),
#                              check.names = FALSE)
#     primary.df <- cbind(data.frame(variable = row.names(primary.df)), primary.df)
#
#     # note: secondary does not have heywood flag b/c that is a variable level indicator, not variable by factor level indicator
#     secondary.df <- data.frame(mean.s = tapply(secondary$est.std, secondary$rhs, mean),
#                                range.s = paste(format(round(tapply(secondary$est.std, secondary$rhs, min), digits = digits), nsmall = digits), "-",
#                                                format(round(tapply(secondary$est.std, secondary$rhs, max), digits = digits), nsmall = digits)),
#                                `loading flag.s` = tapply(secondary$est.std, secondary$rhs, function(x) sum(x < flag)),
#                                check.names = FALSE)
#     secondary.df <- cbind(data.frame(variable = row.names(secondary.df)), secondary.df)
#
#     kl <- merge(primary.df, secondary.df, by = "variable", all = TRUE, sort = FALSE) # sorting has to happen in next lines
#     kl$variable <- factor(kl$variable, levels = vnames)
#     # row.names(kl) <- NULL
#     klambdas[[n]] <- kl[order(kl$variable),]
#
#   } else {
#
#     kl <- data.frame(mean = tapply(lambdas$est.std, lambdas$rhs, mean),
#                      range = paste(format(round(tapply(lambdas$est.std, lambdas$rhs, min), digits = digits), nsmall = digits), "-",
#                                    format(round(tapply(lambdas$est.std, lambdas$rhs, max), digits = digits), nsmall = digits)),
#                      `loading flag` = tapply(lambdas$est.std, lambdas$rhs, function(x) sum(x < flag)),
#                      `heywood flag` = tapply(thetas$est, thetas$rhs, function(x) sum(x < 0)),
#                      check.names = FALSE)
#     # order dataframe by vnames
#     kl <- cbind(data.frame(variable = factor(row.names(kl), levels = vnames)), kl)
#     klambdas[[n]] <- kl[order(kl$variable),]
#   }
#
#   ## count of folds with a loading under flag threshold
#   lflag[[n]] <- sum(load.flag > 0)
#   hflag[[n]] <- sum(hey.flag > 0)
# }
#
# aggl <- list(loadings = klambdas,
#                   flag = lflag,
#                   heywood = hflag)
#
#
# ############################################################
#
#
# # ------- Testing agg_corrs ----------------------------
#
# models <- cl.example
# flag = .90
# digits = 2
#
# cfas <- models$cfas
# mnames <- models$model.names # model names
# m <- length(mnames)
# k <- length(cfas)
#
#
# kcorrs <- vector("list", m)
# names(kcorrs) <- mnames
# kflag <- vector("integer", m)
# names(kflag) <- mnames
# # Currently assumes the first element is a 1 factor model; need a more robust check
# # kcorrs[[1]] <- NULL
# kflag[[1]] <- NA
# if(m > 1){
#   for(n in 2:m){
#     # n <- 4
#     cor.lv <- vector("list", k) # latent variable correlation matrix
#     cor.flag <- vector("list", k) # count of correlations above flag threshold
#     for(f in 1:k){
#       if(mnames[[n]] %in% names(cfas[[f]])){
#         cor.lv[[f]] <- lavaan::lavInspect(cfas[[f]][[mnames[[n]]]], "cor.lv")
#         cor.flag[[f]] <- rowSums(cor.lv[[f]] > flag) - 1 # minus diagonal which will always be TRUE
#       }
#     }
#
#     ## count of folds with a correlation over flag threshold
#     fc <- unlist(cor.flag)
#     cflag <- tapply(fc, names(fc), function(x) sum(x > 0)) # for each factor
#     kflag[[n]] <- sum(unlist(lapply(cor.flag, sum)) > 0)      # for the model
#
#     ## mean correlation across folds
#     cor.lv <- cor.lv[lengths(cor.lv) != 0] # removes NULL elements for folds were model was not run
#     aggcorrs <- Reduce(`+`, lapply(cor.lv, kfa:::r2z)) / length(cor.lv) # pool results after Fisher's r to z
#     aggcorrs <- kfa:::z2r(aggcorrs)
#     diag(aggcorrs) <- 1       # change diagonals from NaN to 1
#     aggcorrs[upper.tri(aggcorrs, diag = FALSE)] <- NA
#     aggcorrs <- cbind(data.frame(rn = row.names(aggcorrs)), aggcorrs)
#     # cflag names are alphabetical, rather than original factor order so must merge with sort = F
#     aggcorrs <- merge(aggcorrs, data.frame(rn = names(cflag), flag = cflag),
#                       by = "rn", all = TRUE, sort = FALSE)
#     kcorrs[[n]] <- aggcorrs
#   }
#   }
#
# aggc <- list(correlations = kcorrs,
#             flag = kflag)
#
#
#
# ############################################################
#
#
# # ------- Testing agg_rels ----------------------------
#
# models <- cl.example
# flag = .60
# digits = 2
#
# cfas <- models$cfas
# mnames <- models$model.names # model names
# m <- length(mnames) #max(unlist(lapply(cfas, length))) # maximum number of models per fold
# k <- length(cfas) # number of folds
#
# # semTools::reliability does not currently calculate alpha from summary stats with categorical indicators
# what <- if(lavaan::lavInspect(cfas[[1]][[1]], "categorical")) "omega3" else c("omega3", "alpha")
#
# krels <- vector("list", m)
# names(krels) <- mnames
# kflag <- vector("integer", m)
# names(kflag) <- mnames
# for(n in 1:m){
#   rels <- vector("list", k)
#   rel.flag <- vector("integer", k)
#   for(f in 1:k){
#     if(mnames[[n]] %in% names(cfas[[f]])){
#       rels[[f]] <- t(suppressMessages(semTools::reliability(cfas[[f]][[mnames[[n]]]], what = what)))
#       rel.flag[[f]] <- sum(rels[[f]][,"omega3"] < flag) # flag based on omega, not alpha
#     }
#   }
#
#   ## mean reliability across folds
#   aos <- do.call("rbind", rels)
#   if(n == 1){
#     fn <- "f1"
#     fnames <- rep(fn, nrow(aos))
#   } else{
#     rels <- rels[lengths(rels) != 0] # removes NULL elements for folds were model was not run
#     fn <- dimnames(rels[[1]])[[1]]  # grabs unique factor names from first fold; should be the same in all folds
#     fnames <- dimnames(aos)[[1]] # should be the equivalent of rep(fn, nrow(aos))
#   }
#
#   rdf <- data.frame(factor = sort(fn)) # need to use sort b/c that is the order tapply will output
#   for(w in 1:length(what)){
#     test <- data.frame(mean = tapply(aos[, w], fnames, mean),
#                        range = paste(format(round(tapply(aos[, w], fnames, min), digits = digits), nsmall = digits), "-",
#                                      format(round(tapply(aos[, w], fnames, max), digits = digits), nsmall = digits)),
#                        flag = tapply(aos[, w], fnames, function(x) sum(x < flag)))
#     names(test) <- paste(what[[w]], names(test), sep = ".")
#     rdf <- cbind(rdf, test)
#
#   }
#   rdf$factor <- factor(rdf$factor, levels = fn)
#   krels[[n]] <- rdf[order(rdf$factor),]
#
#   ## count of folds with a reliabilities below flag threshold
#   kflag[[n]] <- sum(rel.flag > 0)
# }
#
# aggr <- list(reliabilities = krels,
#             flag = kflag)
#
# #############################################################
#
#
# # -------   Testing model_flags   -------------------
#
# models <- cl.example
# strux <- kstructures
# loads <- klambdas
# cors <- kcorrs
# rels <- krels
#
# # Multiple structures from EFA
# strux <- strux[c("model", "folds")]
# names(strux) <- c("model", "mode structure")
#
# # flags from CFA models
# cfas <- models$cfas
# k <- length(cfas)
# m <- max(unlist(lapply(cfas, length)))
#
# ## Flagging non-convergence and non-positive definite matrix warnings
# cnvgd <- vector("list", k)
# hey <- vector("list", k)
# for(f in 1:k){
#
#   ## convergence status of each model
#   cnvgd[[f]] <- lapply(cfas[[f]], lavaan::lavInspect, "converged")
#   ## presence of non-positive definite matrix (and heywood cases) in each model
#   hey[[f]] <- lapply(cfas[[f]], lavaan::lavInspect, "post.check")
#
# }
#
# # flagged if either indicates an improper solution
# temp <- c(unlist(cnvgd) == FALSE, unlist(hey) == FALSE)
# improper <- tapply(temp, names(temp), sum) # orders output alphabetically
# # adjust order to match model.names and other flags
# improper <- improper[order(factor(names(improper), levels=models$model.names))]
#
# ## joining flags into data.frame
# flags <- data.frame(model = models$model.names,
#                     `improper solution` = improper,
#                     `heywood item` = loads$heywood,
#                     `low loading` = loads$flag,
#                     `high factor correlation` = cors$flag,
#                     `low scale reliability` = rels$flag,
#                     check.names = FALSE)
#
# flags <- merge(strux, flags, by = "model", all.x = FALSE, all.y = TRUE, sort = FALSE)
#
#
# #############################################################
#
#
# # -------    Testing within efa_cfa_syntax   ------------
#
# loadings <- dfa$loadings[[2]]
# simple = FALSE
# threshold = .3
# single.item = "keep"
# identified = TRUE
# constrain0 = TRUE
#
# if(simple == FALSE & is.na(threshold)){
#   stop("threshold must be supplied when simple = FALSE")
# }
#
# # item and factor names
# if(is.null(dimnames(loadings))){
#
#   vnames <- paste0("v", 1:dim(loadings)[[1]]) # variable
#   fnames <- paste0("f", 1:dim(loadings)[[2]]) # factor
#   dimnames(loadings) <- list(vnames, fnames) # only really needed when identified == TRUE
#
# } else {
#
#   vnames <- dimnames(loadings)[[1]] # variable
#   fnames <- dimnames(loadings)[[2]] # factor
# }
#
# # obtaining pattern matrix with NAs elsewhere
# loadings.max <- loadings
# if(simple == TRUE){
#   # largest (absolute) loading for each item
#   maxload <- apply(abs(loadings), 1, max)
#   for(v in 1:length(vnames)){
#
#     thresh <- max(maxload[[v]], threshold, na.rm = TRUE)
#     loadings.max[v, ][abs(loadings.max[v, ]) < thresh] <- NA
#   }
# } else{
#   loadings.max <- t(apply(loadings.max, 1, function(x) ifelse(abs(x) < threshold, NA, x)))
# }
#
# # if not rotationally unique, "" is returned
# if(identified == TRUE){
#
#   # list of variable names for each factor
#   all.items <- apply(loadings.max, 2, function(x) names(x[!is.na(x)]), simplify = FALSE)
#
#   # is
#   id.check <- vector("logical", length(all.items))
#   for(i in 1:length(all.items)){
#     id.check[[i]] <- any(!all.items[[i]] %in% unlist(all.items[-i]))
#   }
#   if(!all(id.check)){
#     cfa.syntax <- ""
#     return(cfa.syntax)
#   }
# }
#
# if(constrain0 == TRUE){
#   # Any loadings below threshold on all factors?
#   dropped <- which(rowSums(is.na(loadings.max)) == ncol(loadings.max))
#   add0 <- paste(paste0("0*", names(dropped)), collapse = " + ")
# } else {
#   add0 <- vector("character", 0)
# }
#
# # returns vector with each element being the lavaan syntax identifying the factor
# cfa.syntax <- c()
# for(fn in 1:length(fnames)){
#   cfa.syntax <- c(cfa.syntax,
#                   paste0(fnames[[fn]], " =~ ",
#                          paste(vnames[!is.na(loadings.max[,fn])],
#                                collapse = " + ")))
#   if(fn == 1 & constrain0 == TRUE){
#     cfa.syntax <- if(length(add0) > 0) paste(cfa.syntax, "+", add0) else cfa.syntax
#   }
# }
#
# # What to do with single item factors?
# if(length(single.item) > 1){
#   single.item <- "keep"
# }
# if(single.item == "keep"){
#
#   # final cfa syntax
#   cfa.syntax <- paste(cfa.syntax, collapse = "\n")
#
# } else if(single.item == "drop"){
#
#   # drops single item factors before collapsing to final syntax
#   cfa.syntax <- cfa.syntax[nchar(cfa.syntax) - nchar(gsub("+", "", cfa.syntax,
#                                                           fixed = TRUE)) > 0]
#   cfa.syntax <- paste(cfa.syntax, collapse = "\n")
#
# } else if(single.item == "none"){
#
#   # check if they exist
#   if(all(nchar(cfa.syntax) - nchar(gsub("+", "", cfa.syntax,fixed = TRUE)) > 0) == TRUE){
#
#     cfa.syntax <- paste(cfa.syntax, collapse = "\n")
#
#   } else {
#     cfa.syntax <- ""
#   }
# }
#
#
# #################################################################
#
#
# # -------------------  run_efa  -------------------------
#
#
# variables = sim.data
# m = 3
# simple = FALSE
# threshold = .3
# single.item = "keep"
# estimator = NULL
# ordered = FALSE
# rotation = "oblimin"
# missing = "listwise"
#
# # The ordered = TRUE functionality not available in lavCor (i.e., not currently equivalent to listing
# # all items), so need to do it manually since I want this functionality for our users
# if(is.logical(ordered)){
#   if(ordered == FALSE){
#     ordered <- NULL
#     if(is.null(estimator)){
#       estimator <- "MLMVS"
#     }
#   } else if(ordered == TRUE){
#     ordered <- names(variables)
#     if(is.null(estimator)){
#       estimator <- "WLSMV"
#     }
#   }
# } else if(is.character(ordered)){
#   if(is.null(estimator)){
#     estimator <- "WLSMV"
#   }
# }
#
# ## calculate and extract sample statistics
# sampstats <- kfa:::sample_stats(variables = variables,
#                           ordered = ordered,
#                           estimator = estimator,
#                           missing = missing)
#
#
# ## Running EFAs, comparing models, converting structure to CFA syntax
# # results objects
# lav.objects <- vector(mode = "list", length = m)
# efa.loadings <- vector(mode = "list", length = m)
# # mod.compare <- data.frame()
#
# for(nf in 1:m){
#
#   ## write efa syntax
#   efa.mod <- write_efa(nf = nf, vnames = names(variables))
#
#   lav.objects[[nf]] <- lavaan::cfa(model = efa.mod,
#                                    sample.cov = sampstats$cov,
#                                    sample.nobs = sampstats$nobs,
#                                    sample.th = sampstats$th,
#                                    WLS.V = sampstats$wls.v,
#                                    NACOV = sampstats$nacov,
#                                    std.lv = TRUE,
#                                    orthogonal = TRUE,
#                                    estimator = estimator,
#                                    missing = missing,
#                                    parameterization = "delta",
#                                    se = "none",
#                                    test = "none")
#
#   # list of unrotated factor loadings
#   efa.loadings[[nf]] <- get_std_loadings(lav.objects[[nf]], type = "std.all")
#   #lavaan::lavInspect(lav.objects[[nf]], "est")$lambda
# }
#
# # NOTE: Rotation section is different than k_efa rotation section b/c m = 1 model is run here
# ## if chosen, applying rotation to standardized factor loadings for models where m > 1
# # oblique rotations
# if(rotation %in% c("oblimin", "oblimax", "quartimin",
#                    "targetQ", "pstQ", "simplimax",
#                    "bentlerQ", "geominQ", "cfQ",
#                    "infomaxQ", "bifactorQ")){
#
#   f <- function(x){
#     try <- tryCatch(expr = GPArotation::GPFoblq(x, method = rotation)$loadings,
#                     error = function(e) return(NA))
#     out <- if(is.logical(try)) x else try
#     return(out)
#   }
#   loadings <- c(list(efa.loadings[[1]]), lapply(efa.loadings[-1], f))
#
#   # orthogonal rotations
# } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
#                           "bentlerT", "tandemI", "tandemII",
#                           "geominT", "cfT", "infomaxT",
#                           "mccammon", "bifactorT")){
#
#   f <- function(x){
#     try <- tryCatch(expr = GPArotation::GPForth(x, method = rotation)$loadings,
#                     error = function(e) return(NA))
#     out <- if(is.logical(try)) x else try
#     return(out)
#   }
#   loadings <- c(list(efa.loadings[[1]]), lapply(efa.loadings[-1], f))
#
# } else {
#   loadings <- efa.loadings
#   message("Reporting unrotated factor loadings")
# }
#
# # converting efa results to cfa syntax
# cfa.syntax <- lapply(loadings, function(x){
#   efa_cfa_syntax(loadings = x,
#                  simple = simple,
#                  threshold = threshold,
#                  single.item = single.item)
# })
#
# efaout <- list(efas = lav.objects,
#                loadings = loadings,
#                cfa.syntax = cfa.syntax)
# # mod.compare = mod.compare, # data.frame of model comparisons results
#
#
# ##########################################################################3
#
#
# #--------     Plotting     ----------------------------
#
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
