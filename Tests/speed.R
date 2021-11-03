# #############################################
# #                                            #
# #        kfa package speed benchmarks        #
# #                                            #
# ##############################################
#
# #### Import data ####
# shortfile <- c("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/EFA&CFA/")
# student <- read.csv(paste0(shortfile, "scaledim/student_survey-latest.csv"))
# ## item map
# itemmaps <- lapply(c("Coach_survey", "Teacher Survey",
#                      "Principal Survey", "Student Survey"), function(x){
#                        readxl::read_excel(paste0(shortfile, "Item construct map_2020_10_25.xlsx"),
#                                           sheet = x)})
# names(itemmaps) <- c("coach", "teacher", "principal", "student")
#
# ## items for analysis
# studentdf <- student[ ,names(student) %in% na.omit(itemmaps$student$`Variable Name`)]
#
#
#
#
# # ---- Time to conduct 10 runs with kfa package ----------------
# for(i in 1:10){
#   tictoc::tic()
#   set.seed(936639) # set seed to reproduce same folds
#
#   kstudent <- kfa(variables = studentdf,
#                   k = 10,
#                   m = 5,
#                   # custom.cfas = list(`Custom 2f` = custom2, Break = custom3),
#                   rotation = "oblimin",
#                   ordered = TRUE,
#                   estimator = "DWLS",
#                   missing = "pairwise")
#   tictoc::toc(log = TRUE)# ~ 60 seconds
# }
# kfa.log <- tictoc::tic.log()
#
#
#
# # ---- Time to conduct 10 runs naively ----------------
# variables <- studentdf
# k <- 10
# m <- 5
# rotation <- "oblimin"
# ordered <- TRUE
# estimator <- "DWLS"
# missing <- "pairwise"
#
#
#
# for(i in 1:10){
# tictoc::tic()
# set.seed(936639)
# testfolds <- caret::createFolds(y = 1:nrow(variables),
#                                 k = k, list = TRUE,
#                                 returnTrain = FALSE)
#
# cfas <- vector(mode = "list", k)
# for(fold in 1:k){
#   ## Running EFAs (no need to run 1-factor b/c we already know the structure)
#   efa.loadings <- vector(mode = "list", length = m)
#
#   for(nf in 2:m){
#
#     ## write efa syntax
#     efa.mod <- write_efa(nf = nf, vnames = names(variables))
#
#     unrotated <- lavaan::cfa(model = efa.mod,
#                              data = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
#                              # sample.cov = sample.cov,
#                              # sample.nobs = sample.nobs,
#                              # sample.mean = sample.mean,
#                              # sample.th = sample.th,
#                              # WLS.V = WLS.V,
#                              # NACOV = NACOV,
#                              std.lv = TRUE,
#                              orthogonal = TRUE,
#                              estimator = estimator,
#                              missing = missing,
#                              parameterization = "delta",
#                              se = "none",
#                              test = "none")
#
#     # list of unrotated factor loadings
#     efa.loadings[[nf]] <- lavaan::lavInspect(unrotated, "est")$lambda
#   }
#
#   ## if chosen, applying rotation to standardized factor loadings for models where m > 1
#   # oblique rotations
#   if(rotation %in% c("oblimin", "oblimax", "quartimin",
#                      "targetQ", "pstQ", "simplimax",
#                      "bentlerQ", "geominQ", "cfQ",
#                      "infomaxQ", "bifactorQ")){
#
#     loadings <- lapply(efa.loadings[-1], function(x){
#       GPArotation::GPFoblq(x, method = rotation)$loadings
#     })
#
#     # orthogonal rotations
#   } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
#                             "bentlerT", "tandemI", "tandemII",
#                             "geominT", "cfT", "infomaxT",
#                             "mccammon", "bifactorT")){
#
#     loadings <- lapply(efa.loadings[-1], function(x){
#       GPArotation::GPForth(x, method = rotation)$loadings
#     })
#
#   } else {
#     loadings <- efa.loadings[-1]
#     message("Reporting unrotated factor loadings")
#   }
#
#   # converting efa results to cfa syntax
#   cfa.syntax <- lapply(loadings, function(x){
#     efa_cfa_syntax(loadings = x,
#                    simple = TRUE,
#                    threshold = NA,
#                    single.item = "none")
#   })
#
#   ## adding the 1-factor model as first element in cfa syntax list
#   onefac <- paste0("f1 =~ ", paste(names(variables), collapse = " + "))
#   cfa.syntax <- c(list(onefac), cfa.syntax)
#
#   cfa <- cfa <- vector(mode = "list", length = length(cfa.syntax))
#   for(c in 1:length(cfa.syntax)){
#
#     if(nchar(cfa.syntax[[c]]) > 0){
#
#       fit <- lavaan::cfa(model = cfa.syntax[[c]],
#                          data = variables[testfolds[[fold]], ],
#                          # sample.cov = sample.cov,
#                          # sample.nobs = sample.nobs,
#                          # sample.mean = sample.mean,
#                          # sample.th = sample.th,
#                          # WLS.V = WLS.V,
#                          # NACOV = NACOV,
#                          estimator = estimator,
#                          parameterization = "delta")
#
#       cfa[[c]] <- fit
#
#     } else {
#
#       cfa[[c]] <- NULL
#     }
#
#   }
#   cfas[[fold]] <- cfa
# }
# tictoc::toc(log = TRUE) # mean = 114.7
# }
# naive.log <- tictoc::tic.log()
# time.comp <- data.frame(Naive = as.numeric(gsub(" sec elapsed", "", unlist(naive.log), fixed = TRUE)),
#                          kfa = as.numeric(gsub(" sec elapsed", "", unlist(naive.log), fixed = TRUE)))
#
# save(naive.log, file = "Tests/time.RData")
#
#
# tictoc::tic.clearlog()
# # ---- Using correlation matrix in lavaan -----------------
# tictoc::tic()
# kefa <- k_efa(variables = items,
#               m = 5,
#               rotation = "oblimin",
#               ordered = names(items),
#               estimator = "DWLS",
#               missing = "pairwise")
# tictoc::toc() # ~10 sec
#
# test <- as.data.frame(lapply(items, as.ordered))
#
# microbenchmark::microbenchmark(
#   ## polychoric correlation matrix
#   prelim = lavaan::lavCor(object = items,
#                           ordered = names(items),
#                           estimator = "DWLS",  # must specify estimator, otherwise defaults to none
#                           missing = "pairwise",
#                           output = "fit",
#                           cor.smooth = FALSE), # could include a cluster =  argument
#   poly = polycor::hetcor(data = as.data.frame(lapply(items,as.ordered)),
#                          use = "pairwise.complete.obs",
#                          std.err = FALSE),
#   times = 20)
#
# prelim <- lavaan::lavCor(object = items,
#                          ordered = names(items),
#                          estimator = "DWLS",  # must specify estimator, otherwise defaults to none
#                          missing = "pairwise",
#                          meanstructure = TRUE,
#                          output = "fit",
#                          cor.smooth = FALSE)
#
#
# start <- lavaan::lavInspect(prelim, "start") #
# itrtns <- lavaan::lavInspect(prelim, "iterations")
#
# nobs <- lavaan::lavInspect(prelim, "nobs")
# wlsv <- lavaan::lavInspect(prelim, "wls.v")
# nacov <- lavaan::lavInspect(prelim, "gamma")
# cormat <- lavaan::lavInspect(prelim, "sampstat")$cov # same as lavaan::lavInspect(prelim, "cor.ov")
# means <- lavaan::lavInspect(prelim, "sampstat")$mean
# th <- lavaan::lavInspect(prelim, "sampstat")$th # same values as lavaan::lavInspect(prelim, "est")$tau
# attr(th, "th.idx") <- lavaan::lavInspect(prelim, "th.idx")
#
#
# efa.mod <- write_efa(3, names(items))
#
# modtest <- lavaan::cfa(model = efa.mod,
#                        sample.cov = cormat,
#                        sample.nobs = nobs,
#                        sample.mean = means,
#                        sample.th = th,
#                        WLS.V = wlsv,
#                        NACOV = nacov,
#                        std.lv = TRUE,
#                        orthogonal = TRUE,
#                        estimator = "DWLS",
#                        parameterization = "delta",
#                        se = "none")
#
#
# lavaan::summary(modtest, standardized = TRUE)
# modtestload <- lavaan::lavInspect(modtest, "est")$lambda
#
#
# fatest <- psych::fa(cormat, 3, n.obs = nobs, rotate = "none")
#
#
# ## fit efa with sample statistics
# matonly <- lavaan::cfa(model = st.mod,
#                        sample.cov = cormat,
#                        sample.nobs = nobs,
#                        sample.mean = means,
#                        sample.th = th,
#                        WLS.V = wlsv,
#                        NACOV = nacov,
#                        estimator = "DWLS",
#                        parameterization = "delta",
#                        se = "none")
#
# ## fit with full dataset
# dat <- lavaan::cfa(model = st.mod,
#                    data = items,
#                    estimator = "DWLS",
#                    missing = "pairwise",
#                    ordered = names(items),
#                    parameterization = "delta",
#                    se = "none")
#
# ## Do the matrix/sample stats and data method produce the same results
# all.equal(lavaan::parameterestimates(matonly, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE),
#           lavaan::parameterestimates(dat, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE))
#
# # check <- dplyr::full_join(lavaan::parameterestimates(matonly, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE),
# #           lavaan::parameterestimates(dat, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE),
# #           by = c("lhs", "op", "rhs", "label"), suffix = c(".mat", ".dat"))
#
#
#
# #### Comparing time difference when cor matrix is provided or not ####
# # expr       min       lq     mean   median       uq      max neval
# # mat  7.232165 17.84711 17.84960 18.65208 19.65406 29.44586    20
# # dat 13.979109 32.33991 32.04155 34.46950 36.70216 51.01137    20
#
# inputtype <- microbenchmark::microbenchmark(
#   mat = lavaan::cfa(model = st.mod,
#                     sample.cov = cormat,
#                     sample.nobs = nobs,
#                     sample.mean = means,
#                     sample.th = th,
#                     WLS.V = wlsv,
#                     NACOV = nacov,
#                     estimator = "DWLS",
#                     parameterization = "delta",
#                     se = "none",
#                     test = "none"),
#   dat = lavaan::cfa(model = st.mod,
#                     data = items,
#                     estimator = "DWLS",
#                     missing = "pairwise",
#                     ordered = names(items),
#                     parameterization = "delta",
#                     se = "none",
#                     test = "none"),
#   times = 20
#
# )
#
# ######################################
