##############################################
#                                            #
#        kfa package speed benchmarks        #
#                                            #
##############################################

#### Import data ####
shortfile <- c("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/EFA&CFA/")
student <- read.csv(paste0(shortfile, "scaledim/student_survey-latest.csv"))
## item map
itemmaps <- lapply(c("Coach_survey", "Teacher Survey",
                     "Principal Survey", "Student Survey"), function(x){
                       readxl::read_excel(paste0(shortfile, "Item construct map_2020_10_25.xlsx"),
                                          sheet = x)})
names(itemmaps) <- c("coach", "teacher", "principal", "student")

## items for analysis
studentdf <- student[ ,names(student) %in% na.omit(itemmaps$student$`Variable Name`)]




# ---- Time to conduct 10 runs with kfa package ----------------
for(i in 1:10){
  tictoc::tic()
  set.seed(936639) # set seed to reproduce same folds

  kstudent <- kfa(variables = studentdf,
                  k = 10,
                  m = 5,
                  # custom.cfas = list(`Custom 2f` = custom2, Break = custom3),
                  rotation = "oblimin",
                  ordered = TRUE,
                  estimator = "DWLS",
                  missing = "pairwise")
  tictoc::toc(log = TRUE)# ~ 60 seconds
}
kfa.log <- tictoc::tic.log()



# ---- Time to conduct 10 runs naively ----------------
variables <- studentdf
k <- 10
m <- 5
rotation <- "oblimin"
ordered <- TRUE
estimator <- "DWLS"
missing <- "pairwise"



for(i in 1:10){
tictoc::tic()
set.seed(936639)
testfolds <- caret::createFolds(y = 1:nrow(variables),
                                k = k, list = TRUE,
                                returnTrain = FALSE)

cfas <- vector(mode = "list", k)
for(fold in 1:k){
  ## Running EFAs (no need to run 1-factor b/c we already know the structure)
  efa.loadings <- vector(mode = "list", length = m)

  for(nf in 2:m){

    ## write efa syntax
    efa.mod <- write_efa(nf = nf, vnames = names(variables))

    unrotated <- lavaan::cfa(model = efa.mod,
                             data = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
                             # sample.cov = sample.cov,
                             # sample.nobs = sample.nobs,
                             # sample.mean = sample.mean,
                             # sample.th = sample.th,
                             # WLS.V = WLS.V,
                             # NACOV = NACOV,
                             std.lv = TRUE,
                             orthogonal = TRUE,
                             estimator = estimator,
                             missing = missing,
                             parameterization = "delta",
                             se = "none",
                             test = "none")

    # list of unrotated factor loadings
    efa.loadings[[nf]] <- lavaan::lavInspect(unrotated, "est")$lambda
  }

  ## if chosen, applying rotation to standardized factor loadings for models where m > 1
  # oblique rotations
  if(rotation %in% c("oblimin", "oblimax", "quartimin",
                     "targetQ", "pstQ", "simplimax",
                     "bentlerQ", "geominQ", "cfQ",
                     "infomaxQ", "bifactorQ")){

    loadings <- lapply(efa.loadings[-1], function(x){
      GPArotation::GPFoblq(x, method = rotation)$loadings
    })

    # orthogonal rotations
  } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
                            "bentlerT", "tandemI", "tandemII",
                            "geominT", "cfT", "infomaxT",
                            "mccammon", "bifactorT")){

    loadings <- lapply(efa.loadings[-1], function(x){
      GPArotation::GPForth(x, method = rotation)$loadings
    })

  } else {
    loadings <- efa.loadings[-1]
    message("Reporting unrotated factor loadings")
  }

  # converting efa results to cfa syntax
  cfa.syntax <- lapply(loadings, function(x){
    efa_cfa_syntax(loadings = x,
                   simple = TRUE,
                   threshold = NA,
                   single.item = "none")
  })

  ## adding the 1-factor model as first element in cfa syntax list
  onefac <- paste0("f1 =~ ", paste(names(variables), collapse = " + "))
  cfa.syntax <- c(list(onefac), cfa.syntax)

  cfa <- cfa <- vector(mode = "list", length = length(cfa.syntax))
  for(c in 1:length(cfa.syntax)){

    if(nchar(cfa.syntax[[c]]) > 0){

      fit <- lavaan::cfa(model = cfa.syntax[[c]],
                         data = variables[testfolds[[fold]], ],
                         # sample.cov = sample.cov,
                         # sample.nobs = sample.nobs,
                         # sample.mean = sample.mean,
                         # sample.th = sample.th,
                         # WLS.V = WLS.V,
                         # NACOV = NACOV,
                         estimator = estimator,
                         parameterization = "delta")

      cfa[[c]] <- fit

    } else {

      cfa[[c]] <- NULL
    }

  }
  cfas[[fold]] <- cfa
}
tictoc::toc(log = TRUE) # mean = 114.7
}
naive.log <- tictoc::tic.log()
time.comp <- data.frame(Naive = as.numeric(gsub(" sec elapsed", "", unlist(naive.log), fixed = TRUE)),
                         kfa = as.numeric(gsub(" sec elapsed", "", unlist(naive.log), fixed = TRUE)))

save(naive.log, file = "Tests/time.RData")


tictoc::tic.clearlog()
