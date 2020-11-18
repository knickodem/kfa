#### kfa package tests ####

## Import data
student <- read.csv("student_survey-latest.csv")
# extract items
items <- student[ ,grepl("^a11", names(student))]

rotation = "oblimin"
k = NULL
rmsea0 = .05
rmseaA = .08
m = floor(ncol(items) / 4)
threshold = NA # might not make threshold an argument
ordered = TRUE
missing = "listwise"

## set seed to get the same folds
set.seed(936639)

#### part of kfold_fa ####

if(!is.null(ordered)){
  if(ordered == TRUE){
    ordered <- names(items)
  }
}

if(is.null(k)){
  
  ## determine number of folds based on power analysis
  k <- findk(items = items, m = m, rmsea0 = rmsea0, rmseaA = rmseaA)
}

trainfolds <- caret::createFolds(y = 1:nrow(items),
                                 k = k, list = TRUE,
                                 returnTrain = TRUE)

lrttest <- run_efa(items = items[trainfolds[[1]], ],
                   extract.method = "lrt",
                   rotation = "oblimin",
                   ordered = ordered, missing = missing)

consensustest <- run_efa(items = items[trainfolds[[1]], ],
                         extract.method = "consensus",
                         rotation = "oblimin",
                         ordered = ordered, missing = missing)
