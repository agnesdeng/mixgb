install.packages('xgboost', repos = c('https://dmlc.r-universe.dev', 'https://cloud.r-project.org'))
devtools::load_all()


imputed.data <- mixgb(data = nhanes3_newborn, m = 5)

imputed.data

params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
cv.results <- mixgb_cv(data = nhanes3, xgb.params = params)
cv.results
cv.results$best.nrounds



data(agaricus.train, package = "xgboost")


dtrain <- with(agaricus.train, xgb.DMatrix(data, label = label, nthread = 2))

cv <- xgb.cv(
  data = dtrain,
  nrounds = 20,
  early_stopping_rounds = 1,
  params = list(
    nthread = 2,
    max_depth = 3,
    objective = "binary:logistic"
  ),
  nfold = 5,
  metrics = list("rmse","auc"),
  prediction = TRUE
)
print(cv)
print(cv, verbose = TRUE)

# Callbacks might add additional attributes, separated by the name of the callback
cv$early_stop$best_iteration
head(cv$cv_predict$pred)
