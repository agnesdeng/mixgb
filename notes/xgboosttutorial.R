data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
dtrain <- with(agaricus.train, xgboost::xgb.DMatrix(data, label = label, nthread = 2))
dtest <- with(agaricus.test, xgboost::xgb.DMatrix(data, label = label, nthread = 2))
watchlist <- list(train = dtrain, eval = dtest)
## A simple xgb.train example:
param <- list(max_depth = 2, eta = 1, verbose = 0, nthread = 2,
              objective = "binary:logistic", eval_metric = "auc")
bst <- xgboost::xgb.train(param, dtrain, nrounds = 2, watchlist)


xgb.fit <- xgboost(
  data = obs.data, label = obs.y, objective = obj.type,
  params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, 
  print_every_n = print_every_n, verbose = verbose
)


obj.type <- "reg:squarederror"
system.time({
  xgb.fit <- xgboost(
    data = obs.data, label = obs.y, objective = obj.type,
    params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose)
})

early_stopping_rounds=NULL
dtrain <- xgboost::xgb.DMatrix(data = obs.data, label = obs.y)
system.time({
  bst <- xgboost::xgb.train(data = dtrain, objective = obj.type,
                            params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose)
})
xgboost::xgb.train(data = dtrain, objective = obj.type,watchlist = watchlist,
                   params = xgb.params, nrounds = 500, 
                   early_stopping_rounds = 2, print_every_n = print_every_n, verbose = verbose)