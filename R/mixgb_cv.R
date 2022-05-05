#' Use cross-validation to find the optimum nrounds
#' @param data a data.frame or a data.table with missing values
#' @param reponse name or column index of the response variable. Default: NULL.
#' @param select_features names or indices of the selected features. Default: NULL (select all features)
#' @export


mixgb_cv <- function(data, nfold = 5, nrounds = 100, early_stopping_rounds = 10, response = NULL, select_features = NULL, stringsAsFactors = FALSE, ...) {
  num.cc <- sum(complete.cases(data))


  if (num.cc == 0) {
    stop("No complete cases in this dataset.")
  }

  if (num.cc > 0 & num.cc < 10) {
    warnings("Less than 10 complete cases. Results may not be reliable.")
  }

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }

  if (stringsAsFactors == TRUE) {
    data <- data.table(data, stringsAsFactors = TRUE)
  }

  cc.data <- data[complete.cases(data), ]
  Names <- colnames(cc.data)
  Types <- feature_type(cc.data)

  if (any(Types == "character")) {
    stop("This datset contains variables with character type. Please set stringsAsFactors = TRUE")
  }



  if (is.null(response)) {
    r.idx <- sample(1:ncol(cc.data), size = 1)
    response <- Names[r.idx]
  } else if (!is.character(response)) {
    response <- Names[response]
  }


  if (is.null(select_features)) {
    select_features <- setdiff(Names, response)
  } else if (!is.character(select_features)) {
    select_features <- Names[select_features]
  }

  p <- length(select_features) + 1
  if (p == 2) {
    obs.data <- sparse.model.matrix(reformulate(select_features, response), data = cc.data)
  } else {
    obs.data <- sparse.model.matrix(reformulate(select_features, response), data = cc.data)[, -1]
  }

  if (Types[response] == "numeric") {
    obj.type <- "reg:squarederror"
    # 1 row vectoc
    obs.y <- cc.data[[response]]
    cv.train <- xgb.cv(data = obs.data, label = obs.y, objective = obj.type, nrounds = nrounds, nfold = nfold, early_stopping_rounds = early_stopping_rounds, ...)
  } else if (Types[response] == "binary") {
    obj.type <- "binary:logistic"
    obs.y <- as.integer(cc.data[[response]]) - 1
    cv.train <- xgb.cv(data = obs.data, label = obs.y, objective = obj.type, nrounds = nrounds, nfold = nfold, early_stopping_rounds = early_stopping_rounds, ...)
  } else {
    obj.type <- "multi:softmax"
    obs.y <- as.integer(cc.data[[response]]) - 1
    N.class <- length(levels(cc.data[[response]]))
    cv.train <- xgb.cv(data = obs.data, label = obs.y, num_class = N.class, objective = obj.type, nrounds = nrounds, nfold = nfold, early_stopping_rounds = early_stopping_rounds, ...)
  }



  evaluation.log <- cv.train$evaluation_log
  best.nrounds <- cv.train$best_iteration
  return(list("best.nrounds" = best.nrounds, "evaluation.log" = evaluation.log))
}
