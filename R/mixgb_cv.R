#' Use cross-validation to find the optimal \code{nrounds}
#' @description Use cross-validation to find the optimal \code{nrounds} for an \code{Mixgb} imputer. Note that this method relies on the complete cases of a dataset to find the optimal \code{nrounds}.
#' @param data A data.frame or a data.table with missing values.
#' @param nfold The number of subsamples which are randomly partitioned and of equal size. Default: 5
#' @param nrounds The max number of iterations in XGBoost training. Default: 100
#' @param early_stopping_rounds An integer value \code{k}. Training will stop if the validation performance hasn't improved for \code{k} rounds.
#' @param response The name or column index of a response variable. Default: \code{NULL} (Randomly select an incomplete variable).
#' @param select_features The names or indices of selected features. Default: \code{NULL} (Select all other variables in the dataset).
#' @param stringsAsFactors A logical value indicating whether character vectors should be converted to factors.
#' @param verbose A logical value. Whether to print out cross-validation results during the process.
#' @param ... Extra arguments to pass to XGBoost.
#' @return A list of the optimal \code{nrounds}, \code{evaluation.log} and the chosen \code{response}.
#' @export
#' @examples
#' cv.results <- mixgb_cv(data = nhanes3)
#' cv.results$best.nrounds
#'
#' imputed.data <- mixgb(data = nhanes3, m = 5, nrounds = cv.results$best.nrounds)
mixgb_cv <- function(data, nfold = 5, nrounds = 100, early_stopping_rounds = 10, response = NULL, select_features = NULL, stringsAsFactors = FALSE, verbose = TRUE, ...) {
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

  na.col <- which(colSums(is.na(data)) != 0)

  if (is.null(response)) {
    # r.idx <- sample(1:ncol(cc.data), size = 1)
    r.idx <- sample(na.col, size = 1)
    response <- Names[r.idx]
    response
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

  if (Types[response] == "numeric" | Types[response] == "integer") {
    obj.type <- "reg:squarederror"
    # 1 row vectoc
    obs.y <- cc.data[[response]]
    cv.train <- xgb.cv(data = obs.data, label = obs.y, objective = obj.type, nrounds = nrounds, nfold = nfold, early_stopping_rounds = early_stopping_rounds, verbose = verbose, ...)
  } else if (Types[response] == "binary") {
    obj.type <- "binary:logistic"
    eval_metric <- "logloss"
    obs.y <- as.integer(cc.data[[response]]) - 1
    cv.train <- xgb.cv(data = obs.data, label = obs.y, objective = obj.type, eval_metric = eval_metric, nrounds = nrounds, nfold = nfold, early_stopping_rounds = early_stopping_rounds, verbose = verbose, ...)
  } else {
    obj.type <- "multi:softmax"
    eval_metric <- "mlogloss"
    obs.y <- as.integer(cc.data[[response]]) - 1
    N.class <- length(levels(cc.data[[response]]))
    cv.train <- xgb.cv(data = obs.data, label = obs.y, num_class = N.class, objective = obj.type, eval_metric = eval_metric, nrounds = nrounds, nfold = nfold, early_stopping_rounds = early_stopping_rounds, verbose = verbose, ...)
  }

  evaluation.log <- cv.train$evaluation_log
  best.nrounds <- cv.train$best_iteration
  return(list("best.nrounds" = best.nrounds, "evaluation.log" = evaluation.log, "response" = response))
}
