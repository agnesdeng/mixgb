library(data.table)
library(xgboost)

# --- 1. Prepare data ---
dt <- as.data.table(iris)

# Introduce some missing values randomly in Sepal.Length
set.seed(123)
na_idx <- sample(1:nrow(dt), 20)
dt[na_idx, Sepal.Length := NA]

# --- 2. Define predictor matrix and response ---
predictors <- setdiff(names(dt), "Species")
response <- "Species"

# Split observed vs missing
obs_idx <- which(!is.na(dt[[response]]))
mis_idx <- which(is.na(dt[[response]])) # None missing in Species here, just for demonstration

obs_data <- dt[obs_idx, ..predictors]
obs_label <- as.integer(dt[obs_idx, get(response)]) - 1  # xgboost uses 0-index

# --- 3. Create XGBoost DMatrix ---
dtrain <- xgb.DMatrix(data = as.matrix(obs_data), label = obs_label)

# --- 4. Set XGBoost parameters ---
params <- list(
  objective = "multi:softprob",   # probability output for multi-class
  num_class = length(unique(obs_label)),
  max_depth = 3,
  eta = 0.3,
  nthread = 2
)

# --- 5. Train XGBoost ---
xgb_fit <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 50,
  verbose = 0
)


predict(xgb_fit, dtrain)
predict(xgb_fit, dtrain,reshape=TRUE)

# --- 6. Predict probabilities (without reshape) ---
yhatprob <- predict(xgb_fit, dtrain)           # returns a vector
yhatprob <- matrix(yhatprob, ncol = params$num_class, byrow = TRUE)

# --- 7. Convert probabilities to class labels ---
yhat_class <- max.col(yhatprob)               # indices of max probability
yhat_class <- levels(dt[[response]])[yhat_class]

# --- 8. Compare predicted vs observed ---
table(predicted = yhat_class, actual = dt$Species[obs_idx])
