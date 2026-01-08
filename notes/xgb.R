library(xgboost)
data("ToothGrowth")
y <- ToothGrowth$supp
x <- ToothGrowth[, -2L]
model <- xgboost(x, y, nthreads = 1L, nrounds = 3L, max_depth = 2L)

pred_prob <- predict(model, x[1:5, ], type = "response")
pred_prob
pred_raw <- predict(model, x[1:5, ], type = "raw")
pred_raw
pred_class <- predict(model, x[1:5, ], type = "class")
pred_class

# Relationships between these
manual_probs <- 1 / (1 + exp(-pred_raw))
manual_class <- ifelse(manual_probs < 0.5, levels(y)[1], levels(y)[2])

# They should match up to numerical precision
round(pred_prob, 6) == round(manual_probs, 6)
pred_class == manual_class
