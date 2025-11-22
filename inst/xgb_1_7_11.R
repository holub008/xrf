# pak::pak(c("xrf", "xgboost"), ask = FALSE)
library(xrf)
library(xgboost)
# xgboost_1.7.11.1 xrf_0.2.2

set.seed(832)
xgb_1_7_11_fit <- xrf(mpg ~ ., data = mtcars[1:28, ], family = "gaussian")
xgb_1_7_11_pred <- predict(xgb_1_7_11_fit, mtcars[29:32, ])
save(xgb_1_7_11_fit, xgb_1_7_11_pred, file = "inst/xgb_1_7_11.RData")

if (!interactive()) {
  q("no")
}
