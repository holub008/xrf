# In November 2025, the new version of xgboost has api changes
# that are breaking changes. See https://github.com/dmlc/xgboost/issues/11430
# This file uses the current CRAN versions to ensure that prediction
# works in updated versions of xrf and xgboost.

test_that('prediction works on xgboost versions < 3.0', {
  load(system.file("xgb_1_7_11.RData", package = "xrf"), verbose = FALSE)
  xgb_3_1_2_1_pred <- predict(xgb_1_7_11_fit, mtcars[29:32, ])

  expect_equal(xgb_1_7_11_pred, xgb_3_1_2_1_pred)
})
