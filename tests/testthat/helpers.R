make_mtcars_test <- function() {
  dataset <- mtcars |>
    rbind(mtcars) |> # double the dataset to avoid small class priors for glmnet cross validation
    dplyr::mutate(
      vs = as.factor(vs),
      am = as.factor(am)
    )
}

test_expected_fields <- function(model, depth, trees) {
  dataset <- make_mtcars_test()

  max_splits <- (2^(depth + 1) - 2) * trees # number of edges in a binary tree for each tree = number of nodes - 1
  max_rules <- 2^depth * trees # number of root -> leaf traversals in a binary tree for each tree

  expected_max_coef <- 1 + # intercept
    1 + # continuous mpg
    1 + # continuous displacement
    length(unique(dataset$cyl)) -
    1 +
    max_rules

  expect_setequal(
    c('glm', 'xgb', 'base_formula', 'rule_augmented_formula', 'rules'),
    names(model)
  )
  expect_s3_class(model$glm, 'glmnot')
  expect_s3_class(model$xgb, 'xgb.Booster')
  expect_s3_class(model$base_formula, 'formula')
  expect_s3_class(model$rule_augmented_formula, 'formula')
  expect_s3_class(model$rules, 'data.frame')

  expect_lte(nrow(model$rules), max_splits)
  expect_lte(dplyr::n_distinct(model$rules$rule_id), max_rules)
  expect_lte(nrow(coef(model, lambda = 'lambda.min')), expected_max_coef)
}
