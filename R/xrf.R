#############################################
## functions for preconditions on user input
#############################################

condition_xgb_control <- function(family, xgb_control, data, response_var, prefit_xgb) {
  # this is a duplicated but necessary check
  if (!(response_var %in% colnames(data))) {
    stop(paste0('Response variable "', response_var, '" not present in supplied data'))
  }

  data_mutated <- data

  if (family == 'multinomial' && is.null(xgb_control$num_class) && is.null(prefit_xgb)) {
    n_classes <- n_distinct(data[[response_var]])
    warning(paste0('Detected ', as.character(n_classes), ' classes to set num_class xgb_control parameter'))
    xgb_control$num_class <- n_distinct(data[[response_var]])
  }

  # xgboost expects multinomial labels to be 0:num_class
  if (family == 'multinomial' &&
      (is.factor(data[[response_var]]) || is.character(data[[response_var]]))) {
    integer_response <- as.integer(as.factor(data[[response_var]]))
    data_mutated[[response_var]] <- integer_response - min(integer_response)
  }
  else if (family == 'binomial' &&
           is.factor(data[[response_var]]) || is.character(data[[response_var]])) {
    integer_response <- as.integer(as.factor(data[[response_var]]))
    data_mutated[[response_var]] <- integer_response - min(integer_response)
  }

  list(xgb_control = xgb_control,
       data = data_mutated)
}

xrf_preconditions <- function(family, xgb_control, glm_control,
                              data, response_var, prefit_xgb) {
  supported_families <- c('gaussian', 'binomial', 'multinomial')
  if (!(family %in% supported_families)) {
    stop(paste0('Family "', family, '" is not currently supported. Supported families are: ', paste0(supported_families, collapse = ', ')))
  }

  if (!('nrounds' %in% names(xgb_control))) {
    stop('Must supply an "nrounds" list element to the xgb_control argument')
  }

  if ('objective' %in% names(xgb_control)) {
    stop('User may not supply an "objective" list element to the xgb_control argument')
  }

  if (!(response_var %in% colnames(data))) {
    stop(paste0('Response variable "', response_var, '" not present in supplied data'))
  }

  if (any(is.na(data[[response_var]]))) {
    stop('Response variable contains missing values which is not allowed')
  }

  if (n_distinct(data[[response_var]]) <= 1) {
    # TODO cv.glmnet will still warn/fail when there is a very small number of observations per class for logistic regression
    stop('Response variable shows no variation, model cannot be fit')
  }

  if (family == 'multinomial' &&
      ((is.null(xgb_control$num_class) || n_distinct(data[[response_var]]) != xgb_control$num_class) &&
      is.null(prefit_xgb))) {
    stop('Must supply a num_class list element in xgb_control when using multinomial objective')
  }

  if (length(intersect(c('type.measure', 'nfolds', 'foldid'), names(glm_control))) < 2) {
    stop('Must supply "type.measure" and ("nfolds" or "foldid") as glm_control parameters')
  }

  allowed_tree_ensemble_classes <- c('xgb.Booster')
  if (!is.null(prefit_xgb) && length(intersect(allowed_tree_ensemble_classes, class(prefit_xgb))) == 0) {
    stop('Prefit tree ensemble must be of class {', paste0(allowed_tree_ensemble_classes, collapse = ','), "}")
  }

  features_with_commas <- grepl(',', colnames(data), fixed = TRUE)
  if (any(features_with_commas)) {
    feature_names <- colnames(data)[features_with_commas]
    stop(paste0('The following column names contain illegal characters: ', paste0("'", features_with_commas, "'", collapse = ',')))
  }

}

## the choice of ensemble loss is currently hidden from the api to protect implementation details
## this may be exposed to the user in the future
get_xgboost_objective <- function(family) {
  if (family == 'gaussian') {
    return('reg:squarederror')
  }
  else if (family == 'binomial') {
    return('binary:logistic')
  }
  else if (family == 'multinomial') {
    return('multi:softmax')
  }

  stop(paste0('Unrecognized family ', family, ' which should have failed fast in preconditions'))
}

#############################################
## functions for extracting xgboost rule sets
#############################################

augment_rules <- function(row, rule_ids, less_than) {
  bind_rows(
    lapply(rule_ids, function(rule_id) {
      list(
        split_id = row$ID,
        rule_id = rule_id,
        feature = row$Feature,
        split = row$Split,
        less_than = less_than)}
      )
  )
}

# this is of course slow, but it shouldn't be a bottleneck due to ensembles generally small and tree depth < 6
rule_traverse <- function(row, tree) {
  if (row$Feature == 'Leaf') {
    return(data.frame(
      split_id = row$ID,
      rule_id = paste0('r', gsub('-', '_', row$ID)), # leaf nodes uniquely identify a rule
      feature = NA,
      split = NA,
      less_than = NA,
      stringsAsFactors = FALSE))
  }
  else {
    # the Yes/No obfuscates the simplicity of the algo - in order tree traversal
    left_child <- tree[tree$ID == row$Yes,]
    stopifnot(nrow(left_child) == 1) # this can be trusted from xgboost, but fail if that changes
    right_child <- tree[tree$ID == row$No,]
    stopifnot(nrow(right_child) == 1)

    # recursion will bubble up the conjunctive rule to this split
    left_rules <- rule_traverse(left_child, tree)
    right_rules <- rule_traverse(right_child, tree)

    left_rules_augmented <- augment_rules(row, unique(left_rules$rule_id), less_than = TRUE)
    right_rules_augmented <- augment_rules(row, unique(right_rules$rule_id), less_than = FALSE)

    return(rbind(left_rules_augmented, right_rules_augmented, left_rules, right_rules,
                 stringsAsFactors = FALSE))
  }
}

# note that xgboost produces nrounds * classes number of trees for multi-class classification - we proceed by extracting all rules from all trees, treating them equally
# this function produces a data frame with columns rule_id (which idenitifies a total conjunction), feature, split, and less_than
# each row represents an individual boolean clause evaluated by feature (less_than ? < : >=) split
extract_xgb_rules <- function(m) {
  rules <- xgb.model.dt.tree(model = m) %>%
    group_by(.data$Tree) %>%
    arrange(.data$Node) %>% # put the root at the top of each tree group
    do(harvested_rules = rule_traverse(.data[1, ], .data) %>%
         filter(!is.na(.data$feature))) %>%
    pull(.data$harvested_rules) %>%
    lapply(drop_zero_row_tbl) %>%
    bind_rows()

  rules
}

drop_zero_row_tbl <- function(tbl) {
  if (nrow(tbl) == 0) {
    return(NULL)
  }

  tbl
}


##################################################
## functions for parsing out model matrix features
##################################################

build_feature_metadata <- function(data) {
  all_features <- data.frame(feature_name = colnames(data),
                             stringsAsFactors = FALSE)

  feature_metadata <- all_features %>%
    mutate(
      is_continuous = sapply(.data$feature_name, function(fname){ is.numeric(data[[fname]]) })
    )

  xlev <- data %>%
    select_if(function(x) { !is.numeric(x) }) %>%
    lapply(function(x) {
      if(is.factor(x)) levels(x) else as.character(unique(x))
    })

  list(
    xlev = xlev,
    feature_metadata = feature_metadata
  )
}

has_matching_level <- function(feature_name, level_remainder, xlev) {
  for (ix in seq_along(feature_name)) {
    fn <- feature_name[ix]
    lr <- level_remainder[ix]

    if (lr %in% xlev[[fn]]) {
      return(TRUE)
    }
  }

  return(FALSE)
}

correct_xgb_sparse_categoricals <- function(rules, feature_metadata, xlev,
                                            # .5 matches what xgboost does with dense matrices
                                            categorical_split_value = .5) {
  if (nrow(rules) == 0) {
    return(rules)
  }

  for (row_ix in 1:nrow(rules)) {
    feature_level <- rules[row_ix, 'feature']
    classified_features <- feature_metadata %>%
      mutate(
        level_remainder = sapply(.data$feature_name, function(fn){ lstrip(feature_level, fn) }),
        may_be_rule_feature = sapply(.data$feature_name, function(fn) { !startsWith(feature_level, fn) })
      )

    feature_level_matches <- classified_features %>%
      filter(!.data$may_be_rule_feature) %>%
      filter(.data$level_remainder == '' | has_matching_level(.data$feature_name, .data$level_remainder, xlev))

    if (nrow(feature_level_matches) > 1) {
      # this means that several feaures and their levels may be concatenated to produce the same column name
      # e.g. feature "ora" with level "nge" and another feature "oran" with level "ge". or even a continuous with name "orange"
      stop(paste0('In attempting to parse sparse design matrix columns, several feature/level matches found for: "', feature_level, '". Conservatively failing to user to change feature/level names or use dense matrices.'))
    }
    else if (nrow(feature_level_matches) == 0) {
      # the feature couldn't be found. this is usually because a transformation was applied via the formula
      stop(paste0('In attempting to parse sparse design matrix columns, no feature/level matches found for: "', feature_level, '". This is often caused by supplying a transformation in the input formula. User may either transform source data and use main effects only formula or set argument sparse=FALSE.'))
    }

    if (!feature_level_matches$is_continuous) {
      # xgb always makes the split value negative, so that "Missing" (= 0 one-hot) really maps to "Yes" (the left, less than split)
      # and the right, greater than split (1 one-hot) maps to "No"
      # as such, we don't have to invert the inequality ("less_than")
      # of course, this is reliant on, as far as I can tell, undocumented/unspecified behavior in XGBoost. So the durability isn't great, but:
      # 1. it doesn't seem liable to change (https://github.com/dmlc/xgboost/issues/1112)
      # 2. that lack of specification (dare I call it a bug) is the whole reason we have to do this exercise in xrf
      rules[row_ix, 'split'] <- categorical_split_value
    }
  }

  rules
}

#############################################
## functions for evaluating rulesets
#############################################

evaluate_rules <- function(rules, data) {
  per_rule_evaluation <- rules %>%
    group_by(.data$rule_id) %>%
    do (
      rule_evaluation = sapply(1:nrow(.data), function(split_ix) {
        split <- .data[split_ix, ]
        feature_ix <- which(split$feature == colnames(data))
        if (length(feature_ix) == 0) {
          stop(paste0('Feature "', split$feature,
                         '" from ruleset is not present in the input data to be evaluated'))
        }
        else if (length(feature_ix) > 1) {
          stop(paste0('Unexpectedly found two columns with the same name in input data (user must resolve): ',
                      split$feature))
        }
        split_comparison <- data[, feature_ix] < split$split
        return(split_comparison == split$less_than)
      }) %>%
      apply(1, all) %>%
      as.integer() %>%
      data.frame()
    )
  rule_features <- bind_cols(per_rule_evaluation$rule_evaluation, .name_repair = "minimal")
  colnames(rule_features) <- per_rule_evaluation$rule_id

  rule_features
}

evaluate_rules_dense_only <- function(rules, data) {
  data_df <- as.data.frame(data)
  per_rule_evaluation <- rules %>%
    group_by(.data$rule_id) %>%
    do (
      # yes, this is gross
      # yes, this is fast
      rule_evaluation = eval(parse(text=paste0(
        paste0('`', .data$feature, '`'),
        ifelse(.data$less_than, ' < ', ' >= '),
        .data$split,
        collapse = ' & '
      )),
      data_df) %>%
        as.integer() %>%
        data.frame()
    )
  rule_features <- bind_cols(per_rule_evaluation$rule_evaluation, .name_repair = "minimal")
  colnames(rule_features) <- per_rule_evaluation$rule_id

  rule_features
}

#############################################
## functions for cleaning up evaluated rules
#############################################

# returns the list of rules with non-zero variance
# if, by an unexpected outcome of the tree fitting process, a rule shows no variance, remove it
remove_no_variance_rules <- function(evaluated_rules) {
  keep_columns <- sapply(evaluated_rules, function(feature) {
    length(unique(feature)) > 1
  })

  return(colnames(evaluated_rules)[keep_columns])
}

# removes any exactly equal rules
dedupe_train_rules <- function(evaluated_rules) {
  as.matrix(evaluated_rules) %>%
    unique(MARGIN=2) %>%
    colnames()
}

#' Fit an eXtreme RuleFit model
#'
#' S3 method for building an "eXtreme RuleFit" model.
#' See \code{\link{xrf.formula}} for preferred entry point
#'
#' @param object an object describing the model to be fit
#' @param ... additional arguments
#'
#' @examples
#' m <- xrf(Petal.Length ~ ., iris,
#'          xgb_control = list(nrounds = 2, max_depth = 2),
#'          family = 'gaussian')
#'
#' @export
xrf <- function(object, ...) {
  UseMethod('xrf', object)
}

#' Fit an eXtreme RuleFit model
#'
#' See Friedman & Popescu (2008) for a description of the general RuleFit algorithm.
#' This method uses XGBoost to fit a tree ensemble, extracts a ruleset as the conjunction of tree
#' traversals, and fits a sparse linear model to the resulting feature set
#' (including the original feature set) using glmnet.
#'
#' @param object a formula prescribing features to use in the model. transformation of the response variable is not supported. when using transformations on the input features (not suggested in general) it is suggested to set sparse=F
#' @param data a data frame with columns corresponding to the formula
#' @param family the family of the fitted model. one of 'gaussian', 'binomial', 'multinomial'
#' @param xgb_control a list of parameters for xgboost. must supply an nrounds argument
#' @param glm_control a list of parameters for the glmnet fit. must supply a type.measure and nfolds arguments (for the lambda cv)
#' @param sparse whether a sparse design matrix should be used
#' @param prefit_xgb an xgboost model (of class xgb.Booster) to be used instead of the model that \code{xrf} would normally fit
#' @param deoverlap if true, the tree derived rules are deoverlapped, in that the deoverlapped rule set contains no overlapped rules
#' @param ... ignored arguments
#'
#' @importFrom xgboost xgboost
#' @importFrom xgboost xgb.model.dt.tree
#' @import dplyr
#' @importFrom Matrix sparse.model.matrix
#' @importFrom rlang .data
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats delete.response
#' @importFrom stats model.matrix
#' @importFrom stats predict
#' @importFrom stats terms
#' @importFrom stats update
#'
#' @references
#' Friedman, J. H., & Popescu, B. E. (2008). Predictive learning via rule
#' ensembles. \emph{The Annals of Applied Statistics, 2}(3), 916-954.
#'
#' @examples
#' m <- xrf(Petal.Length ~ ., iris,
#'          xgb_control = list(nrounds = 2, max_depth = 2),
#'          family = 'gaussian')
#'
#' @export
xrf.formula <- function(object, data, family,
                        xgb_control = list(nrounds = 100, max_depth = 3),
                        glm_control = list(type.measure = 'deviance',
                                           nfolds = 5),
                        sparse = TRUE,
                        prefit_xgb = NULL,
                        deoverlap = FALSE,
                        ...) {
  expanded_formula <- expand_formula(object, data)
  response_var <- get_response(expanded_formula)

  xgboost_conditioned <- condition_xgb_control(family, xgb_control, data, response_var, prefit_xgb)
  xgb_control <- xgboost_conditioned$xgb_control
  data <- xgboost_conditioned$data
  xrf_preconditions(family, xgb_control, glm_control, data, response_var, prefit_xgb)

  model_matrix_method <- if (sparse) sparse.model.matrix else model.matrix
  design_matrix <- model_matrix_method(expanded_formula, data)

  nrounds <- xgb_control$nrounds
  # necessary to remove from params to avoid false positive warnings
  xgb_control <- within(xgb_control, rm(nrounds))

  if (is.null(prefit_xgb)) {
    m_xgb <- xgboost(data = design_matrix,
                   label = data[[response_var]],
                   nrounds = nrounds,
                   objective = get_xgboost_objective(family),
                   params = xgb_control,
                   verbose = 0)
    rules <- extract_xgb_rules(m_xgb)
  }
  else {
    m_xgb <- prefit_xgb
    rules <- extract_xgb_rules(m_xgb)
    if (length(setdiff(rules$feature, colnames(design_matrix))) > 0) {
      stop('prefit_xgb contains features (or factor-levels) not present in the input training data. This is currently not supported.')
      # one simple approach would be to simply remove these feature splits from the rules
      # but that potentially dilutes the power of this method. for now, it's on the user to rectify this issue
    }
  }

  if (sparse) {
    feature_metadata <- build_feature_metadata(data)
    rules <- correct_xgb_sparse_categoricals(rules, feature_metadata$feature_metadata, feature_metadata$xlev)
  }

  if (deoverlap){
    rules <- xrf_deoverlap_rules(rules) %>%
      select(.data$rule_id, .data$feature, .data$split, .data$less_than)
  }

  rule_features <- if (sparse) evaluate_rules(rules, design_matrix) else evaluate_rules_dense_only(rules, design_matrix)

  varying_rules <- remove_no_variance_rules(rule_features)
  rule_features <- rule_features[, varying_rules]
  rules <- rules %>%
    filter(.data$rule_id %in% varying_rules)

  non_duplicate_rules <- dedupe_train_rules(rule_features)
  rule_features <- rule_features[, non_duplicate_rules]
  rules <- rules %>%
    filter(.data$rule_id %in% non_duplicate_rules)

  overlapped_feature_names <- intersect(colnames(rule_features), colnames(data))
  if (length(overlapped_feature_names) > 0) {
    warning(paste0('Found the following overlapped raw feature & rule names (the rule features will be dropped): ',
                   paste(overlapped_feature_names, collapse = ',')))
    rule_features <- rule_features[, !(colnames(rule_features) %in% overlapped_feature_names)]
  }

  # todo we already have a design matrix, so re-generating it with glmnot is a bit wasteful
  full_data <- cbind(data, rule_features,
                     stringsAsFactors = FALSE)

  # todo glmnet is a bottleneck on data size - it may be interesting to fit the glm to much larger data, e.g. with spark or biglasso
  full_formula <- add_predictors(expanded_formula, colnames(rule_features))

  # glmnet automatically adds an intercept
  full_formula <- update(full_formula, . ~ . -1)

  m_glm <- glmnot(full_formula, full_data,
                    family = family,
                    alpha = 1, # this specifies the LASSO
                    sparse = sparse,
                    glm_control = glm_control)

  structure(list(glm = m_glm,
                 xgb = m_xgb,
                 base_formula = expanded_formula,
                 rule_augmented_formula = full_formula,
                 rules = rules),
            class = 'xrf')
}

#' Generate the design matrix from an eXtreme RuleFit model
#'
#' @param object an object of class "xrf"
#' @param data data to generate design matrix from
#' @param sparse a logical indicating whether a sparse design matrix should be used
#' @param ... ignored arguments
#'
#' @importFrom Matrix sparse.model.matrix
#'
#' @examples
#' m <- xrf(Petal.Length ~ ., iris,
#'          xgb_control = list(nrounds = 2, max_depth = 2),
#'          family = 'gaussian')
#' design <- model.matrix(m, iris, sparse = FALSE)
#'
#' @export
model.matrix.xrf <- function(object, data, sparse = TRUE, ...) {
  # TODO: handle missing factor levels more elegantly (both for rule evaluation & glmnet)
  # TODO: when rules have a zero coefficient and we just want to predict, we don't need to evaluate them
  stopifnot(is.data.frame(data))

  trms <- terms(object$base_formula)
  trms <- delete.response(trms)

  design_matrix_method <- if (sparse) sparse.model.matrix else model.matrix

  raw_design_matrix <- design_matrix_method(trms, data)
  rules_features <- if (sparse) evaluate_rules(object$rules, raw_design_matrix) else evaluate_rules_dense_only(object$rules, raw_design_matrix)
  full_data <- cbind(data, rules_features,
                     stringsAsFactors = FALSE)

  full_data
}

#' Draw predictions from a RuleFit xrf model
#'
#' @param object an object of class "xrf"
#' @param newdata data to predict on
#' @param sparse a logical indicating whether a sparse design matrix should be used
#' @param lambda the lasso penalty parameter to be applied
#' @param type the type of predicted value produced
#' @param ... ignored arguments
#'
#' @examples
#' m <- xrf(Petal.Length ~ ., iris,
#'          xgb_control = list(nrounds = 2, max_depth = 2),
#'          family = 'gaussian')
#' predictions <- predict(m, iris)
#'
#' @export
predict.xrf <- function(object, newdata,
                        sparse = TRUE,
                        lambda = 'lambda.min',
                        type = 'response',
                        ...) {
  stopifnot(is.data.frame(newdata))
  full_data <- model.matrix(object, newdata, sparse)

  predict(object$glm, newdata = full_data,
          sparse = sparse, lambda = lambda, type = type)
}

synthesize_conjunctions <- function(rules) {
  rules %>%
    group_by(.data$rule_id)%>%
    arrange(.data$feature, .data$split) %>%
    summarize(
      conjunction =   paste0(
        .data$feature,
        ifelse(.data$less_than, '<', '>='),
        format(.data$split, scientific = FALSE, digits = 4),
        collapse = ' & '
      )
    )
}

#' Produce rules & coefficients for the RuleFit model
#'
#' @param object an object of class "xrf"
#' @param lambda the lasso penalty parameter to be applied as in 'glmnet'
#' @param ... ignored arguments
#'
#' @examples
#' m <- xrf(Petal.Length ~ ., iris,
#'          xgb_control = list(nrounds = 2, max_depth = 2),
#'          family = 'gaussian')
#' linear_model_coefficients <- coef(m, lambda = 'lambda.1se')
#'
#' @export
coef.xrf <- function(object, lambda = 'lambda.min', ...) {
  rule_conjunctions <- synthesize_conjunctions(object$rules)
  glm_coefficients <- coef(object$glm, s = lambda)
  glm_df <- as.data.frame(as.matrix(glm_coefficients))
  colnames(glm_df) <- sapply(lambda, function(lambda_value) { paste0('coefficient_', lambda) })
  glm_df$term <- rownames(glm_df)
  rownames(glm_df) <- NULL
  glm_df %>%
    left_join(rule_conjunctions, by = c('term' = 'rule_id')) %>%
    arrange_at(colnames(glm_df[1])) %>%
    mutate(
      rule = .data$conjunction
    ) %>%
    select(-.data$conjunction)
}

#' Summarize an eXtreme RuleFit model
#'
#' @param object an object of class "xrf"
#' @param ... ignored arguments
#'
#' @import dplyr
#' @importFrom methods show
#'
#' @examples
#' m <- xrf(Petal.Length ~ ., iris,
#'          xgb_control = list(nrounds = 2, max_depth = 2),
#'          family = 'gaussian')
#' summary(m)
#'
#' @export
summary.xrf <- function(object, ...) {
  cat(paste0('An eXtreme RuleFit model of ', n_distinct(object$rules$rule_id), ' rules.'))
  cat(paste0('\n\nOriginal Formula:\n\n'))
  cat(smaller_formula(object$base_formula))
  cat('\n\nTree model:\n\n')
  show(summary(object$xgb))
  cat('\n\nGLM:\n\n')
  show(summary(object$glm))
}

#' Print an eXtreme RuleFit model
#'
#' @param x an object of class "xrf"
#' @param ... ignored arguments
#'
#' @examples
#' m <- xrf(Petal.Length ~ ., iris,
#'          xgb_control = list(nrounds = 2, max_depth = 2),
#'          family = 'gaussian')
#' print(m)
#'
#' @export
print.xrf <- function(x, ...) {
  cat(paste0('An eXtreme RuleFit model of ', n_distinct(x$rules$rule_id), ' rules.'))
  cat(paste0('\n\nOriginal Formula:\n\n'))
  cat(smaller_formula(x$base_formula), "\n")
}

smaller_formula <- function(x, ...) {
  chr_form <- deparse(x, width.cutoff = getOption("width") - 12)
  if (length(chr_form) > 1) {
    chr_form <- paste0(chr_form[1], "[truncated]")
  }
  chr_form
}

