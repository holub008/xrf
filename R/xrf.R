condition_xgb_control <- function(family, xgb_control, data, response_var) {
  # this is a duplicated but necessary check
  if (!(response_var %in% colnames(data))) {
    stop(paste0('Response variable "', response_var, '" not present in supplied data'))
  }
  
  data_mutated <- data
  
  if (family == 'multinomial' && is.null(xgb_control$num_class)) {
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
  
  if (family == 'multinomial' && 
      (is.null(xgb_control$num_class) || n_distinct(data[[response_var]]) != xgb_control$num_class)) {
    stop('Must supply a num_class list element in xgb_control when using multinomial objective')
  }
  
  if (length(intersect(c('type.measure', 'nfolds'), names(glm_control))) < 2) {
    stop('Must supply "type.measure" and "nfolds" as glm_control parameters')
  }
  
  allowed_tree_ensemble_classes <- c('xgb.Booster')
  if (!is.null(prefit_xgb) && length(intersect(allowed_tree_ensemble_classes, class(prefit_xgb))) == 0) {
    stop('Prefit tree ensemble must be of class {', paste0(allowed_tree_ensemble_classes, collapse = ','), "}")
  }
}

## the choice of ensemble loss is currently hidden from the api to protect implementation details
## this may be exposed to the user in the future
get_xgboost_objective <- function(family) {
  if (family == 'gaussian') {
    return('reg:linear')
  }
  else if (family == 'binomial') {
    return('binary:logistic')
  }
  else if (family == 'multinomial') {
    return('multi:softmax')
  }
  
  stop(paste0('Unrecognized family ', family, ' which should have failed fast in preconditions'))
}

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
    group_by(Tree) %>%
    arrange(Node) %>% # put the root at the top of each tree group
    do(harvested_rules = rule_traverse(.[1, ], .) %>% 
         filter(!is.na(feature))) %>%
    pull(harvested_rules) %>%
    bind_rows()
  
  rules
}

evaluate_rules <- function(rules, data) {
  per_rule_evaluation <- rules %>%
    group_by(rule_id) %>%
    do (
      rule_evaluation = sapply(1:nrow(.), function(split_ix) {
        split <- .[split_ix, ]
        # todo this could be a straight up failure, or at least we could de-dupe the warnings
        feature_ix <- which(split$feature == colnames(data))
        if (length(feature_ix) == 0) {
          warning(paste0('Feature ', split$feature, 
                         ' from ruleset is not present in the input data to be evaluated'))
          return(rep(FALSE, nrow(data)))
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
  rule_features <- bind_cols(per_rule_evaluation$rule_evaluation)
  colnames(rule_features) <- per_rule_evaluation$rule_id
  
  rule_features
}

# returns the list of rules with non-zero variance
# if, by an unexpected outcome of the tree fitting process, a rule shows no variance, remove it
remove_no_variance_rules <- function(evaluated_rules) {
  keep_columns <- sapply(evaluated_rules, function(feature) {
    length(unique(feature)) > 1
  })
  
  return(colnames(evaluated_rules)[keep_columns])
}

# returns the list of deduped rule_ids
# note that this is an approximate algorithm, since correlation inequality is not transitive
dedupe_train_rules <- function(evaluated_rules, max_absolute_correlation) {
  if (ncol(evaluated_rules) < 2) {
    return(colnames(evaluated_rules))
  }
  
  duplicate_rule_ixs <- c()
  
  # TODO parallelize
  for (rule1_ix in 1:(ncol(evaluated_rules) -1)) {
    if (rule1_ix %in% duplicate_rule_ixs) next
    
    rule2_candidate_ixs <- setdiff((rule1_ix + 1):ncol(evaluated_rules), duplicate_rule_ixs)
    for (rule2_ix in rule2_candidate_ixs) {
      rho <- cor(evaluated_rules[, rule1_ix], evaluated_rules[, rule2_ix])
      if (abs(rho) > max_absolute_correlation) {
        duplicate_rule_ixs <- c(duplicate_rule_ixs, rule2_ix)
      }
    }
  }
  
  selection_ixs <- if(length(duplicate_rule_ixs) > 0) -duplicate_rule_ixs else 1:ncol(evaluated_rules)
  return(colnames(evaluated_rules)[selection_ixs])
}

#' Fit a RuleFit model
#'
#' S3 method for building an "eXtreme RuleFit" model.
#' See \code{\link{xrf.formula}} for preferred entry point
#'
#' @author kholub
#'
#' @export
xrf <- function(object, ...) {
  UseMethod('xrf', object)
}

#' Fit a RuleFit model
#' 
#' See Friedman & Popescu (2008) for a description of the general RuleFit algorithm.
#' This method uses XGBoost to fit a tree ensemble, extracts a ruleset as the conjunction of tree 
#' traversals, and fits a sparse linear model to the resulting feature set
#' (including the original feature set) using glmnet.
#' 
#' @param object a formula prescribing features to use in the model
#' @param data a data frame with columns corresponding to the formula
#' @param family the family of the fitted model. one of 'gaussian', 'binomial', 'multinomial'
#' @param xgb_control a list of parameters for xgboost. must supply an nrounds argument
#' @param glm_control a list of parameters for the glmnet fit. must supply a type.measure and nfolds arguments (for the lambda cv)
#' @param max_rule_correlation the maxmimal allowed abslute Spearman correlation between any given pair of rules before on of the rules is removed
#' @param sparse whether a sparse design matrix should be used
#' @param prefit_xgb an xgboost model (of class xgb.Booster) to be used instead of the model that xrf would normally fit
#' @param deoverlap if true, the tree derived rules are deoverlapped, in that the deoverlapped rule set contains no overlapped rules
#' 
#' @author kholub
#' 
#' @importFrom xgboost xgboost
#' @importFrom xgboost xgb.model.dt.tree
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom Matrix sparse.model.matrix
#' 
#' @references 
#' Friedman, J. H., & Popescu, B. E. (2008). Predictive learning via rule 
#' ensembles. \emph{The Annals of Applied Statistics, 2}(3), 916-954.
#' 
#' @examples 
#' m <- xrf(Petal.Length ~ ., iris, 
#'          xgb_control = list(nrounds = 20, max_depth = 2),
#'          family = 'gaussian')
#'
#' @export
xrf.formula <- function(object, data, family,
                        xgb_control = list(nrounds = 100, max_depth = 3),
                        glm_control = list(type.measure = 'deviance',
                                           nfolds = 5),
                        max_rule_correlation = .99,
                        sparse = TRUE,
                        prefit_xgb = NULL,
                        deoverlap = FALSE,
                        weights = rep(1, nrow(data))) {
  expanded_formula <- expand_formula(object, data)
  # todo this breaks for naughty formulas
  response_var <- get_response(expanded_formula)
  
  xgboost_conditioned <- condition_xgb_control(family, xgb_control, data, response_var)
  xgb_control <- xgboost_conditioned$xgb_control
  data <- xgboost_conditioned$data
  xrf_preconditions(family, xgb_control, glm_control, data, response_var, prefit_xgb)
  
  model_matrix_method <- if (sparse) sparse.model.matrix else model.matrix 
  design_matrix <- model_matrix_method(expanded_formula, data)
  
  if (is.null(prefit_xgb)) {
    m_xgb <- xgboost(data = design_matrix,
                     label = data[[response_var]],
                     nrounds = xgb_control$nrounds,
                     objective = get_xgboost_objective(family),
                     params = xgb_control)
    rules <- extract_xgb_rules(m_xgb)
  }
  else {
    m_xgb <- prefit_xgb
    rules <- extract_xgb_rules(m_xgb)
    if (length(setdiff(rules$feature, colnames(design_matrix))) > 0) {
      stop('prefit_xgb contains features (or factor-levels) not present in the input training data. This is currently not supported.')
      # TODO one simple approach would be to simply remove these feature splits from the rules
      # but that potentially dilutes the power of this method. for now, it's on the user to rectify this issue
    }
  }
  
  if (deoverlap){
    rules <- xrf_deoverlap_rules(rules)
  }

  rule_features <- evaluate_rules(rules, design_matrix)
  
  varying_rules <- remove_no_variance_rules(rule_features)
  rule_features <- rule_features[, varying_rules]
  rules <- rules %>%
    filter(rule_id %in% varying_rules)
  
  non_duplicate_rules <- dedupe_train_rules(rule_features, max_rule_correlation)
  rule_features <- rule_features[, non_duplicate_rules]
  rules <- rules %>%
    filter(rule_id %in% non_duplicate_rules)
    
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
                  nfolds = glm_control$nfolds,
                  type.measure = glm_control$type.measure,
                  pmax = ifelse(is.null(glm_control$pmax), +Inf, glm_control$pmax),
                  intercept = ifelse(glm_control$intercept == FALSE, FALSE, TRUE),
                  alpha = 1, # this specifies the LASSO
                  sparse = sparse,
                  weights = weights)
  
  structure(list(glm = m_glm,
                 xgb = m_xgb,
                 base_formula = expanded_formula,
                 rule_augmented_formula = full_formula,
                 rules = rules),
            class = 'xrf')
}

#' Generate the design matrix from a RuleFit xrf model
#'
#' @param object an object of class xrf
#' @param sparse a logical indicating whether a sparse design matrix should be used
#'
#' @author yama1968
#'
#' @importFrom Matrix sparse.model.matrix
#'
#' @method generate_design_matrix xrf
#'
#' @export
generate_design_matrix.xrf <- function(object, newdata,
                                       sparse = TRUE) {
  # TODO: handle matrix
  # TODO: handle missing factor levels more elegantly (both for rule evaluation & glmnet)
  # TODO handle missing predictors (continuous) by failing or imputing?
  # TODO: when rules have a zero coefficient, we don't need to evaluate them
  stopifnot(is.data.frame(newdata))
  design_matrix_method <- if (sparse) sparse.model.matrix else model.matrix
  
  raw_design_matrix <- design_matrix_method(object$base_formula, newdata)
  rules_features <- evaluate_rules(object$rules, raw_design_matrix)
  full_data <- cbind(newdata, rules_features, 
                     stringsAsFactors = FALSE)

  full_data
}

#' Draw predictions from a RuleFit xrf model
#'
#' @param object an object of class xrf
#' @param sparse a logical indicating whether a sparse design matrix should be used
#' @param lambda the lasso penalty parameter to be applied
#' @param type the type of predicted value produced
#'
#' @author kholub
#' 
#' @export
predict.xrf <- function(object, newdata,
                        sparse = TRUE,
                        lambda = 'lambda.min',
                        type = 'response') {
  stopifnot(is.data.frame(newdata))
  full_data <- generate_design_matrix(object, newdata, sparse)

  predict(object$glm, newdata = full_data, 
          sparse = sparse, lambda = lambda, type = type)
}

synthesize_conjunctions <- function(rules) {
  rules %>%
    mutate(
      inequality_operator = ifelse(less_than, '<', '>='),
      expression = paste(feature, inequality_operator, format(split, scientific = FALSE, digits = 4))
    ) %>%
    group_by(rule_id) %>%
    summarize(
      conjunction = paste(expression, collapse = ' & ')
    )
}

#' Produce rules & coefficients for the RuleFit model
#'
#' @param object an object of class xrf
#' @param lambda the lasso penalty parameter to be applied
#'
#' @author kholub
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
      rule = conjunction
    ) %>%
    select(-conjunction)
}

#' Summarize an eXtreme RuleFit model
#' 
#' @param object an object of class xrf
#' 
#' @author kholub
#' 
#' @import dplyr
#'
#' @export
summary.xrf <- function(object, ...) {
  cat(paste0('An eXtreme RuleFit model of ', n_distinct(object$rules$rule_id), ' rules.'))
  cat(paste0('\n\nFormula:\n\n'))
  show(object$base_formula)
  cat('\n\nTree model:\n\n')
  show(summary(object$xgb))
  cat('\n\nGLM:\n\n')
  show(summary(object$glm))
}

#' Print an eXtreme RuleFit model
#'
#' @param x an xrf object to be printed
#' 
#' @author kholub
#'
#' @export
print.xrf <- function(x, ...) {
  cat(paste0('An eXtreme RuleFit model of ', n_distinct(x$rules$rule_id), ' rules.'))
  cat(paste0('\n\nFormula:\n\n'))
  show(x$base_formula)
}  

#' Show an eXtreme RuleFit model
#'
#' @param x an xrf object to be shown
#' 
#' @author kholub
#'
#' @export
show.xrf <- function(object) {
  print(object)
}