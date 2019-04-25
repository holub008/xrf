# glmnot quite glmnet
# this wrapper allows training & prediction from a data.frame, and provides mechanisms for handling new levels in prediction
# for current release, this class is not exposed in public API

coef.glmnot <- function(object, ...) {
  coef(object$model, ...)
}

# Accepts a data frame or design matrix for newdata, dependent on what the model was trained with. 
# If a model was trained with a data.frame, it can be predicted with either a matrix or a data.frame
#
#' @importFrom Matrix sparse.model.matrix
predict.glmnot <- function(object, newdata, 
                           sparse = TRUE,
                           lambda = 'lambda.min',
                           type = 'response') {
  # convert a data-frame to a matrix matching the expected design matrix
  # this is really clunky design, but so be it
  if (is.data.frame(newdata)) {
    if (is.null(object$xlev)) {
      stop('Cannot predict from a dataframe using a glmnot object which was trained on a matrix - train using a data frame, or predict from a matrix of the same form')
    }
    design_matrix_method <- if (sparse) sparse.model.matrix else model.matrix
    
    no_response_formula <- delete.response(terms(object$formula))
    
    # handle the case of new levels in newdata not in original data xlev
    for (column in all.vars(no_response_formula)) {
      column_data <- newdata[[column]]
      if(!is.numeric(column_data)) {
        # to avoid factor / character confusion, coerce everything to character and take the performance penalty
        column_data <- as.character(column_data)
        
        reference_level <- object$xlev[[column]][1]
        newdata[[column]] <- ifelse(column_data %in% object$xlev[[column]],
                                    column_data, reference_level)
      }
    }
    
    newdata <- design_matrix_method(no_response_formula, newdata,
                                    xlev = object$xlev) # this case for when newdata is missing levels in the original dataframe
  }
  
  predict(object$model, newx = newdata, 
          s = lambda, type = type)
}

glmnot <- function(object, ...) {
  UseMethod('glmnot', object)
}

# X is a design matrix (sparse or otherwise) consisting of predictors to train on
# y is a vector of responses to train on
# alpha is the elastic net parameter
# formula is the optional formula specify the design matrix
#
#' @import glmnet
glmnot.default <- function(X, y, family,
                           alpha = 1,
                           formula = NULL,
                           xlev = NULL,
                           glm_control = list()
                           ) {
  cv.glmnet.args <- list(X, y, family = family,
                alpha = alpha)
  cv.glmnet.args <- append(cv.glmnet.args, glm_control)

  m <- do.call(cv.glmnet, cv.glmnet.args)
  
  structure(list(model = m,
                 formula = formula,
                 xlev = xlev), class = 'glmnot')
}

#' @importFrom Matrix sparse.model.matrix
#' @import dplyr
glmnot.formula <- function(formula, data, family,
                           alpha = 1,
                           type.measure = 'auc',
                           sparse = TRUE,
                           glm_control
                           ) {
  constant_factors <- colnames(
    data %>%
      select_if(function(column) {
        if(!is.numeric(column)) {
          return(n_distinct(column) < 2)
        }
        else {
          return(FALSE)
        }
      })
  )
  
  # terms() produces a formula object which represents the expanded formula (i.e. if a '.' is included)
  final_formula <- terms(formula, data = data)
  constant_factors_in_formula <- intersect(constant_factors, all.vars(final_formula))
  
  terms_matrix <- attr(final_formula, "factors")
  for (column in colnames(terms_matrix)) {
    # todo technically need to check if sum == order of term
    if (sum(terms_matrix[constant_factors_in_formula, column]) > 0) {
      final_formula <- update(final_formula, paste('. ~ . -', column))
    }
  }
  
  # ensure there are at least two terms left
  stopifnot(length(all.vars(final_formula)) > 1)
  response_variable <- get_response(final_formula)
  columns <- get_predictors(final_formula)
  
  factor_column_indicator <- sapply(columns, function(column) {
    !is.numeric(data[[column]]) && column != response_variable
  })
  factor_columns <- columns[factor_column_indicator]
  xlev <- lapply(data %>% select(factor_columns), function(x) {
    # respect original ordering of levels if factor, otherwise arbitrary ordering
    if(is.factor(x)) levels(x) else as.character(unique(x))
  })
  
  model_mat_method <- if (sparse) sparse.model.matrix else model.matrix
  X <- model_mat_method(final_formula, data, xlev = xlev)
  y <- data[[response_variable]]
  
  stopifnot(nrow(X) == length(y)) # this can be expected to occur when there are NA cases present in the original data (model.matrix will drop such rows)
  
  glmnot(X, y, family, 
         alpha = alpha, 
         formula = final_formula,
         xlev = xlev,
         glm_control = glm_control)
}
