# glmnot quite glmnet. also, it's glmnot a problem to use
# this wrapper allows training & prediction from a data.frame, and provides mechanisms for handling new levels in prediction
# TODO does this even need to be exported?

#' Delegates a cv.glmnet
#'
#' @author kholub
#'
#'@export
coef.glmnot <- function(object, ...) {
  coef(object$model, ...)
}

#' Delegates a cv.glmnet
#'
#' @author kholub
#'
#' @export
print.glmnot <- function(object, ...) {
  print(object$model, ...)
}

#' Produces regularization path plot (delegated to optimal glmnet fit)
#'
#' @author kholub
#'
#'@export
plot.glmnot <- function(object, ...) {
  plot(object$model$glmnet.fit, label = TRUE, ...)
  coef_l1 <- sum(abs(coef(object)))
  abline(v = coef_l1)
}

#' Delegates a cv.glmnet
#'
#' Accepts a data frame or design matrix, dependent on what the model was trained with. 
#' If a model was trained with a data.frame, it can be predicted with either a matrix or a data.frame
#'
#' @param object a glmnnot object to predict from
#' @param newdata a matrix or dataframe to draw predictions from, dependent on how the model was trained
#' @param sparse if true and supplied data is a dataframe, the constructed design matrix is sparse
#'
#' @author kholub
#' @examples
#' dat <- data.frame(
#'   x = c(1,2,3,4,55,6,2,3, 1,2,3),
#'   z = as.factor(c('a','b','c','a','c','b','c', 'c', 'a', 'b', 'c')),
#'   y = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
#' test_dat <- data.frame(x = c(10,5), z = c('c', 'd'))
#' predict(m, newdata = test_dat)
#'
#' @importFrom Matrix sparse.model.matrix
#'
#'@export
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

#' Elastic net regularized regression
#'
#' S3 method for elastic net fits - a wrapper on glmnet with some quality of life improvements.
#' See \code{\link{glmnot.formula}} for preferred entry point
#'
#' @author kholub
#'
#' @export
glmnot <- function(object, ...) {
  UseMethod('glmnot', object)
}

#' Elastic net regularization algorithm
#'
#' @param X a design matrix (sparse or otherwise) consisting of predictors to train on
#' @param y a vector of responses to train on
#' @param family the family of the model to be fitted
#' @param alpha the elastic net parameter
#' @param formula the formula specify the design matrix (optional)
#' @param xlev per feature levels (optional)
#' @param weights weights on observations in X
#'
#' @author kholub
#'
#' @import glmnet
#'
#' @export
glmnot.default <- function(X, y, family,
                           alpha = 1,
                           nfolds = 5,
                           type.measure = 'auc',
                           pmax = +Inf,
                           formula = NULL,
                           xlev = NULL,
                           weights = rep(1, nrow(X))) {
  m <- cv.glmnet(X, y, family = family,
              alpha = alpha,
              type.measure = type.measure,
              pmax = pmax,
              nfolds = nfolds,
              weights = weights,
              parallel = TRUE)
  structure(list(model = m,
                 formula = formula,
                 xlev = xlev), class = 'glmnot')
}

#' Elastic net regularization
#'
#' @param formula a specification of the features used to build the design matrix
#' @param data a data frame used to build the model
#' @param family family of the fitted model
#' @param alpha the elastic net parameter
#' @param sparse logical indicating if the constructed design matrix should be sparse or dense
#' @param weights weights on observations in the train data
#'
#' @author kholub
#'
#' @importFrom Matrix sparse.model.matrix
#' @import dplyr
#'
#' @export 
glmnot.formula <- function(formula, data, family,
                           alpha = 1,
                           nfolds = 5,
                           type.measure = 'auc',
                           pmax = +Inf,
                           sparse = TRUE,
                           weights = rep(1, nrow(data))) {
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
         nfolds = nfolds,
         type.measure = type.measure,
         pmax = pmax,
         formula = final_formula,
         xlev = xlev,
         weights = weights)
}
