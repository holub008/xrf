get_response <- function(formula) {
  # this code may execute before xrf_preconditions, so the exception originates here
  if (length(formula) != 3) {
    stop('Supplied formula does not appear to follow expected form of response ~ predictors')
  }
  all.vars(formula[[2]])
}

get_predictors <- function(formula) {
  all.vars(formula[[3]])
}

expand_formula <- function(formula, data) {
  expanded_formula <- terms(formula, data = data)
  formula_terms <- attr(expanded_formula, 'term.labels')
  response <- all.vars(expanded_formula)[1]
  as.formula(paste0(response, '~', 
                    paste0(formula_terms, collapse = '+')))
}

add_predictors <- function(base_formula, new_predictors) {
  if (length(new_predictors) == 0) {
    return (base_formula)
  }
  
  new_part <- paste(new_predictors, collapse = ' + ')
  base_formula_char <- Reduce(paste, deparse(base_formula))
  
  as.formula(paste0(as.character(base_formula_char), ' + ', new_part))
}

startsWith <- function(base, prefix) {
  substr(base, 1, nchar(prefix)) == prefix
}

lstrip <- function(full, to_remove){
  sub(sprintf("^%s", to_remove), "", full)
}
