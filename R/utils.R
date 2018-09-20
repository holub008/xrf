# this is a breakable implementation, for instance in the case of a missing response (e.g. ~ a + b)
get_response <- function(formula) {
  all.vars(formula[[2]])
}

get_predictors <- function(formula) {
  all.vars(formula[[3]])
}

expand_formula <- function(formula, data) {
  formula_terms <- all.vars(terms(formula, data = data))
  as.formula(paste0(formula_terms[1], '~', 
                    paste0(formula_terms[-1], collapse = '+')))
}

add_predictors <- function(base_formula, new_predictors) {
  if (length(new_predictors) == 0) {
    return (base_formula)
  }
  
  new_part <- paste(new_predictors, collapse = ' + ')
  base_formula_char <- Reduce(paste, deparse(base_formula))
  
  as.formula(paste0(as.character(base_formula_char), ' + ', new_part))
}
