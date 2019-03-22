library(xml2)
library(dplyr)
library(lubridate)

# masks base::startsWith to prevent R > 3.3.0 requirement
startsWith <- function(base, prefix) {
  substr(base, 1, nchar(prefix)) == prefix
}

lstrip <- function(full, to_remove){
  sub(sprintf("^%s", to_remove), "", full)
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

# this is unfortunately an imprecise operation - consider the case in which feature names and levels may overlap to an extent
# it is ignored for now, opting to explode in the event that naming is ambiguous
parse_feature_level <- function(feature_level, feature_metadata, xlev, rules) {
  if (feature_level == '(Intercept)') {
    return(list(feature_name = feature_level,
                level = NA,
                is_continuous = TRUE,
                is_rule = FALSE,
                is_intercept = TRUE))
  }
  
  classified_features <- feature_metadata %>%
    mutate(
      level_remainder = sapply(feature_name, function(fn){ lstrip(feature_level, fn) }),
      may_be_rule_feature = sapply(feature_name, function(fn) { !startsWith(feature_level, fn) })
    )
  
  feature_level_matches <- classified_features %>%
    filter(!may_be_rule_feature) %>%
    filter(level_remainder == '' | has_matching_level(feature_name, level_remainder, xlev))
  
  if (nrow(feature_level_matches) > 1) {
    stop(paste0('Conservatively failing - several feature/level matches found for feature_level: "', feature_level, '"'))
  }
  
  if (nrow(feature_level_matches) == 0) {
    # if no main effects match, attempt to find a matching rule
    matching_rules <- any(rules$rule_id == feature_level)
    
    if (!matching_rules) {
      stop(paste0("Feature level in fitted model neither matches main effect feature levels or rule feature: ", feature_level))
    }
    
    return(list(
      feature_name = feature_level,
      level = NA,
      is_continuous = FALSE,
      is_rule = TRUE,
      is_intercept = FALSE
    ))
  }
  else {
    # we have a unique, main effects feature level match
    return(list(
      feature_name = feature_level_matches$feature_name,
      level = feature_level_matches$level_remainder,
      is_continuous = feature_level_matches$level_remainder == '',
      is_rule = FALSE,
      is_intercept = FALSE
    ))
  }
}

# obviously this strcuture does not yet support main effect interactions in the glm
extract_glmnot_coefficients <- function(model, s) {
  coeffs <- coef(model$glm, s = s)
  model_frame <- data.frame(feature_level = coeffs@Dimnames[[1]][coeffs@i + 1], coefficient = coeffs@x,
                                   stringsAsFactors = FALSE)
  
  parsed_model <- data.frame()
  for (row_ix in 1:nrow(model_frame)) {
    row <- model_frame[row_ix,]
    parsed_feature_level <- parse_feature_level(row$feature_level, model$feature_metadata, model$xlev, model$rules)
    parsed_feature_level$coefficient <- row$coefficient
    parsed_model <- rbind(parsed_model, parsed_feature_level,
                          stringsAsFactors = FALSE)
  }
  
  return(parsed_model)
}

# note this function currently does not support the case where the linear model (i.e. the main effect / non-rule-based features) contains interaction terms
pmml.xrf <- function(model, s='lambda.min') {
  if (!model$family %in% c('binomial', 'guassian')) {
    stop('No current support for export of xrf multinomial models')
  }
  
  glm_coefficients <- extract_glmnot_coefficients(model, s)
  
  pmml_doc <- xml_new_root('PMML', .version="4.1", xmlns="http://www.dmg.org/PMML-4_1")
  
    pmml_doc %>%
      xml_add_child('Header', description='RuleFit Model') %>%
        xml_add_child("Application", name="xrf") %>%
        xml_add_sibling('Timestamp', as.character(now('GMT')))
  
    feature_list <- pmml_doc %>%
      xml_add_child("DataDictionary")
    
      for (ix in 1:nrow(model$feature_metadata)) {
        row <- model$feature_metadata[ix,]
        if (row$is_continuous) {
          feature_list %>% xml_add_child('DataField', dataType='double', name=row$feature_name, optype='continuous')
        }
        else {
          level_list <- feature_list %>% xml_add_child('DataField', dataType='string', name=row$feature_name, optype='categorical')
          levels <- model$xlev[[row$feature_name]]
          ignored = sapply(levels, function(l) { 
            level_list %>% xml_add_child('Value', value = l)
          })
        }
      }
    
    transformation_feature_list <- pmml_doc %>%
      xml_add_child("TransformationDictionary")
      
      
    
    
}
