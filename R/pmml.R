library(xml2)
library(dplyr)
library(lubridate)

extract_glmnot_features <- function(s) {
  model_coefficients <- as.data.frame(as.matrix(coef(m_xrf$glm, s = 'lambda.min')))
  
}

# note this function currently does not support the case where the linear model (i.e. the non-rule-based features) contains interaction terms
pmml.xrf <- function(model, s='lambda.min') {
  if (!model$family %in% c('binomial', 'guassian')) {
    stop('No current support for export of xrf multinomial models')
  }
  
  pmml_doc <- xml_new_root('PMML', .version="4.1", xmlns="http://www.dmg.org/PMML-4_1")
  
    pmml_doc %>%
      xml_add_child('Header', description='RuleFit Model') %>%
        xml_add_child("Application", name="xrf") %>%
        xml_add_sibling('Timestamp', as.character(now('GMT')))
  
    predictor_list <- pmml_doc %>%
      xml_add_child("DataDictionary")
    
    
}