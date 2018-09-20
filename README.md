# eXtreme RuleFit

## About

## Install

```R
devtools::install_git('https://github.com/holub008/xrf')
```

## Examples

```R
library(RCurl)

library(pre)
library(xrf)
library(glmnet)
library(xgboost)

census_income_text <- getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data')
census_income <- read.csv(textConnection(census_income_text), header=F, stringsAsFactors = F)
colnames(census_income) <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status',
                            'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss',
                            'hours_per_week', 'native_country', 'above_50k')
census_income <- census_income %>%
  mutate(
    above_50k = above_50k == ' >50K'
  )

train_ix <- sample(nrow(census_income), floor(nrow(census_income) * .66))
census_train <- census_income[train_ix, ]
census_test <- census_income[-train_ix, ]
# system.time(m_pre <- pre(above_50k ~ ., census_train, family = 'binomial', ntrees = 10, maxdepth = 2))
system.time(m_xrf <- xrf(above_50k ~ ., census_train, family = 'binomial', 
             xgb_control = list(nrounds = 50, max_depth = 3)))

census_mat <- model.matrix(above_50k ~ ., census_income)
census_train_mat <- census_mat[train_ix, ]
census_test_mat <- census_mat[-train_ix, ]
system.time(m_glm <- cv.glmnet(census_train_mat, census_train$above_50k, alpha = 1))
system.time(m_big_glm <- cv.biglasso(as.big.matrix(census_train_mat), census_train$above_50k))
m_xgb <- xgboost(census_train_mat, census_train$above_50k, max_depth = 3, nrounds = 50)

auc(predict(m_xrf, census_test), census_test$above_50k)
auc(predict(m_glm, newx = census_test_mat, s = 'lambda.min'), census_test$above_50k)
auc(as.vector(predict(m_big_glm, as.big.matrix(census_test_mat))), census_test$above_50k)
auc(predict(m_xgb, newdata = census_test_mat, s = 'lambda.min'), census_test$above_50k)

```
