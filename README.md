# eXtreme RuleFit

## Install
```R
devtools::install_git('https://github.com/holub008/xrf')
```

## About
RuleFit (described in [Friedman & Popescu](http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf)) is a clever model combining tree ensembles and linear models. The goal is to produce a model with comparable performance to a tree ensemble, with the interpretability of a linear model.

The general algorithm follows:

* Fit a tree ensemble to the data
    * xrf uses gradient boosting to fit the ensemble. For a description of tree boosting & gradient boosting, see [this document](https://github.com/holub008/snippets/blob/master/tree_learning/tree_learning_overview.pdf).
* Build a set of rules as the set of root -> leaf traversals from each tree.
    * Each traversal becomes a rule via conjunction
* Evaluate the rules on the data to build a rule feature set
* Fit a linear model to the original feature set joined with the rule feature set
    * The LASSO may be used to build a sparse final model

### Comparison to 'pre'
[pre](https://cran.r-project.org/web/packages/pre/index.html) is a package on CRAN for fitting prediction rule ensembles, including RuleFit. xrf improves on some aspects of pre by:
* Building more accurate models
* Building models faster
* Building models that predict from missing data and new factor-levels
* Providing a more limited interface to produce fewer bugs

As noted in the last point, xrf does provide less flexibility (e.g. cannot derive rules from RandomForest, or fitting trees using different algorithms) & fewer interpretive tools (this is in progress) than pre.

## Example

Here we predict whether an individual's income is greater than $50,000 using census data.
```R
library(RCurl)
library(xrf)

# grabbing data from uci
census_income_text <- getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data')
census_income <- read.csv(textConnection(census_income_text), header=F, stringsAsFactors = F)
colnames(census_income) <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status',
                            'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss',
                            'hours_per_week', 'native_country', 'above_50k')
                            
m_xrf <- xrf(above_50k ~ ., census_income, family = 'binomial', 
             xgb_control = list(nrounds = 100, max_depth = 3))
```

### Comparison
Here we employ out-of-the-box RuleFit from pre & xrf, as well as raw xgboost & glmnet models.

```R
library(dplyr)
library(pre)
library(glmnet)
library(xgboost)

auc <- function (prediction, actual) {
  stopifnot(length(unique(actual)) == 2)
  stopifnot(length(prediction) == length(actual))
  mann_whit <- wilcox.test(prediction ~ actual)$statistic
  unname(1 - mann_whit/(sum(actual) * as.double(sum(!actual))))
}

census_income <- census_income %>%
  # pre is picky about data types 
  mutate_if(is.character, as.factor) %>%
  mutate(
    above_50k = as.character(above_50k) == ' >50K'
  )

set.seed(55455)
train_ix <- sample(nrow(census_income), floor(nrow(census_income) * .66))
census_train <- census_income[train_ix, ]
census_test <- census_income[-train_ix, ]
census_mat <- model.matrix(above_50k ~ ., census_income)
census_train_mat <- census_mat[train_ix, ]
census_test_mat <- census_mat[-train_ix, ]

system.time(m_pre <- pre(above_50k ~ ., na.omit(census_train), 
                         family = 'binomial', ntrees = 100, maxdepth = 3, tree.unbiased = TRUE))
system.time(m_xrf <- xrf(above_50k ~ ., census_train, family = 'binomial', 
             xgb_control = list(nrounds = 100, max_depth = 3)))
m_xgb <- xgboost(census_train_mat, census_train$above_50k, max_depth = 3, nrounds = 100, objective = 'binary:logistic')
m_glm <- cv.glmnet(census_train_mat, census_train$above_50k, alpha = 1)

auc(predict(m_pre, census_test), census_test$above_50k)
auc(predict(m_xrf, census_test), census_test$above_50k)
auc(predict(m_glm, newx = census_test_mat, s = 'lambda.min'), census_test$above_50k)
auc(predict(m_xgb, newdata = census_test_mat, s = 'lambda.min'), census_test$above_50k)
```

With results:

| Model | Time (s) |
| ----- | -------- |
| xrf   | 89       |    
| pre   | 211      |

On the test set:

| Model    |  AUC     |
| -------- | -------- |
| xrf      | .923     |      
| pre      | .906     |
| xgboost  | .924     | 
| glmnet   | .888     |

