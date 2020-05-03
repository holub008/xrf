# xrf

[![R build status](https://github.com/holub008/xrf/workflows/R-CMD-check/badge.svg)](https://github.com/holub008/xrf/actions)
[![Codecov test coverage](https://codecov.io/gh/holub008/xrf/branch/master/graph/badge.svg)](https://codecov.io/gh/holub008/xrf?branch=master)
[![xrf on CRAN](https://cranlogs.r-pkg.org/badges/xrf)](https://CRAN.R-project.org/package=xrf)

## Install
For current CRAN release:

```R
install.packages('xrf')
```

For the current development version:

```R
devtools::install_dev('xrf')
```

## About
RuleFit (described in [Friedman & Popescu](http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf)) is a clever model combining tree ensembles and linear models. The goal is to produce a model with comparable performance to a tree ensemble, with the interpretability of a linear model.

The general algorithm follows:

* Fit a tree ensemble to the data
* Build a set of rules as the set of root -> leaf traversals from each tree.
    * Each traversal becomes a rule via conjunction
* Evaluate the rules on the data to build a rule feature set
* Fit a linear model to the original feature set joined with the rule feature set
    * The LASSO may be used to build a sparse final model
    
### Specifics to xrf

* A gradient boosted tree ensemble is used to generate rules
   * Specifically, [XGBoost](https://xgboost.readthedocs.io/en/latest/)
   * For a general description of tree boosting & gradient boosting, see [this document](https://github.com/holub008/snippets/blob/master/tree_learning/tree_learning_overview.pdf).
* Overlapped rules belonging to the same subspace can be de-overlapped to improve model interpretability
    * For an illustration of de-overlapping xrf models, see the "De-overlapping rules" section below
    * For a description of this algorithm, see [this document](https://github.com/holub008/snippets/blob/master/overlapped_hyperrectangles/overlapped_hyperrectangles.pdf)

### Comparison to alternatives
Several implementations of RuleFit are available for R: [pre](https://CRAN.R-project.org/package=pre), [horserule](https://CRAN.R-project.org/package=horserule), and [rulefit](https://github.com/Zelazny7/rulefit). xrf improves on some aspects of these by:
* Usually building more accurate models at fixed number of parameters
* Usually building models faster
* Building models that predict for new factor-levels
* Providing a more concise and limited interface
* Tested & actively maintained for fewer bugs

On the last point, as of April 2019, the 'pre' and 'rulefit' packages fail to build a model on the census income example below due to bugs.

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
library(rulefit) # installed via devtools::install_git("https://github.com/Zelazny7/rulefit")
library(horserule)
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

# note, as of 2019-04-17, the pre example fails to work (with an error for a new level in model.frame). as such, the below comparison is not one to one
system.time(m_pre <- pre(above_50k ~ ., na.omit(census_train), 
                         family = 'binomial', ntrees = 100, maxdepth = 3, tree.unbiased = TRUE))
# note, as of 2019-04-25, this example fails by attempting to access names() of a sparse matrix (seems it should be using colnames())
system.time({
  m_gbm <- gbm.fit(census_train_mat, census_train$above_50k, distribution="bernoulli", interaction.depth = 3, shrinkage = 0.1, verbose = FALSE)
  rf_plan <- rulefit(m_gbm, n.trees = 100)
  m_rf <- train(rf_plan, census_train_mat, y = census_train$above_50k, family = "binomial")
})
# note that this involves no hyperparameter tuning
system.time({
  m_hrf <- HorseRuleFit(census_train_mat, census_train$above_50k, ntree=100, L=3)
})
system.time(m_xrf <- xrf(above_50k ~ ., census_train, family = 'binomial', 
             xgb_control = list(nrounds = 100, max_depth = 3)))
m_xgb <- xgboost(census_train_mat, census_train$above_50k, max_depth = 3, nrounds = 100, objective = 'binary:logistic')
m_glm <- cv.glmnet(census_train_mat, census_train$above_50k, alpha = 1)

auc(predict(m_pre, census_test), census_test$above_50k)
auc(predict(m_hrf, census_test_mat), census_test$above_50k)
auc(predict(m_xrf, census_test), census_test$above_50k)
auc(predict(m_glm, newx = census_test_mat, s = 'lambda.min'), census_test$above_50k)
auc(predict(m_xgb, newdata = census_test_mat, s = 'lambda.min'), census_test$above_50k)
```

With results (on a 2018 Macbook Pro, 16Gb Memory, 6 core i7, mojave):

| Model       | Time (s) |
| ----------- | -------- |
| xrf         | 73       |    
| pre         | 211*     |
| horserule   | 29       |

On the test set:

| Model     |  AUC     |
| --------- | -------- |
| xrf       | .924     |      
| pre       | .906*    |
| horserule | .892     |
| xgboost   | .926     | 
| glmnet    | .892     |

xrf has the highest accuracy among RuleFit models, nearly comparable to the tree ensemble. horserule delivered a model much faster, but its accuracy was no better than the LASSO fit. This is likely due to aggressive rule regularization, which may be tuned by altering the out of the box hyperparameters.

## De-overlapping rules
Overlapped rules occur when two or more rules belonging to the same subspace are not in mutual exclusion; de-overlapping guarantees that all rules belonging to the same subspace are in mutual exclusion. For example, the rules:

  * age < 50 & income < 50,000
  * age < 60 & income > 40,000
  
belong to the same subspace but are not in mutual exclusion (e.g. at age=45 & income=45,000). They can be de-overlapped to, for example: 

  * age < 50 & income < 40,000
  * age < 50 & income >= 40,000 & income < 50,000
  * age < 60 & age >= 50 & income >= 40,000 & income < 50,000
  * age < 60 & income < 60

which is one of infinite possible de-overlappings; this de-overlapping is ideal because
  * it is small
  * it allows all "effects" from the original 2 rules to be exactly captured (i.e. none of the boundaries are broken)

The following example is somewhat contrived (in that it only uses one feature), but demonstrates how de-overlapping can prove useful in interpreting your model. To de-overlap the derived ruleset, simply specify `deoverlap=TRUE`:

```{r}
set.seed(55455)
m_xrf_overlap <- xrf(above_50k ~ capital_gain, census_income, family = 'binomial', 
                     xgb_control = list(nrounds = 100, max_depth = 1), deoverlap = FALSE)
m_xrf_deoverlap <- xrf(above_50k ~ capital_gain, census_income, family = 'binomial', 
                       xgb_control = list(nrounds = 100, max_depth = 1), deoverlap = TRUE)
                       
coef_overlap <- coef(m_xrf_overlap, lambda = 'lambda.1se')
coef_deoverlap <- coef(m_xrf_deoverlap, lambda = 'lambda.1se')
```

Looking at the overlapped model:
```r
coef_overlap %>%
 filter(coefficient_lambda.1se != 0) %>%
 arrange(rule)
```

```
   coefficient_lambda.1se        term               rule
1            3.757519e+00 (Intercept)               <NA>
2           -1.811956e+00        r0_1  capital_gain<5119
3            3.465927e-09        r0_2 capital_gain>=5119
4           -3.586284e+00        r1_1  capital_gain<7074
5            4.433387e-09        r1_2 capital_gain>=7074
6           -2.026456e+00       r10_1  capital_gain<4244
7            1.402758e-09       r10_2 capital_gain>=4244
8           -1.842004e+00       r11_1  capital_gain<3048
9            1.423088e-10       r11_2 capital_gain>=3048
10           2.758517e+00       r14_1  capital_gain<3120
11          -1.295516e-10       r14_2 capital_gain>=3120
12           8.893485e-01       r50_1  capital_gain<5316
13          -1.447534e-10       r50_2 capital_gain>=5316
14           5.068947e-01       r59_1  capital_gain<4401
15          -4.496979e-11       r59_2 capital_gain>=4401
16          -8.325515e-03       r86_1  capital_gain<7566
17           1.895608e-11       r86_2 capital_gain>=7566
```

Notice that the rules are not in exclusion. To understand the impact of capital gain on income, we have to add up no less than 7 coefficients for *any* value of capital gain. Matters are made more confusing by effectively 0 coefficients on many of the '>=' rules, likely a numerical issue in the LASSO as a result of the substantial collinearity.

Now for the de-overlapped model:
```r
coef_deoverlap %>%
 filter(coefficient_lambda.1se != 0) %>%
 arrange(term)
```

```
  coefficient_lambda.1se              term                                   rule
1              -1.007898       (Intercept)                                   <NA>
2              -0.345907 X1_capital_gain_1                      capital_gain<3048
3               2.366051 X1_capital_gain_2 capital_gain>=3048 & capital_gain<3120
4              -1.520116 X1_capital_gain_3 capital_gain>=3120 & capital_gain<4244
5               1.728206 X1_capital_gain_4 capital_gain>=4244 & capital_gain<4401
6               2.888030 X1_capital_gain_6 capital_gain>=5119 & capital_gain<5316
7               3.206441 X1_capital_gain_8 capital_gain>=7074 & capital_gain<7566
8               3.927737 X1_capital_gain_9                     capital_gain>=7566
```

How slick is that! We have:

  * fewer coefficients
  * mutually exclusive coefficients
  * numerical stability
  
Effects are immediately available by doing a lookup in the exclusive rules. This is a great win for interpretability.

As mentioned above, this example is contrived in that it uses `depth=1` trees (i.e. conjunctions of size 1). As depth increases, interpretability can suffer regardless de-overlapping if the final ruleset is non-sparse. However, for certain problems, particularly small depth or sparse effects, de-overlapping can be a boon for interpretability.

