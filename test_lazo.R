library(ISLR)
library(dplyr)
library(magrittr)
library(glmnet)
library(stats)
library(mgcv)
library(splines)

college  = College
set.seed(1234)


train = slice_sample(college, prop = 0.6)
test = anti_join(college, train)

lm_fit = lm(Apps ~ ., data = train)

summary(lm_fit)

sqrt(mean((test$Apps - predict(lm_fit, test))^2)) # 982.6215

# hace el lazo rigt
ridge_fit = cv.glmnet(
  x = model.matrix(Apps ~ ., data = train)[, -1],
  y = train$Apps,
  alpha = 0
)

ridge_fit$lambda

opt_lambda = ridge_fit$lambda.min
log_opt_lambda = log(opt_lambda)

test_matrix = model.matrix(Apps ~ ., data = test)[, -1]
sqrt(mean((test$Apps - predict(ridge_fit, s = opt_lambda, newx = test_matrix))^2)) # 957.7957
 
##############################################
lm_fit = lm(Apps ~ poly(Accept, degree = 4) + ns(Enroll) +s(Top10perc) , data = train)
sqrt(mean((test$Apps - predict(lm_fit, test))^2)) #

gam_fit <- gam(
  formula = Apps ~ mgcv::s(Accept, bs = "cr") + mgcv::s(Enroll, bs = "cr") + mgcv::s(Top10perc, bs = "cr"),
  data = train,
  method = "REML"
)

