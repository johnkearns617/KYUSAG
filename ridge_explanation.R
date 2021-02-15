library(rsample)  # data splitting 
library(glmnet)   # implementing regularized regression approaches
library(dplyr)    # basic data manipulation procedures
library(ggplot2)  # plotting


set.seed(123)
split_2017 <- initial_split(train[!is.na(train$score_diff),], prop = .7)
train_2017 <- training(split_2017)
test_2017  <- testing(split_2017)

# Create training and testing feature model matrices and response vectors.
# we use model.matrix(...)[, -1] to discard the intercept
train_x <- model.matrix(score_diff ~ team+opponent+location, train_2017)[, -1]
train_y <- train_2017$score_diff

test_x <- model.matrix(score_diff ~ team+opponent+location, test_2017)[, -1]
test_y <- test_2017$score_diff

# What is the dimension of of your feature matrix?
dim(train_x)

# Apply Ridge regression to ames data
ridge <- glmnet(
  x = train_x,
  y = train_y,
  alpha = 0
)

plot(ridge, xvar = "lambda")


# lambdas applied to penalty parameter
ridge$lambda %>% head()
## [1] 5202.782 4740.581 4319.441 3935.714 3586.076 3267.499

# coefficients for the largest and smallest lambda parameters
coef(ridge)[c("teamWilliam & Mary","opponentWilliam & Mary"), 100]
##   teamWilliam & Mary opponentWilliam & Mary 
##    1.690289              -2.224156 
coef(ridge)[c("teamWilliam & Mary","opponentWilliam & Mary"), 1] 
##   teamWilliam & Mary opponentWilliam & Mary 
##     3.008575e-36          -1.954136e-36 


# Apply CV Ridge regression to ames data
cv_ridge <- cv.glmnet(
  x = train_x,
  y = train_y,
  alpha = 0
)

# plot results
plot(cv_ridge)

ridge_min <- glmnet(
  x = train_x,
  y = train_y,
  alpha = 0
)

plot(ridge_min, xvar = "lambda")
abline(v = log(ridge$lambda.1se), col = "red", lty = "dashed")


# predict
pred <- predict(cv_ridge, s = cv_ridge$lambda.min, newx = test_x)
mean((test_y - pred)^2)
## [1] 127.0505
mean((train$score_diff-train$pred_score_diff)^2,na.rm=TRUE)
## [1] 106.5144 
# Ridge is slightly worse than pure linear regression in predicting outcomes

round(100 * mean(sign(pred) == sign(test_y), na.rm = T), 1)
# 73.4

round(100 * mean(sign(train$pred_score_diff) == sign(train$score_diff), na.rm = T), 1)
# 76.8

