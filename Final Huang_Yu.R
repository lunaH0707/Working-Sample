sink("Final Huang_Yu.txt")
library(caTools)
library(caret)
library(ISLR2)

data(Hitters)
set.seed(666)
sample.data <- sample.split(Hitters, SplitRatio = 0.75)
train_data <- subset(Hitters, sample.data==T)
test_data <- subset(Hitters, sample.data==F )

#(a)
library(tree)
bbsal <- subset(Hitters, !is.na(Salary), select=c("Salary","Years", "Hits", "Runs", "HmRun", "Errors"))
bbsal$sal <- log(bbsal$Salary); bbsal$Salary <- NULL
subset_train <- bbsal[1:nrow(train_data), ]
tree_Hitters <- tree(sal ~ Years + Hits + Runs + HmRun + Errors, data=subset_train)
summary(tree_Hitters)
plot(tree_Hitters)
text(tree_Hitters, pretty = 0)
print(tree_Hitters)

subset_test <- bbsal[1:nrow(test_data), ]
tree_pred = predict(tree_Hitters,subset_test)
test_mse = mean((tree_pred-subset_test$sal)^2)
test_mse



#(b)
set.seed(666)
tree.T0 <- tree(sal ~ Years + Hits + Runs + HmRun + Errors, data=subset_train)
summary(tree.T0)
prune.tree(tree.T0,best=5)
summary(prune.tree(tree.T0,best=5))
tree.seq <- prune.tree(tree.T0)
plot(tree.seq,ylim=c(0,200))
cv.T0 <- cv.tree(tree.T0); plot(cv.T0,ylim=c(0,200))
cv.T0$size[cv.T0$dev==min(cv.T0$dev)]
res.tree <- prune.tree(tree.T0,best=7)
mean( (subset_test$sal - predict(res.tree,newdata=subset_test))^2 )
summary(res.tree)$dev/nrow(subset_train)



#(c)

library(randomForest)
tree.bag <- randomForest(sal ~ Years + Hits + Runs + HmRun + Errors, data=subset_train, ntree=500, mtry=2,importance = TRUE)
importance(tree.bag)
sal.pred.bag <- predict(tree.bag, subset_test)
mean((subset_test$sal - sal.pred.bag)^2)




#(d)
tree.rf <- randomForest(sal ~ Years + Hits + Runs + HmRun + Errors, data=subset_train, ntree=500, mtry=1,importance = TRUE)
importance(tree.rf)
sal.pred.rf <- predict(tree.rf, subset_test)
mean((subset_test$sal - sal.pred.rf)^2)

tree.rf <- randomForest(sal ~ Years + Hits + Runs + HmRun + Errors, data=subset_train, ntree=500, mtry=2/3,importance = TRUE)
importance(tree.rf)
sal.pred.rf <- predict(tree.rf, subset_test)
mean((subset_test$sal - sal.pred.rf)^2)


#Q4
#(a)
setwd('C:\\Users\\yuerl\\Desktop\\Gu Econ\\Adv DA\\final')
set.seed(666)
load("fmls.rda")
sample.data <- sample.split(fmls, SplitRatio = 0.75)
train_data <- subset(fmls, sample.data==T)
test_data <- subset(fmls, sample.data==F )
model_list_price <- lm(LIST_PRICE ~ NBHD + YEAR + SQ_FT + BEDS + BATHS + BUILT_YEAR, data = train_data)
summary(model_list_price)

model_ppsf <- lm(PPSF ~ NBHD + YEAR + BEDS + BATHS + `BUILT_YEAR`, data = train_data)
summary(model_ppsf)

#(b)

agent_dummies <- model.matrix(~ AGENT_NAME - 1, data = train_data)
design_matrix <- cbind(train_data[, c("LIST_PRICE", "NBHD", "YEAR", "SQ_FT", "BEDS", "BATHS", "BUILT_YEAR")], agent_dummies)
num_agent_columns <- ncol(agent_dummies)
num_columns_added <- num_agent_columns
num_columns_added
model_agent <- lm(LIST_PRICE ~ ., data = design_matrix)
R2_agent <- summary(model_agent)$r.squared
R2_no_agent <- summary(model_list_price)$r.squared
R2_difference <- R2_agent - R2_no_agent

R2_difference


#(c)
library(mgcv)
model_additive <- gam(LIST_PRICE ~ s(SQ_FT) + s(BUILT_YEAR), data = train_data)
plot(model_additive, select = 1:2, scale = 0)

#(d)
library(glmnet)
x <- model.matrix(LIST_PRICE ~ NBHD + YEAR + SQ_FT + BEDS + BATHS + BUILT_YEAR, alpha=1, data = train_data)
y <- train_data$LIST_PRICE
lasso_model <- cv.glmnet(x, y, alpha=1, nfolds = 10)
opt_lambda <- lasso_model$lambda.min
lasso_fit <- glmnet(x, y, lambda = opt_lambda)
print(coef(lasso_fit))

#(e)
library(randomForest)
X <- train_data[, c("NBHD", "YEAR", "SQ_FT", "BEDS", "BATHS", "BUILT_YEAR")]
y <- train_data$LIST_PRICE
rf_model <- randomForest(x = X, y = y, ntree = 500)
print(rf_model)
test_X <- test_data[, c("NBHD", "YEAR", "SQ_FT", "BEDS", "BATHS", "BUILT_YEAR")]
predictions <- predict(rf_model, newdata = test_X)
actual_prices <- test_data$LIST_PRICE
mse_e <- mean((actual_prices - predictions)^2)
mse_e

#(f)
mse_a <- mean((test_data$LIST_PRICE - predict(model_list_price, newdata = test_data))^2)
mse_c <- mean((test_data$LIST_PRICE - predict(model_agent, newdata = design_matrix))^2)
mse_d <- mean((test_data$LIST_PRICE - predictions)^2)
mse_list <- c(mse_a, mse_c, mse_d, mse_e)
regressions <- c("OLS", "OLS with Agent Name", "Random Forest", "Lasso")
comparison <- data.frame(Regression = regressions, Test_MSE = mse_list)
comparison

sink()
      