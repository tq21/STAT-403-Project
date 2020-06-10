# Cross Validation

source("cleaning.R")

fit <- lm(Chance.of.Admit ~
                        GRE.Score +
                        TOEFL.Score +
                        University.Rating +
                        SOP +
                        LOR +
                        CGPA +
                        Research, data = df)
library(DAAG)

summary(fit)$r.squared
summary(fit)$adj.r.squared

#Get RMSE
rss <- c(crossprod(fit$residuals))
mse <- rss / length(fit$residuals)
rmse <- sqrt(mse)

#Cross validate results
cv <- cv.lm(df, fit, m=5, plotit=FALSE)
cv.rmse <- sqrt(attr(cv,"ms"))
cat("RMSE for full model: ",rmse)
cat("RMSE for CV: ",cv.rmse)

library(tidyverse)
library(caret)

attach(df)

set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
model <- train(Chance.of.Admit ~., data = df, method = "lm",
               trControl = train.control)
model$results


set.seed(414)
train.control <- trainControl(method = "cv", number = 10)
model <- train(Chance.of.Admit ~., data = df, method = "lm",
               trControl = train.control)
model$results$RMSE

B = 10
rmse_vals = matrix(NA, nrow = B, ncol = 7)
for (i in 1:B) {
  set.seed(1314*i)
  train.control <- trainControl(method = "cv", number = 10)
  model_7 <- train(Chance.of.Admit ~., data = df, method = "lm",
                 trControl = train.control)
  model_6 <- train(Chance.of.Admit ~ GRE.Score +
                   TOEFL.Score +
                   University.Rating +
                   LOR +
                   CGPA +
                   Research, data = df, method = "lm", trControl = train.control)
  model_5 <- train(Chance.of.Admit ~ GRE.Score +
                   TOEFL.Score +
                   LOR +
                   CGPA +
                   Research, data = df, method = "lm", trControl = train.control)
  model_4 <- train(Chance.of.Admit ~ GRE.Score +
                   LOR +
                   CGPA +
                   Research, data = df, method = "lm", trControl = train.control)
  model_3 <- train(Chance.of.Admit ~ GRE.Score +
                   LOR +
                   CGPA, data = df, method = "lm", trControl = train.control)
  model_2 <- train(Chance.of.Admit ~ GRE.Score +
                   CGPA, data = df, method = "lm", trControl = train.control)
  model_1 <- train(Chance.of.Admit ~ CGPA, data = df, method = "lm", trControl = train.control)
  
  rmse_vals[i,1] = model_1$results$RMSE
  rmse_vals[i,2] = model_2$results$RMSE
  rmse_vals[i,3] = model_3$results$RMSE
  rmse_vals[i,4] = model_4$results$RMSE
  rmse_vals[i,5] = model_5$results$RMSE
  rmse_vals[i,6] = model_6$results$RMSE
  rmse_vals[i,7] = model_7$results$RMSE
}

cl <- rainbow(B)
plot(1, type="n", xlab = "Number of features in model",
     ylab = "RMSE", xlim=c(1, 7), ylim=c(0.058, 0.067))
for (i in 1:B) {
  lines(rmse_vals[i,], type = "l", col = cl[i])
}

## backward
library(ISLR)
library(leaps)
regfit.bwd = regsubsets(Chance.of.Admit ~ ., data = df, method = "exhaustive")
summary(regfit.bwd)


train.control <- trainControl(method = "cv", number = 1)
model_5 <- train(Chance.of.Admit ~ GRE.Score +
                   TOEFL.Score +
                   LOR +
                   CGPA +
                   Research, data = df, method = "lm", trControl = train.control)

model_1 <- train(Chance.of.Admit ~ CGPA, data = df, method = "lm", trControl = train.control)

summary(model_1)

model_1$results



