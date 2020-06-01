# Cross Validation

df = datasets_14872_228180_Admission_Predict_Ver1_1

names(df)[names(df) == "Chance of Admit"] <- "Chance.of.Admit"
names(df)[names(df) == "GRE Score"] <- "GRE.Score"
names(df)[names(df) == "TOEFL Score"] <- "TOEFL.Score"
names(df)[names(df) == "University Rating"] <- "University.Rating"



fit <- lm(Chance.of.Admit ~
                        GRE.Score +
                        TOEFL.Score +
                        University.Rating +
                        SOP +
                        LOR +
                        CGPA +
                        Research, data = df)
#library(DAAG)

summary(fit)$r.squared
summary(fit)$adj.r.squared

#Get RMSE
rss <- c(crossprod(fit$residuals))
mse <- rss / length(fit$residuals)
rmse <- sqrt(mse)

#Cross validate results
cv <- cv.lm(df,fit,m=5,plotit=FALSE)
cv.rmse <- sqrt(attr(cv,"ms"))
cat("RMSE for full model: ",rmse)
cat("RMSE for CV: ",cv.rmse)

