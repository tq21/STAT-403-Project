source("cleaning.R")
set.seed(5312020)

# Linear regression model
lin_model_gre = lm(Chance.of.Admit ~ GRE.Score, data = df)
summary(lin_model_gre)

# residual plot
plot(x = df$GRE.Score, y = lin_model_gre$residuals, ylim = c(-0.3, 0.3),
     xlab = "GRE Score", ylab = "Residuals", pch = 16, col = "orange")
abline(h = 0, col = "black", lwd = 2)

# 90% CI of the slope
slope_fit = summary(lin_model_gre)$coeff[2,1]
slope_se = summary(lin_model_gre)$coeff[2,2]
slope_fit-qnorm(0.95)*slope_se
# lower bound
slope_fit+qnorm(0.95)*slope_se
# upper bound

# Wild bootstrap
n = nrow(df)
y_predict = predict(lin_model_gre)
fit_coeff = lin_model_gre$coefficients
B = 5000
coeff_BT_wild = matrix(NA, nrow = B, ncol = 2)

for(i_BT in 1:B){
  set.seed(i_BT * 341)
  w = sample(n, n, replace = T)
  y_bt = y_predict + rnorm(n, sd = abs(lin_model_gre$residuals))
  data1_BT = data.frame(GRE.Score = df$GRE.Score,
                        Chance.of.Admit = y_bt)
  fit_BT = lm(Chance.of.Admit ~ GRE.Score, data = data1_BT)
  coeff_BT_wild[i_BT,] = fit_BT$coefficients
}

colnames(coeff_BT_wild) = c("Intercept","Slope")

var(coeff_BT_wild)
mean((coeff_BT_wild[,1]-fit_coeff[1])^2)

quantile(coeff_BT_wild[,'Intercept'], c(0.05,0.95))
quantile(coeff_BT_wild[,'Slope'], c(0.05,0.95))

# plots and histograms
hist(coeff_BT_wild[,'Intercept'], col = "skyblue",
     main = "BT estimates of Intercept",
     xlab = "GRE Score")
abline(v = fit_coeff[1], lwd = 3, col = "red")

hist(coeff_BT_wild[,'Slope'], col = "palegreen",
     main = "BT estimates of Slope",
     xlab = "GRE Score")
abline(v = fit_coeff[2], lwd = 3, col = "brown")

plot(coeff_BT_wild, cex=0.5)
points(x = fit_coeff[1], y = fit_coeff[2], cex = 5, pch = "+", col = "red")





















