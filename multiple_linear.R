## Creating a multiple linear regression on all variables
library("caret")
source("cleaning.R")

attach(df)

## train/test split, 80/20
sample_size <- floor(0.8 * nrow(df))
set.seed(2020)
train_ind <- sample(seq_len(nrow(df)), size = sample_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

multi_lin_model <- lm(Chance.of.Admit ~
                        GRE.Score +
                        TOEFL.Score +
                        University.Rating +
                        SOP +
                        LOR +
                        CGPA +
                        Research, data = train)

pred <- predict.lm(multi_lin_model, test)
mean((test$Chance.of.Admit-pred)^2)




