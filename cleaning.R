## cleaning
library("dplyr")

df <- read.csv("./data/Admission_Predict_Ver1.1.csv", stringsAsFactors = FALSE)
df <- df %>%
  select(-Serial.No.)

df$Research <- as.factor(df$Research)