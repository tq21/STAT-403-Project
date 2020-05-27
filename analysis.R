library("dplyr")
library("ggplot2")
library("gridExtra")

# read in dataframe
df <- read.csv("./data/Admission_Predict_Ver1.1.csv", stringsAsFactors = FALSE)

# cleaning
df <- df %>%
  select(-Serial.No.)

# summary plots
# scatter_sum_plot <- plot(df, pch = 19)

# standardized test scores
gre_mean <- mean(df$GRE.Score)
gre_hist <- ggplot(df, aes(x = GRE.Score)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 3) +
  ggtitle("Histogram of GRE Scores") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = gre_mean, color = "red", size = 1.5)

toefl_mean <- mean(df$TOEFL.Score)
toefl_hist <- ggplot(df, aes(x = TOEFL.Score)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 2) +
  ggtitle("Histogram of TOEFL Scores") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = toefl_mean, color = "red", size = 1.5)


gpa_mean <- mean(df$CGPA)
gpa_hist <- ggplot(df, aes(x = CGPA)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 0.2) +
  ggtitle("Histogram of GPA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = gpa_mean, color = "red", size = 1.5)













