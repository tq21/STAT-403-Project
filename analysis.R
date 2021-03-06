library("ggplot2")
library("gridExtra")
source("cleaning.R")

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

rating_hist <- ggplot(df, aes(x = University.Rating)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 1) +
  ggtitle("Histogram of University Rating") +
  theme(plot.title = element_text(hjust = 0.5))

sop_hist <- ggplot(df, aes(x = SOP)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 0.5) +
  ggtitle("Histogram of Statement of Purpose Strength") +
  theme(plot.title = element_text(hjust = 0.5))

lor_hist <- ggplot(df, aes(x = LOR)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 0.5) +
  ggtitle("Histogram of Letter of Recommendation Strength") +
  theme(plot.title = element_text(hjust = 0.5))

res_hist <- ggplot(df, aes(x = Research)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 1) +
  ggtitle("Histogram of Research Experience") +
  theme(plot.title = element_text(hjust = 0.5))

chance_hist <- ggplot(df, aes(x = Chance.of.Admit)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 0.05) +
  ggtitle("Histogram of Confidence of Success") +
  theme(plot.title = element_text(hjust = 0.5))

box_1 <- ggplot(df, aes(x = factor(University.Rating), y = GRE.Score, fill = factor(Research))) + 
  geom_boxplot()

box_2 <- ggplot(df, aes(x = factor(University.Rating), y = TOEFL.Score, fill = factor(Research))) + 
  geom_boxplot()

box_3 <- ggplot(df, aes(x = factor(University.Rating), y = SOP, fill = factor(Research))) + 
  geom_boxplot()

box_4 <- ggplot(df, aes(x = factor(University.Rating), y = LOR, fill = factor(Research))) + 
  geom_boxplot()

box_5 <- ggplot(df, aes(x = factor(University.Rating), y = CGPA, fill = factor(Research))) + 
  geom_boxplot()

box_6 <- ggplot(df, aes(x = factor(University.Rating), y = Chance.of.Admit, fill = factor(Research))) + 
  geom_boxplot()









