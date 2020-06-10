## Some histograms:
source("analysis.R")
source("cleaning.R")
library("dplyr")
library("ggplot2")
library("gridExtra")

grid.arrange(gre_hist, toefl_hist, gpa_hist, rating_hist,
             sop_hist, lor_hist, res_hist, chance_hist,
             ncol = 4)


## Some box plots:

grid.arrange(box_1, box_2, box_3, box_4, box_5, box_6, ncol = 2)

