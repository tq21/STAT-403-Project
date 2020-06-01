data0 = read.table('/Users/qinyu/Desktop/Admission_Predict_Ver1.1.csv', sep = ',')
data0
names(data0) = c('Serial No.', 'GRE Score', 'TOEFL Score',
                 'University Rating', 'SOP', 'LOR', 'CGPA', 'Research', 'Chance of Admit')
data_R = data0$`Chance of Admit`[data0$Research=='1']
data_NR = data0$`Chance of Admit`[data0$Research=='0']
t.test(data_R, data_NR)


set.seed(195163)
n_R = length(data_R)
n_NR = length(data_NR)
n = n_R+n_NR

data_pull = c(data_R, data_NR)
diff_med = abs(median(data_R)-median(data_NR))

N_per = 10000
diff_med_per = rep(NA, N_per)
for(i_per in 1:N_per){
  w_per = sample(n, n, replace=F)
  data_per = data_pull[w_per]
  data_R_new = data_per[1:n_R]
  data_NR_new = data_per[(n_R+1):n]
  diff_new = abs(median(data_R_new)-median(data_NR_new))
  diff_med_per[i_per] = diff_new
}

which(diff_med_per>diff_med)

(length(which(diff_med_per > diff_med))+1)/N_per

hist(diff_med_per, col="cyan", xlim = c(0, 0.16))
abline(v = diff_med, col="red", lwd=6)


diff_mean = abs(mean(data_R)-mean(data_NR))

N_per = 10000
diff_mean_per = rep(NA, N_per)
for(i_per in 1:N_per){
  w_per = sample(n, n, replace=F)
  data_per = data_pull[w_per]
  data_R_new = data_per[1:n_R]
  data_NR_new = data_per[(n_R+1):n]
  diff_new = abs(mean(data_R_new)-median(data_NR_new))
  diff_mean_per[i_per] = diff_new
}

which(diff_mean_per>diff_mean)

(length(which(diff_mean_per > diff_mean))+1)/N_per

hist(diff_mean_per, col="cyan", xlim = c(0, 0.17))
abline(v = diff_mean, col="red", lwd=6)
