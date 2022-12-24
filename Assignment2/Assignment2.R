time_data = read.table("DeliveryTimes.csv",header=TRUE, sep=",")
time_data

########### Problem 1
## 1.
sample_avg = mean(time_data$DeliveryTime)
sample_sd = sd(time_data$DeliveryTime)
sample_var = var(time_data$DeliveryTime)
sample_avg ### 45.10607
sample_sd ### 2.469763
sample_var ### 6.099727

##2. 
${\tfrac {1}{12}}(b-a)^{2}$