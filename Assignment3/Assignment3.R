setwd("/Users/fanfan/Documents/2022fallA/GBA462R/Assignment3")
rfj_data = read.csv("rfj.csv", header=TRUE, sep=",")
inte_data = read.csv("inte_data.csv", header=TRUE, sep=",")
############## Question 1

#### We first use the whole market profit as Rm to linear fit since we find
#### that some of them are just start ups during the 1990s, it's reasonable
#### to use the profit of all stocks in the market as a portfolio profit.

result_func_market = function(sign){
    inte_data$temp = inte_data[[sign]] - inte_data$RF
    fit_hd = lm(temp~Rm.Rf, data = inte_data)
    average_hd = mean(na.omit(inte_data[[sign]])) 
    
    alpha_hd = fit_hd$coefficients[['(Intercept)']]
    alpha_hd_std = summary(fit_hd)$coefficients[,2][[1]]
    t_hd_alpha = (alpha_hd-0)/alpha_hd_std
    p_hd_alpha = 2*pnorm(-abs(t_hd_alpha))
    
    beta_hd = fit_hd$coefficients[['Rm.Rf']]
    beta_hd_std = summary(fit_hd)$coefficients[,2][[2]]
    t_hd_beta = (beta_hd-0)/beta_hd_std
    p_hd_beta = 2*pnorm(-abs(t_hd_beta))
    
    R2_hd = summary(fit_hd)$r.squared
    return(list(average_return = average_hd,
                E_alpha = alpha_hd,
                P_alpha = p_hd_alpha,
                E_beta = beta_hd,
                P_beta = p_hd_beta,
                R2 = R2_hd))
}

#### For HD
res_HD = result_func_market('R_HD')

average_hd = res_HD$average_return #0.03442089
alpha_hd = res_HD$E_alpha # 0.01824811
p_hd_alpha = res_HD$P_alpha #0.005329507
beta_hd = res_HD$E_beta #1.069054
p_hd_beta = res_HD$P_beta # 3.073321e-11
R2_hd = res_HD$R2 #0.2738766

#### For AAPL

res_AAPL = result_func_market('R_AAPL')
average_aapl = res_AAPL$average_return #0.01907947
alpha_aapl = res_AAPL$E_alpha #0.002124545
p_aapl_alpha = res_AAPL$P_alpha #0.8688089
beta_aapl = res_AAPL$E_beta #1.137749
p_aapl_beta = res_AAPL$P_beta # 0.0003187107
R2_aapl = res_AAPL$R2 #0.09970289

#### For VZ
res_VZ = result_func_market('R_VZ')
average_VZ = res_VZ$average_return #0.009518852
alpha_VZ = res_VZ$E_alpha #-0.0005642262
p_aapl_VZ = res_VZ$P_alpha # 0.9158122
beta_VZ = res_VZ$E_beta #0.5341991
p_aapl_VZ = res_VZ$P_beta # 4.642913e-05
R2_VZ = res_VZ$R2 #0.1241768

### For CSCO

res_CSCO = result_func_market('R_CSCO')
average_CSCO = res_CSCO$average_return #0.06354
alpha_CSCO = res_CSCO$E_alpha #0.04250946
p_aapl_CSCO = res_CSCO$P_alpha # 1.659964e-05
beta_CSCO = res_CSCO$E_beta #1.50628
p_aapl_CSCO = res_CSCO$P_beta # 3.97091e-10
R2_CSCO = res_CSCO$R2 #0.2538626

### However, it seems that the linear regression results are not good
### (considering the R2 value), we could not tell if it attributes to 
### the lack of factors or the wrong selection of index, we would like
### to use the Wilshire 5000 index for another try to find that if a
### different pool of stock is more suitable for the regression.

result_func_5000 = function(sign){
    inte_data$temp = inte_data[[sign]] - inte_data$RF
    fit_hd = lm(temp~RW5000.RF, data = inte_data)
    average_hd = mean(na.omit(inte_data[[sign]])) 
    
    alpha_hd = fit_hd$coefficients[['(Intercept)']]
    alpha_hd_std = summary(fit_hd)$coefficients[,2][[1]]
    t_hd_alpha = (alpha_hd-0)/alpha_hd_std
    p_hd_alpha = 2*pnorm(-abs(t_hd_alpha))
    
    beta_hd = fit_hd$coefficients[['RW5000.RF']]
    beta_hd_std = summary(fit_hd)$coefficients[,2][[2]]
    t_hd_beta = (beta_hd-0)/beta_hd_std
    p_hd_beta = 2*pnorm(-abs(t_hd_beta))
    
    R2_hd = summary(fit_hd)$r.squared
    return(list(average_return = average_hd,
                E_alpha = alpha_hd,
                P_alpha = p_hd_alpha,
                E_beta = beta_hd,
                P_beta = p_hd_beta,
                R2 = R2_hd))
}
### And we then do the calculation again

#### For HD
res_HD_5000 = result_func_5000('R_HD')

average_hd_5000 = res_HD_5000$average_return #0.03442089
alpha_hd_5000 = res_HD_5000$E_alpha #0.0206268
p_hd_alpha_5000 = res_HD_5000$P_alpha ##0.001524037
beta_hd_5000 = res_HD_5000$E_beta #1.062709
p_hd_beta_5000 = res_HD_5000$P_beta # 9.021643e-11
R2_hd_5000 = res_HD_5000$R2 #0.2642563

#### For AAPL

res_AAPL_5000 = result_func_5000('R_AAPL')
average_aapl_5000 = res_AAPL_5000$average_return #0.01907947
alpha_aapl_5000 = res_AAPL_5000$E_alpha #0.004527436
p_aapl_alpha_5000 = res_AAPL_5000$P_alpha #0.7215513
beta_aapl_5000 = res_AAPL_5000$E_beta #1.144956
p_aapl_beta_5000 = res_AAPL_5000$P_beta # 0.0003472357
R2_aapl_5000 = res_AAPL_5000$R2 #0.09859017

#### For VZ
res_VZ_5000 = result_func_5000('R_VZ')
average_VZ_5000 = res_VZ_5000$average_return #0.009518852
alpha_VZ_5000 = res_VZ_5000$E_alpha #0.0004652197
p_aapl_VZ_5000 = res_VZ_5000$P_alpha # 0.9294906
beta_VZ_5000 = res_VZ_5000$E_beta #0.5483006
p_aapl_VZ_5000 = res_VZ_5000$P_beta #3.483911e-05
R2_VZ_5000 = res_VZ_5000$R2 #0.1277358

### For CSCO

res_CSCO_5000 = result_func_5000('R_CSCO')
average_CSCO_5000 = res_CSCO_5000$average_return #0.06354
alpha_CSCO_5000 = res_CSCO_5000$E_alpha #0.04581354
p_aapl_CSCO_5000 = res_CSCO_5000$P_alpha # 2.957047e-06
beta_CSCO_5000 = res_CSCO_5000$E_beta #1.498821
p_aapl_CSCO_5000 = res_CSCO_5000$P_beta # 9.612617e-10
R2_CSCO_5000 = res_CSCO_5000$R2 #0.2454165

### Unluckily, we find that the R2 value, as well as the alpha and beta value
### doesn't change much, which means the changing of portfolio profit doesn't
### affect the regression result. And it's unreasonable to try with some strong
### index like Dow Jones because most of the companies doesn't belong to this
### pool in 1990s even though they are stocks in that pool now. The most possible
### reason why the regression model doesn't perform well is because the lack of
### factors and this is why we find there are some common models like FF3 model.

### To answer the discuss question

### 1)
### We use the whole market profit and the Wilshire 5000 index respectively 
### as the return on the market portfolio for regression. Though we are not 
### sure whether they belong to any index pool in 1990s, considering that some of 
### the companies are founded at that time, we decide to apply the index of small companies 
### (whole market profit and Wilshire 5000). For the data frequency, we use the monthly 
### data to balance the accuracy and the efficiency of linear regression.

### 2) todo!!!
### 3) todo!!!



############## Question 2
########### a
#### For Tropicana

fit_trop = lm(q1~p1, data=rfj_data)
coe_trop = fit_trop$coefficients[['p1']]
ela_trop = coe_trop * mean(rfj_data$p1)/mean(rfj_data$q1)
int_trop = fit_trop$coefficients[['(Intercept)']]
ela_trop #-3.296077

#### For Minute Maid

fit_min = lm(q2~p2, data = rfj_data)
coe_min = fit_min$coefficients[['p2']]
int_min = fit_min$coefficients[['(Intercept)']]
ela_min = coe_min * mean(rfj_data$p2)/mean(rfj_data$q2)
ela_min #-3.912924

### For Private Label

fit_pri = lm(q3~p3, data = rfj_data)
coe_pri = fit_pri$coefficients[['p3']]
int_pri = fit_pri$coefficients[['(Intercept)']]
ela_pri = coe_pri * mean(rfj_data$p3)/mean(rfj_data$q3)
ela_pri #-3.525809

#beta1_hat = cov(rfj_data$p1, rfj_data$q1) / var(rfj_data$p1)
#beta1_hat
#plot(rfj_data$p1,rfj_data$q1)
#abline(lm(rfj_data$q1~rfj_data$p1), col="red")

######## b

profit_func1 = function(p){
    return((coe_trop*p+int_trop)*(p-0.01))
}
profit_func2 = function(p){
    return((coe_min*p+int_min)*(p-0.01))
}
profit_func3 = function(p){
    return((coe_pri*p+int_pri)*(p-0.01))
}

optimize(profit_func1, c(0,0.05),tol = 0.0001, maximum = T)

#plot((coe_trop*rfj_data$p1+int_trop)*(rfj_data$p1-0.01)~rfj_data$p1)

optimize(profit_func2,c(0,0.041),tol = 0.0001, maximum = T)

#plot((coe_min*rfj_data$p2+int_min)*(rfj_data$p2-0.01)~rfj_data$p2)

optimize(profit_func3, c(0,0.04),tol = 0.0001, maximum = T)

#plot((coe_pri*rfj_data$p3+int_pri)*(rfj_data$p3-0.01)~rfj_data$p3)


