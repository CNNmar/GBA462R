######## Clear
rm(list=ls()); # deleting all objects from the memory
gc(); # garbage collection - releasing memory when an object is no longer used 

library(sandwich)
library(lmtest)

setwd("/Users/fanfan/Documents/2022fallA/GBA462R/Assignment5")
library("readxl")
data2 = read_excel("Store24Data.xlsx")

plot(Profit~ MTenure, main= "Scatterplot of Profit vs. MTenure", cex=0.6, pch=19, spread= FALSE, lty='dashed', data=data2)

fit = lm(log(Profit)~MTenure,data=data2)
summary(fit)


fit2 = lm(Profit~log(MTenure+1),data=data2)
summary(fit2)

fit3 = lm(Profit~MTenure+I(MTenure**2),data=data2)
summary(fit3)
with(data2, points(MTenure, fitted(fit5), col="red", pch=20))


fit0 = lm(Profit~MTenure,data=data2)
summary(fit0)

fit3 = lm(Profit~MTenure+I(MTenure**2),data=data2)
summary(fit3)

fit4 = lm(Profit~CTenure+I(CTenure**2),data=data2)
summary(fit4)

fit5 = lm(Profit~MTenure+I(MTenure**2)+CTenure+I(CTenure**2)+Pop+Comp+Visibility+PedCount+Res+Hours24,data=data2)
summary(fit5)

fit6 = lm(Profit~MTenure+I(MTenure**2)+CTenure+I(CTenure**2), data = data2)
summary(fit6)

profit_func3 = function(p){
    return(fit5$coefficients[['MTenure']] * p + p**2 * fit5$coefficients[['I(MTenure^2)']])
}

optimize(profit_func3, c(min(data2$MTenure),max(data2$MTenure)),tol = 0.01, maximum = T)

profit_func3(179)
profit_func3(180)
