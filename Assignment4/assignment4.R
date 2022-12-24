setwd("/Users/fanfan/Documents/2022fallA/GBA462R/Assignment4")
library("readxl")
data2 = read_excel("ShipCaseData.xlsx", sheet = "Data")
fit = lm(SalePrice~DWT+Age+Cindex,data=data2)
summary(fit)

predict(fit, data.frame(DWT=172,Age=11,Cindex=12479), interval="confidence", level = 0.95)
predict(fit, data.frame(DWT=172,Age=11,Cindex=12479), interval="prediction", level = 0.95)