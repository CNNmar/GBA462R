########### Assignment 1#############

########### Problem 1
##Q1

prob_no_conflict = pnorm(5,4.5,0.37)
prob_cut_in = 1 - prob_no_conflict

## prob_cut_in = 0.08829145
## Therefore, the prob that the baseball game will cut into that program is 0.08829145

##Q2

prob_show = pnorm(4.5,4.5,0.37)

## prob_show = 0.5
## Therefore, the prob of the 0.5h show is 0.5

##Q3

prob_long_show = pnorm(4,4.5,0.37)

## prob_long_show = 0.08829145
## Therefore, the prob of the 1h show is 0.08829145

##Q4
library("lubridate")
cutoff_time = qnorm(0.99, 16.5, 0.37)
min_time = (cutoff_time - round(cutoff_time))*60
date_cut = make_datetime(hour = round(cutoff_time), min = min_time)
finaltime = as.character(date_cut, format='%H:%M:%S')

## finaltime = "17:21:00"
## Therefore, the 99% ensured cut-off time is 17:21:00 

## Q5
cutoff_time_tail1 = qnorm(0.025, 16.5, 0.37)
cutoff_time_tail2 = qnorm(0.975, 16.5, 0.37)
min_time_1 = (cutoff_time_tail1 - round(cutoff_time_tail1))*60
min_time_2 = (cutoff_time_tail2 - round(cutoff_time_tail2))*60
date_cut_1 = make_datetime(hour = round(cutoff_time_tail1), min = min_time_1)
date_cut_2 = make_datetime(hour = round(cutoff_time_tail2), min = min_time_2)
finaltime_1 = as.character(date_cut_1, format='%H:%M:%S')
finaltime_2 = as.character(date_cut_2, format='%H:%M:%S')

## finaltime_1 =  "15:47:00"
## finaltime_2 =  "17:13:00"
## Therefore, the time slot of 95% insurance is 15:47:00 - 17:13:00 



########## Problem2

####### a
df = data.frame(c(0.58,0.27,NaN),c(0.12,0.03,NaN), c(NaN, NaN, 1.00))
rownames(df) = c("X = 0", "X = 1", "Total")
colnames(df) = c("Y = 0", "Y = 1", "Total")
df[1:2,"Total"] = df$`Y = 0`[1:2] + df$`Y = 1`[1:2]
df["Total",] = df["X = 0",] + df["X = 1",]

#######Therefore, the table will be like
######         Y = 0 Y = 1 Total
######  X = 0  0.58  0.12   0.7
######  X = 1  0.27  0.03   0.3
######  Total  0.85  0.15   1.0

####### b

### It belongs to the discrete distribution
### And the distribution is Bernoulli distribution.

###### c
E_Y = df['Total',1]*0 + df['Total',2]*1 

## Therefore, the E(Y) is 0.15 and this number means 15% earnings needs to be restated 
## no matter that whether IFE serves on the board.

##### d

weight_x = c(0,1,0)
E_X = sum(df$Total*weight_x)

## Therefore, the E(X) is 0.3 and this number means in 30% cases,
## IFE serves on the firm's board.

##### e
E_Y_0 = df['X = 0',1]*0/df['X = 0',3] + df['X = 0',2]*1/df['X = 0',3]
E_Y_1 = df['X = 1',1]*0/df['X = 1',3] + df['X = 1',2]*1/df['X = 1',3]

## Therefore, the E(Y|X=0) is 0.1714286 and the E(Y|X=1) is 0.1
## E(Y|X=0) means that if there is no IFE served on the board, the expectation of restated earning is 0.1714286, which means that 17.14286% earning needs to be restated with no IFE served.
## E(Y|X=1) means that if there is IFE served on the board, the expectation of restated earning is 0.1, which means that 10% earning needs to be restated with IFE served.
## And it tells that having IFEs on corporate boards can reduce the possibility of restated earnings from the data of this table.

##### f

### if I know it has to restates its earning
P_X_1 = df$`Y = 1`[2]*1/df$`Y = 1`[3]

### if I don't know it had to restate its earnings
P_X = df$Total[2]*1/df$Total[3]

## Therefore, if I know it has to restate its earning, the prob is 0.2
## If I don't know whether it has to restate its earning, the prob is 0.3

##### g

## They are not independent, because if they are independent, P(X = 1) = P(X = 1|Y = 1)
## However, in question f, we have proved that those two values are not equal
## Therefore, they are not independent.