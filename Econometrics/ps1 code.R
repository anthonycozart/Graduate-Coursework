setwd("~/Desktop/Michigan/PoliSci 787/Problem Set 1")
library(foreign)

data <- read.dta("AR_data_distribution.dta")
dim(data) # 35 rows, 67 columns

nt = 18
nc = 17

###### Question 3 ###### 
# function calculating all summary stats
f_sum_stats <- function(x) {
  c(min(x), mean(x), max(x), sd(x), var(x))
} 

t_stats <- t_stats()

# create list of column numbers for interesting covariates
x <- c(30:33)
x <- append(x, 50:58, after = 33)
x
# summary statistic names
naming <- c('Min', 'Mean', 'Max', 'SD', 'Var')

M = c()
for(i in x){
  sum_stats <- tapply(data[,i],data$dshort_term,f_sum_stats) #applying function by group (dshort_term, either 0 or 1)
  t <- abs(sum_stats$`0`[2]-sum_stats$`1`[2])/sqrt(sum_stats$`0`[5]/nc+sum_stats$`1`[5]/nt)
  p.value = 2*pt(-abs(t), df=length(data)-1)
  #t_stats <- c(t_stats,t)
  #print(colnames(data)[i])
  #print(naming)
  #print(sum_stats)
  #print(t)
  # this looks atrocious, but is better than printing each bit!
  M = append(M,colnames(data)[i])
  M = append(M,sum_stats$`0`[1:5])
  M = append(M,sum_stats$`1`[1:5])
  M = append(M,t)
  M = append(M,p.value)
}
# reshape M
M = matrix(M, nrow = 13, byrow = TRUE)

###### Question 4 ###### 
l = choose(35,18)
l 
p_assignment = 1/l
p_assignment

z = data$dshort_term #observed treatment assignment

# create S possible values of differences-in-means
S = 1000 
T 	= numeric(S)
meantr	= numeric(S)
meanco 	= numeric(S)

## 4b Absention Rate ##
data$abs_rate # observed outcome
Tobs = mean(data$abs_rate[data$dshort_term==1]) - mean(data$abs_rate[data$dshort_term==0])
# check 
abs_rate_all <- split(data$abs_rate,data$dshort_term)
Y0 <- abs_rate_all$`0` 
Y1 <- abs_rate_all$`1`
check = mean(Y1) - mean(Y0)

for(j in 1:S)  {
  t = sample(z,replace=FALSE) # replace refers to whether some elements can be drawn twice. Get one of the possible zs.
  meantr[j] 	= mean(data$abs_rate[t==1])
  meanco[j] 	= mean(data$abs_rate[t==0])
  T[j]		=  meantr[j]- meanco[j]
}
hist(T, breaks=10)
abline(v=Tobs, col="red", lwd=2)
pval = sum(abs(T) >= abs(Tobs))/S
cat("The exact p-value is", pval, "\n")

## 4c Bills Introduced ##
data$bills_intro
Tobs = mean(data$bills_intro[data$dshort_term==1]) - mean(data$bills_intro[data$dshort_term==0])

for(j in 1:S)  {
  t = sample(z,replace=FALSE) # replace refers to whether some elements can be drawn twice. Get one of the possible zs.
  meantr[j] 	= mean(data$bills_intro[t==1])
  meanco[j] 	= mean(data$bills_intro[t==0])
  T[j]		=  meantr[j]- meanco[j]
}
hist(T, breaks=10)
abline(v=Tobs, col="red", lwd=2)
pval = sum(abs(T) >= abs(Tobs))/S
cat("The exact p-value is", pval, "\n")

## 4d calculate Wilcoxon rank sum statistics ##

T_rank = numeric(S)
rank = numeric(S)
for(j in 1:S)  {
  t = sample(z,replace=FALSE) # replace refers to whether some elements can be drawn twice. Get one of the possible zs.
  # rank[j] = rank(data$abs_rate[t==1])
  T_rank[j] = sum(rank(data$abs_rate[t==1]))
}
hist(T_rank, breaks=10)

Robs = sum(rank(data$abs_rate[data$dshort_term==1]))

# calculate wilcoxon statistic ???
sum(rank(data$abs_rate[z==1]))

ranktr[j] = rank(data$abs_rate[t==1])
rankco[j] = rank(data$abs_rate[t==0])

rank(data$abs_rate)
sum(rank(data$abs_rate[data$dshort_term==1]))
sum(rank(data$abs_rate[data$dshort_term==0]))

t = sample(z,replace=FALSE)
t
temp = data$abs_rate[t==1]

## 4e

diff_means = function(x) {
  
  Tobs = mean(x[data$dshort_term==1]) - mean(x[data$dshort_term==0])
  
  for(j in 1:S)  {
    t = sample(z,replace=FALSE) # replace refers to whether some elements can be drawn twice. Get one of the possible zs.
    meantr[j] 	= mean(x[t==1])
    meanco[j] 	= mean(x[t==0])
    T[j]		=  meantr[j]- meanco[j]
  }
}
# check that the results are the same
diff_means(data$abs_rate)
hist(T, breaks=10)
abline(v=Tobs, col="red", lwd=2)
pval = sum(abs(T) >= abs(Tobs))/S
cat("The exact p-value is", pval, "\n")


## 4f
for(i in x){
  diff_means(data$x)
  pval = sum(abs(T) >= abs(Tobs))/S
  cat("The exact p-value is", pval, "\n")
}

diff_means(data$children)
hist(T, breaks=10)
abline(v=Tobs, col="red", lwd=2)
pval = sum(abs(T) >= abs(Tobs))/S
cat("The exact p-value is", pval, "\n")


###### Question 5 ###### 
  
NSW_data <- read.csv("LaLonde_1986.csv")
z = NSW_data$treat
n1 = sum(NSW_data$treat)
n0 = nrow(NSW_data) - n1
Ybar1	= mean(NSW_data$earn78[z==1])
Ybar0	= mean(NSW_data$earn78[z==0])

## 5a1 ## 
ATE = Ybar1 - Ybar0 # average treatment effect (5a1)

s2.1 = sum((NSW_data$earn78[z==1] - Ybar1)^2)/(n1-1)
s2.0 = sum((NSW_data$earn78[z==0] - Ybar0)^2)/(n0-1)

V = s2.0/n0 + s2.1/n1

## 5a2 ##
# confidence interval (5a2)
CI.lower = ATE + qnorm(0.025) * sqrt(V)
CI.upper = ATE + qnorm(0.975) * sqrt(V)

## 5b2 ##

S = 1000 
T 	= numeric(S)
meantr	= numeric(S)
meanco 	= numeric(S)
Tobs = mean(NSW_data$earn78[NSW_data$treat==1]) - mean(NSW_data$earn78[NSW_data$treat==0])
for(j in 1:S)  {
  t = sample(z,replace=FALSE) # replace refers to whether some elements can be drawn twice. Get one of the possible zs.
  meantr[j] 	= mean(NSW_data$earn78[t==1])
  meanco[j] 	= mean(NSW_data$earn78[t==0])
  T[j]		=  meantr[j]- meanco[j]
}
hist(T, breaks=10)
abline(v=Tobs, col="red", lwd=2)
pval = sum(abs(T) >= abs(Tobs))/S
cat("The exact p-value is", pval, "\n")

# estimating cdfs  
cdf0 = ecdf(NSW_data$earn78[NSW_data$treat==0])  
cdf1 = ecdf(NSW_data$earn78[NSW_data$treat==1])  
plot(cdf0)
plot(cdf1)
max()

# K-S
T_ks = numeric(S)
cdf1 = numeric(S)
cdf0 = numeric(S)
cdf1<-c()
cdf0<-c()
for(j in 1:S){
  t = sample(z,replace=FALSE)
  cdf1[j] <- ecdf(NSW_data$earn78[t==1])(NSW_data$earn78[i])
  cdf0[j] <- ecdf(NSW_data$earn78[t==0])(NSW_data$earn78[i])  
  T_ks[j] <- max(abs(cdf1[j]-cdf0[j])) 
}
cdf1_obs <- ecdf(NSW_data$earn78[z==1])(NSW_data$earn78[i])
cdf0_obs <- ecdf(NSW_data$earn78[z==0])(NSW_data$earn78[i])
T_ks_obs <- max(abs(cdf1_obs-cdf0_obs)) 
hist(T_ks, breaks=10)
abline(v=T_ks_obs, col="red", lwd=2)
pval = sum(abs(T_ks) >= abs(T_ks_obs))/S
cat("The exact p-value is", pval, "\n")


# quantile
T_q = numeric(S)
qtr	= numeric(S)
qco = numeric(S)
for(j in 1:S)  {
  t = sample(z,replace=FALSE)
  qtr[j] 	= quantile(NSW_data$earn78[t==1])[4]
  qco[j] 	= quantile(NSW_data$earn78[t==0])[4]
  T_q[j]  = qtr[j] - qco[j]
}

qtr_obs = quantile(NSW_data$earn78[z==1])[4]
qco_obs = quantile(NSW_data$earn78[z==0])[4]
T_q_obs = qtr_obs - qco_obs
hist(T_q, breaks=10)
abline(v=T_q_obs, col="red", lwd=2)
pval = sum(abs(T_q) >= abs(T_q_obs))/S
cat("The exact p-value is", pval, "\n")