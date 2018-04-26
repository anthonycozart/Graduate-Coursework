setwd("~/Desktop/Michigan/PoliSci 787/Problem Set 3")
library(foreign)
library(MASS)
library(Matching)

#### Question 1 ####
set.seed(123)

B1 = 2
B2 = 0
B3 = 3
M = 1000
B2_hat = numeric(M)
lower_bound = numeric(M)
upper_bound = numeric(M)
covered = numeric(M)

## Question 1a
for(i in 1:M){
  x = rnorm(1000,0,1)
  t = rbinom(1000,1,0.3)
  e = rnorm(1000,0,1)
  y = B1 + B2*t + B3*x + e
  lmout = lm(y~t+x)
  B2_hat[i] = lmout$coefficients[2]
  B2_se = summary(lmout)$coef[[5]]
  # build confidence interval
  lower_bound[i] = B2_hat[i] - 1.96*B2_se
  upper_bound[i] = B2_hat[i] + 1.96*B2_se
  covered[i] = ifelse(lower_bound[i] <= 0 & upper_bound[i] >= 0,1,0)
}
mean(covered)
# confidence intervals have close to correct coverage. 949/1000 the confidence interval included 0.

## Question 1b
M = 100 # number of simulations
N = 100 # number of bootstraps

q025 = ceiling(N*0.025)
q975 = ceiling(N*0.975)

B2_ols = matrix(nrow=M,ncol=N)
B2_means = matrix(nrow=M,ncol=N)
B2_match = matrix(nrow=M,ncol=N)

for(j in 1:M){
  x = rnorm(1000,0,1)
  t = rbinom(1000,1,0.3)
  e = rnorm(1000,0,1)
  
  for(i in 1:N) {
    xboot = sample(x, replace=TRUE)
    tboot = sample(t, replace=TRUE)
    eboot = sample(e, replace=TRUE)
    yboot = B1 + B2*tboot + B3*xboot + eboot
    bootsample = data.frame(yboot,tboot,xboot,eboot)
    
    # Calculate 3 Estimators for B2
    # (1) OLS
    lmout = lm(yboot~tboot+xboot)
    B2_ols[j,i] = lmout$coefficients[2]
    # (2) Means
    B2_means[j,i] = mean(bootsample$yboot[bootsample$tboot==1]) - mean(bootsample$yboot[bootsample$tboot==0])
    # (3) Matching
    mout = Match(Y=bootsample$yboot, Tr=bootsample$tboot, X=bootsample$xboot, estimand="ATT", ties=FALSE)
    B2_match[j,i] = mout$est[[1]]
  }
}

# Construct confidence intervals and calculate coverage
# Using the percentile method
# this only works because I've set M = N. It's cheating.

CI_ols = matrix(nrow=M,ncol=2)
CI_means = matrix(nrow=M,ncol=2)
CI_match = matrix(nrow=M,ncol=2)

covered_ols = numeric(M)
covered_means = numeric(M)
covered_match = numeric(M)

for(j in 1:M){
  CI_ols[j,] = quantile(B2_ols[,j],p=c(0.025,0.975))
  CI_means[j,] = quantile(B2_means[,j],p=c(0.025,0.975))
  CI_match[j,] = quantile(B2_match[,j],p=c(0.025,0.975))
  
  covered_ols[j] = ifelse(CI_ols[j,1] <= 0 & CI_ols[j,2] >= 0,1,0)
  covered_means[j] = ifelse(CI_means[j,1] <= 0 & CI_means[j,2] >= 0,1,0)
  covered_match[j] = ifelse(CI_match[j,1] <= 0 & CI_match[j,2] >= 0,1,0)
}
CI_ols
CI_means
CI_match
mean(covered_ols)
mean(covered_means)
mean(covered_match)


# Do the same using the basic bootstrap
# t is the estimate from the original sample. It's 0?

# reshape matrix as vector
B2_ols = as.vector(B2_ols)
B2_means = as.vector(B2_means)
B2_match = as.vector(B2_match)

# Plot the distribution of B2 
hist(B2_ols, main="", xlab="Bootstrapped OLS Estimate of B2")
hist(B2_means, main="", xlab="Bootstrapped Difference in Means by Treatment")
hist(B2_match, main="", xlab="Bootstrapped Matching Estimate")

######################
## Question 2

# Question 2c
# run OLS estimator with non-experimental data, using only the observed data for the control
NOexp_data = read.dta("lalonde-NOexp.dta")
NOexp_control = subset(NOexp_data, NOexp_data$treat == 0)
NOexp_treated = subset(NOexp_data, NOexp_data$treat == 1)

n = dim(NOexp_data)[1]
nc = dim(NOexp_control)[1]
nt = dim(NOexp_treated)[1]

y = NOexp_control$re78
x = NOexp_control$re75
lmout = lm(y~x)
summary(lmout)

# Question 2f
# estimate the average potential outcome for the treated units had you given them the control.
yc = mean(NOexp_control$re78)
betac = lmout$coefficients[[2]]
xt = mean(NOexp_treated$re75)
xc = mean(NOexp_control$re75)
y0_d1 = yc + betac * (xt - xc)
y1_d1 = mean(NOexp_treated$re78)

# Point estimator for the average treatment effect on treated (ATT) for the non-experimental data
att_NOexp = y1_d1 - y0_d1

# Question 2g
exp_data = read.dta("lalonde-exp.dta")

# compare linear estimator from above, for p.e. for treated had you given control with:
# control mean from experimental data
yc_exp = mean(exp_data$re78[exp_data$treat==0])
# unadjusted mean of the non-experimental control group
yt_NOexp = mean(NOexp_data$re78[NOexp_data$treat==1])

estimates = data.frame(yc_exp,yt_NOexp,y0_d1,att_NOexp)
colnames(estimates) = c("Control-Exp","Control-NOexp","Treatment Under Control-NOexp","ATT-NOexp")
estimates

summary(exp_data$re78)
summary(NOexp_data$re78)
hist(NOexp_data$re78)
hist(exp_data$re78)

# Question 2h 
# now estimate att with the experimental data
exp_control = subset(exp_data, exp_data$treat == 0)
exp_treated = subset(exp_data, exp_data$treat == 1)

n = dim(exp_data)[1]
nc = dim(exp_control)[1]
nt = dim(exp_treated)[1]

y = exp_control$re78
x = exp_control$re75
lmout = lm(y~x)
summary(lmout)

yc = mean(exp_control$re78)
betac = lmout$coefficients[[2]]
xt = mean(exp_treated$re75)
xc = mean(exp_control$re75)
y0_d1 = yc + betac * (xt - xc)
y1_d1 = mean(exp_treated$re78)

att_exp = y1_d1 - y0_d1
att_exp
att_NOexp

hist(NOexp_data$re78[NOexp_data$treat==1])
hist(NOexp_data$re78[NOexp_data$treat==0])

# Question 2i: estimate ATT using nearest-neighbor matching, where re75 is the only covariate
# Using experimental data
exp_mout <- Match(Y=exp_data$re78, Tr=exp_data$treat, X=exp_data$re75, estimand="ATT")
summary(exp_mout)
cat("The estimated effect using matching is", exp_mout$est, "with p-value",  2 * (1-pnorm(abs(exp_mout$est/exp_mout$se))),"\n")
cat("The estimated effect using the experimental data is", att_exp)
