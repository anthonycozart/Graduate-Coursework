setwd("~/Desktop/Michigan/PoliSci 787/Problem Set 2")
library(foreign)
library(MASS)

#### Question 1 ####

data = read.csv("q1_data.csv")
dim(data)
names(data)

# create indicator for observed
data$dobserved = !is.na(data$accepted)
summary(data$accepted)

Y = data$accepted 
Z = data$hispanic 
R = data$dobserved

# reproduce table from question prompt
table(Z)              # by treatment
table(R)              # by reported
table(Z,R)            
summary(Y[R == 1]) 
summary(Y[R == 0])

# calculate observed ATEs
ave_y1_obs = mean(Y[Z==1 & R==1])
#cat("Observed mean outcome for hispanics is:", ave_y1_obs, "\n")
ave_y0_obs = mean(Y[Z==0 & R==1])
#cat("Observed mean outcome for whites is:", ave_y0_obs, "\n")

summary(Y[Z==1 & R==1]) # mean = likelihood of being accepted if hispanic 
summary(Y[Z==0 & R==1]) # mean = likelihood of being accepted if white
t.test(Y[Z==0 & R==1], Y[Z==1 & R==1])
# we fail to reject the null hypothesis that the likelihoods of being accepted are the same.

ate_obs =  ave_y1_obs - ave_y0_obs
#cat("Observed ATE of having an ID requested and accepted is", ate_obs, "\n")

# calculate probabilities of reporting by group
summary(R[Z==1])
p1 = mean(R[Z==1])

summary(R[Z==0])
p0 = mean(R[Z==0])

## 1b ##
gamma_H = 1 # all accepted
gamma_L = 0 # all rejected

e0_max = ave_y0_obs * p0 +  gamma_H * (1-p0)
e0_min = ave_y0_obs * p0 +  gamma_L * (1-p0)

e1_max = ave_y1_obs * p1 +  gamma_H * (1-p1)
e1_min = ave_y1_obs * p1 +  gamma_L * (1-p1)

ate_min = e1_min - e0_max
ate_max = e1_max - e0_min

cat("Bounds on ATE: the identifcation region for ATE is [", ate_min, ",", ate_max,"] \n")
cat("ATE on observed sample only:", ate_obs, "\n")

## 1c ##
set.seed(123)
B = 1000    
results = numeric(B)

# Does it make sense to make two draws? 
# One each for the likelihood of acceptance E(Y_1|R=0) and E(Y_0|R=0)? Both could be between 0,1.
for(i in 1:B){
  boot.gamma0<-runif(1)
  boot.gamma1<-runif(1)
  results[i]<- (ave_y1_obs*p1 + boot.gamma1*(1-p1))-(ave_y0_obs*p0 + boot.gamma0*(1-p0))
}
length(results)
boot.draw = c(sort(results)[25], sort(results)[975])

# But above is a Monte-Carlo. Instead bootstrap, using samples from data.
ate_minb = numeric(B)
ate_maxb = numeric(B)

for(i in 1:B) {
  datab = data[sample(1:nrow(data),replace=TRUE),]

  Yb = datab$accepted 
  Zb = datab$hispanic
  Rb = datab$dobserved
  ave_y1_obs = mean(Yb[Zb==1 & Rb==1])
  ave_y0_obs = mean(Yb[Zb==0 & Rb==1])
  p1 = mean(Rb[Zb==1])
  p0 = mean(Rb[Zb==0])
  
  e0_max = ave_y0_obs * p0 +  gamma_H * (1-p0)
  e0_min = ave_y0_obs * p0 +  gamma_L * (1-p0)
  
  e1_max = ave_y1_obs * p1 +  gamma_H * (1-p1)
  e1_min = ave_y1_obs * p1 +  gamma_L * (1-p1)
  
  ate_minb[i] = e1_min - e0_max
  ate_maxb[i] = e1_max - e0_min
}

mean(ate_minb)
mean(ate_maxb)

lower.bound = quantile(ate_minb,p=c(0.025))
upper.bound = quantile(ate_maxb,p=c(0.975))
cat("Bounds on ATE: the identifcation region for ATE is [", lower.bound, ",", upper.bound,"] \n")


#### Question 2 ####
q2.data = read.csv("question2.csv")
B<-1000
results<-numeric(length=B)

for (i in 1:B) {
  bootsample = q2.data[sample(1:nrow(q2.data),replace=TRUE),]
  bx1 = bootsample$x1 
  bx2 = bootsample$x2
  boot.diff = mean(bx1) - mean(bx2)
  results[i] = boot.diff
}

# Check results
mean(q2.data$x1) - mean(q2.data$x2) # Full sample estimator
mean(results)  # Bootstrap estimator

## 2a ##
# Bootstrapped 95% confidence interval (show basic bootstrap as well)
c(sort(results)[25], sort(results)[975])

## 2b ##
# Bootstrapped SE
sqrt(var(results))
# Check: sqrt(var(x1)/length(x1)+var(x2)/length(x2))

plot(density(results))
abline(v=mean(q2.data$x1)-mean(q2.data$x2), col = "red")

#### Question 3 ####

q34.data = read.csv("questions3_4.csv")
head(q34.data)

# OLS function unchanged from Rocio's code
ols<-function(y,x) {
  n=nrow(x)
  k=ncol(x)
  beta.hats<-ginv(t(x)%*%x)%*%t(x)%*%y
  residu<-y-x%*%beta.hats
  sigma2<-(t(residu)%*%residu)/(n-k)
  varcovar<-sigma2[1,1]*(solve(t(x)%*%x))
  std.dev<-sqrt(diag(varcovar))
  list(betas=beta.hats,std.dev=std.dev)
}

# The hand-made function above takes a matrix. Create one here.
ols.mat<-as.matrix(q34.data)
dim(ols.mat)
y<-ols.mat[,1]
x<-cbind(1,ols.mat[,2:ncol(ols.mat)])  # column of 1s is the constant
data<-cbind(y,x)

# Run OLS estimation (will use this later, when calculating bootstrap bias)
output.ols<-ols(y,x)

# Non-parametric bootstrap
B<-1000
bboot<-matrix(nrow=B,ncol=ncol(data)-1)
dim(bboot)
k = 1:nrow(data)
for(i in 1:B){
  indxboot <- sample(k,replace=TRUE) # Sample the indices
  datab <-data[indxboot,] #draw row using drawn index place     
  yb <-datab[,1]
  xb <-datab[,2:ncol(data)]
  bboot[i,]<-t(ols(yb,xb)$betas)
}
dim(bboot)

## Question 3a ##
# Bootstrap estimators
apply(bboot,2,mean) 
# Bootstrapped confidence intervals (show basic bootstrap)
apply(bboot,2,function(x) quantile(x,p=c(0.025,0.975)))

## Question 3b ##
# Bootstrap bias
# create matrix to hold bias for each of B=1000 samples
diff<-matrix(nrow=B,ncol=4) 
# calculate bias for each coefficient 
for(i in 1:4){
  diff[,i]<-bboot[,i]-output.ols$betas[i]
}
bias<-apply(diff,2,mean)
bias

## Question 3c ##

# confidence interval
q3c = c(sort(bboot[,2])[25], sort(bboot[,2])[975])
q3c
cat("The 95% confidence interval of B_1 is [",q3c[1],",",q3c[2],"], which does not contain the boostrapped estimate for B_2, ",output.ols$betas[3,1]," \n")

# plot
head(bboot)
t.boot = bboot[,2] - bboot[,3]
t.sample = output.ols$betas[2,1] - output.ols$betas[3,1]
hist(t.boot)
abline(v = t.sample, col="red", lwd=3) # full sample test statistic does not fall in the critical region.

#### Question 4 ####

# calculate estimated betas for full sample
lmout<-lm(q34.data$Y~q34.data$X1+q34.data$X2+q34.data$X3)
betas.fs<-cbind(as.vector(lmout$coefficients))

# bootstrap
B<-1000
bboot.par<-matrix(nrow=B,ncol=ncol(data)-1) # Subtract 1 because data includes y
for(i in 1:B){
  bootresid<-sample(lmout$residuals,replace=TRUE) # sample the estimated residuals, with replacement
  yboot<-x%*%betas.fs+bootresid # calculate the fitted value using the sampled residual
  bboot.par[i,]<-t(ols(yboot,x)$betas) # regress fitted value on x values, saving betas
}

## Question 4a ##
apply(bboot.par,2,function(x) quantile(x,p=c(0.025,0.975)))

## Question 4b ##
# Estimate bootstrapped variance-covariance matrix of these OLS estimators
k = dim(bboot.par)[2]
M <- matrix(nrow=k,ncol=k)
for(k in 1:4){
  for (r in 1:4){
    M[r,k] = cov(bboot.par[,r],bboot.par[,k])
  }
}
M #check

#### Question 5 ####
q56.data = read.csv("questions5_6.csv")
ols.mat<-as.matrix(q56.data)
y<-ols.mat[,1]
x<-cbind(1,ols.mat[,2])
data<-cbind(y,x)

output.ols<-ols(y,x)

# Non-parametric bootstrap
B<-1000
bboot<-matrix(nrow=B,ncol=ncol(data)-1)
dim(bboot)
k = 1:nrow(data)
for(i in 1:B){
  indxboot <- sample(k,replace=TRUE) # Sample the indices
  datab <-data[indxboot,] #draw row using drawn index place     
  yb <-datab[,1]
  xb <-datab[,2:ncol(data)]
  bboot[i,]<-t(ols(yb,xb)$betas)
}

## Question 5a ##
# Bootstrapped standard errors
sqrt(var(bboot[,1]))
sqrt(var(bboot[,2]))
   
# Bootstrap bias
diff<-matrix(nrow=B,ncol=2) 
# calculate bias for each coefficient 
for(i in 1:2){
  diff[,i]<-bboot[,i]-output.ols$betas[i]
}
bias<-apply(diff,2,mean)
bias

## Question 5b ##
# Explain why you cannot use non-parametric bootstrapping in this case
# Sampling rows breaks the structure of the data. The starting point matters.

## Question 5c NEW ##
lmout<-lm(q56.data$y~q56.data$lagy)
betas.fs<-cbind(as.vector(lmout$coefficients))
B<-1000
bboot.par<-matrix(nrow=B,ncol=ncol(data)-1) # Subtract 1 because data includes y
for(i in 1:B){
  # sample the estimated residuals, with replacement. This is okay since they are iid
  bootresid<-sample(lmout$residuals,replace=TRUE) 
  yboot<-x%*%betas.fs+bootresid
  bboot.par[i,]<-t(ols(yboot,x)$betas)
}

## Question 5d ##
# Bootstrapped standard errors
sqrt(var(bboot.par[,1]))
sqrt(var(bboot.par[,2]))

# Bootstrap bias
diff<-matrix(nrow=B,ncol=2) 
# calculate bias for each coefficient 
for(i in 1:2){
  diff[,i]<-bboot.par[,i]-output.ols$betas[i]
}
bias<-apply(diff,2,mean)
bias

#### Question 6 ####

# Question 6a
quantile(bboot.par[,1],p=c(0.025,0.975))
quantile(bboot.par[,2],p=c(0.025,0.975))
# reject the null that p = 0

truehist(bboot.par[,2],xlim=c(0,0.6))
abline(v = 0, col="red",lwd=3)
# bootstrapped p-value is zero (not the most precise answer, admittedly)

# Question 6b
## Question 5c OLD -- NOW Question 6b (?) ##
# Can you use parametric bootstrapping? Yes. Sample 1 row, then recreate the data, then bootstrap?
# calculate estimated betas for full sample

lmout<-lm(q56.data$y~q56.data$lagy)
betas.fs<-cbind(as.vector(lmout$coefficients))

# bootstrap
# B<-1000
# k = 1:nrow(data)
bboot.mc = matrix(nrow=B,ncol=ncol(data)-1)

for(i in 1:B){
  # first, create new data, from one randomly sampled row from full sample
  u = rnorm(1000,0,1)
  z = numeric(1000)
  # set z1 = 0
  z[1] = 0
  # set z2 = to random draw from data. This will become first value of y. (z1 is the first lagged y)
  indxboot = sample(k,replace=TRUE)[1]
  z[2] = data[indxboot,][1]
  # fill in data, using formula from before
  for (j in 3:1000)  z[j]<-3+0.5*z[j-1]+u[j]
  # create y, lagy from z
  y<-z[2:1000]
  lagy<-z[1:999]
  
  # Set up data for bootstrap
  # lmout<-lm(sampled.data[,1]~sampled.data[,3]) #can't use $ here - atomic operators?
  # betas.boot<-cbind(as.vector(lmout$coefficients))
  
  lmout<-lm(y~lagy) #can't use $ here - atomic operators?
  betas.mc<-cbind(as.vector(lmout$coefficients))
  
  x = cbind(1,lagy)
  mc.data = cbind(y,x)
  mc.data = as.matrix(mc.data)
  # check: head(sampled.data)
  
  # now run parametric bootstrap (as in Question 4), sampling residual from recreated data
  bootresid<-sample(lmout$residuals,replace=TRUE)
  # yboot<-x%*%betas.fs+bootresid
  yboot<-x%*%betas.mc+bootresid
  bboot.mc[i,]<-t(ols(yboot,x)$betas)
}

sqrt(var(bboot.mc[,1]))
sqrt(var(bboot.mc[,2]))

diff<-matrix(nrow=B,ncol=2) 
for(i in 1:2){
  diff[,i]<-bboot.mc[,i]-output.ols$betas[i]
}
bias<-apply(diff,2,mean)
bias

#### Question 7 ####
theta = 1
n = 1000
x = runif(n)
max.x = max(x)
B = 599

## Question 7a: Non-parametric bootstrap
T.nonpar = numeric(B)
for(i in 1:B) {
  xboot = sample(x, replace=TRUE)
  T.nonpar[i] = n * (max.x - max(xboot))
}
truehist(T.nonpar)
# funky bunching at T = 0

## Question 7b: parametric bootstrap

# max.par = numeric(B)
# max.par[i] = max(x)

T.par = numeric(B)
for(i in 1:B) {
  x = runif(n)
  T.par[i] = n * (max.x - max(x))
}
truehist(T.par)

## Question 7c
# As n increases, we see more bunching in our histogram -- the probability that the max(x) in the full sample is equal to the max(x) in the bootstrap increases. The non-parametric bootstrap fails because the distribution of the test statistic is not continuous, but instead truncated at zero. 
min(T.nonpar)
min(T.par)

# if you have studentized version of test statistic, use studentized CI. Need formula for standard error. If doing least squares (or sample) you know it. 
# confidence interval is the collection of all values for hypotheses that are not rejected.



