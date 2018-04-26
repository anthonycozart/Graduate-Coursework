setwd("~/Desktop/Michigan/PoliSci 787/Problem Set 4")
library(foreign)
library(MASS)
library(Matching)
library(ggplot2)

#### Question 3 ####
raw_data = read.dta("Huber AJPS/ethnicvoting.dta")
dim(raw_data)
data = subset(raw_data, grouptype == "fearon")
continuous_covariates = c("pr","propmmd","lndm","ELF_ethnic","EP_ethnic","ratio1",
                          "lngdp","polity2","fed","resseg_std","wvs","afro2","afro3")
summarized_covariates = aggregate(data[,continuous_covariates], list(data$ccode), mean) # don't worry about outcomes
summarized_covariates = summarized_covariates[complete.cases(summarized_covariates), ]

# Question 3 b
# if this were an experiment, you'd likely run t-tests
t.test(summarized_covariates$propmmd[summarized_covariates$pr==1],summarized_covariates$propmmd[summarized_covariates$pr==0])
t.test(summarized_covariates$lndm[summarized_covariates$pr==1],summarized_covariates$lndm[summarized_covariates$pr==0])
t.test(summarized_covariates$ELF_ethnic[summarized_covariates$pr==1],summarized_covariates$ELF_ethnic[summarized_covariates$pr==0])
t.test(summarized_covariates$ratio1[summarized_covariates$pr==1],summarized_covariates$ratio1[summarized_covariates$pr==0])
t.test(summarized_covariates$lngdp[summarized_covariates$pr==1],summarized_covariates$lngdp[summarized_covariates$pr==0])
t.test(summarized_covariates$polity2[summarized_covariates$pr==1],summarized_covariates$polity2[summarized_covariates$pr==0])
t.test(summarized_covariates$fed[summarized_covariates$pr==1],summarized_covariates$fed[summarized_covariates$pr==0])

# Function to calculate standardized mean difference
balance <- function(x, Tr, xname, alpha=0.05) {
  Tr = Tr[!is.na(x)]
  x  =  x[!is.na(x)]     
  mut = mean(x[Tr==1])
  muc = mean(x[Tr==0])
  s2t  = var(x[Tr==1])
  s2c  = var(x[Tr==0])    
  delta = (mut - muc)/sqrt((s2t+s2c)/2)
  
  ql_c = quantile(x[Tr==0], probs = alpha/2)
  qh_c = quantile(x[Tr==0], probs = 1-alpha/2)
  ql_t = quantile(x[Tr==1], probs = alpha/2)
  qh_t = quantile(x[Tr==1], probs = 1-alpha/2)
  
  pi_t = sum(x[Tr==1] < ql_c)/(length(Tr==1)) + sum(x[Tr==1] > qh_c)/(length(Tr==1))
  pi_c = sum(x[Tr==0] < ql_t)/(length(Tr==0)) + sum(x[Tr==0] > qh_t)/(length(Tr==0))
  
  ratio_STD = log(s2t/s2c)
  
  cat("------------------------------------------\n")    
  cat("Variable ", xname, "\n")
  cat("Standardized difference", delta, "\n")
  cat("Log ratio of the sample standard deviations", ratio_STD, "\n")
  cat("Mass of treated distribution outside the 5 and 95 control quantiles is", round(pi_t*100, 2), "% \n")
  cat("Mass of control distribution outside the 5 and 95 treated quantiles is", round(pi_c*100, 2), "% \n")
  cat("------------------------------------------\n")
  return(list(mut=mut,muc=muc, s2t=s2t, s2c=s2c, delta=delta, pi_t=pi_t, pi_c=pi_c, ratio_STD))
}

Tr = summarized_covariates$pr
for(i in 1:ncol(summarized_data)) {
  balance(x=summarized_data[,i], Tr=Tr, xname=colnames(summarized_data)[i])
}

# Now use un-summarized data, and cluster standard errors -- xx FINISH
panel_covariates = data[,c("ccode","year",continuous_covariates)]
panel_covariates = panel_covariates[complete.cases(panel_covariates), ]
Tr = panel_covariates$pr
for(i in 1:ncol(panel_covariates)) {
  balance(x=panel_covariates[,i], Tr=Tr, xname=colnames(panel_covariates)[i])
}

# Question 3 c
# Measures of overlap: 
# standardized difference (above), mass outside alpha = 0.05 (above), log ratio of STD (above)
# histograms of lndm distribution by treatment status (it's the most unbalanced)

hist(panel_covariates$lndm[Tr==1], col="blue", main="Histogram of Ln(DM) for PR (Tr)")
hist(panel_covariates$lndm[Tr==0], col="red", main="Histogram of Ln(DM) for Non-PR (Co)")

# we can also estimate propensity scores by treatment group
X = cbind(panel_covariates$propmmd, panel_covariates$lndm, panel_covariates$ELF_ethnic,
          panel_covariates$EP_ethnic, panel_covariates$ratio1,
          panel_covariates$lngdp, panel_covariates$polity2,
          panel_covariates$fed, panel_covariates$resseg_std, panel_covariates$wvs,
          panel_covariates$afro2, panel_covariates$afro3)

colnames(X) = c("propmmd","lndm","ELF_ethnic","EP_ethnic","ratio1",
             "lngdp","polity2","fed","resseg_std","wvs","afro2","afro3")

pscore<-glm(Tr~X,family=binomial(link=logit)) 
phat<-pscore$fitted.values
hist(phat, col="blue")

par(mfrow=c(1,2))
hist(phat[Tr==1], col="blue", main="Histogram of Pscore for PR (Tr)")
hist(phat[Tr==0], col="red", main="Histogram of Pscore for Non-PR (Co)")
boxplot(phat~Tr,range=5)
summary(phat[Tr==1])
summary(phat[Tr==0])

muhat<-pscore$linear.predictors
par(mfrow=c(1,2))
hist(muhat[Tr==1], col="blue", main="Hist of Linearized Pscore (Tr)")
hist(muhat[Tr==0], col="red", main="Hist of  Linearized Pscore (Co)")
boxplot(muhat~Tr,range=5)

# Question 3 d
# Nearest Neighbor Matching, and check balance after
mout <- Match(Tr=Tr, X=X, estimand="ATT", ties=FALSE)
summary(mout)

mdataTr = panel_covariates[mout$index.treated,]
mdataCo = panel_covariates[mout$index.control,]
# So most countries are getting matched to the same pair, regardless of year
data.frame(mdataTr$ccode, mdataCo$ccode)
MatchBalance(Tr ~ X, match.out=mout, nboots=10)
# minor improvement in the more unbalanced covariates (propmmd, lndm, ELF_ethnic, EP_ethnic, ratio1)

# Question 3 g
# now include outcomes in data set
# narrow focus to specifications in columns 1a, 2a, 3a, and 4a from Table 3 (for simplicity of presentation)

outcomes = c("VF_std","VP_std","PVF_std","PVP_std")
included_covariates = c("lndm_std","ELF_std","ratio1","lngdp","polity2",
                        "fed", "resseg_std", "wvs", "afro2", "afro3")
reg_data = data[,c("ccode",outcomes,"pr",included_covariates)]
reg_data = reg_data[complete.cases(reg_data),]

X = cbind(reg_data$lndm_std, reg_data$ELF_std, reg_data$ratio1,
          reg_data$lngdp, reg_data$polity2,
          reg_data$fed, reg_data$resseg_std, reg_data$wvs,
          reg_data$afro2, reg_data$afro3)
colnames(X) = included_covariates

mout <- Match(Y=reg_data$VF_std, Tr=reg_data$pr, X=X, estimand="ATT")
summary(mout)
cat("Matching effect is", mout$est, "with p-value",  2 * (1-pnorm(abs(mout$est/mout$se))),"\n")

# Question 4
## Question 4a
set.seed(123)

n = 100
M = 100
B = 99

alpha = 0.3
tau = 2
p_score = alpha/(1+alpha)
ATT.dif = numeric(M) ; ATT.mat = numeric(M) 
cov.dif = numeric(M) ; cov.mat1 = numeric(M); cov.mat2 = numeric(M)

for(i in 1:M) {
  cat("Iteration ", i, " of ", M, "\n")
  X = runif(n)
  W = as.numeric(runif(n, max=1,min=0) >= 1-alpha)
  Y = rnorm(n,0,1)
  Y[W==1] = tau
  
  # Observed ATT
  ATT.dif[i] = Y[W==1] - Y[W==0]
  # Matching ATT
  mout = Match(Y=Y, Tr=W, X=X, estimand="ATT")
  ATT.mat = mout$est
  
  dat = data.frame(Y,W,X)
  ATT.boot = numeric(B)
  for(j in 1:B) {
    datb = dat[sample((1:nrow(dat)),replace=TRUE),]
    moutb = Match(Y=datb$Y, Tr=datb$W, X=datb$X)
    ATT.boot[j] = moutb$est
  }
  ci.boot1 = c(sort(ATT.boot)[(alpha/2)*(B+1)], sort(ATT.boot)[(1-alpha/2)*(B+1)]) 
  cov.mat1[i] =  (tau >= ci.boot1[1] & tau <= ci.boot1[2] )
  
  ## using bootstrapped confidence interval
  qh = sort(ATT.boot)[(1-alpha/2)*(B+1)]
  ql = sort(ATT.boot)[(alpha/2)  *(B+1)]
  ci.boot2 = c(2*ATT.mat - qh, 2*ATT.mat - ql)
  cov.mat2[i] =  (tau >= ci.boot2[1] & tau <= ci.boot2[2] )
}

# Question 4b
mean(ATT.dif)

# Question 4c
mean(ci.boot1) # Using the basic method
mean(ci.boot2) # Using the percentile method

# Question 4d
mean(cov.mat1) # Using the basic method
mean(cov.mat2) # Using the percentile method

# Question 4e
Abadie and Imbens explain why the bootstrap fails (pg 1546). The matching estimator becomes linear after conditioning 