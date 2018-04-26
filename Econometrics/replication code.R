setwd("~/Desktop/Michigan/PoliSci 787/Replication Project/Files")
library(rdrobust)
library(rddensity)
library(rdlocrand)
library(foreign)

data = read.dta("dataset.dta")

dim(data)
mean(complete.cases(data$INVSALES))
mean(complete.cases(data$score))

Investment_Sales = data$INVSALES
Score = data$score
Margin = data$score - 75 

#--------------------------------------------------------------------------------#
# PLOTS

# Great example how scatter doesn't show much of a difference to the left and right of the cutoff
scatter = plot(Score, Investment_Sales, main = "Figure 1. Scatter of all firms", xlab = "Application Score", ylab = "Investment/Sales")

# Figure 2, panel A
figure2 = rdplot(Investment_Sales, Score, p = 1, c = 75, x.lim = c(20,100), y.lim = c(-0.1,0.2), title="Figure 2. RD Plot from Pg. 115", x.label= "Application Score", y.label = "Investment/Sales")

# Now with just even spacing: too much bunching
figure2_e = rdplot(Investment_Sales, Score, p = 1, c = 75, binselect = "es", x.lim = c(20,100), y.lim = c(-0.1,0.2), title="RD Plot (with ES)", x.label= "Application Score", y.label = "Investment/Sales")

# Now using choosing the bins to mimic the variance criterion using quantiles
figure2_qsmv = rdplot(Investment_Sales, Score, p = 1, c = 75, binselect = "qsmv", x.lim = c(20,100), y.lim = c(-0.1,0.2), title="RD Plot (selecting bins using QSMV)", x.label= "Application Score", y.label = "Investment/Sales")
# having more points allows us to see the variability of investment.


#--------------------------------------------------------------------------------#
# MANIPULATION
hist(Margin, breaks=25, xlim=c(-50,50), main = "Density Check", xlab = "Distance from Cutoff")
abline(v=0, col="red")
box()

# Density manipulation test
dens = rddensity(Margin)
rdplotdensity(dens, Margin)

#--------------------------------------------------------------------------------#
# BALANCE

X = data.frame(data$SALES, data$INV, data$ASSETS, data$INVA, data$LCSALES, data$INVSALES, data$INVTSALES, data$INVINTSALES)
covariates_list = cbind("Sales", "Value_Added", "Assets", "Roa", "Labor_Sales", "Investment_Sales","TInvestment_Sales", "ITInvestment_Sales")
colnames(X) = cbind(covariates_list)

T = data$treat
diff_means = numeric(ncol(X))
diff_pvalues = numeric(ncol(X))
vars = numeric(ncol(X))
for(i in 1:ncol(X)) {
  temp = t.test(X[,i][T==0], X[,i][T==1])
  diff_means[i] = temp$estimate[2]-temp$estimate[1]
  diff_pvalues[i] = temp$p.value
  vars[i] = covariates_list[i]
}

table1 = data.frame(Variable, diff_means, diff_pvalues)
colnames(table1) = cbind("Variable", "Difference in Means", "P-Value")
kable(table1)

summary(Assets[Score<75])
plot(Score,Assets, main = "Figure 4. Firm Assets by Score", xlab = "Application Score", ylab = "Assets")
abline(v=75, col="red")
abline(h=300000, col="blue", lty=2)




#--------------------------------------------------------------------------------#
# PLACEBOS + CHECKS

ROA = data$INVA 
Assets = data$ASSETS
Sales = data$SALES
placebo1 = rdplot(ROA, Score, p = 1, c = 75, binselect = "qsmv", x.lim = c(20,100), y.lim = c(-0.1,0.2), title="RD Plot Placebo", x.label= "Application Score", y.label = "ROA")
placebo2 = plot(Score,ROA)

# The intercepts are different, but there data points overlap enough to conclude there's a difference. (They estimate model to do placebos, however.)

# They also check the continuity assumption by seeing if variables are smooth before the program.
y = data$SALES
T = data$treat
x = data$s
x2 = x^2
x3 = x^3
T_x1 = T*x
T_x2 = T*x2
T_x3 = T*x3
C_x1 = (1-T)*x
C_x2 = (1-T)*x2
C_x3 = (1-T)*x3

lmout = lm(y~T+T_x1+C_x1+T_x2+C_x2+T_x3+C_x3)
summary(lmout)

#--------------------------------------------------------------------------------#
# MAIN RESULTS

out1 = rdrobust(Investment_Sales, Margin, p = 1, bwselect = "mserd")
summary(out1)
out2 = rdrobust(Investment_Sales, Margin, p = 2, bwselect = "mserd")
summary(out2)
out3 = rdrobust(Investment_Sales, Margin, p = 3, bwselect = "mserd")
summary(out3)
# Conclusion: changes in the polnomial order in the estimation do not change the conclusions of the study

estimates = t(cbind(0.064, 0.110, -0.079, out3$Estimate[[1]])) 
results = t(cbind("Full Sample", "Wide-Window", "Narrow-Window", "MSE Optimal"))
table2 = data.frame(results, estimates)
colnames(table2) = cbind("Bandwidth", "Estimated RDD Effect")
table2
kable(table2)


out3$bws # h is the main bandwidth. Can I back out the quantiles from this?
bandwidth.length = out3$bws[1,1]
percentile = ecdf(Margin)
width = (percentile(out3$bws[1,1]) - percentile(-out3$bws[1,1]))*100
width

# Does the choice of the kernel matter? NO.
out4 = rdrobust(Investment_Sales, Margin, p = 1, bwselect = "mserd", kernel = "uniform")
out5 = rdrobust(Investment_Sales, Margin, p = 1, bwselect = "mserd", kernel = "epanechnikov")
out1$Estimate[[1]]
out4$Estimate[[1]]
out5$Estimate[[1]]

# NOW INCLUDE A CONTROL FOR FIRM SIZE
size = data$largem
out6 = rdrobust(Investment_Sales, Margin, p = 1, bwselect = "mserd", covs = size)
summary(out6)

out1$Estimate[[1]]
out6$Estimate[[1]]
# RD estimate does not change. And it is pretty different than in the paper.
# Can also do this with intensity.


plot(Investment_Sales,data$ASSETS, main = "Figure 6. Investments by Firm Size", xlab = "Assets", ylab = "Investment/Sales")
abline(lm(Investment_Sales ~ data$ASSETS), col = "blue")
summary(lm(Investment_Sales ~ data$ASSETS))

