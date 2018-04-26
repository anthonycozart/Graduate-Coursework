#--------------------------------------------------------------------------------#
# Problem Set 5, Question 4 Code

setwd("~/Desktop/Michigan/PoliSci 787/Problem Set 5")
library(rdrobust)
library(rddensity)
library(rdlocrand)
library(foreign)
#--------------------------------------------------------------------------------#

data = read.dta("CaugheySekhon2011.dta")
View(data)

Y = data$DWinNxt #Dem margin of victory at election t+1
X = data$DifDPct #Dem margin of victory at election t
summary(Y)
summary(X)

# Plots
scatter = plot(X, Y, xlab="Dem Margin of Victory at Election t", ylab="Dem Margin of Victory at Election t+1", col=1, pch = 20)
plot1 = rdplot(Y, X, x.label="Dem Margin of Victory at Election t", y.label="Dem Margin of Victory at Election t+1", binselect="esmv")

# Falsification & Validation

## Placebo test
# Xlag = data$DWinPrv
Xlag = data$DifDPPrv
summary(lm(Xlag ~ X)) # hmm, this is showing a positive p-value

## Plot predetermined characteristics
plot(X, data$DemInc)
plot(X, data$DSpndPct)
plot(X, data$DDonaPct)

plot(X, data$PrvTrmsD, xlab="Dem Margin of Victory at Election t", ylab="Previous Terms Served by Dem", col=1, pch = 20)
plot(data$PrvTrmsD, Y, xlab="Previous Terms Served by Dem at t", ylab="Dem Margin of Victory at Election t+1", col=1, pch = 20)

#--------------------------------------------------------------------------------#
# Randomization-based approach

out1 = rdrandinf(Y, X, wl = -0.5, wr = 0.5, seed = 123 )
# reject the sharp null hypothesis that the intercepts are equal.

#--------------------------------------------------------------------------------#
# Continuity-based approach, using local polynomial methods with MSE optimal bandwidth

out2 = rdrobust(Y, X, kernel = "triangular", p = 1, bwselect = "mserd")
out2$coef
out2$ci

