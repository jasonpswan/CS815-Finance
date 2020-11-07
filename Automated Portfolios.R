# PART 1: Construction of a portfolio using the GA package

library(quantmod)

mystocks <- c("RB.L", "RMV.L", "STJ.L", "SDR.L", "TSCO.L", 
              "CNA.L", "PSN.L", "EXPN.L", "BA.L", "CPG.L", 
              "SMT.L", "SMIN.L", "SSE.L", "PRU.L", "AHT.L", 
              "RTO.L", "RR.L", "BATS.L", "CCH.L", "SPX.L", 
              "VOD.L", "TUI.L", "EZJ.L", "CCL.L", "ANTO.L")
# This list of stocks were chosen from Yahoo in an attempt to find stocks which
# came from different industries, hoping to avoid colinearity

getSymbols(mystocks, src = "yahoo", from = "2018-01-01", to = "2019-01-01")
# A 1 year time window was chosen to cover the whole of the 2018 year.

# A Variable name, matching the company identifier was applied to each companies data.
# A subset of each companies listed data was taken which was solely the adjusted final price.
# The name of the column within each dataset was changed to match the company identifier.

RB <- (RB.L)
RB <- subset(RB, select = RB.L.Adjusted)
colnames(RB) <- c("RB")

RMV <- (RMV.L)
RMV <- subset(RMV, select = RMV.L.Adjusted)
colnames(RMV) <- c("RMV")

STJ <- (STJ.L)
STJ <- subset(STJ, select = STJ.L.Adjusted)
colnames(STJ) <- c("STJ")

SDR <- (SDR.L)
SDR <- subset(SDR, select = SDR.L.Adjusted)
colnames(SDR) <- c("SDR")

TSCO <- (TSCO.L)
TSCO <- subset(TSCO, select = TSCO.L.Adjusted)
colnames(TSCO) <- c("TSCO")

CNA <- (CNA.L)
CNA <- subset(CNA, select = CNA.L.Adjusted)
colnames(CNA) <- c("CNA")

PSN <- (PSN.L)
PSN <- subset(PSN, select = PSN.L.Adjusted)
colnames(PSN) <- c("PSN")

EXPN <- (EXPN.L)
EXPN <- subset(EXPN, select = EXPN.L.Adjusted)
colnames(EXPN) <- c("EXPN")

BA <- (BA.L)
BA <- subset(BA, select = BA.L.Adjusted)
colnames(BA) <- c("BA")

CPG <- (CPG.L)
CPG <- subset(CPG, select = CPG.L.Adjusted)
colnames(CPG) <- c("CPG")

SMT <- (SMT.L)
SMT <- subset(SMT, select = SMT.L.Adjusted)
colnames(SMT) <- c("SMT")

SMIN <- (SMIN.L)
SMIN <- subset(SMIN, select = SMIN.L.Adjusted)
colnames(SMIN) <- c("SMIN")

SSE <- (SSE.L)
SSE <- subset(SSE, select = SSE.L.Adjusted)
colnames(SSE) <- c("SSE")

PRU <- (PRU.L)
PRU <- subset(PRU, select = PRU.L.Adjusted)
colnames(PRU) <- c("PRU")

AHT <- (AHT.L)
AHT <- subset(AHT, select = AHT.L.Adjusted)
colnames(AHT) <- c("AHT")

RTO <- (RTO.L)
RTO <- subset(RTO, select = RTO.L.Adjusted)
colnames(RTO) <- c("RTO")

RR <- (RR.L)
RR <- subset(RR, select = RR.L.Adjusted)
colnames(RR) <- c("RR")

BATS <- (BATS.L)
BATS <- subset(BATS, select = BATS.L.Adjusted)
colnames(BATS) <- c("BATS")

CCH <- (CCH.L)
CCH <- subset(CCH, select = CCH.L.Adjusted)
colnames(CCH) <- c("CCH")

SPX <- (SPX.L)
SPX <- subset(SPX, select = SPX.L.Adjusted)
colnames(SPX) <- c("SPX")

VOD <- (VOD.L)
VOD <- subset(VOD, select = VOD.L.Adjusted)
colnames(VOD) <- c("VOD")

TUI <- (TUI.L)
TUI <- subset(TUI, select = TUI.L.Adjusted)
colnames(TUI) <- c("TUI")

EZJ <- (EZJ.L)
EZJ <- subset(EZJ, select = EZJ.L.Adjusted)
colnames(EZJ) <- c("EZJ")

CCL <- (CCL.L)
CCL <- subset(CCL, select = CCL.L.Adjusted)
colnames(CCL) <- c("CCL")

ANTO <- (ANTO.L)
ANTO <- subset(ANTO, select = ANTO.L.Adjusted)
colnames(ANTO) <- c("ANTO")

# The information related to the adjusted closing price for all 25 companies were then combined.
# These 25 companies make up the portfolio.
P1 <- data.frame(as.xts(merge(RB, RMV, STJ, SDR, TSCO, CNA, PSN, EXPN, BA, CPG, SMT, SMIN, SSE, PRU, AHT, RTO, RR, BATS, CCH, SPX, VOD, TUI, EZJ, CCL, ANTO)))
# The following formula will calculate the daily return on the stock price.
# The first row was removed as there would be no row denoted as the 0th row for it to use in it's calculation.

for (i in 1:ncol(P1)) {
  prices = P1[,i] 
  prices_prev = c(NA,prices[1:(length(prices)-1)]) 
  returns = (prices-prices_prev)/prices_prev 
  P1[,i] = returns 
}
asset_returns_2018 = P1[2:nrow(P1),1:ncol(P1)]
asset_returns_2018 = na.omit(asset_returns_2018)


# The outputted asset returns shows the daily change for each of the stocks,
# based upon the difference between the closing price on consecutive days.

# We now have the daily returns on each asset.
# It is necessary to combine these with the associated weight of each asset,
# in order to find the return on the portfolio made up of these assets.

portfolio_returns_2018 = function(x) {
  portfolio.returns = 0
  # Set the intital return on the portfolio to begin at zero
  for (i in 1:length(x)) {
    portfolio.returns = portfolio.returns + asset_returns_2018[,1] * x[i]
    # Multiply the return on each asset with the associated weighting of the asset
    # Adding this to portolfio.returns recursively sums up the returns on the portolfio
  }
  return (portfolio.returns)
}

# We have found the daily returns on each asset, as well as defined a function which will caluate the returns on the portfolio
# We must now consider how to combine this with the risk associated with each asset
# A common method from Finance which combines these both is the Sharpe ratio
# The ratio was created by Sir William Sharpe in 1996 and he was awarded the nobel prize
# for the CAPM asset pricing model which employs this
# This model, along with the Sharpe ratio, is taught throughout Undergraduate degrees in Finance and Economics at Strathclyde
# I thus surmise that this ratio could be used here for the purposes of Portfolio Optimisation

sharpe = function(x) {
  portfolio.returns = portfolio_returns_2018(x)
  return (mean(portfolio.returns)/sqrt(var(portfolio.returns)))
}

# Taking the mean of the portfolio returns serves to find the average return on the portfolio
# This is done at this stage as we have three years worth of entries and to assess each one on a daily change would prove fruitless
# By considering the average return over the period we can find how they move over this time scale
# The square root of the variance (AKA the Standard Deviation - σ) is also calculated
# The purpose of this is to find how much the returns differ from the mean
# The standard deviation is a measure of how much the values differ from the mean

# The next step is to write a function which will satsifty the predefined constraints
# The constraints for this problem are threefold:
# The weights must all be greater than zero (no shorting of stocks)
# No asset can hold a weight of one, as this would not be a portfolio
# The weights must all sum to 1 (or close enough to it that it is neglible)

penalty = function(x) {
  constraint = (sum(x)-1)**2
  # This checks that the sum of the weights are = 1
  for (i in 1:length(x)) {
    constraint = constraint +
    max(c(0, x[i]-1))**2 +
      # This constraint checks that x <= 1
      max(c(0, -x[i]))**2
    # This constraint checks that x >= 0
    }
  return(constraint)
}

# The function above checks the 3 constraints outlined
# The reason we square the values associated with the x[i] is to
# ensure that they are all positive
# This allows the employment of the max built-in function in R.

# Now that we have the sharpe function (which takes care of risk & return),
# as well as the penalty function which ensures the constraints are met,
# the final part of the problem is to combine them in to the function which
# we wish to optimise.
# One thing to note is that as we wish to Maximise the Sharpe ratio
# we must multiply it by -1 in or that it becomes an optimisation problem.

Optimisable = function(x) {
  return (-sharpe(x) + 100 * penalty(x))
}

# The final part of the problem is now to run the Genetic Algorithm
# The GA package will find the optimum weighting for each of the assets with the portfolio

library("GA")
lbound = rep(0,ncol(asset_returns_2018))
ubound = rep(1,ncol(asset_returns_2018))

ga_portfolio = ga (type = "real-valued", function(x){-Optimisable(x)},
                   lower = lbound, upper = ubound, maxiter=100000,
                   run = 1000)

weightings_2018 = as.vector(summary(ga_portfolio)$solution)
weightings_2018
sum(weightings_2018)
weight_2018 = cbind(names(asset_returns_2018), weightings_2018)
weight_2018
# We have now computed the weightings associated with a 25 assetportfolio over 2018.
# These weights will be used throughout the rest of this document for analysis.




# Part 2: Evaluation of the portfolio on unseen "future" data

getSymbols(mystocks, src = "yahoo", from = "2019-01-01", to = "2020-01-01")
# A one year time frame for the year following the initial data was chosen this time.

# The dataframe was created in the same manner as before, only over a different time period.

RB <- (RB.L)
RB <- subset(RB, select = RB.L.Adjusted)
colnames(RB) <- c("RB")

RMV <- (RMV.L)
RMV <- subset(RMV, select = RMV.L.Adjusted)
colnames(RMV) <- c("RMV")

STJ <- (STJ.L)
STJ <- subset(STJ, select = STJ.L.Adjusted)
colnames(STJ) <- c("STJ")

SDR <- (SDR.L)
SDR <- subset(SDR, select = SDR.L.Adjusted)
colnames(SDR) <- c("SDR")

TSCO <- (TSCO.L)
TSCO <- subset(TSCO, select = TSCO.L.Adjusted)
colnames(TSCO) <- c("TSCO")

CNA <- (CNA.L)
CNA <- subset(CNA, select = CNA.L.Adjusted)
colnames(CNA) <- c("CNA")

PSN <- (PSN.L)
PSN <- subset(PSN, select = PSN.L.Adjusted)
colnames(PSN) <- c("PSN")

EXPN <- (EXPN.L)
EXPN <- subset(EXPN, select = EXPN.L.Adjusted)
colnames(EXPN) <- c("EXPN")

BA <- (BA.L)
BA <- subset(BA, select = BA.L.Adjusted)
colnames(BA) <- c("BA")

CPG <- (CPG.L)
CPG <- subset(CPG, select = CPG.L.Adjusted)
colnames(CPG) <- c("CPG")

SMT <- (SMT.L)
SMT <- subset(SMT, select = SMT.L.Adjusted)
colnames(SMT) <- c("SMT")

SMIN <- (SMIN.L)
SMIN <- subset(SMIN, select = SMIN.L.Adjusted)
colnames(SMIN) <- c("SMIN")

SSE <- (SSE.L)
SSE <- subset(SSE, select = SSE.L.Adjusted)
colnames(SSE) <- c("SSE")

PRU <- (PRU.L)
PRU <- subset(PRU, select = PRU.L.Adjusted)
colnames(PRU) <- c("PRU")

AHT <- (AHT.L)
AHT <- subset(AHT, select = AHT.L.Adjusted)
colnames(AHT) <- c("AHT")

RTO <- (RTO.L)
RTO <- subset(RTO, select = RTO.L.Adjusted)
colnames(RTO) <- c("RTO")

RR <- (RR.L)
RR <- subset(RR, select = RR.L.Adjusted)
colnames(RR) <- c("RR")

BATS <- (BATS.L)
BATS <- subset(BATS, select = BATS.L.Adjusted)
colnames(BATS) <- c("BATS")

CCH <- (CCH.L)
CCH <- subset(CCH, select = CCH.L.Adjusted)
colnames(CCH) <- c("CCH")

SPX <- (SPX.L)
SPX <- subset(SPX, select = SPX.L.Adjusted)
colnames(SPX) <- c("SPX")

VOD <- (VOD.L)
VOD <- subset(VOD, select = VOD.L.Adjusted)
colnames(VOD) <- c("VOD")

TUI <- (TUI.L)
TUI <- subset(TUI, select = TUI.L.Adjusted)
colnames(TUI) <- c("TUI")

EZJ <- (EZJ.L)
EZJ <- subset(EZJ, select = EZJ.L.Adjusted)
colnames(EZJ) <- c("EZJ")

CCL <- (CCL.L)
CCL <- subset(CCL, select = CCL.L.Adjusted)
colnames(CCL) <- c("CCL")

ANTO <- (ANTO.L)
ANTO <- subset(ANTO, select = ANTO.L.Adjusted)
colnames(ANTO) <- c("ANTO")

# Portfolio is now P2 instead of the initial name of P1
P2 <- data.frame(as.xts(merge(RB, RMV, STJ, SDR, TSCO, CNA, PSN, EXPN, BA, CPG, SMT, SMIN, SSE, PRU, AHT, RTO, RR, BATS, CCH, SPX, VOD, TUI, EZJ, CCL, ANTO)))

# The following formula will calculate the daily return on the stock price
# The first row was removed as there would be no 0th row  for it to use in it's calculation

for (i in 1:ncol(P2)) {
  prices = P2[,i] 
  prices_prev = c(NA,prices[1:(length(prices)-1)]) 
  returns = (prices-prices_prev)/prices_prev 
  P2[,i] = returns 
}
asset_returns_2019 = P2[2:nrow(P2),1:ncol(P2)]
asset_returns_2019 = na.omit(asset_returns_2019)

# We now have the asset_returns calculated for 2019 in the same manner as we calculated them previously for 2018.


library(PerformanceAnalytics)
# This library allows the portfolio return to be calculated using the asset returns and the weightings found.

# The return on the 2018 portfolio was calculated as this would allow a comparison with 2019.
return_2018 = Return.portfolio(asset_returns_2018, weights=weightings_2018, rebalance_on = "months")
# This generates the daily return on the portfolio.
# To calculate the annual return we can sum all the daily sums.
sum(return_2018)
# We would have lost money in 2018 had we used the weighted portfolio calculated by the GA.


return_2019 = Return.portfolio(asset_returns_2019, weights=weightings_2018, rebalance_on = "months")
# This generates the daily return on the portfolio.
# To calculate the annual return we can sum all the daily sums.
sum(return_2019)
# The return for 2019, based on the weights found for 2018 was positive.
# This means we would have made money in 2019 had we used the weighted portfolio calculated by the GA.

# So apparently we would have lost our shirt based on the suggested weights in 2018.
# Had we maintained these weightings within our portfolio however then the return on the portfolio
# would have been in the black for 2019.
# There is a number of theories which seek to explain how a portfolio can perform,
# with them ranging greatly in what they propose and expect.
# This analysis shows the varying returns which can be seen within a portfolio-
# one or two assets could have overperfomed in 2019 which led to the huge increase in portfolio returns, for example.

# Part 3: Comparison of the evolved portfolio with balanced and random portfolios

# First I will generate an equally weighted portfolio
# which will then be used with the asset returns for 2018 and 2019
# in order to compute the Portfolio returns for each year.
# I will compare the return equally weighted portfolio versus the 
# returns on the GA generated portfolio 

weights_equal = rep(0.04, 25)
# we have 25 assets within our portfolio
# 1/25 = 0.04
# The above function generates a vector of length 25 with each asset being weighted as 0.04.

return_2018_equal = Return.portfolio(asset_returns_2018, weights=weights_equal, rebalance_on = "months")
sum(return_2018_equal)
sum(return_2018)
# Comparing an equally weighted portfolio to the portfolio with generated weights
# suggests that the equally weighted portfolio performs worse for 2018.

return_2019_equal = Return.portfolio(asset_returns_2019, weights=weights_equal, rebalance_on = "months")
sum(return_2019_equal)
sum(return_2019)
# Comparing an equally weighted portfolio to the portfolio with generated weights
# suggests that the equally weighted portfolio performs worse for 2019.

# So for an equally weighted portfolio (every asset haing a weighting of 0.04),
# we see that for 2018 & 2019 it performs worse than the portfolio with weights
# generated using the GA package.

install.packages("rportfolios")
library(rportfolios)
# This package allows the generation of a random portfolio with 25 different weights
# The weights are i.i.d (Independent and Identically Distributed)
# I'll generate 10 of these and then take the mean of them

r1 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r2 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r3 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r4 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r5 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r6 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r7 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r8 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r9 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)
r10 = random.benchmark(n = 25, k = n, segments = NULL, x.t = 1)

random_weights = (r1+r2+r3+r4+r5+r6+r7+r8+r9+r10)/10
random_weights

return_2018_random = Return.portfolio(asset_returns_2018, weights=random_weights, rebalance_on = "months")
sum(return_2018_random)
sum(return_2018)
# Comparing an equally weighted portfolio to the portfolio with generated weights
# suggests that the randomly generated weighted portfolio performs worse for 2018.

return_2019_random = Return.portfolio(asset_returns_2019, weights=random_weights, rebalance_on = "months")
sum(return_2019_random)
sum(return_2019)
# Comparing an equally weighted portfolio to the portfolio with generated weights
# suggests that the randomly generated weighted portfolio performs worse for 2019.

# When we generated a portfolio populated with random weight allocations
# it performs worse than the portfolio which was generated using the GA package.
# This suggests that creating a portfolio based upon Genetic Algorithms is superior to randomly
# allocating assets to a portfolio.










# Part 4: Creation and evaluation of portfolios with differently balaced risk and return

# If we edit the sharpe ratio we can adjust the risk & return
# By multiplying the denominator we can edit the risk
# A multiplier between 0 & 1 will increase the risk
# A multipler greater than 1 will decrease the risk

# First we consider decreasing the risk tolerance & increasing the expected return.
# To do this I have taken the square root of the risk, thus reducing it further.
# I have also increased the weighting of the mean returns within the numerator.

sharpe_risky_less = function(x) {
  portfolio.returns = portfolio_returns_2018(x)
  return ((mean(portfolio.returns)^2)/sqrt(sqrt(var(portfolio.returns))))
}

Optimisable_risky_less = function(x) {
  return (-sharpe_risky_less(x) + 100 * penalty(x))
}

library("GA")
lbound = rep(0,ncol(asset_returns_2018))
ubound = rep(1,ncol(asset_returns_2018))

ga_portfolio = ga (type = "real-valued", function(x){-Optimisable_risky_less(x)},
                   lower = lbound, upper = ubound, maxiter=100000,
                   run = 1000)

weightings_2018_risky_less = as.vector(summary(ga_portfolio)$solution)
cbind(names(asset_returns_2018), weightings_2018_risky_less)

return_2018_risky_less = Return.portfolio(asset_returns_2018, weights=weightings_2018_risky_less, rebalance_on = "months")
sum(return_2018_risky_less)
sum(return_2018)
# We can directly compare the return on the portfolio based upon editing the level of risk.
# By reducing the risk we still see negative returns for 2018, 
# although we lose less than previously.

return_2019_risky_less = Return.portfolio(asset_returns_2019, weights=weightings_2018_risky_less, rebalance_on = "months")
sum(return_2019_risky_less)
sum(return_2019)
# We can directly compare the return on the portfolio based upon editing the level of risk.
# By reducing the risk we still see positive returns for 2019, 
# although these are less than with the balanced risk/return portfolio.




# Now we consider decreasing the expected return & increasing the risk tolerance.
# To do this I have taken the square root of the portfolio, thus reducing it further.
# I have also decreased the weighting of the risk within the denominator.

sharpe_risky_more = function(x) {
  portfolio.returns = portfolio_returns_2018(x)
  return (sqrt(mean(portfolio.returns))/var(portfolio.returns))
}

Optimisable_risky_more = function(x) {
  return (-sharpe_risky_more(x) + 100 * penalty(x))
}

library("GA")
lbound = rep(0,ncol(asset_returns_2018))
ubound = rep(1,ncol(asset_returns_2018))

ga_portfolio = ga (type = "real-valued", function(x){-Optimisable_risky_more(x)},
                   lower = lbound, upper = ubound, maxiter=100000,
                   run = 1000)

weightings_2018_risky_more = as.vector(summary(ga_portfolio)$solution)
cbind(names(asset_returns_2018), weightings_2018_risky_less)

return_2018_risky_more = Return.portfolio(asset_returns_2018, weights=weightings_2018_risky_more, rebalance_on = "months")
sum(return_2018_risky_more)
sum(return_2018)
# We can directly compare the return on the portfolio based upon editing the level of risk.
# By increasing the risk  we still see negative returns for 2018, 
# although we lose less than the balanced risk/return portfolio.

return_2019_risky_more = Return.portfolio(asset_returns_2019, weights=weightings_2018_risky_more, rebalance_on = "months")
sum(return_2019_risky_more)
sum(return_2019)
# We can directly compare the return on the portfolio based upon editing the level of risk.
# By increasing the risk  we still see positive returns for 2019, 
# although we do not see higher returns than with the balanced risk/return portfolio.

# Finally I present the results for each year I have considered.
# The format of the portfolio along with the returns for each year are presented below.

returns_2018_varying = c(sum(return_2018), sum(return_2018_equal), sum(return_2018_random), sum(return_2018_risky_less), sum(return_2018_risky_more))
returns_2018_varying
names_2018 = c("Standard", "Equally-Weighted", "Random", "Less-Risk", "More-Risk")
Portfolio_2018 = cbind(names_2018, returns_2018_varying)
Portfolio_2018

barplot(returns_2018_varying, main="Varying Portfolio Returns (2018)", 
        ylab = "Portfolio Returns" , xlab = "Portfolio",
        names.arg = c("Standard", "Equally Weighted", "Random", "Lower Risk", "Higher Risk"),
        cex.names = 0.75)

# The portfolio which performs best is the ones where risk/return are varied as opposed to being balanced

returns_2019_varying = c(sum(return_2019), sum(return_2019_equal), sum(return_2019_random), sum(return_2019_risky_less), sum(return_2019_risky_more))
returns_2019_varying
names_2019 = c("Standard", "Equally-Weighted", "Random", "Less-Risk", "More-Risk")
Portfolio_2019 = cbind(names_2019, returns_2019_varying)
Portfolio_2019

barplot(returns_2019_varying, main="Varying Portfolio Returns (2019)", 
        ylab = "Portfolio Returns" , xlab = "Portfolio",
        names.arg = c("Standard", "Equally Weighted", "Random", "Lower Risk", "Higher Risk"),
        cex.names = 0.75)

# The clear winner here is the portfolio which was generated using a genetic algorithm based on the 2018 stock valuations.
