#===================================#
#CONTRIBUTIONS: JASON SWAN      50%
#               HAMISH MICTHELL 50%
#===================================#


#======================================#
#**************************************#

# SECTION 1: BACKGROUND TO THE PROBLEM

#**************************************#
#======================================#


# Traders have been looking for ways to improve their returns on trades for decades.
# Economists and financiers have been creating new models, and updating old models
# in order to see greater returns on investment.

# Given the exponential growth in both computer power and the number of packages
# which can be applied to optimising solutions to financial problems it should
# come as no suprise that traders, and trading agents, have looked to technology
# for assistance.
# This area is known as Algorithmic Trading, which has been growing for a number of years now.
# In fact as far back as 2011, over 73% of U.S. equity trades were completed using Algorithmic Trading.
# [Treleavan, P., Galas, M. and Lalchand, V. (2013)]

# There exist programs which will invest your money for you.
# There exist programs which will help you decide how and where to invest your money.
# Most notably for the purposes of this investigation, there exists programs which will
# help you to automatically formulate trading rules for the purposes of deciding which position
# to take on a given stock at any given time.

# Two common approaches exist which employ one of a number of different techniques to generate,
# or optimise trading rules. 
# One seeks to build up trading rules by considering the predicted high and low values
# for the next day of trading is available.
# The second method rather seeks to optimise the pararmeters used within existing trading rules.
# This second method is the one which we will employ during our investigation.
# We will seek to optimise the parameters involved in the Mean, Average, Convergence/Divergence (MACD) model.
# By using a pre-existing model and aiming to optimise the parameters involved we seek to maximise the profit achieved.

# In order for us to optimise these parameters we could employ a number of different techniques.
# Some examples of possible techniques include: Genetic Algorithms, Genetic Programming, and Neural Networks.
# The method which we have chosen to employ is Genetic Algorithms (GA).
# The reason for this is the evolution of the population towards a solution.
# The steps involved in the GA will be explored at each stage as opposed to providing a summative overview at this point.
# The main purpose of the GA will be to find rules which maximise one particular measure of fitness. [Allen, F. and Karjalainen, R. (1999)]

# As noted the method we have chosen to employ is a GA which seeks to optimise the signals
# used by the MACD model.
# The MACD model is designed to reveal changes in the strength, direction, momentum and duration of a trend in a stock's price.
# There are a number of steps within the MACD model but this can be broken down into equations ala Enke, D., and Wiles. P. S. (2015)
# The standard rules values employed for the EMA are as follows:
# The 12 day exponential moving average (EMA) is calculated- EMA(fast)
# The 26 day ema is calculated - EMA(slow)
# The MACD is calculated: MACD = EMA(fast) - EMA(slow)
# The signal line is created- which is the 9 day moving average of the MACD.
# The equations for this can be found in the paper highlighted above.
# Our purpose will be to identify values for :
# EMA(fast) - The SHORT
# EMA(slow) - The LONG
# 9 day moving average - The SIGNAL


# A GA will be used to identify optimal values for the short, long, and signal values for the MACD.
# By carefully controlling the parameters we can reduce the occurence of excessive signalling
# or improve sensitivity of the indicator.

# Once we find the optimal values for the MACD we will implement a trading strategy.
# This trading strategy will be based on the optimal values found for the MACD.
# We will measure the profit generated based upon the signals of the MACD.
# In order to compute the profit the quantstrat library will be used.
# This library allows the addition of rules & signals to a trading strategy.
# This makes it perfect to explore how the optimised values for the MACD signals perform relative to suggested values.
# This will be explored in Section 4 and Section 5.



#===================================#
#***********************************#

# SECTION 2: AN OVERVIEW OF THE DATA

#***********************************#
#===================================#

# First we will load all libraries necessary for the purposes of this investigation at the beginning.
library(quantmod)
library(xts)
library(TTR)
library(parallel)
library(doParallel)
library(GA)
devtools::install_github("braverock/quantstrat")
library(quantstrat)

# We have chosen to look at Tesla's stock. Their stock ticker is TSLA.
# The reason we have chosen Tesla is that the company is known for being a volatile stock.
# Some attribute this to Elon Musk, who in turn blames short sellers.
# By considering the data we can hopefully identify any patterns in the stock.

# This sets up our portfolio of only one stock, with the only ticker noted being that of Tesla.
stock <- c("TSLA")

# We then scrape the stock data for Tesla from Yahoo Finance.
# A 6 year time window is chosen as this will ensure a range of data is generated.
# We will split the data into test (2014 - 2018) & train (2019).
# Backtesting later will allow us to check the effectively of the model we have generated.
# This will allow us to measure the profit achieved by employing the parameters generated for the MACD model.

tesla <- getSymbols(stock, env=NULL, src = "yahoo", from = "2014-01-01", to = "2020-01-01")


# By checking the head of the data we can see the index and the columns stored.
head(tesla)

# The index is the date.
# The column names are: TSLA.Open, TSLA.High, TSLA.Low, TSLA.Close, TSLA.Volume, and TSLA.Adjusted.
# TSLA.Open is the stock price at the opening of the market.
# TSLA.High is the highest sale price achieved over the course of that day.
# TSLA.Low is the lowest sale price achieved over the course of that day.
# TSLA.Close is the stock price at the closing of the market.
# TSLA.Volume is the number of transactions over the course of that day.
# TSLA. Adjusted is the closing price adjusted for dividends etc.

# Now we choose to look at the TSLA.Adjusted column for the purposes of this invesigation.
# As noted above this is the closing price for each day after it has been adjusted for
# any dividends and other such payments.

tesla <- subset(tesla, select = TSLA.Adjusted)
colnames(tesla) <- c("Adjusted Closing Price")
head(tesla)


# We now split the data as follows:
# tesla will be our test data set
# tesla_train will be our training data set
tesla_train <- window(tesla, start = "2014-01-01", end = "2018-12-31")
tesla_test <- window(tesla, start = "2019-01-01", end = "2019-12-31")


chartSeries(tesla, theme="white")
title(main = "Time Series Plot for Tesla", x = "Date", y = "Adjusted Closing Price")
# The time series plot for Tesla shows how volatile the company is.
# There was a period where the price was around 200 - 250 however there was a sharp increase
# which can be seen in early 2017 where the price jumped and its new low price
# matches the previous higher end prices seen of around 250. It's new high prices tended to
# be around the range of 350.


# We can split the time series data up by year which will allow us to detect
# any trends which occur each year.
# This will also allow the generation and interpretation of plots for each year.



tesla14 <- window(tesla, start = "2014-01-01", end = "2014-12-31")
chartSeries(tesla14, theme="white")
title(main = "2014 Time Series Plot for Tesla", x = "Date", y = "Adjusted Closing Price")
max14 <- max(tesla14)
min14 <- min(tesla14)

# The time series for Tesla over 2014 shows high volatility.
# The minimum value comes at the beginning over the year, while the maximum value comes
# at some point in September. The final price for the year is closer to the high value than the low.
# The whole plot sees an overall increase although is populated with peaks and troughs.



tesla15 <- window(tesla, start = "2015-01-01", end = "2015-12-31")
chartSeries(tesla15, theme="white")
title(main = "2015 Time Series Plot for Tesla", x = "Date", y = "Adjusted Closing Price")
max15 <- max(tesla15)
min15 <- min(tesla15)

# While the prices for 2015 are higher than those over 2014, the graph still shows high volatility.
# There as significant increases for a number of months, before it fell sharply, recovered slightly,
# then fell even further before finishing the year around 20 points higher than it started.

tesla16 <- window(tesla, start = "2016-01-01", end = "2016-12-31")
chartSeries(tesla16, theme="white")
title(main = "2016 Time Series Plot for Tesla", x = "Date", y = "Adjusted Closing Price")
max16 <- max(tesla16)
min16 <- min(tesla16)

# Early 2016 sees a huge fall in the stock price for Tesla.
# It quickly recovers but fails to match the previous highs seen in 2015.
# In fact the final price for Tesla is very close to the starting price, albeit lower.


tesla17 <- window(tesla, start = "2017-01-01", end = "2017-12-31")
chartSeries(tesla17, theme="white")
title(main = "2017 Time Series Plot for Tesla", x = "Date", y = "Adjusted Closing Price")
max17 <- max(tesla17)
min17 <- min(tesla17)

# 2017 is a year of huge growth for Tesla.
# Whilst there is obvious fluctuations in the price, the year sees huge peaks and finishes strong.
# The high prices seen here are higher than previous and after mid 2017 sees the price remain strong above 300.


tesla18 <- window(tesla, start = "2018-01-01", end = "2018-12-31")
chartSeries(tesla18, theme="white")
title(main = "2018 Time Series Plot for Tesla", x = "Date", y = "Adjusted Closing Price")
max18 <- max(tesla18)
min18 <- min(tesla18)

# 2018 is highly volatile.
# This year sees peaks and troughs throughout it.
# While there is significant highs, there is also significant lows too.
# This year should provide some interesting results.

# We wil also plot the time series for 2019, oue test year which will be used later for backtesting.
chartSeries(tesla_test, theme="white")
title(main = "2019 Time Series Plot for Tesla", x = "Date", y = "Adjusted Closing Price")

# We can compare the maximum adjusted closing price for the 5 years.
# We will tabulate this information as follows.
Maximum_Price = c(max14, max15, max16, max17, max18)
names_max = c("Max Price 2014", "Max Price 2015", "Max Price 2016", "Max Price 2017", "Max Price 2018")
max_prices = cbind(names_max, Maximum_Price)
max_prices
# The worst year for the Maximum price achieved was 2016, with it being around 17 points lower than the next lowest.
# The best year for Maximum price achieved was 2017, with 2018 being a close second.

# We can compare the minimum adjusted closing price for the 5 years.
# We will tabulate this information as follows.
Minimum_Price = c(min14, min15, min16, min17, min18)
names_min = c("Min Price 2014", "Min Price 2015", "Min Price 2016", "Min Price 2017", "Min Price 2018")
min_prices = cbind(names_min, Minimum_Price)
min_prices
# The worst year for the Minimum price achieved was 2014, which is to be expected given this was the earliest we have considered.
# 2016 was also a poor year, which could be expected given the lowest Maximum price was also witnessed this year.
# The highest minimum price was for 2018, with it's lowest price actually being close to 2016's highest price.

# By considering the time series graphs, as well as the minimum and maximum prices, we can see that
# the price of Tesla stock has been volatile and varied widely.
# By choosing a stock such as this we hope to achieve the optimalal values for the signals associated with MACD.



#==================================================================#
#******************************************************************#

# SECTION 3: GENETIC ALGORITHM FOR OPTIMISATION OF VALUES FOR MACD.

#******************************************************************#
#==================================================================#



# So we know that traditionally the following values are used in the MACD:
# Short = 12 days
# Long = 26 days
# Signal = 9 days

# Our task is to use Genetic Algorithms to optimise these values.
# We can then compare our model to the standard model.

# We initally wanted a chromosone of length 3 as we hoped to optimise 3 parameters: (S, L, Sig).
# Following initial testing it was decided to not optimise the value for Signal as this resulted in 
# the value for the fast (short) being greater than the value for slow (long) which caused the problem to be unworkable.
# Thus we have decided to only optimise for the Short & Long instead.
# We will maintain a signal value of 9 throughout the analysis.



#=====================#
#THE FITNESS FUNCTION
#=====================#

#The fitness function used in this example returns a value based on the difference between the average MACD value 
#and average signal given by the 'MACD' function within R. 
#First, two counters and two totals are set up. One of each is for the MACD value and the signal. Next is simply a case
#of summing our MACD values and signal values, whilst counting their number, before taking the average value of both and 
#finding the difference.

#The reason such a scenic route was take was the functions used to calculate the mean are not compatible with the NA values 
#given for the first few entries of both MACD and signal.


fittesla = function(x) {
  macd_train = data.frame(MACD(tesla_train,nFast=x[1],nSlow=x[2]))  #x[1] is fast, x[2] is slow.
  counter1 = 0
  total1 = 0
  counter2 = 0
  total2 = 0
  for (i in length(macd_train[,1])){
    if (!is.na(macd_train[,1][i])){
      counter1 = counter1 + 1
      total1 = total1+macd_train[,1][i]
    }
  }
  for (i in length(macd_train[,2])){
    if (!is.na(macd_train[,2][i])){
      counter2 = counter2 + 1
      total2 = total2+macd_train[,2][i]
    }
  }
  meanmacd = total1/counter1
  meansigma = total2/counter2
  val = meanmacd-meansigma
  return(val)
}

#=====================#
#THE GENETIC ALGORITHM
#=====================#

#To ensure we got a good result from the genetic algorithm, we decided on a large population size.
pop = 2000
#This causes the algorithm to run very slowly: you can change this value to shorten computation time.


#The GA was to find a real valued 2 element vector. Initially this was run with intention of also optimising Signal, but due
#to this addition making the values for long and short unlogical, this was cut out.

#Instead the algorithm looks to optimise the long and short values. These were chosen to be between 1 and 100 with min and 
#max values for long being larger than short. A mutatuion rate of 0.02 was also applied to allow variety in the population,
#and elitism was used to ensure good chromosomes were passed down.

teslaGA = ga(type="real-valued",fitness = fittesla,
           lower = c(1,2),upper = c(99,100),
           popSize=pop,maxiter = 50000,run=25,
           parallel=TRUE, # Exploit multi-core properties of your CPU, comment out if needed.
           pmutation = 0.02,  #mutation rate
           elitism = pop*0.3,    #being elite helps with this particular fitness function  
           seed=23)


#As there was multiple solution vectors, they were saved as a dataframe

sol = as.data.frame(summary(teslaGA)$solution)

sum(sol[1])/5

sum(sol[2])/5

#============================#
#VALS: nFast is  1.731088
#      nSlow is 10.58676
#============================#

macd <- MACD(tesla_train, nFast = 1.731088, nSlow = 10.58676, nSig = 9, maType = "EMA")
tail(macd)
# Couple of things to note at this point. 
# First, because of the nature of the GA package the answers returned were floating point values. 
# As stated previously, the reason that signal was not calculated with this GA was that in tests, 
# the addition of the sigma would cause the fast to have a higher value than the slow.

chartSeries(tesla_train, theme="white")
title(main = "Time Series Plot for Tesla")
addMACD(1.731088, 10.58676, 9)
title(main = "Time Series Plot for Tesla, with MACD")

chartSeries(macd, theme="white")
title(main = "Mean Average Convergence Divergence Plot")


#====================================================#
#****************************************************#

# SECTION 4: PRESENTATION OF RESULTS OBTAINED

#****************************************************#
#====================================================#


# So we have used a Genetic Algorithm to find the optimal values for use with the MACD.
# This was found based over a 5 year time period.

# We chose to maintain the signal = 9days, as not doing so caused problems within optimisation,
# specifically it caused the short to be greater than the long and not vice versa as required.

# We successfully optimised the 2 most vital components of the MACD.

# We found that for the short (aka nFast), the optimal value was 1.731088.
# This compares with the suggested short of 12 days.

# We found that for the long (aka nSlow), the optimal value was 10.58676.
# This compares with the suggested long of 26 days.

# An explanation for this could be the extreme volatility within the stock of Tesla.
# As discussed earlier there are a great deal of peaks and troughs when considering how their stock price moves.
# By looking at shorter period we hope to maximise profit, as would be the case for every investor.

# We will now set up a trading strategy based on the MACD values optimised by the GA.
# As mentioned earlier we use the quantstrat library for this purpose.


# Reset the portfolio and account to be blank before beginning.
rm.strat(name = "macd", silent = TRUE)

# First we must state what stock we are using (TSLA)
stock.str='TSLA'

# Optimised MA parameters for MACD
fastMA = 1.731088 
slowMA = 10.58676
signalMA = 9
maType = "EMA"

# Set up the currency of the stock, along with ensuring this matches up.
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

# State the start date, end date, and the initial equity (chosen as 100,000 arbitrarily)
startDate = '2014-01-01'
endDate = '2018-12-31'
initEq=100000
portfolio.st='macd'
account.st='macd'

# The following 3 functions create the initial portfolio, initial account, and initial order details.
initPortf(portfolio.st,symbols=stock.str)
initAcct(account.st,portfolios=portfolio.st)
initOrders(portfolio=portfolio.st)

# We now define the strategy and store it.
strat.st<-portfolio.st
strategy(strat.st, store=TRUE)


# We seek to add an indicator to our trading strategy.
# This indicator uses the MACD values we optimised, based on the closing data of the TSLA stock.
add.indicator(strat.st, name = "MACD", 
              arguments = list(x=quote(Cl(mktdata)),
                               nFast=fastMA, 
                               nSlow=slowMA),
              label='_' 
)

# We add two signals to our trading strategy
# These are based on the MACD
# One for when we cross the threshold (zero) from below, and one for when we cross it from above.
add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="gt",
                            threshold=0,
                            cross=TRUE),
           label="signal.gt.zero"
)

add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="lt",
                            threshold=0,
                            cross=TRUE),
           label="signal.lt.zero"
)

# We must now implement two rules for trading.
# One determines when we enter the market (buy), while the other determines when we exit the market (sell).
# Where no decision is made to enter or leave the market, the model assumes that the stock is held over this period.
# This rule is implicit as opposed to having to be defined separately.

# Rule for entry
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.gt.zero",
                          sigval=TRUE, 
                          orderqty=100, 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL),
         type='enter',
         label='enter',
         storefun=FALSE
)

# Rule for exit
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.lt.zero",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL,
                          orderset='exit2'),
         type='exit',
         label='exit'
)

# We now run the model based upon what we have defined above.
getSymbols(stock.str,from=startDate, to=endDate)
out<-applyStrategy(strat.st , portfolios=portfolio.st,parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),verbose=TRUE)
# The 100 and -100 signals the purchase and sale of 100 assets of Tesla stock respectively.

# We must now update the portfolio based on the information generated by applying the strategy.
updatePortf(Portfolio=portfolio.st)

# Get the trade statistics for the updated portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the number of trades made over the time period
tstats$Num.Trades
# So we made a total of 45 trades over the portfolio.

# Print the total Profit/Loss for portfolio over the time period
tstats$Net.Trading.PL
# Ooh dear, it looks like we lost ~$9472.99
# Perhaps our model with optimised values will perform better over the test data.
# Let's find out in Section 5.




#====================================#
#************************************#

# SECTION 5: PERFORMANCE OF THE MODEL

#************************************#
#====================================#

# In Section 3 we used a GA to find the optimal values for the MACD.
# In Section 4 we presented our solution and even ran a model using quantstrat to test our optimised model.
# While the model developed did not perform superbly over the training data (AHHHH), we hope it will perform over the test data.
# Let's see what we can do then....

# The code for the testing of the data is virtually identical to the code we run in section 4  when presenting our model.

# Reset the portfolio and account to be blank before beginning.
rm.strat(name = "macd", silent = TRUE)

# First we must state what stock we are using (TSLA)
stock.str='TSLA'

# Optimised MA parameters for MACD (Same values as used in Section 4)
fastMA = 1.731088 
slowMA = 10.58676
signalMA = 9
maType = "EMA"

# Set up the currency of the stock, along with ensuring this matches up.
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

# State the start date, end date, and the initial equity (chosen as 100,000 arbitrarily)
# Note that we are now running the model over the test period.
startDate = '2019-01-01'
endDate = '2019-12-31'
initEq=100000
portfolio.st='macd'
account.st='macd'

# The following 3 functions create the initial portfolio, initial account, and initial order details.
initPortf(portfolio.st,symbols=stock.str)
initAcct(account.st,portfolios=portfolio.st)
initOrders(portfolio=portfolio.st)

# We now define the strategy and store it.
strat.st<-portfolio.st
strategy(strat.st, store=TRUE)


# We seek to add an indicator to our trading strategy.
# This indicator uses the MACD values we optimised, based on the closing data of the TSLA stock.
add.indicator(strat.st, name = "MACD", 
              arguments = list(x=quote(Cl(mktdata)),
                               nFast=fastMA, 
                               nSlow=slowMA),
              label='_' 
)

# We add two signals to our trading strategy
# These are based on the MACD
# One for when we cross the threshold (zero) from below, and one for when we cross it from above.
add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="gt",
                            threshold=0,
                            cross=TRUE),
           label="signal.gt.zero"
)

add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="lt",
                            threshold=0,
                            cross=TRUE),
           label="signal.lt.zero"
)

# We must now implement two rules for trading.
# One determines when we enter the market (buy), while the other determines when we exit the market (sell).
# Where no decision is made to enter or leave the market, the model assumes that the stock is held over this period.
# This rule is implicit as opposed to having to be defined separately.

# Rule for entry
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.gt.zero",
                          sigval=TRUE, 
                          orderqty=100, 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL),
         type='enter',
         label='enter',
         storefun=FALSE
)

# Rule for exit
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.lt.zero",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL,
                          orderset='exit2'),
         type='exit',
         label='exit'
)

# We now run the model based upon what we have defined above.
getSymbols(stock.str,from=startDate, to=endDate)
out<-applyStrategy(strat.st , portfolios=portfolio.st,parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),verbose=TRUE)
# The 100 and -100 signals the purchase and sale of 100 assets of Tesla stock respectively.

# We must now update the portfolio based on the information generated by applying the strategy.
updatePortf(Portfolio=portfolio.st)

# Get the trade statistics for the updated portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the number of trades made over the time period
tstats$Num.Trades
# So we made a total of 8 trades over the portfolio.

# Print the total Profit/Loss for portfolio over the time period
tstats$Net.Trading.PL
# So with the optimised values over the test data set we see a profit of $14,607.99

# When running the optimised model over the train data set we see a loss of $9472.99.
# When running the optimised model over the test data set we see a gain of $14,607.99.
# This suggests that while we made a loss over the initial 5 year period (training),
# our optimised values provide us with a profit overall.
# This suggests that our optimised values are good.

# Perhaps we should compare our MACD with optimised values to the standard approach.
# The "normal" approach to MACD uses the values 12, 26, 9 for short, long, and signal.
# In Section 6 we will conduct this comparison.




#===============================================#
#***********************************************#

# SECTION 6: COMPARISON AGAINST OTHER APPROACHES

#***********************************************#
#===============================================#

# We know that the conventional values for the MACD are 12, 26, 9.
# These values will therefore be used for comparison.
# We will compare the return based on these with the return achieved based on our optimised values.
# Both the training data (2014-2018) and the testing data (2019) will be checked and compared.


# First we will run the generic model over the time period 2014 - 2018 (Training)

# Reset the portfolio and account to be blank before beginning.
rm.strat(name = "macd", silent = TRUE)

# First we must state what stock we are using (TSLA)
stock.str='TSLA'

# Optimised MA parameters for MACD (Changed to generic values)
fastMA = 12
slowMA = 26
signalMA = 9
maType = "EMA"

# Set up the currency of the stock, along with ensuring this matches up.
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

# State the start date, end date, and the initial equity (chosen as 100,000 arbitrarily)
# Note that we are running the model over the training period.
startDate = '2014-01-01'
endDate = '2018-12-31'
initEq=100000
portfolio.st='macd'
account.st='macd'

# The following 3 functions create the initial portfolio, initial account, and initial order details.
initPortf(portfolio.st,symbols=stock.str)
initAcct(account.st,portfolios=portfolio.st)
initOrders(portfolio=portfolio.st)

# We now define the strategy and store it.
strat.st<-portfolio.st
strategy(strat.st, store=TRUE)


# We seek to add an indicator to our trading strategy.
# This indicator uses the MACD values we optimised, based on the closing data of the TSLA stock.
add.indicator(strat.st, name = "MACD", 
              arguments = list(x=quote(Cl(mktdata)),
                               nFast=fastMA, 
                               nSlow=slowMA),
              label='_' 
)

# We add two signals to our trading strategy
# These are based on the MACD
# One for when we cross the threshold (zero) from below, and one for when we cross it from above.
add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="gt",
                            threshold=0,
                            cross=TRUE),
           label="signal.gt.zero"
)

add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="lt",
                            threshold=0,
                            cross=TRUE),
           label="signal.lt.zero"
)

# We must now implement two rules for trading.
# One determines when we enter the market (buy), while the other determines when we exit the market (sell).
# Where no decision is made to enter or leave the market, the model assumes that the stock is held over this period.
# This rule is implicit as opposed to having to be defined separately.

# Rule for entry
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.gt.zero",
                          sigval=TRUE, 
                          orderqty=100, 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL),
         type='enter',
         label='enter',
         storefun=FALSE
)

# Rule for exit
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.lt.zero",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL,
                          orderset='exit2'),
         type='exit',
         label='exit'
)

# We now run the model based upon what we have defined above.
getSymbols(stock.str,from=startDate, to=endDate)
out<-applyStrategy(strat.st , portfolios=portfolio.st,parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),verbose=TRUE)
# The 100 and -100 signals the purchase and sale of 100 assets of Tesla stock respectively.

# We must now update the portfolio based on the information generated by applying the strategy.
updatePortf(Portfolio=portfolio.st)

# Get the trade statistics for the updated portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the number of trades made over the time period
tstats$Num.Trades
# So we made a total of 15 trades over the portfolio.

# Print the total Profit/Loss for portfolio over the time period
tstats$Net.Trading.PL
# So with the generic values over the train data set we see a loss of ~$3646.99
# This is less than the loss witnessed by using the optimised values of the training data set.
# Perhaps the MACD model isn't appropriate to use when considering investing over the training period.



# We will now run the portfolio based on the test data set, with the generic values.
# This means running it over 2019, with the following values for the MA: 12, 26, 9

# Reset the portfolio and account to be blank before beginning.
rm.strat(name = "macd", silent = TRUE)

# First we must state what stock we are using (TSLA)
stock.str='TSLA'

# Optimised MA parameters for MACD (Changed to generic values)
fastMA = 12
slowMA = 26
signalMA = 9
maType = "EMA"

# Set up the currency of the stock, along with ensuring this matches up.
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

# State the start date, end date, and the initial equity (chosen as 100,000 arbitrarily)
# Note that we are running the model over the training period.
startDate = '2019-01-01'
endDate = '2019-12-31'
initEq=100000
portfolio.st='macd'
account.st='macd'

# The following 3 functions create the initial portfolio, initial account, and initial order details.
initPortf(portfolio.st,symbols=stock.str)
initAcct(account.st,portfolios=portfolio.st)
initOrders(portfolio=portfolio.st)

# We now define the strategy and store it.
strat.st<-portfolio.st
strategy(strat.st, store=TRUE)


# We seek to add an indicator to our trading strategy.
# This indicator uses the MACD values we optimised, based on the closing data of the TSLA stock.
add.indicator(strat.st, name = "MACD", 
              arguments = list(x=quote(Cl(mktdata)),
                               nFast=fastMA, 
                               nSlow=slowMA),
              label='_' 
)

# We add two signals to our trading strategy
# These are based on the MACD
# One for when we cross the threshold (zero) from below, and one for when we cross it from above.
add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="gt",
                            threshold=0,
                            cross=TRUE),
           label="signal.gt.zero"
)

add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="lt",
                            threshold=0,
                            cross=TRUE),
           label="signal.lt.zero"
)

# We must now implement two rules for trading.
# One determines when we enter the market (buy), while the other determines when we exit the market (sell).
# Where no decision is made to enter or leave the market, the model assumes that the stock is held over this period.
# This rule is implicit as opposed to having to be defined separately.

# Rule for entry
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.gt.zero",
                          sigval=TRUE, 
                          orderqty=100, 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL),
         type='enter',
         label='enter',
         storefun=FALSE
)

# Rule for exit
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.lt.zero",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL,
                          orderset='exit2'),
         type='exit',
         label='exit'
)

# We now run the model based upon what we have defined above.
getSymbols(stock.str,from=startDate, to=endDate)
out<-applyStrategy(strat.st , portfolios=portfolio.st,parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),verbose=TRUE)
# The 100 and -100 signals the purchase and sale of 100 assets of Tesla stock respectively.

# We must now update the portfolio based on the information generated by applying the strategy.
updatePortf(Portfolio=portfolio.st)

# Get the trade statistics for the updated portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the number of trades made over the time period
tstats$Num.Trades
# So we made a total of 1 trade over the portfolio.

# Print the total Profit/Loss for portfolio over the time period
tstats$Net.Trading.PL
# The profit for the generic MACD values over the time period was $16,530.
# Ooh damn, the generic values have beaten our optimised values.

#===========#
#***********#

#CONCLUSION

#***********#
#===========#

# So to summarise the findings of Section 4, Section 5, and Section 6.
# At all times, traders started with $100,000

# Over the training time period, both the generic model and the optimised model lost money.
# The Optimised model made 45 trades and lost $9,472.99
# The Generic model made 15 trades and lost $3,646.99

# Over the test time period, both the generic model and the optimised model made money.
# The Optimised model made 8 trades and made a profit of $14,607.99
# The Generic model made 1 trade and made a profit of $16,530.

# It looks like our model is trading too often, increasing the risk and thus losing money.
# Perhaps our optimised mode considers too short a time period.
# Remember our values for the MACD were ~1.73 and ~10.59 as opposed to the generic models values of 12 and 26.
# Perhaps if we had succeeded in optimising the value of the signal (9), we would have found better results.