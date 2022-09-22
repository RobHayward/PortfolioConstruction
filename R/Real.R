# Real.R
# This will plot the real interest rate
da <- read.csv('../Data/RealInterestRate.csv', col.names = c("Date", "R.Rate"))
head(da)
da$Date <- as.Date(da$Date)
str(da)
plot(da$Date, da$R.Rate, type = 'l', xlab = "Date", ylab = "Rate", main = "US 1-year real rate")
abline(h  = 0, col = 'brown')
#===================

da <- read.csv('../Data/REAINTRATREARAT10Y.csv', col.names = c("Date", "R.Rate"))
head(da)
da$Date <- as.Date(da$Date)
plot(da$Date, da$R.Rate, type = 'l', xlab = "Date", ylab = "Rate", main = "US 10-year real rate")
abline(h  = 0, col = 'brown')
