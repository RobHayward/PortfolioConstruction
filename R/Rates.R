da <- read.csv('./Data/Rate.csv', skip = 5)
da$Date <- as.Date(da$Time.Period, format = "%Y-%m-%d")
head(da)
plot(da$Date, da[,2], type = 'l', main = "US Official Interest Rate", 
     xlab = "Year", ylab = "Rate")
da2 <- read.csv('./Data/Res.csv')
head(da2)
da2$Date <- as.Date(da2$DATE, format =  "%Y-%m-%d")
#https://stackoverflow.com/questions/28159936/formatting-large-
#  currency-or-dollar-values-to-millions-billions
da2$WALCL <- round(da2$WALCL /1e6, 1)
plot(da2$Date, da2$WALCL, type = 'l', main = "Total Reserves of the 
     US Federal Reserve", xlab = "Year", ylab = "Tn")

