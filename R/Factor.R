# FactorVal.R
# This is assessmnent of Bloomberg data
da <- read.csv('../Data/USWorld.csv')
head(da)
tail(da)
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
colnames(da) <- c("Date", "MSCIUS", "MSCIWorld")
head(da)
plot(da$Date, da$MSCIUS, type = 'l', 
     main = "US and World Valuation", xlab = "Year", 
     ylab = "PE", ylim = c(10, 80))
lines(da$Date, da$MSCIWorld, type = 'l', col = 'red')
#=========================================
da$MSCIWorldRB <- benchmark(da$MSCIWorld, specify = TRUE, 
                            date = as.Date("2019-12-31"))
da$MSCIUSRB <- benchmark(da$MSCIUS, specify = TRUE, 
                         date = as.Date("2019-12-31"))
head(da)
tail(da)
plot(da$Date, da$MSCIWorldRB, type = 'l', main = 
       "US and World Equity Values", xlab = "Year", 
     ylab = "Index = 100 on 2019-12-31", ylim = c(60, 200))
lines(da$Date, da$MSCIUSRB, type = 'l', col = 'red', 
      lty = 2)
legend("topleft", inset = 0.03, legend = c("MSCI US", "MSCI World"), 
       col = c('red', 'black'), lty = c(2,1), cex = 0.8)




#===============================
# Growth and value performance
da2 <- read.csv('../Data/RAGRAV.csv')
head(da2)
tail(da2)
da2 <- da2[, c(1, 2, 3)]
colnames(da2) <- c("Date", "Value", "Growth")
da2$Date <- as.Date(da2$Date, format = "%d/%m/%Y")
da2$ValueB <- benchmark(da2$Value, specify = TRUE, date = as.Date("2019-12-31"))
da2$GrowthB <- benchmark(da2$Growth, specify = TRUE, date = as.Date("2019-12-31"))
plot(da2$Date, da2$ValueB, type = 'l', ylim = c(60, 180), lty = 2, 
     main = "Russell Growth and Value", xlab = "Year", ylab = "Index = 100 Dec 2019")
lines(da2$Date, da2$GrowthB, type = 'l', col = 'red',
      main = "Russell Value and Growth")
legend('topleft', inset = 0.04, legend = c("Russell Value", "Russell Growth"), 
       col = c('black', 'red'), lty = c(1,2), cex = 0.8)
#================================================
# Benchmark
?benchmark
benchmark <- function(x, specify = FALSE, 
                      date = as.Date("2008-01-01")) {
  ifelse(specify == FALSE, 
      mydata <- x/x[date] * 100, 
           mydata <- x/x[which(da$Date == as.Date(date))] * 100 )
  return(mydata)
}
#===============================
# Growth and value valuation
da2 <- read.csv('../Data/RAGRAV.csv')
head(da2)
tail(da2)
da2 <- da2[, c(1, 7, 8)]
head(da2)
tail(da2)
colnames(da2) <- c("Date", "Value", "Growth")
da2$Date <- as.Date(da2$Date, format = "%d/%m/%Y")
da2$ValueB <- benchmark(da2$Value, specify = TRUE, date = as.Date("2019-12-31"))
da2$GrowthB <- benchmark(da2$Growth, specify = TRUE, date = as.Date("2019-12-31"))
# First plot the absolute valuation 
plot(da2$Date, da2$Value, type = 'l', lty = 1, 
     main = "Russell Growth and Value", xlab = "Year", ylab = "PE ratio", 
     ylim = c(10, 50))
lines(da2$Date, da2$Growth, type = 'l', col = 'red',
      main = "Russell Value and Growth", lty = 2)
legend('topleft', inset = 0.04, legend = c("Russell Value PE", "Russell Growth PE"), 
       col = c('black', 'red'), lty = c(1,2), cex = 0.8)
# Now plot relative to Dec 2019 base
plot(da2$Date, da2$ValueB, type = 'l', ylim = c(60, 180), lty = 2, 
     main = "Russell Growth and Value", xlab = "Year", ylab = "Index = 100 Dec 2019")
lines(da2$Date, da2$GrowthB, type = 'l', col = 'red',
      main = "Russell Value and Growth")
legend('topleft', inset = 0.04, legend = c("Russell Value", "Russell Growth"), 
       col = c('black', 'red'), lty = c(1,2), cex = 0.8)
#