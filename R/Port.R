# portfolio.R
# This will rebase SPY and TLT, 
# post the SPY and TLT performance
# Calculate the returnsha
da <- read.csv('../Data/port3.csv')
da$Date <- as.Date(da$Date, format = "%Y-%m-%d")
head(da)
tail(da)
rebase <- function(x){
  myrebasedseries <- x/x[1] * 100
  return(myrebasedseries)
}
da$SPYRB <- rebase(da$SPY)
da$TLTRB <- rebase(da$TLT)
da$VGSLXRB <- rebase(da$VGSLX)
da$port <- 0.5 * da$SPYRB   + 0.4 * da$TLTRB + 0.1 * da$VGSLXRB
#================================================
# Full plot
plot(da$Date, da$SPYRB, ylab = "Price", xlab = "Year", 
     main = "Portfolio and SPY-TLT", type = 'l', col = 'red')
lines(da$Date, da$port, col = 'black', type = 'l')
lines(da$Date, da$TLTRB, col = 'blue', type = 'l')
lines(da$Date, da$VGSLXRB, col = 'green', type = 'l')
legend('topleft', inset = 0.02, legend = c("Portfolio", "SPY", "TLT", "VGSLX"), 
       col = c("black", "red", "blue", "green"), lty = c(1,1,1,1), cex = 0.8)
#=================================================================
# 2002 to 2010
da1 <- subset(da, subset = da$Date %in% 
                as.Date("2000-01-01"):as.Date("2010-12-31"))
plot(da1$Date, da1$SPYRB, ylab = "Price", xlab = "Year", 
     main = "Portfolio and SPY-TLT (2002 - 2010", type = 'l', col = 'red',
     ylim = c(80, 250))
lines(da1$Date, da1$port, col = 'black', type = 'l')
lines(da1$Date, da1$TLTRB, col = 'blue', type = 'l')
lines(da1$Date, da1$VGSLXRB, col = 'green', type = 'l')
legend('topleft', inset = 0.02, legend = c("Portfolio", "SPY", "TLT"), 
       col = c("black", "red", "blue", "green"), lty = c(1,1,1,1), cex = 0.8)
#===============================================================
# 2011 to 2022
da2 <- subset(da, subset = Date %in% as.Date("2011-01-01"):
                as.Date("2022-06-30"))
da2$SPYRB <- rebase(da2$SPY)
da2$TLTRB <- rebase(da2$TLT)
da2$VGSLXRB <- rebase(da2$VGSLX)
da2$port <- 0.5 * da2$SPYRB + 0.4 * da2$TLTRB + 0.1 * da2$VGSLXRB
plot(da2$Date, da2$SPYRB, ylab = "Price", xlab = "Year", 
     main = "Portfolio and SPY-TLT (2011 - 2022)", type = 'l', col = 'red')
lines(da2$Date, da2$port, col = 'black', type = 'l')
lines(da2$Date, da2$TLTRB, col = 'blue', type = 'l')
lines(da2$Date, da2$VGSLXRB, col = "green", type = 'l')
legend('topleft', inset = 0.02, legend = c("Portfolio", "SPY", "TLT", "VGSLX"), 
       col = c("black", "red", "blue"), lty = c(1,1,1,1), cex = 0.8)
#================================================================
# Calculate the stats for each period
myreturns <- function(x){
 myreturns <- c(NA, x[2:length(x)]/x[1:1-length(x)] -1)
 return(myreturns)
}
#==================================
mystats <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x)
  stdev <- sd(x)
  AnnRet <- (1 + m)^252 - 1
  AnnVol <- stdev * sqrt(252)
  Sh <- AnnRet/AnnVol
  DD <- min(x)
  mydataframe <- data.frame(
    "Annual Return" = AnnRet, 
    "Volatility" = AnnVol,
    "Sharpe Ratio" = Sh,
    "Draw Down" = DD
  )
  
  return(mydataframe)
  }
da$SPYR <- myreturns(da$SPY)
da$TLTR <- myreturns(da$TLT)
da$VGSLXR <- myreturns(da$VGSLX)
da$portR <- myreturns(da$port)
mystats(da$SPYR)
mystats(da$TLTR)
mystats(da$VGSLXR)
mystats(da$portR)
#======================================
