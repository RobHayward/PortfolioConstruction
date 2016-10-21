
da <- read.csv('./Data/etf.csv')
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
str(da)
head(da)
tail(da)
SPY.R <- diff(da$SPY)/da$SPY[-length(da$SPY)] 
TLT.R <- diff(da$TLT)/da$TLT[-length(da$TLT)]
start <- as.Date(as.yearmon('1993-01'))
end <- as.Date(as.yearmon('2016-10'))
length(seq(from = start, to = end, by = "month"))

seq(from = start, to = end, by = "month")
z <- zoo(cbind(SPY.R[1:286], TLT.R[1:286], order.by = seq(from = start, to = end, by = "month")))
head(z)
tail(z)
str(dat)
corr <- rollapply(z, 21, function(x) cor(x[,1], x[,2]), by.column = FALSE)
plot(corr, )
dat <- cbind(da[-length(da),], SPY.R, TLT.R)
head(dat)
tail(dat)
plot(z)
