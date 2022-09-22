#realcommodity.R
# chart real commodity prices
da <- read.csv('../Data/Realcommodityprices.csv', skip = 1)
head(da)
colnames(da)[1] <- "Year"
rebase <- function(x = data, y = year){
  x[1]/x[1:length(x)] * 100
}
plot(da$Year, rebase(da$Beef), type = 'l')
plot(da$Date, rebase(da$Beef), type = 'l')
# rebase all
da2 <- apply(subset(da, da$Date > 1950), 2, rebase)
head(da2)
# Just plot from set date
plot(da$Year[da$Year > 1950], rebase(da$Lead[da$Year > 1950]), type = 'l', ylim = c(40, 600), 
     ylab = "Price (100 = 1950", main = "Selected real commodity prices", xlab = "Year")
lines(da$Year[da$Year > 1950], rebase(da$Petroleum[da$Year > 1950]), type = 'l', col = 'blue')
lines(da$Year[da$Year > 1950], rebase(da$Tea[da$Year > 1950]), type = 'l', col = 'red')
legend('topleft', inset = 0.05, cex = 0.6, legend = c("Lead", "Petroleum", "Tea"), 
       lty = c(1,1,1), col = c('black', 'blue', 'red'))
colnames(da)

