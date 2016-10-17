# Matrix optimisation
# this takes the variance-covariance matrix from the excel file. 
#mysigma <- matrix(c(0.041227, 0.02266, 0.03056, 0.0000, 
                    0.0227, 0.0479, 0.015, 0.00000, 03056, 
                    0.01500, 0.09030, 0.0000, 0.00, 0.00, 
                    0.00, 0.00), nrow = 4)
sigma.mat <- matrix(c(0.0100, 0.0018, 0.0011, 0.0018, 0.0109, 0.0026, 
               0.0011, 0.0026, 0.0199), ncol = 3)
asset.names <- c("MSFT", "NORD", "SBUX")
mu.vec <- c(0.0427, 0.0015, 0.0285)
#mu.vec <- c(0.1774, 0.2161, 0.908, 0.05)
# dimnames must be a list
names(mu.vec) <- asset.names
dimnames(sigma.mat) <- list(asset.names, asset.names)
sigma.mat
# equal weights
x.vec <- rep(1/3, 3)
#x.vec <- c(0.136, 0.39, 0.120, 0.352)
names(x.vec) = asset.names
sum(x.vec) 
# compute mean, variance and standard deviation
mu.p.x <- crossprod(x.vec, mu.vec)
mu.p.x
sig2.p.x <- t(x.vec %*% sigma.mat %*% x.vec)
sig.p.x <- sqrt(sig2.p.x)
sig.p.x
# now introduce short positions
y.vec <- c(0.8, 0.4, -0.2)
sum(y.vec)
mu.p.y <- crossprod(y.vec, mu.vec)
mu.p.y
sig2.p.y <- t(y.vec %*% sigma.mat %*% y.vec)
sig.p.y <- sqrt(sig2.p.y)
sig.p.y
#------------------------------
plot(mu.p.y, sig.p.y, main = "Variance-covariance matrix", xlab = "Risk", 
     ylab = "Return", type = 'b')
points(mu.p.x, sig.p.x, type = 'b')
text(x = mu.p.y,  y = sig.p.y, labels = "Short", pos = 4)
text(x= mu.p.x,  y = sig.p.x, labels = "Equal", pos = 5)
