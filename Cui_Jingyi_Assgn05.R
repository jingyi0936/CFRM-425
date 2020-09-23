## 1
# 1a
TypeFlag1 <- "c"
S1 <- 100
X1 <- 105
Time1 <- 0.5
sigma1 <- 0.325
b1 <- 0
r1 <- 0.01
result.GBS1 <- GBSOption(TypeFlag1, S1, X1, Time1, r1, b1, sigma1)
(summ1 <- summary(result.GBS1))
print(summ1@price)
char.GBS1 <- GBSCharacteristics(TypeFlag1, S1, X1, Time1, r1, b1, sigma1)
print(char.GBS1)

# 1b
newS <- 95
newResult.GBS <- GBSOption("p", newS, X1, Time1, r1, b1, sigma1)
(newSumm <- summary(newResult.GBS))
print(newSumm@price)
newchar.GBS <- GBSCharacteristics("p", newS, X1, Time1, r1, b1, sigma1)
print(newchar.GBS)

# 2
# 2a
Time2 <- 6/12
r2 <- 0.01
b2 <- 0.06
sigma2 <- 0.325
X2 <-seq(from = 85, to = 115, by = 1) # Strike price
S2 <- 100
Time3 <- 0.1/365
result.GBS3 <- GBSOption("p", S2, X2, Time3, r2, b2, sigma2)
sum.Mat <- summary(result.GBS3)
matPrice <- sum.Mat@price
plot(x = X2, y = matPrice, type = "b", col = "darkred",
     main = "European Put Option Prices",
     xlab = "Strike Price", ylab = "Option Price",lty=1, lwd = 1.5, pch = 16)

# 2b
result.GBS4 <- GBSOption("p", S2, X2, Time2, r2, b2, sigma2)
sum.Mat4 <- summary(result.GBS4)
matPrice4 <- sum.Mat4@price
lines(x = X2, y = matPrice4, type = "b", col = "darkblue",
      lty=1, lwd = 1.5, pch = 16, cex=0.5)

# 2c
result.BAW <- BAWAmericanApproxOption("p", S2, X2, Time2, r2, b2, sigma2)
summ.BAW <- summary(result.BAW)
(BAW.prices <- summ.BAW@price)
lines(x = X2, y = BAW.prices, type = "b", col = "darkgreen",
      lty=1, lwd = 1.5, pch = 16, cex=0.5)



# 2d
# Interpretation



# 3
# 3a
X3 = 95
S3 = 100
Time = 0.5
r3 = 0.01
b3 = 0
sigma3 = seq(from = 0.1, to = 0.5, by = 0.05)
prices <- vector("double", 0L)
for(sig in sigma3){
  put.JR <- JRBinomialTreeOption("pa", S3, X3, Time, r3, b3, sig, n=100,
                                 title = "American 6M Puts", description = "Varing options prices")
  prices <- c(prices, put.JR@price)
}
print(prices)

plot(sigma3, prices, type="b", col = 'goldenrod3', lwd=1.5, pch=16, 
     main = "Option values vs Volatilities", xlab = "Volatilities", ylab = "Option Prices")

# 3d
# As the volatilities increases, the option prices increase.