# 3
mtx2 = matrix(c(100, 5, 200, 6, 300, 7, 400, 8), nrow = 2)
rants <- tMtx(mtx2, 100)
n = nrow(rants)
rants[c(1, (n-2):n), ]
#            [,1]        [,2]      [,3]        [,4]
# [1,] -0.5365183  0.08664128  1.188166 -0.95162990
# [2,]  0.3191958 -1.30616843 -2.393765  0.04340505
# [3,]  1.0042908 -0.30899017 -3.896520 -0.01757910
# [4,]  0.1595094 -0.49096263  2.424273 -2.95057967

# 4(b)
ans <- apply(rants, 2, FUN = posCount)
print(ans)
# > print(ans)
# [1] 51 59 45 51

# 5(a)
rants.df <- data.frame(rants)
colnames(rants.df) <- c("Seed 1", "Seed 2", "Seed 3", "Seed 4")
n = nrow(rants.df)
rants.df[c(1, (n-2):n), ]
# > rants.df[c(1, (n-2):n), ]
#         Seed 1      Seed 2    Seed 3      Seed 4
# 1   -0.5365183  0.08664128  1.188166 -0.95162990
# 98   0.3191958 -1.30616843 -2.393765  0.04340505
# 99   1.0042908 -0.30899017 -3.896520 -0.01757910
# 100  0.1595094 -0.49096263  2.424273 -2.95057967

# 5(b)
skews = apply(rants.df, 2, skewness)
kurts = apply(rants.df, 2, kurtosis)
firstMoments = apply(rants.df, 2, moment)

# 5(c)
stats <- data.frame(skew, kurt, momt)
print(stats)
# > print(stats)
#               skew      kurt        momt
# Seed 1 -0.47620548 2.3077934  0.01388230
# Seed 2  0.01965037 1.7518132  0.04850987
# Seed 3 -0.05082397 0.7818734 -0.05051284
# Seed 4  1.03847466 2.6220909  0.18598242


# 6(a)
rantsMod <- matrix(c(exp(rants[,1]), -exp(rants[, 2]), (rants[, 3])^2, -(rants[, 4])^2), ncol = 4)
n = nrow(rantsMod)
rantsMod[c(1, (n-2):n), ]
# > rantsMod[c(1, (n-2):n), ]
#           [,1]       [,2]      [,3]       [,4]
# [1,] 0.5847808 -1.0905054  1.411739  -0.9055994573
# [2,] 1.3760207 -0.2708559  5.730111  -0.0018839986
# [3,] 2.7299704 -0.7341880 15.182865  -0.0003090249
# [4,] 1.1729352 -0.6120369  5.877099  -8.7059203959

# 6(b)
plot(rantsMod[,1], col = "goldenrod4", lwd = 0.5, main = "Modified Random Scenarios from t-Distributions", 
     xlab = "x", ylab = "Mod t Variate", xlim = c(0, 100), ylim = c(min(rantsMod), max(rantsMod)))
for (i in 2:4){
  lines(rantsMod[, i], col = i, lwd = 0.5)
}