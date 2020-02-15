# 1
mktData <- read.csv("adjStockReturns.csv", header = T)
is.data.frame(mktData)
# the answer is True, meaning that mktData is a data frame

# 2 
n <- nrow(mktData) # return row numbers
mktData[c(1, (n-2):n), ]
#           Date        MSFT       AAPL          XOM          BP       SBUX
# 1    2/1/2000 -0.09085036 0.09968177 -0.095687368 -0.13286719 0.09317756
# 238 11/1/2019  0.05436451 0.07169601  0.008253474 -0.01300965 0.01023604
# 239 12/1/2019  0.04429822 0.09720179  0.035917593  0.02424760 0.03371626
# 240  1/1/2020  0.02770404 0.06279172 -0.008346602  0.02821499 0.03432230

# 3
standardize <- function(a){
  if (is.vector(a) == F){
    stop("Error: Input argument must be a vector")
  }else if(typeof(a) != "double"){
    stop("Error: Input argument must be a vector")
  }else{
    difference <- (a - mean(a))
    stan_rtn <- difference/sd(difference)
  }
}

# 4
msft <- standardize(mktData$MSFT)
xom <- standardize(mktData$XOM)
sbux <- standardize(mktData$SBUX)
stand.df <- data.frame(msft, xom, sbux)

# 5
n1 <- nrow(stand.df)
stand.df[c(1, (n1-2):n1), ]
# > stand.df[c(1, (n-2):n), ]
# msft         xom        sbux
# 1   -1.1736301 -1.97013902  0.93530490
# 238  0.5711491  0.07246983 -0.04052814
# 239  0.4502010  0.61611536  0.23572402
# 240  0.2508193 -0.25374902  0.24285433