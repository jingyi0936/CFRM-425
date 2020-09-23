library(emulator)   # Contains the quad.form(.) function
                    # which you may find useful for
                    # computing quadratic forms.

## Pblm 1: Implement a common helper function that computes the
# volatility of a portfolio.  This way, you need not duplicate
# the same code in multiple function calls in your models
# implementations.  As you may recall, this is called 
# "code refactoring":
volat <- function(rtns, wts)
{
  wts <- matrix(wts, ncol = 1)
  #sig.p <- quad.form(cov(rtns), wts)
  sig.p <- t(wts)%*%cov(rtns)%*%wts
  vol <- sqrt(sig.p)
  return(vol)
}


## Pblm 2:  Implement this function to compute the parametric
# *relative* Value-at-Risk (VaR) of a portfolio.  It should
# take in an xts object rtns (one column for each asset), 
# a vector wts (asset weights), the desired confidence level
# (alpha), the annualizing offset (defaulted at 252 for daily
# returns), and ptflVal (the value of the portfolio, eg in USD;
# defaulted to one).  Hint: you should be able to do
# this in 2-3 lines of code.
paramVar <- function(rtns, wts, alpha, offset = 252, ptflVal = 1)
{
  portfolio.sig <- volat(rtns, wts)
  var <- ptflVal*portfolio.sig*sqrt(offset)*qnorm(1-Alpha)
  return(var)
}

## Pblm 3:  Same as pblm 2, but compute the historical *relative* VaR.  
# Again, this should not require more than 2-3 lines of code.
# You may also find the quantile(.) function useful in base R
# (we discussed this in class).
histVar <- function(rtns, wts, alpha, offset = 252, ptflVal = 1)
{
  portfolio.rtn <- rtns%*%wts
  histvar <- ptflVal*quantile(portfolio.rtn, alpha)*sqrt(offset)
  return(histvar)
}

## Pblm 4:  Calculate the parametric expected shortfall (ES)
# of a portfolio.  Input parameters again the same as in 
# pblms 2 & 3.  Also, this only be 2-3 lines of code.
paramExpShtf <- function(rtns, wts, alpha, offset = 252, ptflVal = 1)
{
  portfolio.sig <- volat(rtns, wts)
  paraes <- rtns%*%wts + portfolio.sig*dnorm(qnorm(alpha))/alpha
  return(paraes)
}

## Pblm 5:  Calculate the historical expected shortfall (ES)
# of a portfolio.  Input parameters again the same as in 
# pblms 2 & 3.  This should only be 4-5 lines of code.
histExpShtf <- function(rtns, wts, alpha, offset = 252, ptflVal = 1)
{
  portfolio.rtn <- rtns%*%wts
  subset <- quantile(portfolio.rtn, alpha)
  histes <- mean(portfolio.rtn[portfolio.rtn <= subset])*sqrt(offset)*ptflVal
  return(histes)
}

# Pblm 6:  rf = risk-free rate.  In this problem, using the 
# assumptions of the CAPM, compute the beta coefficients 
# of each set of asset returns on a benchmark. You may assume 
# the mkt benchmark is in the 1st column of the rtns object.
# You should be able to do this in about 5-6 lines of code.
calcBetas <- function(rtns, rf){
  val = ncol(rtns)-1
  for (i in seq(1:val))
    bet[i] = cov(rtns[,i+1], rtns[,1])/var(rtns[,1])
  return(bet)
}  

## Pblm 7: This problem will involve the most coding, 
# but still, it probably shouldn't require more than
# 15-20 lines.  What you need to do is calculate the portfolio
# weights that yield the minimum variance for the portfolio,
# using basic Markowitz theory.  The arguments rtns and wts 
# are the same as above.  minRtn is the target return for
# the portfolio.  wtsMin is a vector containing the minimum
# weights for each asset in the portfolio.  Likewise, wtsMax
# is a vector of the maximum allocation for each asset.  These
# should only be included in the model if constraints are 
# provided for each asset weight (including zeroes).
optWeights <- function(rtns, minRtn, wtsMin, wtsMax)
{
  meanvec <- colMeans(coredata(rtns))
  covmtx <- cov(coredata(rtns))
  D <- 2*covmtx
  dvec <- rep(0, length(meanvec))
  amat <- cbind(rep(1, length(meanvec)), meanvec, wtsMin, wtsMax)
  bvec <- c(1, minRtn, rep(0, length(meanvec)))
  solQp <- solve.QP(D, dvec, amat, bvec=bvec, meq = 2)
  return(solQp$solution) 
}


