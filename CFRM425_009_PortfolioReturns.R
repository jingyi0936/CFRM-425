library(quantmod) 

syms <- c("AMZN", "SPY", "IBM")
getSymbols(Symbols = syms, from = "2010-12-31", 
           to = "2017-12-31")

## Some Preliminaries:
# 1. The apply(.) function.  Unlike sapply(.) and lapply(.),
# apply(.) is not limited to dataframe objects.  It can
# accept both xts and matrix objects, in addition to
# general n-dimensional arrays.  Need to include
# the MARGIN = {1, 2} argument: 1 => apply by row,
# and 2 => apply by column.

# Simple example on daily prices:
dailyClose <- merge(Ad(AMZN), Ad(SPY), Ad(IBM))
(meanPrices <- sapply(dailyClose, FUN = mean))

class(meanPrices)
is.vector(meanPrices)  # TRUE: *not* an xts object, but
                # carries the dates as names(res)
typeof(meanPrices)

# sapply(.) also works with zoo objects:
zooClose <- as.zoo(dailyClose)
(zooMeans <- sapply(zooClose, FUN = mean))


# 2. Components of an xts object: index and core data.
# The index of an xts object is the date/time column.
# The core data is the data in the columns to the left
# of the index.

head(index(dailyClose), 3)
class(index(dailyClose))   # Date
is.vector(class(index(dailyClose)))   # TRUE

tail(coredata(dailyClose), 3)
class(coredata(dailyClose))  # matrix

## End of Preliminaries ##

## Now, back to our show:
# Conversion to monthly data and then extract
# Adjusted Closing prices (as in previous section):
for(symbol in syms) {
  x <- get(symbol)  # Converts from string to object of same name
  x <- to.period(x, "months")
  colnames(x) <- gsub("x", symbol, colnames(x))  # Restore column name
  assign(symbol, x) # Assign monthly returns to sec code object name
}

# Quick check:
head(AMZN, 3)
tail(SPY, 3)
head(IBM, 3)

# Now, after adjusting to monthly, extract the
# adjusted closing prices only:
adjPrices <- merge(Ad(AMZN), Ad(SPY), Ad(IBM))

# Calculate log returns (result is an xts object also):
rtns <- diff(log(adjPrices), lag = 1)
rtns <- rtns[-1,]
head(rtns, 3)
tail(rtns, 3)
class(rtns)   # xts

# Simplify the column headings:
# Use colnames(.) and rownames(.) for a matrix
# and names(.) for a dataframe
for (j in 1:ncol(rtns))
{
  colnames(rtns)[j] <- gsub(".Adjusted", "", colnames(rtns)[j])
}

# Check:
colnames(rtns)  # "AMZN" "SPY"  "IBM" 

## Suppose our portfolio holds 20% Amazon,
# 60% SPY, and 20% IBM.
# To get the portfolio return for each month,
# take the dot product of the asset weights
# with each month's returns.  Assume no short 
# selling and no leverage such that the weights
# are non-negative and sum to 1; eg:
wts <- c(0.2, 0.6, 0.2)

# Then,
dotWts <- function(v)
{
  v %*% wts     # wts set outside function (captured by R)
}

# Again, use the apply function; MARGIN = 1 => by row.
# Not limited to dataframe; works for matrix here:
compRtn <- apply(rtns, MARGIN = 1, FUN = dotWts)
head(compRtn)
tail(compRtn)
class(compRtn)  # numeric, not xts
# And, in fact:
is.vector(compRtn)    # vector

compRtn.xts <- as.xts(compRtn)
index(compRtn.xts[1,]) == index(rtns[1,]) # not comparable - why?
# Reason is that the date formats are different:
class(index(compRtn.xts[1,]))   # "POSIXct" "POSIXt" (carries TZ)
class(index(rtns[1,]))          # "Date"

# Naively attempt to merge:
allRtns <- merge(rtns, compRtn.xts)
# Cannot join on dates because formats are different:
head(allRtns, 3)
tail(allRtns, 3)

# Try again, but change the date format:
compRtn.xts <- as.xts(compRtn, dateFormat = "Date")
class(index(compRtn.xts[1,]))   # "Date"
index(compRtn.xts[1,]) == index(rtns[1,])   # TRUE!

# We can now merge the two:
allRtns <- merge(rtns, compRtn.xts)

# We're good now:
head(allRtns, 3)
tail(allRtns, 3)

# Calculate Mean Monthly Portfolio Return
(meanRtns <- sapply(rtns, mean))
(meanPortRtn <- meanRtns %*% wts)
