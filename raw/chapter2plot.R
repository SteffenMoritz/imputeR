# plot a single time series 
require(latticeExtra)
require(ggplot2)
require(reshape2)
suppressPackageStartupMessages(
  require(googleVis)
)
require(quantmod)
require(PerformanceAnalytics)
require(xtsExtra)
require(rCharts)


# get S&P 500 data from FRED (St. Louis Fed)
sp500 <- na.omit( 
  getSymbols(
    "SP500",
    src = "FRED",
    from = "1949-12-31",
    auto.assign = FALSE
  )
)

# use monthly data
sp500.monthly <- sp500[endpoints(sp500, on ="months")]
sp500 <- sp500.monthly
sp500.monthly <- CalculateReturns(sp500, method = "log")
sp500.df <- data.frame(
  index(sp500.monthly),
  coredata(sp500.monthly),
  stringsAsFactors=FALSE
)
# name columns
colnames( sp500.df ) <- c( "date", "sp500" )
# go back in time to plot.default from the graphics library
graphics::plot.default(
  x = sp500.df$date,
  y = sp500.df$sp500,
  type = "l",
  xlab = "Date",
  ylab = "Closing Value",
  main = "S&P 500"
)


stats::plot.ts(
  ts(sp500.monthly,
     start = c(
       as.numeric(format(index(sp500.monthly)[1],"%Y")),
       as.numeric(format(index(sp500.monthly)[1],"%m"))
     ),
     frequency = 12
  ), # some backwards conversion to ts from xts
  xlab = "Date",
  ylab = "Closing Value",
  main = "S&P 500 (stats::plot.ts)"
)

xyplot(
  sp500 ~ date,
  data = sp500.df,
  type = "l",
  main = "S&P 500 (lattice::xyplot)"
)

asTheEconomist(
  xyplot(
    sp500.monthly,
    scales = list( y = list( rot = 0 ) ),
    main = "S&P 500 (lattice::xyplot.xts)"  
  )
)

charts.PerformanceSummary(
  ROC(sp500, n = 1, type = "discrete"),
  main = "S&P 500 (PerformanceAnalytice::charts.PerformanceSummary)"
)


xts::plot.xts(
  sp500.monthly[1:120],
  ylab = "Continuous Return",
  main = "S&P 500"  
)
abline(h = 0, col = "grey")



# plot2
z <- ts(matrix(rnorm(6000), 120, 50), start=c(1961, 1), frequency=12)
colnames(z) <- 1:50
# ggplot2 version. data transformation is critical
ggplot( melt(data.frame(time=as.numeric(time(z)), z), id.vars="time"), aes(time, value)) +
geom_line() +
geom_blank()


# plot 3
plot(z, plot.type = "single", col = 1:50, xlab = "Year", ylab = "value")


#plot4
z <- ts(matrix(rnorm(1200), 120, 10), start=c(1961, 1), frequency=12)
plot(z)


# plot5
horizonplot(z)
horizonplot(z, layout = c(1,50), colorkey = TRUE, origin = 0)
