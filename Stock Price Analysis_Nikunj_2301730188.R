# Load package
if (!require(quantmod)) {
  install.packages("quantmod")
}
library(quantmod)

# Fetch stock data for Microsoft (MSFT) from 2023-01-01
getSymbols("MSFT", src = "yahoo", from = "2023-01-01", to = Sys.Date())

# View data
head(MSFT)

# Calculate 30-day and 100-day Simple Moving Averages
sma30 <- SMA(Cl(MSFT), n = 30)
sma100 <- SMA(Cl(MSFT), n = 100)

# Plot stock with moving averages
chartSeries(MSFT, theme = chartTheme("white"), TA = NULL)
addSMA(n = 30, col = "green")
addSMA(n = 100, col = "purple")

