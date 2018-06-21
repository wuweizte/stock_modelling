rm(list = ls())

suppressMessages(library(forecast))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(ggplot2))
suppressMessages(library(FinTS))
suppressMessages(library(rugarch))
suppressMessages(library(fNonlinear))
suppressMessages(library(vars))

source("d://MyR//stock//DrawMonthlyKDJDForecastPlot.R")
source("d://MyR//stock//DrawWeeklyKDJDForecastPlot.R")
source("d://MyR//stock//ReturnArimaRollingForecast.r")

monthly.original.data <- read.csv("d://MyR//stock//goldmonthly.csv")

monthly.close.price <- monthly.original.data[,5]

## The last month has not passed completely. So it is cancelled.
monthly.close.price <- monthly.close.price[-length(monthly.close.price)]

## start time: 1990.01.01
monthly.close.price <- ts(monthly.close.price, start = c(1990, 1), frequency = 12)
monthly.compound.return <- diff(log(monthly.close.price)) * 100

## when the last kdj is added manually and not real, it is removed
monthly.kdj.k <- monthly.original.data[,7]
monthly.kdj.d <- monthly.original.data[,8]
if(length(unique(tail(monthly.kdj.k,2))) == 1){
        monthly.kdj.k <- monthly.kdj.k[-length(monthly.kdj.k)]
        monthly.kdj.d <- monthly.kdj.d[-length(monthly.kdj.d)]
}

monthly.kdj.k <- ts(monthly.kdj.k, start = c(1990, 1), frequency = 12)
monthly.kdj.d <- ts(monthly.kdj.d, start = c(1990, 1), frequency = 12)

DrawMonthlyKDJDForecastPlot(monthly.close.price,
                            monthly.kdj.k,
                            monthly.kdj.d,
                            arg.forecast.period = 5,
                            arg.ylabel.offset = 80,
                            arg.xlim.offset = 80,
                            arg.date = monthly.original.data[, 1])

dev.copy(png, file = "d://MyR//stock//PrepareDataAndDrawMonthlyKDJDPlot.png", units= "px", width=1000, height=800)

dev.off()
