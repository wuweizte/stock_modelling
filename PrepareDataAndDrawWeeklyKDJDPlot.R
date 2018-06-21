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

weekly.original.data <- read.csv("d://MyR//stock//goldweekly.csv")

weekly.close.price <- as.ts(weekly.original.data[,5])
weekly.kdj.k <- as.ts(weekly.original.data[,7])
weekly.kdj.d <- as.ts(weekly.original.data[,8])

names(weekly.close.price) <- weekly.original.data$Time
names(weekly.kdj.k) <- weekly.original.data$Time
names(weekly.kdj.d) <- weekly.original.data$Time

weekly.compound.return <- diff(log(weekly.close.price)) * 100

names(weekly.compound.return) <- weekly.original.data$Time[-1]

DrawWeeklyKDJDForecastPlot(weekly.close.price,
                           weekly.kdj.k,
                           weekly.kdj.d,
                           arg.forecast.period = 5,
                           arg.ylabel.offset = 80,
                           arg.xlim.offset = 80,
                           arg.regression.offset = 400,
                           arg.date = weekly.original.data[, 1])

dev.copy(png, file = "d://MyR//stock//PrepareDataAndDrawWeeklyKDJDPlot.png", units= "px", width=1000, height=800)

dev.off()
