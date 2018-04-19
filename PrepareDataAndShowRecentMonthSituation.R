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


recent.month.count <- 5

recent.monthly.return <- round(tail(monthly.compound.return, recent.month.count),digits = 2)

## fit arima model using rolling data
recent.mean.sigma <- ReturnArimaRollingForecast(arg.object = monthly.compound.return, 
                                                arg.rolling.period = recent.month.count)


month.flag.return.larger.than.sigma <- ((recent.monthly.return > recent.mean.sigma$mean + recent.mean.sigma$sigma) |
                                                (recent.monthly.return < recent.mean.sigma$mean - recent.mean.sigma$sigma) )


d <- data.frame(recent.monthly.return = paste0(recent.monthly.return,"%"),
                mean.forecast = paste0(recent.mean.sigma$mean,"%"),
                sigma.forecast = paste0(recent.mean.sigma$sigma,"%"),
                absolute.return.larger.than.1.sigma = month.flag.return.larger.than.sigma)


start.year <- trunc(tsp(recent.monthly.return)[1])
start.month <- round((tsp(recent.monthly.return)[1] - start.year) / 0.0833333) + 1

row.names(d) <- substr(seq(as.Date(paste0(start.year,"-",start.month,"-1")),by = "months", length=recent.month.count),1,7)

colnames(d)[1] <- "实际月收益率"
colnames(d)[2] <- "月收益率的mean预测"
colnames(d)[3] <- "月收益率的sd预测"
colnames(d)[4] <- "实际月收益率是否超出(mean + sd)预测区间"

g <- tableGrob(d)
grid.newpage()
grid.draw(g)