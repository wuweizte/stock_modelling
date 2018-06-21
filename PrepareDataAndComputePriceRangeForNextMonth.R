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

fit.arima.monthly.return <- auto.arima(monthly.compound.return, seasonal = FALSE)


fc.arima.monthly.return <- forecast(fit.arima.monthly.return, h = 1, 
                                    level = c(80,95))

monthly.price.2.upper.level <- as.numeric(tail(monthly.close.price,1)) * exp((fc.arima.monthly.return$mean  + sqrt(fc.arima.monthly.return$model$sigma2) * 2) / 100)

### 1 sigma upper level
monthly.price.1.upper.level <- as.numeric(tail(monthly.close.price,1)) * exp((fc.arima.monthly.return$mean  + sqrt(fc.arima.monthly.return$model$sigma2)) / 100)

### close price last month
monthly.price.last.month <- as.numeric(tail(monthly.close.price,1))

### 1 sigma lower level
monthly.price.1.lower.level <- as.numeric(tail(monthly.close.price,1)) * exp((fc.arima.monthly.return$mean  - sqrt(fc.arima.monthly.return$model$sigma2)) / 100)

### 2 sigma lower level
monthly.price.2.lower.level <- as.numeric(tail(monthly.close.price,1)) * exp((fc.arima.monthly.return$mean  - sqrt(fc.arima.monthly.return$model$sigma2) * 2) / 100)


### the possibility that the price will descend next month

possibility.descend.next.month <- pnorm(0, mean = fc.arima.monthly.return$mean, 
                                        sd = sqrt(fc.arima.monthly.return$model$sigma2))

### the possibility that the price will ascend next month

possibility.ascend.next.month <- pnorm(0, mean = fc.arima.monthly.return$mean, 
                                       sd = sqrt(fc.arima.monthly.return$model$sigma2),
                                       lower.tail = FALSE)

d <- data.frame(monthly.price.2.upper.level = round(monthly.price.2.upper.level,digits = 2),
                monthly.price.1.upper.level = round(monthly.price.1.upper.level,digits = 2),
                monthly.price.last.month = round(monthly.price.last.month,digits = 2),
                monthly.price.1.lower.level = round(monthly.price.1.lower.level,digits = 2),
                monthly.price.2.lower.level = round(monthly.price.2.lower.level,digits = 2),
                monthly.return.mean.forecast = paste0(round(fc.arima.monthly.return$mean,digits = 2),"%"),
                monthly.return.sigma.forecast = 
                        paste0(round(sqrt(fc.arima.monthly.return$model$sigma2),digits = 1),"%"),
                possibility.descend.next.month = 
                        paste0(round(possibility.descend.next.month * 100, digits = 1),"%"),
                possibility.ascend.next.month = 
                        paste0(round(possibility.ascend.next.month * 100,digits = 1),"%"))

colnames(d)[1] <- "本月收益率的 (mean + 2*sd) 置信区间上限对应金价"
colnames(d)[2] <- "本月收益率的 (mean + sd) 置信区间上限对应金价"
colnames(d)[3] <- "上月收盘价"
colnames(d)[4] <- "本月收益率的 (mean - sd) 置信区间下限对应金价"
colnames(d)[5] <- "本月收益率的 (mean - 2*sd) 置信区间下限对应金价"
colnames(d)[6] <- "本月收益率mean预测"
colnames(d)[7] <- "本月收益率sd预测"
colnames(d)[8] <- "本月下跌概率"
colnames(d)[9] <- "本月上涨概率"

g <- tableGrob(t(d))
grid.newpage()
grid.draw(g)