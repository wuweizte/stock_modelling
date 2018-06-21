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

fit.arima.object <- auto.arima(weekly.compound.return[1:469], seasonal = FALSE) 

myspec.aparch.std.52.ver1 <- ugarchspec(variance.model = list(model = "apARCH",garchOrder = c(5,2)),
                                        mean.model = list(armaOrder = c(fit.arima.object$arma[1],
                                                                        fit.arima.object$arma[2])),
                                        distribution.model = "std",
                                        fixed.pars=list(beta1 = 0))

myfit.aparch.std.52.ver1 <- ugarchfit(myspec.aparch.std.52.ver1, data = weekly.compound.return[1:469], solver = "hybrid")


setfixed(myspec.aparch.std.52.ver1) <- as.list(coef(myfit.aparch.std.52.ver1))

suppressWarnings(fc.aparch.std.52.ver1 <- ugarchforecast(myspec.aparch.std.52.ver1,data = weekly.compound.return, n.ahead = 1))

weekly.price.2.upper.level <- as.numeric(tail(weekly.close.price,1) * exp((fc.aparch.std.52.ver1@forecast$seriesFor  + 2 * fc.aparch.std.52.ver1@forecast$sigmaFor) / 100))

### 1 sigma upper level
weekly.price.1.upper.level <- as.numeric(tail(weekly.close.price,1) * exp((fc.aparch.std.52.ver1@forecast$seriesFor  + fc.aparch.std.52.ver1@forecast$sigmaFor) / 100))

### close price last week
weekly.price.last.week <- as.numeric(tail(weekly.close.price,1))

### 1 sigma lower level
weekly.price.1.lower.level <- as.numeric(tail(weekly.close.price,1) * exp((fc.aparch.std.52.ver1@forecast$seriesFor  - fc.aparch.std.52.ver1@forecast$sigmaFor) / 100))

### 2 sigma lower level
weekly.price.2.lower.level <- as.numeric(tail(weekly.close.price,1) * exp((fc.aparch.std.52.ver1@forecast$seriesFor  - 2 * fc.aparch.std.52.ver1@forecast$sigmaFor) / 100))


### the possibility that the price will descend next week

possibility.descend.next.week <- pnorm(0, mean = fc.aparch.std.52.ver1@forecast$seriesFor, 
                                       sd = fc.aparch.std.52.ver1@forecast$sigmaFor)

### the possibility that the price will ascend next week

possibility.ascend.next.week <- pnorm(0, mean = fc.aparch.std.52.ver1@forecast$seriesFor, 
                                      sd = fc.aparch.std.52.ver1@forecast$sigmaFor,
                                      lower.tail = FALSE)

d <- data.frame(weekly.price.2.upper.level = round(weekly.price.2.upper.level,digits = 2),
                weekly.price.1.upper.level = round(weekly.price.1.upper.level,digits = 2),
                weekly.price.last.week = round(weekly.price.last.week,digits = 2),
                weekly.price.1.lower.level = round(weekly.price.1.lower.level,digits = 2),
                weekly.price.2.lower.level = round(weekly.price.2.lower.level,digits = 2),
                weekly.return.mean.forecast = 
                        paste0(round(fc.aparch.std.52.ver1@forecast$seriesFor,digits = 2),"%"),
                weekly.return.sigma.forecast = 
                        paste0(round(fc.aparch.std.52.ver1@forecast$sigmaFor,digits = 2),"%"),
                possibility.descend.next.week = 
                        paste0(round(possibility.descend.next.week * 100, digits = 2),"%"),
                possibility.ascend.next.week = 
                        paste0(round(possibility.ascend.next.week * 100,digits = 2),"%"))

colnames(d)[1] <- "本周收益率的 (mean + 2*sd) 置信区间上限对应金价"
colnames(d)[2] <- "本周收益率的 (mean + sd) 置信区间上限对应金价"
colnames(d)[3] <- "上周收盘价"
colnames(d)[4] <- "本周收益率的 (mean - sd) 置信区间下限对应金价"
colnames(d)[5] <- "本周收益率的 (mean - 2*sd) 置信区间下限对应金价"
colnames(d)[6] <- "本周收益率mean预测"
colnames(d)[7] <- "本周收益率sd预测"
colnames(d)[8] <- "本周下跌概率"
colnames(d)[9] <- "本周上涨概率"

g <- tableGrob(t(d))
grid.newpage()
grid.draw(g)
