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

myfilter.aparch.std.52.ver1 <- ugarchfilter(myspec.aparch.std.52.ver1, weekly.compound.return, n.old = 469)

recent.week.count <- 5

recent.weekly.return <- round(tail(weekly.compound.return, recent.week.count),digits = 2)

## Since parameters in garch model are fixed, it is no need to fit garch model again
mean.forecast <- as.numeric(round(tail(fitted(myfilter.aparch.std.52.ver1),
                                       recent.week.count),
                                  digits = 2))

sigma.forecast <- as.numeric(round(tail(myfilter.aparch.std.52.ver1@filter$sigma,
                                        recent.week.count),
                                   digits = 2))


week.flag.return.larger.than.sigma <- ((recent.weekly.return > mean.forecast + sigma.forecast) |
                                               (recent.weekly.return < mean.forecast - sigma.forecast) )


d <- data.frame(recent.weekly.return = paste0(recent.weekly.return,"%"),
                mean.forecast = paste0(mean.forecast,"%"),
                sigma.forecast = paste0(sigma.forecast,"%"),
                absolute.return.larger.than.1.sigma = week.flag.return.larger.than.sigma)

row.names(d) <- names(recent.weekly.return)

colnames(d)[1] <- "实际周收益率"
colnames(d)[2] <- "周收益率的mean预测"
colnames(d)[3] <- "周收益率的sd预测"
colnames(d)[4] <- "实际周收益率是否超出(mean + sd)预测区间"


g <- tableGrob(d)
grid.newpage()
grid.draw(g)