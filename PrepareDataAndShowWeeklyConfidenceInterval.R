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

######

xlim.offset <- 160
date.value <- weekly.original.data[-1, 1]

plot(as.numeric(weekly.compound.return), 
     type = "l",
     axes = FALSE,
     col = "blue",
     main = "周复合收益率的2倍标准差区间",
     xlab = "",
     ylab = "收益率(%)",
     xlim = c(length(weekly.compound.return) - xlim.offset, length(weekly.compound.return)),
     ylim = c(-10, 10))

lines(as.numeric(fitted(myfilter.aparch.std.52.ver1) + 2*sigma(myfilter.aparch.std.52.ver1)),
      type = "l", 
      lty = "dashed",
      lwd = 2,
      col = "darkorange")

lines(as.numeric(fitted(myfilter.aparch.std.52.ver1) - 2*sigma(myfilter.aparch.std.52.ver1)),
      type = "l",
      lty = "dashed",
      lwd = 2,      
      col = "darkorange")

axis(1,
     at = seq(length(weekly.compound.return) - xlim.offset,
              length(weekly.compound.return), 10),
     labels = FALSE)

date.label <- date.value[seq(length(weekly.compound.return) - xlim.offset,
                           length(weekly.compound.return), 10)]

text(seq(length(weekly.compound.return) - xlim.offset,
         length(weekly.compound.return), 10),
     par("usr")[3] - 1,
     labels = substr(date.label, 1, 10),
     srt = 45,
     pos = 1,
     xpd = TRUE)

axis(2, at = seq(-20 ,20, 5), labels = TRUE, las = 1)

abline(h = seq(-10,10, 5),
       col = "springgreen4",
       lty = "dashed",
       lwd = par("lwd"))

abline(v = seq(length(weekly.compound.return) - xlim.offset,
               length(weekly.compound.return), 10),
       col = "springgreen4",
       lty = "dashed",
       lwd = par("lwd"))


box()

dev.copy(png, file = "d://MyR//stock//PrepareDataAndShowWeeklyConfidenceInterval.png", units= "px", width=1000, height=800)

dev.off()
