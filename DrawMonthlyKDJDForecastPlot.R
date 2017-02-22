DrawMonthlyKDJDForecastPlot <- function(arg.close.price, 
                                        arg.kdj.k, 
                                        arg.kdj.d, 
                                        arg.forecast.period, 
                                        arg.ylabel.offset,
                                        arg.xlim.offset,
                                        arg.date){
        
        
        
        fit.arima.kdj.d <- auto.arima(arg.kdj.d,
                                      max.order = 5,
                                      stepwise = FALSE,
                                      approximation = FALSE)
        
        pvalue.d <- Box.test(residuals(fit.arima.kdj.d), lag=10, 
                           fitdf=sum(fit.arima.kdj.d$arma[c(1,2)]),
                           type = "Lj")$p.value
        
        
        if(pvalue.d < 0.05){
                fit.arima.kdj.d <- auto.arima(arg.kdj.d,
                                              max.order = 9,
                                              stepwise = FALSE,
                                              approximation = FALSE)
        }
        
        estimated.kdj.d <- forecast(fit.arima.kdj.d,  h = arg.forecast.period)
        
        fit.arima.kdj.k <- auto.arima(arg.kdj.k,
                                      max.order = 5,
                                      d = 0,
                                      stepwise = FALSE,
                                      approximation = FALSE)
        
        pvalue.k <- Box.test(residuals(fit.arima.kdj.k), lag=10, 
                             fitdf=sum(fit.arima.kdj.k$arma[c(1,2)]), 
                             type = "Lj")$p.value
        
        if(pvalue.k < 0.05){
                fit.arima.kdj.k <- auto.arima(arg.kdj.k,
                                              max.order = 9,
                                              d = 0,
                                              stepwise = FALSE,
                                              approximation = FALSE)
        }
        
        estimated.kdj.k <- forecast(fit.arima.kdj.k,  h = arg.forecast.period)
        
        plot(estimated.kdj.d,
             plot.conf = FALSE, 
             axes = FALSE,
             type = "l", 
             col = "blue",
             fcol = "deepskyblue",
             lwd = 2,
             main = "Monthly KDJ Figure",
             ylim = c(5,95),
             xlim = c(length(arg.kdj.d) - arg.xlim.offset,
                      length(arg.kdj.d) + arg.forecast.period))
        
        lines(estimated.kdj.d$mean, type = "o", col = "deepskyblue")
        
        lines(estimated.kdj.k$mean, type = "l", col = "red",lwd = 2)
        lines(window(arg.kdj.k, start = length(arg.kdj.d) - arg.xlim.offset),
              type = "l", col = "darkorange")

        kdj.d.arima.name <- paste0("Arima(",
                                   fit.arima.kdj.d$arma[1],",",
                                   fit.arima.kdj.d$arma[6],",",
                                   fit.arima.kdj.d$arma[2],
                                   ")")
        
        kdj.k.arima.name <- paste0("Arima(",
                                   fit.arima.kdj.k$arma[1], ",",
                                   fit.arima.kdj.k$arma[6], ",",
                                   fit.arima.kdj.k$arma[2],
                                   ")")

        axis(1,
             at = seq(length(arg.kdj.d) - arg.xlim.offset,
                      length(arg.kdj.d) + arg.forecast.period, 5),
             labels = FALSE)
        
        text(seq(length(arg.kdj.d) - arg.xlim.offset,
                 length(arg.kdj.d) + arg.forecast.period, 5),
             par("usr")[3] - 5,
             labels = arg.date[seq(length(arg.kdj.d) - arg.xlim.offset,
                                        length(arg.kdj.d) + arg.forecast.period, 5)],
             srt = 45,
             pos = 1,
             xpd = TRUE)
        
        axis(2, at = seq(5 ,95, 10), labels = TRUE, las = 1)
        
        abline(v = seq(length(arg.kdj.d) - arg.xlim.offset,
                       length(arg.kdj.d) + arg.forecast.period, 5),
               col = "springgreen4",
               lty = "dashed",
               lwd = par("lwd"))
        
        abline(h = seq(5,95, 10),
               col = "springgreen4",
               lty = "dashed",
               lwd = par("lwd"))

        legend("bottomleft", 
               col = c("blue","deepskyblue","darkorange","red"),
               lty = 1, 
               lwd = 2,
               legend = c("Actual Monthly KDJ.D",
                          paste0("Forecast Monthly KDJ.D--",kdj.d.arima.name),
                          "Actual Monthly KDJ.K",
                          paste0("Forecast Monthly KDJ.K--",kdj.k.arima.name)),
               bty = "n")        
        
        box()
        
        
}

