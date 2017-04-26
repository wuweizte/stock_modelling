DrawWeeklyPriceForecastPlot <- function(arg.close.price, 
                                        arg.kdj.k, 
                                        arg.kdj.d, 
                                        arg.forecast.period, 
                                        arg.ylabel.offset,
                                        arg.xlim.offset,
                                        arg.regression.offset,
                                        arg.date){
        
        close.price.data <- window(arg.close.price, 
                                   start = length(arg.close.price) - arg.regression.offset)

        fit.price <- auto.arima(close.price.data, 
                                stepwise = FALSE, 
                                approximation = FALSE,
                                max.order = 5)
        
        pvalue.price <- Box.test(residuals(fit.price), lag=10, 
                             fitdf=sum(fit.price$arma[c(1,2)]), 
                             type = "Lj")$p.value
        
        
        if(pvalue.price < 0.05){
                fit.price <- auto.arima(close.price.data, 
                                        stepwise = FALSE, 
                                        approximation = FALSE,
                                        max.order = 9)
        }
        
        fc.arima <- forecast(fit.price, h = arg.forecast.period)

        y.upper.limit <- signif(max(c(fc.arima$upper,
                                      tail(fc.arima$x, arg.xlim.offset))), digits = 2) + 100
        
        y.lower.limit <- signif(min(c(fc.arima$lower,
                                      tail(fc.arima$x, arg.xlim.offset))), digits = 2) - 100
        
        plot(fc.arima, 
             xlim = c(length(arg.close.price) - arg.xlim.offset,
                      length(arg.close.price) + arg.forecast.period),
             ylim = c(y.lower.limit, y.upper.limit),
             type = "l", 
             PI = TRUE, 
             shaded = TRUE, 
             pi.col = "purple",
             fcol = "darkgrey",
             axes = FALSE, 
             main = "Weekly Gold Price",
             xlab = "")
        
        lines(fc.arima$mean , type = "o", col = "red")
        
        axis(1,
             at = seq(length(arg.close.price) - arg.xlim.offset,
                      length(arg.close.price) + arg.forecast.period, 5),
             labels = FALSE)
        
        date.label <- 
                arg.date[seq(length(arg.close.price) - arg.xlim.offset,
                             length(arg.close.price) + arg.forecast.period, 5)]
        
        text(seq(length(arg.close.price) - arg.xlim.offset,
                 length(arg.close.price) + arg.forecast.period, 5),
             par("usr")[3] - 50,
             labels = substr(date.label, 1, 10),
             srt = 45,
             pos = 1,
             xpd = TRUE)
        
        
        axis(2, at = round(seq(y.lower.limit, y.upper.limit, length.out = 11),digits = 0),
             labels = TRUE, las = 1)

                
        
        abline(v = seq(length(arg.close.price) - arg.xlim.offset,
                       length(arg.close.price) + arg.forecast.period, 5),
               col = "springgreen4",
               lty = "dashed",
               lwd = par("lwd"))
        
        abline(h = seq(y.lower.limit, y.upper.limit, length.out = 11),
               col = "springgreen4",
               lty = "dashed",
               lwd = par("lwd"))
    
        price.arima.name <- paste0("Arima(",
                                   fit.price$arma[1],",",
                                   fit.price$arma[6],",",
                                   fit.price$arma[2],
                                   ")")
        
        legend("bottomleft", 
               col = c("black","red"),
               lty = 1, 
               lwd = 2,
               legend = c("Actual Weekly Gold Price",
                          paste0("Forecast Weekly Gold Price--", price.arima.name)),
               bty = "n")            
        box()
        
}

