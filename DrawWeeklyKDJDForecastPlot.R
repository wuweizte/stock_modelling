DrawWeeklyKDJDForecastPlot <- function(arg.close.price, 
                                        arg.kdj.k, 
                                        arg.kdj.d, 
                                        arg.forecast.period, 
                                        arg.ylabel.offset,
                                        arg.xlim.offset,
                                        arg.regression.offset,
                                       arg.date){
        
        
        par(mfrow = c(2,1))

        if(arg.regression.offset > length(arg.kdj.d) - 1){
                print(paste0("arg.regression.offset should be less than length(arg.kdj.d), which is ",
                             length(arg.kdj.d)))
                return()
        }
                
        kdj.d.data <- window(arg.kdj.d, start = length(arg.kdj.d) - arg.regression.offset)
        
        fit.arima.kdj.d.stepwise.maxorder <- auto.arima(kdj.d.data,
                                                        max.order = 9,
                                                        stepwise = FALSE,
                                                        approximation = FALSE)
        
        estimated.kdj.d.stepwise.maxorder <- forecast(fit.arima.kdj.d.stepwise.maxorder, 
                                                      h = arg.forecast.period)
        
        plot(estimated.kdj.d.stepwise.maxorder,
             plot.conf = FALSE, 
             axes = FALSE,
             type = "l", 
             fcol = "red",
             ylim = c(5,95),
             xlim = c(length(arg.kdj.d) - arg.xlim.offset,
                      length(arg.kdj.d) + arg.forecast.period),
             ylab = "Weekly KDJ.D")
        
        lines(estimated.kdj.d.stepwise.maxorder$mean, type = "o", col = "red")
        
        lines(window(arg.kdj.k, start = length(arg.kdj.k) - arg.xlim.offset),
              type = "l", col = "orange")
        
        legend("bottomleft", 
               col = c("black","red","orange"),
               lty = 1, 
               lwd = 2,
               legend = c("Actual Weekly KDJ.D",
                          "Forecast Weekly KDJ.D",
                          "Actual Weekly KDJ.K"),
               bty = "n")
        
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
        
        box()
        
        close.price.data <- window(arg.close.price, start = length(arg.close.price) - arg.regression.offset)
        
        #######***********
        fit.xreg.object.1 <- auto.arima(kdj.d.data,
                                        max.order = 9,
                                        stepwise = FALSE,
                                        approximation = FALSE)                
        
        fc.xreg.object.1 <- forecast(fit.xreg.object.1, h = arg.forecast.period)
        
        xreg.object.est.1 <- c(kdj.d.data, fc.xreg.object.1$mean) 
                               
        xreg.lagged.df.1 <- cbind(xreg.object.est.1,       
                                  ts(c(NA, xreg.object.est.1[-length(xreg.object.est.1)])),
                                  ts(c(NA, NA, head(xreg.object.est.1, length(xreg.object.est.1) - 2))),
                                  ts(c(NA, NA, NA, head(xreg.object.est.1, length(xreg.object.est.1) - 3))))
        
        xreg.object.1 <- xreg.lagged.df.1[1:length(kdj.d.data),]
        
        xreg.object.test.1 <- xreg.lagged.df.1[(length(kdj.d.data) + 1):
                                                       (length(kdj.d.data) + arg.forecast.period),]
        
        # fit.arima.object.1 <- auto.arima(training.set.object.1,
        #                                  max.order = arg.maxorder,
        #                                  stepwise = FALSE,
        #                                  approximation = FALSE, 
        #                                  xreg = xreg.object.1)                
        
        
        # fc.arima.object.1 <- forecast(fit.arima.object.1, h = length(test.set.object.1), 
        #                               xreg = xreg.object.test.1)        
        
        
        ######************
        
        fit.lagged.arima <- auto.arima(close.price.data, 
                                       stepwise = FALSE, 
                                       approximation = FALSE,
                                       max.order = 9,
                                       xreg = xreg.object.1)
        
        fc.lagged.arima <- forecast(fit.lagged.arima, h = arg.forecast.period,
                                    xreg = xreg.object.test.1)
        
        
        y.upper.limit <- signif(max(c(fc.lagged.arima$upper,
                                      tail(fc.lagged.arima$x, arg.xlim.offset))), digits = 2) + 100
        
        y.lower.limit <- signif(min(c(fc.lagged.arima$lower,
                                      tail(fc.lagged.arima$x, arg.xlim.offset))), digits = 2) - 100
        
        plot(fc.lagged.arima, 
             xlim = c(length(arg.close.price) - arg.xlim.offset,
                      length(arg.close.price) + arg.forecast.period),
             ylim = c(y.lower.limit, y.upper.limit),
             type = "l", 
             plot.conf = TRUE, 
             shaded = TRUE, 
             pi.col = "purple",
             fcol = "red",
             axes = FALSE, 
             xlab = "",
             ylab = "Weekly Gold Price")
        
        lines(fc.lagged.arima$mean, type = "o", col = "red") 
        
        axis(1,
             at = seq(length(arg.close.price) - arg.xlim.offset,
                      length(arg.close.price) + arg.forecast.period, 5),
             labels = FALSE)
        
        text(seq(length(arg.close.price) - arg.xlim.offset,
                 length(arg.close.price) + arg.forecast.period, 5),
             par("usr")[3] - 50,
             labels = arg.date[seq(length(arg.close.price) - arg.xlim.offset,
                                        length(arg.close.price) + arg.forecast.period, 5)],
             srt = 45,
             pos = 1,
             xpd = TRUE)
        
        
        axis(2, at = round(seq(y.lower.limit, y.upper.limit, length.out = 11),digits = 0),
             labels = TRUE, las = 1)
        
        box()
        
        abline(v = seq(length(arg.close.price) - arg.xlim.offset,
                       length(arg.close.price) + arg.forecast.period, 5),
               col = "springgreen4",
               lty = "dashed",
               lwd = par("lwd"))
        
        abline(h = seq(y.lower.limit, y.upper.limit, length.out = 11),
               col = "springgreen4",
               lty = "dashed",
               lwd = par("lwd"))
        
}

