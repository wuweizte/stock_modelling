DrawWeeklyKDJDForecastPlot <- function(arg.close.price, 
                                        arg.kdj.k, 
                                        arg.kdj.d, 
                                        arg.forecast.period, 
                                        arg.ylabel.offset,
                                        arg.xlim.offset,
                                       arg.regression.offset,
                                        arg.date){
        
 
        source("d://MyR//stock//GetWeeklyKDJDForecast.R")
        source("d://MyR//stock//GetWeeklyKDJKForecast.R")
        
        if(arg.regression.offset > length(arg.kdj.d) - 1){
                print(paste0("arg.regression.offset should be less than length(arg.kdj.d), which is ",
                             length(arg.kdj.d)))
                return()
        }
        
        kdj.d.data <- window(arg.kdj.d, start = length(arg.kdj.d) - arg.regression.offset)
        kdj.k.data <- window(arg.kdj.k, start = length(arg.kdj.k) - arg.regression.offset)

        
        estimated.kdj.d <- GetWeeklyKDJDForecast(arg.kdj.k = kdj.k.data , 
                                                 arg.kdj.d = kdj.d.data, 
                                                 arg.forecast.period)
        
        
        estimated.kdj.k <- GetWeeklyKDJKForecast(arg.kdj.k = kdj.k.data , 
                                                 arg.kdj.d = kdj.d.data, 
                                                 arg.forecast.period)
        
        # browser()
        plot(estimated.kdj.d,
             PI = FALSE, 
             axes = FALSE,
             type = "l", 
             col = "blue",
             fcol = "deepskyblue",
             lwd = 2,
             main = "Weekly KDJ Figure",
             ylim = c(5,95),
             xlim = c(length(arg.kdj.d) - arg.xlim.offset,
                      length(arg.kdj.d) + arg.forecast.period))
        
        lines(estimated.kdj.d$mean, type = "o", col = "deepskyblue")
        
        lines(kdj.k.data, type = "l", col = "darkorange")
        
        kdj.k.x.axis <- length(arg.kdj.k) + 1:arg.forecast.period
        
        lines(x = kdj.k.x.axis, 
              y = estimated.kdj.k, 
              type = "l", 
              col = "red",
              lwd = 2)
        

        # kdj.d.arima.name <- paste0("Arima(",
        #                            fit.arima.kdj.d$arma[1],",",
        #                            fit.arima.kdj.d$arma[6],",",
        #                            fit.arima.kdj.d$arma[2],
        #                            ")")
        
        # kdj.k.arima.name <- paste0("Arima(",
        #                            fit.arima.kdj.k$arma[1], ",",
        #                            fit.arima.kdj.k$arma[6], ",",
        #                            fit.arima.kdj.k$arma[2],
        #                            ")")

        axis(1,
             at = seq(length(arg.kdj.d) - arg.xlim.offset,
                      length(arg.kdj.d) + arg.forecast.period, 5),
             labels = FALSE)
        
        date.label <- arg.date[seq(length(arg.kdj.d) - arg.xlim.offset,
                                   length(arg.kdj.d) + arg.forecast.period, 5)]
        
        text(seq(length(arg.kdj.d) - arg.xlim.offset,
                 length(arg.kdj.d) + arg.forecast.period, 5),
             par("usr")[3] - 5,
             labels = substr(date.label, 1, 10),
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
               ncol = 2,
               legend = c("Actual Weekly KDJ.D",
                          paste0("Forecast of Weekly KDJ.D"),
                          "Actual Weekly KDJ.K",
                          paste0("Forecast of Weekly KDJ.K")),
               bty = "n")        
        
        box()
        
        
}

