DrawMonthlyKDJDForecastPlot <- function(arg.close.price, 
                                        arg.kdj.k, 
                                        arg.kdj.d, 
                                        arg.forecast.period, 
                                        arg.ylabel.offset,
                                        arg.xlim.offset,
                                        arg.date){
        
        source("GetMonthlyKDJDForecast.R")
        source("GetMonthlyKDJKForecast.R")
        
        
        estimated.kdj.d <- GetMonthlyKDJDForecast(arg.kdj.k, 
                                                 arg.kdj.d, 
                                                 arg.forecast.period)
        
        
        estimated.kdj.k <- GetMonthlyKDJKForecast(arg.kdj.k , 
                                                 arg.kdj.d, 
                                                 arg.forecast.period)
        
        
        plot(estimated.kdj.d,
             PI = FALSE, 
             axes = FALSE,
             type = "l", 
             col = "blue",
             fcol = "deepskyblue",
             lwd = 2,
             main = "Monthly KDJ Figure",
             ylim = c(5,95),
             xlim = c(tsp(arg.kdj.d)[2] - arg.xlim.offset / tsp(arg.kdj.d)[3],
                      tsp(arg.kdj.d)[2] + arg.forecast.period / tsp(arg.kdj.d)[3])
             )
        
        lines(estimated.kdj.d$mean, type = "o", col = "deepskyblue")

        lines(arg.kdj.k,type = "l", col = "darkorange")

        kdj.k.x.axis <- tsp(arg.kdj.d)[2] + (1:arg.forecast.period)/ tsp(arg.kdj.d)[3]
        
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
        # 
        # kdj.k.arima.name <- paste0("Arima(",
        #                            fit.arima.kdj.k$arma[1], ",",
        #                            fit.arima.kdj.k$arma[6], ",",
        #                            fit.arima.kdj.k$arma[2],
        #                            ")")

        axis(1,
             at = seq(tsp(arg.kdj.d)[2] - arg.xlim.offset / tsp(arg.kdj.d)[3],
                      tsp(arg.kdj.d)[2] + arg.forecast.period / tsp(arg.kdj.d)[3], 
                      5 / tsp(arg.kdj.d)[3]),
             labels = FALSE)

        date.label <- 
                arg.date[seq(length(arg.kdj.d) - arg.xlim.offset,
                                   length(arg.kdj.d) + arg.forecast.period, 5)] 
                
        text(seq(tsp(arg.kdj.d)[2] - arg.xlim.offset / tsp(arg.kdj.d)[3],
                 tsp(arg.kdj.d)[2] + arg.forecast.period / tsp(arg.kdj.d)[3], 
                 5 / tsp(arg.kdj.d)[3]),
             par("usr")[3] - 5,
             labels = substr(date.label, 1, 10),
             srt = 45,
             pos = 1,
             xpd = TRUE)
        
        axis(2, at = seq(5 ,95, 10), labels = TRUE, las = 1)
        
        abline(v = seq(tsp(arg.kdj.d)[2] - arg.xlim.offset / tsp(arg.kdj.d)[3],
                       tsp(arg.kdj.d)[2] + arg.forecast.period / tsp(arg.kdj.d)[3], 
                       5 / tsp(arg.kdj.d)[3]),
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
               legend = c("Actual Monthly KDJ.D",
                          paste0("Forecast Monthly KDJ.D"),
                          "Actual Monthly KDJ.K",
                          paste0("Forecast Monthly KDJ.K")),
               bty = "n")        
        
        box()
        
        
}

