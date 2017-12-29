GetWeeklyKDJKForecast <- function(      arg.kdj.k, 
                                        arg.kdj.d, 
                                        arg.forecast.period){
        
 

        weekly.kdj.k <- arg.kdj.k
        weekly.kdj.d <- arg.kdj.d

        weekly.kdj <- cbind(weekly.kdj.k, weekly.kdj.d)

        var <- VAR(weekly.kdj, p=1, type="const", lag.max = 5, ic = "SC" )
        fcst <- predict(var, n.ahead = arg.forecast.period)
        estimated.kdj.k <- fcst$fcst[["weekly.kdj.k"]][,"fcst"]


        if(max(Mod(roots(var))) >= 1) {
                fit.arima.kdj.k <- auto.arima(weekly.kdj.k,
                                              max.order = 5,
                                              d = 0,
                                              stepwise = FALSE,
                                              approximation = FALSE)
                
                pvalue.k <- Box.test(residuals(fit.arima.kdj.k), lag=10, 
                                     fitdf=sum(fit.arima.kdj.k$arma[c(1,2)]), 
                                     type = "Lj")$p.value
                
                if(pvalue.k < 0.05){
                        fit.arima.kdj.k <- auto.arima(weekly.kdj.k,
                                                      max.order = 9,
                                                      d = 0,
                                                      stepwise = FALSE,
                                                      approximation = FALSE)
                }
                
                estimated.kdj.k <- forecast(fit.arima.kdj.k,  h = arg.forecast.period)$mean
                
                
        } 

        return(estimated.kdj.k)
        
}

