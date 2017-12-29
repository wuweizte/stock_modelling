GetMonthlyKDJKForecast <- function(      arg.kdj.k, 
                                        arg.kdj.d, 
                                        arg.forecast.period){
        
 

        monthly.kdj.k <- arg.kdj.k
        monthly.kdj.d <- arg.kdj.d

        monthly.kdj <- cbind(monthly.kdj.k, monthly.kdj.d)

        var <- VAR(monthly.kdj, p=1, type="const", lag.max = 5, ic = "SC" )
        fcst <- predict(var, n.ahead = arg.forecast.period)
        estimated.kdj.k <- fcst$fcst[["monthly.kdj.k"]][,"fcst"]


        if(max(Mod(roots(var))) >= 1) {
                fit.arima.kdj.k <- auto.arima(monthly.kdj.k,
                                              max.order = 5,
                                              d = 0,
                                              stepwise = FALSE,
                                              approximation = FALSE)
                
                pvalue.k <- Box.test(residuals(fit.arima.kdj.k), lag=10, 
                                     fitdf=sum(fit.arima.kdj.k$arma[c(1,2)]), 
                                     type = "Lj")$p.value
                
                if(pvalue.k < 0.05){
                        fit.arima.kdj.k <- auto.arima(monthly.kdj.k,
                                                      max.order = 9,
                                                      d = 0,
                                                      stepwise = FALSE,
                                                      approximation = FALSE)
                }
                
                estimated.kdj.k <- forecast(fit.arima.kdj.k,  h = arg.forecast.period)$mean
                
                
        } 

        return(estimated.kdj.k)
        
}

