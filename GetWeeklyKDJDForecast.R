GetWeeklyKDJDForecast <- function(      arg.kdj.k, 
                                        arg.kdj.d, 
                                        arg.forecast.period){
        
 

        kdj.d.data <- arg.kdj.d
        
        fit.arima.kdj.d <- auto.arima(kdj.d.data,
                                      max.order = 5,
                                      d = 0,
                                      stepwise = FALSE,
                                      approximation = FALSE)
        
        pvalue.d <- Box.test(residuals(fit.arima.kdj.d), lag=10, 
                           fitdf=sum(fit.arima.kdj.d$arma[c(1,2)]),
                           type = "Lj")$p.value
        
        
        if(pvalue.d < 0.05){
                fit.arima.kdj.d <- auto.arima(kdj.d.data,
                                              max.order = 9,
                                              d = 0,
                                              stepwise = FALSE,
                                              approximation = FALSE)
        }
        
        estimated.kdj.d <- forecast(fit.arima.kdj.d,  h = arg.forecast.period)
        return(estimated.kdj.d)

}

