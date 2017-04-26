CompareObjectZeroDiffAccuracy <- function(arg.object, 
                                  arg.forecast.period,
                                  arg.training.set.endpoint, 
                                  arg.comparison.period,
                                  arg.maxorder){
        
        training.set.object.1 <- window(arg.object, start = 1, 
                                        end = arg.training.set.endpoint)
        
        test.set.object.1 <- window(arg.object, start = arg.training.set.endpoint + 1,
                                    end = arg.training.set.endpoint + arg.forecast.period)
        
        
        #browser()
        fit.arima.object.1 <- auto.arima(training.set.object.1,
                                         max.order = arg.maxorder,
                                         stepwise = FALSE,
                                         approximation = FALSE,
                                         d = 0)                
        
        
        fc.arima.object.1 <- forecast(fit.arima.object.1, h = length(test.set.object.1))        

        pvalue <- Box.test(residuals(fit.arima.object.1), lag=10, 
                           fitdf=sum(fit.arima.object.1$arma[c(1,2)]))$p.value
        
        result <- t(array(c(arg.training.set.endpoint,
                            fit.arima.object.1$arma[c(1,6,2)],
                            ("drift" %in% names(fit.arima.object.1$coef)) * 1,
                            round(accuracy(fc.arima.object.1, test.set.object.1)[2,2], digits = 2),
                            round(pvalue, digits = 2)),
                          dim = c(7,1)))
        
        
        for(i in 1:arg.comparison.period){
                
                end.point2 <- arg.training.set.endpoint + i
                training.set.object.2 <- window(arg.object, start = 1, end = end.point2)
                test.set.object.2 <- window(arg.object, start = end.point2 + 1,
                                            end = end.point2 + arg.forecast.period)
                
                fit.arima.object.2 <- auto.arima(training.set.object.2,
                                                 max.order = arg.maxorder,
                                                 stepwise = FALSE,
                                                 approximation = FALSE,
                                                 d = 0)                          
                
                fc.arima.object.2 <- forecast(fit.arima.object.2, h = length(test.set.object.2))        
                
                pvalue <- Box.test(residuals(fit.arima.object.2), lag=10, 
                                   fitdf=sum(fit.arima.object.2$arma[c(1,2)]))$p.value
                
                
                result2 <- t(array(c(arg.training.set.endpoint + i,
                                     fit.arima.object.2$arma[c(1,6,2)],
                                     ("drift" %in% names(fit.arima.object.2$coef)) * 1,
                                     round(accuracy(fc.arima.object.2, test.set.object.2)[2,2], digits = 2),
                                     round(pvalue, digits = 2)),
                                   dim = c(7,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "p","d","q", "dr", "RMSE", "p.v")
        
        return(df.result)        
        
}
