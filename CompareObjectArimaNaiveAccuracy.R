CompareObjectArimaNaiveAccuracy <- function(arg.object, 
                                  arg.forecast.period,
                                  arg.training.set.endpoint, 
                                  arg.comparison.period,
                                  arg.maxorder){

        # Use the average of forecast results of Arima model and naive model
        # 
        # 
        # Args:
        #   arg.object: time series used for modelling
        # 
        #   arg.forecast.period: how long the forecast will be made every time
        # 
        #   arg.training.set.endpoint: the end specification of the training set in the time series
        # 
        #   arg.comparison.period:    how many times the comparison will be made
        # 
        #   arg.maxorder: parameter sent to auto.arima function
        # 
        # Returns:
        #   data frame containing the forecast performance: ("seq", "p","d","q", "dr", "RMSE", "p.v")
        
                
        # browser()
        training.set.object.1 <- window(arg.object, start = 1, 
                                        end = arg.training.set.endpoint)
        
        test.set.object.1 <- window(arg.object, start = arg.training.set.endpoint + 1,
                                    end = arg.training.set.endpoint + arg.forecast.period)
        
        
        # browser()
        fit.arima.object.1 <- auto.arima(training.set.object.1,
                                         max.order = arg.maxorder,
                                         stepwise = FALSE,
                                         approximation = FALSE)


        fc.arima.object.1 <- forecast(fit.arima.object.1, h = length(test.set.object.1))

        fit.naive.object.1 <- naive(training.set.object.1,h = length(test.set.object.1))
        
        predicted.value.1 <- ((fc.arima.object.1$mean + fit.naive.object.1$mean) / 2) - test.set.object.1
                

        rmse.1 <- sqrt(mean((predicted.value.1)^2))
        
        pvalue <- Box.test(residuals(fit.arima.object.1), lag=10,
                           fitdf=sum(fit.arima.object.1$arma[c(1,2)]))$p.value

        result <- t(array(c(arg.training.set.endpoint,
                            fit.arima.object.1$arma[c(1,6,2)],
                            ("drift" %in% names(fit.arima.object.1$coef)) * 1,
                            # round(accuracy(fc.arima.object.1, test.set.object.1)[2,2], digits = 1),
                            round(rmse.1,digits = 1),
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
                                                 approximation = FALSE)

                fc.arima.object.2 <- forecast(fit.arima.object.2, h = length(test.set.object.2))
                

                fit.naive.object.2 <- naive(training.set.object.2,h = length(test.set.object.2))
                

                predicted.value.2 <- ((fc.arima.object.2$mean + fit.naive.object.2$mean) / 2) - test.set.object.2

                rmse.2 <- sqrt(mean((predicted.value.2)^2))
                
                pvalue <- Box.test(residuals(fit.arima.object.2), lag=10,
                                   fitdf=sum(fit.arima.object.2$arma[c(1,2)]))$p.value
                

                result2 <- t(array(c(arg.training.set.endpoint + i,
                                     fit.arima.object.2$arma[c(1,6,2)],
                                     ("drift" %in% names(fit.arima.object.2$coef)) * 1,
                                     # round(accuracy(fc.arima.object.2, test.set.object.2)[2,2], digits = 1),
                                     round(rmse.2,digits = 1),
                                     round(pvalue, digits = 2)),
                                   dim = c(7,1)))
                

                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "p","d","q", "dr", "RMSE", "p.v")
        
        return(df.result)        
        
}


