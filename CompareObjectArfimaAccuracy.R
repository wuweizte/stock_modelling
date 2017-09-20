CompareObjectArfimaAccuracy <- function(arg.object, 
                                  arg.forecast.period,
                                  arg.training.set.endpoint, 
                                  arg.comparison.period,
                                  arg.dist){
        
        time.attribute <- tsp(arg.object)
        
        training.set.object.1 <- 
                window(arg.object,  
                       end = time.attribute[1] + (arg.training.set.endpoint - 1) / time.attribute[3])
        
        test.set.object.1 <- window(arg.object, 
                                    start = time.attribute[1] + arg.training.set.endpoint / time.attribute[3],
                                    end = time.attribute[1] + (arg.training.set.endpoint + arg.forecast.period - 1)/ time.attribute[3])
        
        

        fit.arima.object.1 = autoarfima(data = training.set.object.1, ar.max = 3, ma.max = 2, 
                         criterion = "AIC", method = "full", distribution.model = arg.dist)
        
        fc.arima.object.1 <- arfimaforecast(fit.arima.object.1$fit, n.ahead = length(test.set.object.1))
        
        pvalue <- Box.test(residuals(fit.arima.object.1$fit), lag=10,
                           fitdf=sum(fit.arima.object.1$rank.matrix[1, 1:4]))$p.value
        

        result <- t(array(c(arg.training.set.endpoint,
                            fit.arima.object.1$fit@model$modelinc[2:3],
                            round(sqrt(mean((fc.arima.object.1@forecast$seriesFor - test.set.object.1)^2)), digits = 1),
                            round(pvalue, digits = 2)),
                          dim = c(5,1)))
        
        for(i in 1:arg.comparison.period){
                
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])
                
                fit.arima.object.2 = autoarfima(data = training.set.object.2, ar.max = 3, ma.max = 2, 
                                                criterion = "AIC", method = "full", distribution.model = arg.dist)
                
                fc.arima.object.2 <- arfimaforecast(fit.arima.object.2$fit, n.ahead = length(test.set.object.2))
                
                pvalue <- Box.test(residuals(fit.arima.object.2$fit), lag=10,
                                   fitdf=sum(fit.arima.object.2$rank.matrix[1, 1:4]))$p.value
                
                result2 <- t(array(c(arg.training.set.endpoint + i,
                                    fit.arima.object.2$fit@model$modelinc[2:3],
                                    round(sqrt(mean((fc.arima.object.2@forecast$seriesFor - test.set.object.2)^2)), digits = 1),
                                    round(pvalue, digits = 2)),
                                  dim = c(5,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "p","q", "RMSE", "p.v")
        
        return(df.result)        
        
}
