ResultZeroDiffARIMA <- function(arg.object, 
                                  arg.forecast.period,
                                  arg.training.set.endpoint, 
                                  arg.comparison.period,
                                  arg.maxorder){
        
        # training.set.object.1 <- window(arg.object, start = 1, 
        #                                 end = arg.training.set.endpoint)
        # 
        # test.set.object.1 <- window(arg.object, start = arg.training.set.endpoint + 1,
        #                             end = arg.training.set.endpoint + arg.forecast.period)
        # 
        
        time.attribute <- tsp(arg.object)
        
        training.set.object.1 <- 
                window(arg.object,  
                       end = time.attribute[1] + (arg.training.set.endpoint - 1) / time.attribute[3])
        
        test.set.object.1 <- window(arg.object, 
                                    start = time.attribute[1] + arg.training.set.endpoint / time.attribute[3],
                                    end = time.attribute[1] + (arg.training.set.endpoint + arg.forecast.period - 1)/ time.attribute[3])
        
        
        
        #browser()
        fit.arima.object.1 <- auto.arima(training.set.object.1,
                                         max.order = arg.maxorder,
                                         stepwise = FALSE,
                                         approximation = FALSE,
                                         d = 0)                
        
        
        fc.arima.object.1 <- forecast(fit.arima.object.1, h = length(test.set.object.1))        

        # browser()
        fc.result <- t(array(fc.arima.object.1$mean,
                          dim = c(length(test.set.object.1),1)))
        
        test.set.result <- t(array(test.set.object.1,
                             dim = c(length(test.set.object.1),1)))
        
        
        for(i in 1:arg.comparison.period){
                
                # end.point2 <- arg.training.set.endpoint + i
                # training.set.object.2 <- window(arg.object, start = 1, end = end.point2)
                # test.set.object.2 <- window(arg.object, start = end.point2 + 1,
                #                             end = end.point2 + arg.forecast.period)
                # 
                  
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])
                
                      
                fit.arima.object.2 <- auto.arima(training.set.object.2,
                                                 max.order = arg.maxorder,
                                                 stepwise = FALSE,
                                                 approximation = FALSE,
                                                 d = 0)                          
                
                fc.arima.object.2 <- forecast(fit.arima.object.2, h = length(test.set.object.2))        
                

                
                fc.result.2 <- t(array(fc.arima.object.2$mean,
                                     dim = c(length(test.set.object.2),1)))
                
                test.set.result.2 <- t(array(test.set.object.2,
                                           dim = c(length(test.set.object.2),1)))
                
                fc.result <- rbind(fc.result, fc.result.2)
                test.set.result <- rbind(test.set.result, test.set.result.2)
                
                
        }
        
        result <- list(fc.result = fc.result, test.set.result = test.set.result)
        return(result)        
        
}
