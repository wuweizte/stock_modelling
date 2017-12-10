CompareKDJDEstimatedKDJKXregAccuracy <- function(arg.object, 
                                                 arg.forecast.period,
                                                 arg.training.set.endpoint, 
                                                 arg.comparison.period,
                                                 arg.maxorder,
                                                 arg.reg.variable,
                                                 arg.zero.d.flag){

        training.set.object.1 <- window(arg.object, start = 1, 
                                        end = arg.training.set.endpoint)
        
        test.set.object.1 <- window(arg.object, start = arg.training.set.endpoint + 1,
                                    end = arg.training.set.endpoint + arg.forecast.period)
        

        ## whether zero difference should be set mandatorily        
        if(arg.zero.d.flag == TRUE){

                fit.xreg.object.1 <- auto.arima(arg.reg.variable[1:arg.training.set.endpoint],
                                                max.order = arg.maxorder,
                                                stepwise = FALSE,
                                                approximation = FALSE,
                                                d = 0)
        }else{

                fit.xreg.object.1 <- auto.arima(arg.reg.variable[1:arg.training.set.endpoint],
                                                max.order = arg.maxorder,
                                                stepwise = FALSE,
                                                approximation = FALSE)
        }

        fc.xreg.object.1 <- forecast(fit.xreg.object.1, h = length(test.set.object.1))
        
        fc.object <- numeric(0)
        fc.object[1] <- tail(training.set.object.1, 1) * 2/3 + fc.xreg.object.1$mean[1] * 1/3
        
        for(i in 2:arg.forecast.period){
                fc.object[i] <- fc.object[i - 1] * 2/3 + fc.xreg.object.1$mean[i] * 1/3
        }
        
        result <- t(array(c(arg.training.set.endpoint,
                            sqrt(mean((fc.object - test.set.object.1)^2))),
                          dim = c(2,1)))
        
        for(i in 1:arg.comparison.period){
                end.point2 <- arg.training.set.endpoint + i
                training.set.object.2 <- window(arg.object, start = 1, end = end.point2)
                test.set.object.2 <- window(arg.object, start = end.point2 + 1,
                                            end = end.point2 + arg.forecast.period)
                
                if(arg.zero.d.flag == TRUE){
                        
                        fit.xreg.object.2 <- auto.arima(arg.reg.variable[1:end.point2],
                                                        max.order = arg.maxorder,
                                                        stepwise = FALSE,
                                                        approximation = FALSE,
                                                        d = 0)                                        
                }else{
                        
                        fit.xreg.object.2 <- auto.arima(arg.reg.variable[1:end.point2],
                                                        max.order = arg.maxorder,
                                                        stepwise = FALSE,
                                                        approximation = FALSE)                                        
                }

                
                fc.xreg.object.2 <- forecast(fit.xreg.object.2, h = length(test.set.object.2))
                
                fc.object.2 <- numeric(0)
                fc.object.2[1] <- tail(training.set.object.2, 1) * 2/3 + fc.xreg.object.2$mean[1] * 1/3
                
                for(j in 2:arg.forecast.period){
                        fc.object.2[j] <- fc.object.2[j - 1] * 2/3 + fc.xreg.object.2$mean[j] * 1/3
                }
                
                
                result2 <- t(array(c(arg.training.set.endpoint + i,
                                     sqrt(mean((fc.object.2 - test.set.object.2)^2))),
                                   dim = c(2,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq",  "RMSE")

        return(df.result)
        

}
        