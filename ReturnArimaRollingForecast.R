ReturnArimaRollingForecast <- function(arg.object, arg.rolling.period){
        
        training.set.endpoint <- length(arg.object) - arg.rolling.period
        
        time.attribute <- tsp(arg.object)
        
        training.set.object.1 <- 
                window(arg.object,  
                       end = time.attribute[1] + (training.set.endpoint - 1) / time.attribute[3])
        
        # browser()
        fit.arima.object.1 <- auto.arima(training.set.object.1, seasonal = FALSE)                

        fc.arima.object.1 <- forecast(fit.arima.object.1, h = 1)        


        result <- t(array(c(training.set.endpoint + 1,
                            round(fc.arima.object.1$mean,digits = 2),
                            round(sqrt(fit.arima.object.1$sigma2),digits = 2)),
                          dim = c(3,1)))
        # browser()
        
        for(i in 1:(arg.rolling.period - 1)){
                
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (training.set.endpoint - 1 + i) / time.attribute[3])
                
                fit.arima.object.2 <- auto.arima(training.set.object.2, seasonal = FALSE)                          
                
                fc.arima.object.2 <- forecast(fit.arima.object.2, h = 1)        

                result2 <- t(array(c(training.set.endpoint + 1 + i,
                                     round(fc.arima.object.2$mean,digits = 2),
                                     round(sqrt(fit.arima.object.2$sigma2),digits = 2)),
                                   dim = c(3,1)))
                
                result <- rbind(result, result2)
                # browser()
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "mean","sigma")
        
        return(df.result)        
        
}
