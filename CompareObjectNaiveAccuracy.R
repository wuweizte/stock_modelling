CompareObjectNaiveAccuracy <- function(arg.object, 
                                  arg.forecast.period,
                                  arg.training.set.endpoint, 
                                  arg.comparison.period,
                                  arg.maxorder){
        
        # browser()
        time.attribute <- tsp(arg.object)
        
        training.set.object.1 <- 
                window(arg.object,  
                       end = time.attribute[1] + (arg.training.set.endpoint - 1) / time.attribute[3])
        
        test.set.object.1 <- window(arg.object, 
                                    start = time.attribute[1] + arg.training.set.endpoint / time.attribute[3],
                                    end = time.attribute[1] + (arg.training.set.endpoint + arg.forecast.period - 1)/ time.attribute[3])
        
        fit.naive.object.1 <- naive(training.set.object.1,h = length(test.set.object.1))
                                    
        
        # fc.arima.object.1 <- forecast(fit.arima.object.1, h = length(test.set.object.1))        

        pvalue <- Box.test(residuals(fit.naive.object.1), lag=10)$p.value
        
        result <- t(array(c(arg.training.set.endpoint,
                            0,1,0,
                            0,
                            round(accuracy(fit.naive.object.1, test.set.object.1)[2,2], digits = 1),
                            round(pvalue, digits = 2)),
                          dim = c(7,1)))
        
        
        for(i in 1:arg.comparison.period){

                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])

                fit.naive.object.2 <- naive(training.set.object.2,h = length(test.set.object.2))
                # fc.arima.object.2 <- forecast(fit.arima.object.2, h = length(test.set.object.2))

                pvalue <- Box.test(residuals(fit.naive.object.2), lag=10)$p.value


                result2 <- t(array(c(arg.training.set.endpoint + i,
                                     0,1,0,
                                     0,
                                     round(accuracy(fit.naive.object.2, test.set.object.2)[2,2], digits = 1),
                                     round(pvalue, digits = 2)),
                                   dim = c(7,1)))

                result <- rbind(result, result2)

        }

        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "p","d","q", "dr", "RMSE", "p.v")
        
        return(df.result)        
        
}
