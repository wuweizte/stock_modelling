CompareObjectGoldDollarVARAccuracy <- function(arg.object, 
                                  arg.forecast.period,
                                  arg.training.set.endpoint, 
                                  arg.comparison.period){
        
        time.attribute <- tsp(arg.object)
        
        training.set.object.1 <- 
                window(arg.object,  
                       end = time.attribute[1] + (arg.training.set.endpoint - 1) / time.attribute[3])
        
        test.set.object.1 <- window(arg.object, 
                                    start = time.attribute[1] + arg.training.set.endpoint / time.attribute[3],
                                    end = time.attribute[1] + (arg.training.set.endpoint + arg.forecast.period - 1)/ time.attribute[3])
        
        

        var.1 <- VAR(training.set.object.1, p=1, type="const")
        

        fcst.1 <- forecast(var.1,h = length(test.set.object.1[,"monthly.gold.compound.return"]))
        

        pvalue.1 <- serial.test(var.1, lags.pt=10, type="PT.asymptotic")$serial$p.value

        sigma.1 <- summary(var.1)$varresult$monthly.gold.compound.return$sigma
        
        # browser()
        result <- t(array(c(arg.training.set.endpoint,
                            round(sqrt(mean((as.numeric(fcst.1$forecast$monthly.gold.compound.return$mean) - 
                                                     as.numeric(test.set.object.1[,"monthly.gold.compound.return"]))^2)), digits = 1),
                            round(pvalue.1, digits = 2),
                            round(sigma.1, digits = 2)),
                          dim = c(4,1)))
        
        for(i in 1:arg.comparison.period){
                
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])
                

                
                var.2 <- VAR(training.set.object.2, p=1, type="const")
                

                fcst.2 <- forecast(var.2,h = length(test.set.object.2[,"monthly.gold.compound.return"]))
                

                pvalue.2 <- serial.test(var.2, lags.pt=10, type="PT.asymptotic")$serial$p.value
                

                sigma.2 <- summary(var.1)$varresult$monthly.gold.compound.return$sigma
                
                result2 <- t(array(c(arg.training.set.endpoint + i,
                                    round(sqrt(mean((as.numeric(fcst.2$forecast$monthly.gold.compound.return$mean) - 
                                                             as.numeric(test.set.object.2[,"monthly.gold.compound.return"]))^2)), 
                                          digits = 1),
                                    round(pvalue.2, digits = 2),
                                    round(sigma.2, digits = 2)),
                                  dim = c(4,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "RMSE", "p.v", "sigma")
        
        # browser()
        return(df.result)        
        
}
