CompareVARInDiffAccuracy <- function(arg.object, 
                               arg.forecast.period,
                               arg.training.set.endpoint, 
                               arg.comparison.period,
                               arg.comparison.colname){
        
        time.attribute <- tsp(arg.object)
        
        training.set.object.1 <- 
                window(arg.object,  
                       end = time.attribute[1] + (arg.training.set.endpoint - 1) / time.attribute[3])
        
        test.set.object.1 <- window(arg.object, 
                                    start = time.attribute[1] + arg.training.set.endpoint / time.attribute[3],
                                    end = time.attribute[1] + (arg.training.set.endpoint + arg.forecast.period - 1)/ time.attribute[3])
        
        

        var.1 <- VAR(diff(training.set.object.1), p=1, 
                     type="const", lag.max = 5, ic = "SC" )
        fcst.1 <- predict(var.1,
                          n.ahead = length(test.set.object.1[,arg.comparison.colname]))
        fcst.1 <- as.numeric(tail(training.set.object.1[,arg.comparison.colname],1)) +
                cumsum(fcst.1$fcst[[arg.comparison.colname]][,"fcst"])
        
        
        pvalue.1 <- serial.test(var.1, lags.pt=10, type="PT.asymptotic")$serial$p.value

        sigma.1 <- summary(var.1)$varresult[[arg.comparison.colname]]$sigma
        
 
        result <- t(array(c(arg.training.set.endpoint,
                            round(sqrt(mean((as.numeric(fcst.1) - 
                                                     as.numeric(test.set.object.1[,arg.comparison.colname]))^2)), digits = 1),
                            round(pvalue.1, digits = 2),
                            round(sigma.1, digits = 2),
                          max(abs(roots(var.1))),
                          var.1$p),
                          dim = c(6,1)))
        
        for(i in 1:arg.comparison.period){
                # browser()               
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])
                

                
                var.2 <- VAR(diff(training.set.object.2), p=1, 
                             type="const", lag.max = 5, ic = "SC" )
                fcst.2 <- predict(var.2,
                                  n.ahead = length(test.set.object.2[,arg.comparison.colname]))
                fcst.2 <- as.numeric(tail(training.set.object.2[,arg.comparison.colname],1)) +
                        cumsum(fcst.2$fcst[[arg.comparison.colname]][,"fcst"])
                
                pvalue.2 <- serial.test(var.2, lags.pt=10, type="PT.asymptotic")$serial$p.value
                

                sigma.2 <- summary(var.2)$varresult[[arg.comparison.colname]]$sigma
                
                result2 <- t(array(c(arg.training.set.endpoint + i,
                                    round(sqrt(mean((as.numeric(fcst.2) - 
                                                             as.numeric(test.set.object.2[,arg.comparison.colname]))^2)), 
                                          digits = 1),
                                    round(pvalue.2, digits = 2),
                                    round(sigma.2, digits = 2),
                                   max(abs(roots(var.2))),
                                   var.2$p),
                                  dim = c(6,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "RMSE", "p.v", "sigma","max.root","p.order")
        
        # browser()
        return(df.result)        
        
}
