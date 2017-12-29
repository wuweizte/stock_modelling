ResultVARinDiffByRoot <- function(arg.object, 
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
        
        

        var.1 <- VAR(training.set.object.1, p=1, type="const", lag.max = 5, ic = "SC" )
        flag.1 <- 0
        if(max(abs(roots(var.1))) < 1) {
                fcst.1 <- predict(var.1, n.ahead = length(test.set.object.1[,arg.comparison.colname]))
                fcst.1 <- fcst.1$fcst[[arg.comparison.colname]][,"fcst"]
                flag.1 <- 1
                
        } else {
                var.1 <- VAR(diff(training.set.object.1), p=1, 
                             type="const", lag.max = 5, ic = "SC" )
                fcst.1 <- predict(var.1,
                                  n.ahead = length(test.set.object.1[,arg.comparison.colname]))
                fcst.1 <- as.numeric(tail(training.set.object.1[,arg.comparison.colname],1)) +
                        cumsum(fcst.1$fcst[[arg.comparison.colname]][,"fcst"])
                flag.1 <- 2
        } 

        

        fc.result <- t(array(fcst.1,
                             dim = c(length(fcst.1),1)))
        
        test.set.result <- t(array(test.set.object.1[,arg.comparison.colname],
                                   dim = c(length(fcst.1),1)))
 

        for(i in 1:arg.comparison.period){
                # browser()
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])
                

                
                var.2 <- VAR(training.set.object.2, p=1, type="const", lag.max = 5, ic = "SC" )
                

                flag.2 <- 0
                if(max(abs(roots(var.2))) < 1) {
                        fcst.2 <- predict(var.2, n.ahead = length(test.set.object.2[,arg.comparison.colname]))
                        fcst.2 <- fcst.2$fcst[[arg.comparison.colname]][,"fcst"]
                        flag.2 <- 1
                        
                } else{
                        var.2 <- VAR(diff(training.set.object.2), p=1, 
                                     type="const", lag.max = 5, ic = "SC" )
                        fcst.2 <- predict(var.2,
                                          n.ahead = length(test.set.object.2[,arg.comparison.colname]))
                        fcst.2 <- as.numeric(tail(training.set.object.2[,arg.comparison.colname],1)) +
                                cumsum(fcst.2$fcst[[arg.comparison.colname]][,"fcst"])
                        flag.2 <- 2
                }
                

                fc.result.2 <- t(array(fcst.2,
                                     dim = c(length(fcst.2),1)))
                
                test.set.result.2 <- t(array(test.set.object.2[,arg.comparison.colname],
                                           dim = c(length(fcst.2),1)))
                

                fc.result <- rbind(fc.result, fc.result.2)
                test.set.result <- rbind(test.set.result, test.set.result.2)
        }
        
        result <- list(fc.result = fc.result, test.set.result = test.set.result)
        return(result)        
        
        
}
