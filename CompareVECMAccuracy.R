CompareVECMAccuracy <- function(arg.object, 
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
        cot.1 <- ca.jo(training.set.object.1, ecdet = "const", type = "trace", 
                     K = var.1$p, spec = "transitory")

        # browser()
        
        flag.1 <- 0
        if(cot.1@teststat[1] > cot.1@cval[1,2]) {
                fcst.1 <- predict(var.1, n.ahead = length(test.set.object.1[,arg.comparison.colname]))
                fcst.1 <- fcst.1$fcst[[arg.comparison.colname]][,"fcst"]
                
                pvalue.1 <- serial.test(var.1, lags.pt=10, type="PT.asymptotic")$serial$p.value
                
                sigma.1 <- summary(var.1)$varresult[[arg.comparison.colname]]$sigma
                flag.1 <- 1
                
        } else if(cot.1@teststat[2] < cot.1@cval[2,2]){
                var.1 <- VAR(diff(training.set.object.1), p=1, 
                             type="const", lag.max = 5, ic = "SC" )
                fcst.1 <- predict(var.1,
                                  n.ahead = length(test.set.object.1[,arg.comparison.colname]))
                fcst.1 <- as.numeric(tail(training.set.object.1[,arg.comparison.colname],1)) +
                        cumsum(fcst.1$fcst[[arg.comparison.colname]][,"fcst"])
                pvalue.1 <- serial.test(var.1, lags.pt=10, type="PT.asymptotic")$serial$p.value
                
                sigma.1 <- summary(var.1)$varresult[[arg.comparison.colname]]$sigma
                
                flag.1 <- 2
        } else{
                fcst.1 <- predict(vec2var(cot.1, r = 1),
                                  n.ahead = length(test.set.object.1[,arg.comparison.colname]))
                fcst.1 <- fcst.1$fcst[[arg.comparison.colname]][,"fcst"]
 
                pvalue.1 <- serial.test(var.1, lags.pt=10, type="PT.asymptotic")$serial$p.value
                
                sigma.1 <- summary(var.1)$varresult[[arg.comparison.colname]]$sigma
                
                flag.1 <- 3
                if(sum(fcst.1 < -10) > 1){

                        var.1 <- VAR(diff(training.set.object.1), p=1,
                                     type="const", lag.max = 5, ic = "SC" )
                        fcst.1 <- predict(var.1,
                                          n.ahead = length(test.set.object.1[,arg.comparison.colname]))
                        fcst.1 <- as.numeric(tail(training.set.object.1[,arg.comparison.colname],1)) +
                                cumsum(fcst.1$fcst[[arg.comparison.colname]][,"fcst"])

                        pvalue.1 <- serial.test(var.1, lags.pt=10, type="PT.asymptotic")$serial$p.value

                        sigma.1 <- summary(var.1)$varresult[[arg.comparison.colname]]$sigma
                        flag.1 <- 4
                }
        }

        
        result <- t(array(c(arg.training.set.endpoint,
                            round(sqrt(mean((as.numeric(fcst.1) - 
                                                     as.numeric(test.set.object.1[,arg.comparison.colname]))^2)), digits = 1),
                            round(pvalue.1, digits = 2),
                            round(sigma.1, digits = 2),
                          max(abs(roots(var.1))),
                          var.1$p,
                          flag.1),
                          dim = c(7,1)))
        
        for(i in 1:arg.comparison.period){
 
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])
                

                
                var.2 <- VAR(training.set.object.2, p=1, type="const", lag.max = 5, ic = "SC" )
                

                cot.2 <- ca.jo(training.set.object.2, ecdet = "const", type = "trace", 
                               K = var.2$p, spec = "transitory")
                
                # browser()
                flag.2 <- 0
                if(is.na(cot.2@teststat[1])||is.na(cot.2@teststat[2])||(cot.2@teststat[1] > cot.2@cval[1,2])) {
                        fcst.2 <- predict(var.2, n.ahead = length(test.set.object.2[,arg.comparison.colname]))
                        fcst.2 <- fcst.2$fcst[[arg.comparison.colname]][,"fcst"]
                        pvalue.2 <- serial.test(var.2, lags.pt=10, type="PT.asymptotic")$serial$p.value
                        
                        
                        sigma.2 <- summary(var.2)$varresult[[arg.comparison.colname]]$sigma
                        flag.2 <- 1
                        
                } else if(cot.2@teststat[2] < cot.2@cval[2,2]){
                        var.2 <- VAR(diff(training.set.object.2), p=1, 
                                     type="const", lag.max = 5, ic = "SC" )
                        fcst.2 <- predict(var.2,
                                          n.ahead = length(test.set.object.2[,arg.comparison.colname]))
                        fcst.2 <- as.numeric(tail(training.set.object.2[,arg.comparison.colname],1)) +
                                cumsum(fcst.2$fcst[[arg.comparison.colname]][,"fcst"])
                        pvalue.2 <- serial.test(var.2, lags.pt=10, type="PT.asymptotic")$serial$p.value
                        
                        
                        sigma.2 <- summary(var.2)$varresult[[arg.comparison.colname]]$sigma
                        flag.2 <- 2
                } else{
                        fcst.2 <- predict(vec2var(cot.2, r = 1),
                                          n.ahead = length(test.set.object.2[,arg.comparison.colname]))
                        fcst.2 <- fcst.2$fcst[[arg.comparison.colname]][,"fcst"]
                        pvalue.2 <- serial.test(var.2, lags.pt=10, type="PT.asymptotic")$serial$p.value
                        
                        
                        sigma.2 <- summary(var.2)$varresult[[arg.comparison.colname]]$sigma
                        flag.2 <- 3
                        if((sum(fcst.2 < -10) > 1) ||(sum(fcst.2 > 100) > 1)){
                                var.2 <- VAR(diff(training.set.object.2), p=1,
                                             type="const", lag.max = 5, ic = "SC" )
                                fcst.2 <- predict(var.2,
                                                  n.ahead = length(test.set.object.2[,arg.comparison.colname]))
                                fcst.2 <- as.numeric(tail(training.set.object.2[,arg.comparison.colname],1)) +
                                        cumsum(fcst.2$fcst[[arg.comparison.colname]][,"fcst"])
                                pvalue.2 <- serial.test(var.2, lags.pt=10, type="PT.asymptotic")$serial$p.value


                                sigma.2 <- summary(var.2)$varresult[[arg.comparison.colname]]$sigma
                                flag.2 <- 4
                        }
                }
                
                

                result2 <- t(array(c(arg.training.set.endpoint + i,
                                    round(sqrt(mean((as.numeric(fcst.2) - 
                                                             as.numeric(test.set.object.2[,arg.comparison.colname]))^2)), 
                                          digits = 1),
                                    round(pvalue.2, digits = 2),
                                    round(sigma.2, digits = 2),
                                   max(abs(roots(var.2))),
                                   var.2$p,
                                   flag.2),
                                  dim = c(7,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "RMSE", "p.v", "sigma","max.root","p.order","flag")
        
        # browser()
        return(df.result)        
        
}
