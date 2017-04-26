CompareGarchnyblom <- function(arg.object, 
                                arg.forecast.period,
                                arg.training.set.endpoint, 
                                arg.comparison.period,
                                arg.result.arima,
                                arg.var.model,
                                arg.dist.model){
        
        training.set.object.1 <- window(arg.object, start = 1, 
                                        end = arg.training.set.endpoint)
        
        test.set.object.1 <- window(arg.object, start = arg.training.set.endpoint + 1,
                                    end = arg.training.set.endpoint + arg.forecast.period)
        

        #browser()
        myspec.garch.1 <- ugarchspec(variance.model = list(model = arg.var.model,
                                                                   garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(arg.result.arima$p[1],
                                                                           arg.result.arima$q[1])),
                                           distribution.model = arg.dist.model)

        myfit.garch.1 <- ugarchfit(myspec.garch.1, data = training.set.object.1, solver = "hybrid")

        result <- t(c(arg.training.set.endpoint, 
                      (nyblom(myfit.garch.1))$IndividualStat,
                      (nyblom(myfit.garch.1))$IndividualCritical))
        
        for(i in 1:arg.comparison.period){
                end.point2 <- arg.training.set.endpoint + i
                training.set.object.2 <- window(arg.object, start = 1, end = end.point2)
                test.set.object.2 <- window(arg.object, start = end.point2 + 1,
                                            end = end.point2 + arg.forecast.period)

                myspec.garch.2 <- ugarchspec(variance.model = list(model = arg.var.model,
                                                                      garchOrder = c(1,1)),
                                                     mean.model = list(armaOrder = c(arg.result.arima$p[i + 1],
                                                                                     arg.result.arima$q[i + 1])),
                                                     distribution.model = arg.dist.model)

                myfit.garch.2 <- ugarchfit(myspec.garch.2, data = training.set.object.2, solver = "hybrid")


                result2 <- t(c(arg.training.set.endpoint + i,
                               (nyblom(myfit.garch.2))$IndividualStat,
                               (nyblom(myfit.garch.2))$IndividualCritical))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", dimnames((nyblom(myfit.garch.2))$IndividualStat)[[1]],
                                 names((nyblom(myfit.garch.2))$IndividualCritical))

        return(df.result)
        

}
        