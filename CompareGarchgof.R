CompareGarchgof <- function(arg.object, 
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

        df.gof <- as.data.frame(gof(myfit.garch.1, c(20,30,40,50)))
        result <- t(array(c(arg.training.set.endpoint, 
                            round(df.gof$"p-v",digits = 2)),
                          dim = c(5,1)))
        
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


                df.gof.2 <- as.data.frame(gof(myfit.garch.2, c(20,30,40,50)))
                result2 <- t(array(c(arg.training.set.endpoint + i,
                                     round(df.gof.2$"p-v",digits = 2)),
                                   dim = c(5,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", paste0(c(20,30,40,50), "p-v"))

        return(df.result)
        

}
        