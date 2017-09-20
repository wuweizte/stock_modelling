CompareGarchsignbias <- function(arg.object, 
                                arg.forecast.period,
                                arg.training.set.endpoint, 
                                arg.comparison.period,
                                arg.result.arima,
                                arg.var.model,
                                arg.dist.model){
        
        time.attribute <- tsp(arg.object)
        
        training.set.object.1 <- 
                window(arg.object,  
                       end = time.attribute[1] + (arg.training.set.endpoint - 1) / time.attribute[3])
        
        test.set.object.1 <- window(arg.object, 
                                    start = time.attribute[1] + arg.training.set.endpoint / time.attribute[3],
                                    end = time.attribute[1] + (arg.training.set.endpoint + arg.forecast.period - 1)/ time.attribute[3])
        

        #browser()
        myspec.garch.1 <- ugarchspec(variance.model = list(model = arg.var.model,
                                                                   garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(arg.result.arima$p[1],
                                                                           arg.result.arima$q[1])),
                                           distribution.model = arg.dist.model)

        myfit.garch.1 <- ugarchfit(myspec.garch.1, data = training.set.object.1, solver = "hybrid")

        result <- t(array(c(arg.training.set.endpoint, 
                            round((signbias(myfit.garch.1))$prob,digits = 2)),
                          dim = c(5,1)))
        
        for(i in 1:arg.comparison.period){

                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])

                myspec.garch.2 <- ugarchspec(variance.model = list(model = arg.var.model,
                                                                      garchOrder = c(1,1)),
                                                     mean.model = list(armaOrder = c(arg.result.arima$p[i + 1],
                                                                                     arg.result.arima$q[i + 1])),
                                                     distribution.model = arg.dist.model)

                myfit.garch.2 <- ugarchfit(myspec.garch.2, data = training.set.object.2, solver = "hybrid")


                result2 <- t(array(c(arg.training.set.endpoint + i,
                                     round((signbias(myfit.garch.2))$prob,digits = 2)),
                                   dim = c(5,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", row.names(signbias(myfit.garch.2)))

        return(df.result)
        

}
        