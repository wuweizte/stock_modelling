CompareGarchnyblomARMA00 <- function(arg.object, 
                                arg.forecast.period,
                                arg.training.set.endpoint, 
                                arg.comparison.period,
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
                                           mean.model = list(armaOrder = c(0,0)),
                                           distribution.model = arg.dist.model)

        myfit.garch.1 <- ugarchfit(myspec.garch.1, data = training.set.object.1, solver = "hybrid")

        result <- t(c(arg.training.set.endpoint, 
                      (nyblom(myfit.garch.1))$IndividualStat,
                      (nyblom(myfit.garch.1))$IndividualCritical))
        
        for(i in 1:arg.comparison.period){
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])

                myspec.garch.2 <- ugarchspec(variance.model = list(model = arg.var.model,
                                                                      garchOrder = c(1,1)),
                                                     mean.model = list(armaOrder = c(0,0)),
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
        