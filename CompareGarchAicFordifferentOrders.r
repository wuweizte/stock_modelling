
CompareGarchAicFordifferentOrders <- function(arg.training.set, 
                                              arg.arch.order,
                                              arg.garch.order,
                                              arg.var.model,
                                              arg.dist.model,
                                              arg.fit.arima.object){

        inf.result <- NULL

        for(i in arg.arch.order){
                for(j in arg.garch.order ){

                        myspec <- ugarchspec(variance.model = list(model = arg.var.model,
                                                                               garchOrder = c(i,j)),
                                                         mean.model = list(armaOrder = c(arg.fit.arima.object$arma[1],
                                                                                         arg.fit.arima.object$arma[2])),
                                                         distribution.model = arg.dist.model)
                        
                        myfit <- ugarchfit(myspec, data = arg.training.set, solver = "hybrid")
                        
                        
                        inf <- data.frame(i, j, round(t(infocriteria(myfit)),digits = 2))
                        
                        if(is.null(inf.result)){
                                inf.result <- inf
                        }else{
                                inf.result <- rbind(inf.result, inf)
                        }
                }
        }
        
 
        names(inf.result)[1:2] <- c("arch", "garch")

        row.names(inf.result) <- c(1:length(inf.result$arch))
        return(inf.result)

}