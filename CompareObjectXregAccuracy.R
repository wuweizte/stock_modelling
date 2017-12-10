CompareObjectXregAccuracy <- function(arg.object, 
                                      arg.forecast.period,
                                      arg.training.set.endpoint, 
                                      arg.comparison.period,
                                      arg.maxorder,
                                      arg.xreg){
        
        # browser()
        training.set.object.1 <- window(arg.object, start = 1, 
                                        end = arg.training.set.endpoint)
        
        test.set.object.1 <- window(arg.object, start = arg.training.set.endpoint + 1,
                                    end = arg.training.set.endpoint + arg.forecast.period)
        
        xreg.object.1 <- arg.xreg[1:arg.training.set.endpoint,]
        
        xreg.object.test.1 <- arg.xreg[(arg.training.set.endpoint + 1):
                                               (arg.training.set.endpoint + arg.forecast.period), ]
                
        
        fit.arima.object.1 <- auto.arima(training.set.object.1,
                                         max.order = arg.maxorder,
                                         stepwise = FALSE,
                                         approximation = FALSE,
                                         xreg = xreg.object.1)                
        

        fc.arima.object.1 <- forecast(fit.arima.object.1, 
                                      h = arg.forecast.period, 
                                      xreg = xreg.object.test.1)        
        
        
        df.accuracy <- accuracy(fc.arima.object.1, test.set.object.1)
        pvalue <- Box.test(residuals(fit.arima.object.1), lag=10, 
                           fitdf=sum(fit.arima.object.1$arma[c(1,2)]))$p.value
        
        # result <- t(array(c(arg.training.set.endpoint,
        #                     fit.arima.object.1$arma[c(1,6,2)],
        #                     df.accuracy[2,],
        #                     ("drift" %in% names(fit.arima.object.1$coef)) * 1,
        #                     pvalue),
        #                   dim = c(14,1)))
        
        result <- t(array(c(arg.training.set.endpoint,
                            fit.arima.object.1$arma[c(1,6,2)],
                            ("drift" %in% names(fit.arima.object.1$coef)) * 1,
                            round(accuracy(fc.arima.object.1, test.set.object.1)[2,2], digits = 1),
                            round(pvalue, digits = 2)),
                          dim = c(7,1)))
        


        for(i in 1:arg.comparison.period){
                
                end.point2 <- arg.training.set.endpoint + i
                training.set.object.2 <- window(arg.object, start = 1, end = end.point2)
                test.set.object.2 <- window(arg.object, start = end.point2 + 1,
                                            end = end.point2 + arg.forecast.period)
                
                xreg.object.2 <- arg.xreg[1:end.point2,]
                xreg.object.test.2 <- arg.xreg[(end.point2 + 1):
                                                       (end.point2 + arg.forecast.period),]
                
                fit.arima.object.2 <- auto.arima(training.set.object.2,
                                                 max.order = arg.maxorder,
                                                 stepwise = FALSE,
                                                 approximation = FALSE, 
                                                 xreg = xreg.object.2)                          
                
                fc.arima.object.2 <- forecast(fit.arima.object.2, 
                                              h = arg.forecast.period, 
                                              xreg = xreg.object.test.2)        
                
                pvalue <- Box.test(residuals(fit.arima.object.2), lag=10, 
                                   fitdf=sum(fit.arima.object.2$arma[c(1,2)]))$p.value
                
                # result2 <- t(array(c(arg.training.set.endpoint + i,
                #                      fit.arima.object.2$arma[c(1,6,2)],
                #                      accuracy(fc.arima.object.2, test.set.object.2)[2,],
                #                      ("drift" %in% names(fit.arima.object.2$coef)) * 1,
                #                      pvalue),
                #                    dim = c(14,1)))
                
                result2 <- t(array(c(arg.training.set.endpoint + i,
                                     fit.arima.object.2$arma[c(1,6,2)],
                                     ("drift" %in% names(fit.arima.object.2$coef)) * 1,
                                     round(accuracy(fc.arima.object.2, test.set.object.2)[2,2], digits = 1),
                                     round(pvalue, digits = 2)),
                                   dim = c(7,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        # colnames(df.result) <- c("seq", "p","d","q", col.names.accuracy, "drift", "pvalue")
        
        colnames(df.result) <- c("seq", "p","d","q", "dr", "RMSE", "p.v")
        
        return(df.result)        
        
}

####################
# comparison.period <- 1
# 
# forecast.period <- 1
# 
# training.set.endpoint <- length(monthly.close.price) - comparison.period - forecast.period
# 
# maxorder <- 9
# 
# # result.pure.close.price <- CompareObjectAccuracy(arg.object = monthly.close.price,
# #                                                  arg.forecast.period = forecast.period,
# #                                                  arg.training.set.endpoint = training.set.endpoint,
# #                                                  arg.comparison.period = comparison.period,
# #                                                  arg.maxorder = maxorder)
# 
# reg.lagged.df <- data.frame(cbind(monthly.kdj.k,       
#                                   ts(c(NA, monthly.kdj.k[-length(monthly.kdj.k)]))))
# 
# result.actual.xreg.close.price <- CompareObjectXregAccuracy(arg.object = monthly.close.price, 
#                                                             arg.forecast.period = forecast.period,
#                                                             arg.training.set.endpoint = training.set.endpoint, 
#                                                             arg.comparison.period = comparison.period,
#                                                             arg.maxorder = maxorder,
#                                                             arg.xreg = reg.lagged.df)
