
CompareGarchAic <- function(arg.training.set, arg.difference){
        
        
        fit.arima.kdj.k <- auto.arima(arg.training.set,
                                      max.order = 5,
                                      d = arg.difference,
                                      stepwise = FALSE,
                                      approximation = FALSE)

        inf.arima <- t(c(fit.arima.kdj.k$aic, fit.arima.kdj.k$bic, 0, 0)) 
        llh.arima <- fit.arima.kdj.k$loglik

        ####
        if(arg.difference == 0){
                training.set <- arg.training.set
        }else{
                training.set <- diff(arg.training.set)
        }
        
        ####
        myspec.sgarch.norm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                         distribution.model = "norm")
        
        myfit.sgarch.norm <- ugarchfit(myspec.sgarch.norm, data = training.set, solver = "hybrid")
        

        (inf.sgarch.norm <- t(infocriteria(myfit.sgarch.norm)))
        
        (llh.sgarch.norm <- likelihood(myfit.sgarch.norm))
        
        ####
        myspec.igarch.norm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                         distribution.model = "norm")
        
        myfit.igarch.norm <- ugarchfit(myspec.igarch.norm, data = training.set, solver = "hybrid")

        (inf.igarch.norm <- t(infocriteria(myfit.igarch.norm)))
        
        (llh.igarch.norm <- likelihood(myfit.igarch.norm))
        
        ####
        myspec.egarch.norm <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                         distribution.model = "norm")
        
        myfit.egarch.norm <- ugarchfit(myspec.egarch.norm, data = training.set, solver = "hybrid")
        

        (inf.egarch.norm <- t(infocriteria(myfit.egarch.norm)))
        
        (llh.egarch.norm <- likelihood(myfit.egarch.norm))
        
        ###########
        myspec.gjrgarch.norm <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                           distribution.model = "norm")
        
        myfit.gjrgarch.norm <- ugarchfit(myspec.gjrgarch.norm, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.norm <- t(infocriteria(myfit.gjrgarch.norm)))
        
        (llh.gjrgarch.norm <- likelihood(myfit.gjrgarch.norm))
        
        ##################
        myspec.sgarch.snorm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                          distribution.model = "snorm")
        
        myfit.sgarch.snorm <- ugarchfit(myspec.sgarch.snorm, data = training.set, solver = "hybrid")
        

        (inf.sgarch.snorm <- t(infocriteria(myfit.sgarch.snorm)))
        
        (llh.sgarch.snorm <- likelihood(myfit.sgarch.snorm))
        
        ################
        myspec.igarch.snorm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                          distribution.model = "snorm")
        
        myfit.igarch.snorm <- ugarchfit(myspec.igarch.snorm, data = training.set, solver = "hybrid")
        

        (inf.igarch.snorm <- t(infocriteria(myfit.igarch.snorm)))
        
        (llh.igarch.snorm <- likelihood(myfit.igarch.snorm))
        
        
        
        ######
        myspec.egarch.snorm <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                          distribution.model = "snorm")
        
        myfit.egarch.snorm <- ugarchfit(myspec.egarch.snorm, data = training.set, solver = "hybrid")
        

        (inf.egarch.snorm <- t(infocriteria(myfit.egarch.snorm)))
        
        (llh.egarch.snorm <- likelihood(myfit.egarch.snorm))
        
        
        
        ######
        myspec.gjrgarch.snorm <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                            mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                            distribution.model = "snorm")
        
        myfit.gjrgarch.snorm <- ugarchfit(myspec.gjrgarch.snorm, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.snorm <- t(infocriteria(myfit.gjrgarch.snorm)))
        
        (llh.gjrgarch.snorm <- likelihood(myfit.gjrgarch.snorm))
        
        
        
        
        
        myspec.sgarch.std <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                        distribution.model = "std")
        
        myfit.sgarch.std <- ugarchfit(myspec.sgarch.std, data = training.set, solver = "hybrid")
        
        
        (inf.sgarch.std <- t(infocriteria(myfit.sgarch.std)))
        
        (llh.sgarch.std <- likelihood(myfit.sgarch.std))
        
        
        
        ######
        myspec.igarch.std <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                        distribution.model = "std")
        
        myfit.igarch.std <- ugarchfit(myspec.igarch.std, data = training.set, solver = "hybrid")
        

        (inf.igarch.std <- t(infocriteria(myfit.igarch.std)))
        
        (llh.igarch.std <- likelihood(myfit.igarch.std))
        
        
        
        ######
        myspec.egarch.std <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                        distribution.model = "std")
        
        myfit.egarch.std <- ugarchfit(myspec.egarch.std, data = training.set, solver = "hybrid")
        

        (inf.egarch.std <- t(infocriteria(myfit.egarch.std)))
        
        (llh.egarch.std <- likelihood(myfit.egarch.std))
        
        
        
        ######
        myspec.gjrgarch.std <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                          distribution.model = "std")
        
        myfit.gjrgarch.std <- ugarchfit(myspec.gjrgarch.std, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.std <- t(infocriteria(myfit.gjrgarch.std)))
        
        (llh.gjrgarch.std <- likelihood(myfit.gjrgarch.std))
        
        
        
        
        ######
        myspec.sgarch.sstd <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                         distribution.model = "sstd")
        
        myfit.sgarch.sstd <- ugarchfit(myspec.sgarch.sstd, data = training.set, solver = "hybrid")
        

        (inf.sgarch.sstd <- t(infocriteria(myfit.sgarch.sstd)))
        
        (llh.sgarch.sstd <- likelihood(myfit.sgarch.sstd))
        
        
        
        ######
        myspec.igarch.sstd <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                         distribution.model = "sstd")
        
        myfit.igarch.sstd <- ugarchfit(myspec.igarch.sstd, data = training.set, solver = "hybrid")
        

        (inf.igarch.sstd <- t(infocriteria(myfit.igarch.sstd)))
        
        (llh.igarch.sstd <- likelihood(myfit.igarch.sstd))
        
        
        
        ######
        myspec.egarch.sstd <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                         distribution.model = "sstd")
        
        myfit.egarch.sstd <- ugarchfit(myspec.egarch.sstd, data = training.set, solver = "hybrid")
        

        (inf.egarch.sstd <- t(infocriteria(myfit.egarch.sstd)))
        
        (llh.egarch.sstd <- likelihood(myfit.egarch.sstd))
        
        
        
        ######
        myspec.gjrgarch.sstd <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                           distribution.model = "sstd")
        
        myfit.gjrgarch.sstd <- ugarchfit(myspec.gjrgarch.sstd, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.sstd <- t(infocriteria(myfit.gjrgarch.sstd)))
        
        (llh.gjrgarch.sstd <- likelihood(myfit.gjrgarch.sstd))
        
        
        
        
        ######
        myspec.sgarch.ged <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                        distribution.model = "ged")
        
        myfit.sgarch.ged <- ugarchfit(myspec.sgarch.ged, data = training.set, solver = "hybrid")
        

        (inf.sgarch.ged <- t(infocriteria(myfit.sgarch.ged)))
        
        (llh.sgarch.ged <- likelihood(myfit.sgarch.ged))
        
        
        
        
        ######
        myspec.igarch.ged <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                        distribution.model = "ged")
        
        myfit.igarch.ged <- ugarchfit(myspec.igarch.ged, data = training.set, solver = "hybrid")
        

        (inf.igarch.ged <- t(infocriteria(myfit.igarch.ged)))
        
        (llh.igarch.ged <- likelihood(myfit.igarch.ged))
        
        
        
        
        ######
        myspec.gjrgarch.ged <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                          distribution.model = "ged")
        
        myfit.gjrgarch.ged <- ugarchfit(myspec.gjrgarch.ged, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.ged <- t(infocriteria(myfit.gjrgarch.ged)))
        
        (llh.gjrgarch.ged <- likelihood(myfit.gjrgarch.ged))
        
        
        
        
        
        ######
        myspec.sgarch.sged <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                         distribution.model = "sged")
        
        myfit.sgarch.sged <- ugarchfit(myspec.sgarch.sged, data = training.set, solver = "hybrid")
        

        (inf.sgarch.sged <- t(infocriteria(myfit.sgarch.sged)))
        
        (llh.sgarch.sged <- likelihood(myfit.sgarch.sged))
        
        
        
        
        ######
        myspec.igarch.sged <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                         distribution.model = "sged")
        
        myfit.igarch.sged <- ugarchfit(myspec.igarch.sged, data = training.set, solver = "hybrid")
        

        (inf.igarch.sged <- t(infocriteria(myfit.igarch.sged)))
        
        (llh.igarch.sged <- likelihood(myfit.igarch.sged))
        
        
        
        
        ######
        myspec.gjrgarch.sged <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(fit.arima.kdj.k$arma[1],fit.arima.kdj.k$arma[2])),
                                           distribution.model = "sged")
        
        myfit.gjrgarch.sged <- ugarchfit(myspec.gjrgarch.sged, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.sged <- t(infocriteria(myfit.gjrgarch.sged)))
        
        (llh.gjrgarch.sged <- likelihood(myfit.gjrgarch.sged))
        # 
        
        
        
        
        ######
        
        aic.name <- c("inf.sgarch.norm",
                      "inf.sgarch.snorm",
                      "inf.sgarch.std",
                      "inf.sgarch.sstd",
                      "inf.sgarch.ged",
                      "inf.sgarch.sged",
                      "inf.igarch.norm",
                      "inf.igarch.snorm",
                      "inf.igarch.std",
                      "inf.igarch.sstd",
                      "inf.igarch.ged",
                      "inf.igarch.sged",
                      "inf.egarch.norm",
                      "inf.egarch.snorm",
                      "inf.egarch.std",
                      "inf.egarch.sstd",
                      "inf.gjrgarch.norm",
                      "inf.gjrgarch.snorm",
                      "inf.gjrgarch.std",
                      "inf.gjrgarch.sstd",
                      "inf.gjrgarch.ged",
                      "inf.gjrgarch.sged",
                      "inf.arima")
        
        aic.value <- rbind(inf.sgarch.norm,
                           inf.sgarch.snorm,
                           inf.sgarch.std,
                           inf.sgarch.sstd,
                           inf.sgarch.ged,
                           inf.sgarch.sged,
                           inf.igarch.norm,
                           inf.igarch.snorm,
                           inf.igarch.std,
                           inf.igarch.sstd,
                           inf.igarch.ged,
                           inf.igarch.sged,
                           inf.egarch.norm,
                           inf.egarch.snorm,
                           inf.egarch.std,
                           inf.egarch.sstd,
                           inf.gjrgarch.norm,
                           inf.gjrgarch.snorm,
                           inf.gjrgarch.std,
                           inf.gjrgarch.sstd,
                           inf.gjrgarch.ged,
                           inf.gjrgarch.sged,
                           inf.arima)
        
        #browser()
        aic.df <- data.frame(name = aic.name, round(as.data.frame(aic.value),digits = 2))
        
        print(aic.df[order(aic.df[[3]], aic.df[[2]]),])
        
        cat("\n")
        
        ######
        
        llh.name <- c("llh.sgarch.norm",
                      "llh.sgarch.snorm",
                      "llh.sgarch.std",
                      "llh.sgarch.sstd",
                      "llh.sgarch.ged",
                      "llh.sgarch.sged",
                      "llh.igarch.norm",
                      "llh.igarch.snorm",
                      "llh.igarch.std",
                      "llh.igarch.sstd",
                      "llh.igarch.ged",
                      "llh.igarch.sged",
                      "llh.egarch.norm",
                      "llh.egarch.snorm",
                      "llh.egarch.std",
                      "llh.egarch.sstd",
                      "llh.gjrgarch.norm",
                      "llh.gjrgarch.snorm",
                      "llh.gjrgarch.std",
                      "llh.gjrgarch.sstd",
                      "llh.gjrgarch.ged",
                      "llh.gjrgarch.sged",
                      "llh.arima")
        
        llh.value <- c(llh.sgarch.norm,
                       llh.sgarch.snorm,
                       llh.sgarch.std,
                       llh.sgarch.sstd,
                       llh.sgarch.ged,
                       llh.sgarch.sged,
                       llh.igarch.norm,
                       llh.igarch.snorm,
                       llh.igarch.std,
                       llh.igarch.sstd,
                       llh.igarch.ged,
                       llh.igarch.sged,
                       llh.egarch.norm,
                       llh.egarch.snorm,
                       llh.egarch.std,
                       llh.egarch.sstd,
                       llh.gjrgarch.norm,
                       llh.gjrgarch.snorm,
                       llh.gjrgarch.std,
                       llh.gjrgarch.sstd,
                       llh.gjrgarch.ged,
                       llh.gjrgarch.sged,
                       llh.arima)
        
        llh.df <- data.frame(name = llh.name, llh = llh.value)
        
        
        print(llh.df[order(llh.df$llh, decreasing = TRUE),])
        
        
        
        
       
        
}