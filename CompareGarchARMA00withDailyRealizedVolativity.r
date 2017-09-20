
CompareGarchARMA00withDailyRealizedVolativity <- function(arg.training.set, arg.monthly.RV){
        
        

        training.set <- arg.training.set
        monthly.RV <- arg.monthly.RV

        testset.length <- length(monthly.RV)
        
        ####
        myspec.sgarch.norm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "norm")
        
        myfit.sgarch.norm <- ugarchfit(myspec.sgarch.norm, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")

        suppressWarnings(fc.sgarch.norm <- ugarchforecast(myfit.sgarch.norm, 
                                                         n.ahead = 1,
                                                         n.roll = testset.length - 1))
        
        fc.volatility.sgarch.norm <- (fc.sgarch.norm@forecast$sigmaFor)^2
        fc.mean.sgarch.norm <- fc.sgarch.norm@forecast$seriesFor         

        mse.sgarch.norm <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.sgarch.norm))^2 )
        

        ###
        myspec.igarch.norm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "norm")

        myfit.igarch.norm <- ugarchfit(myspec.igarch.norm, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")
        
        suppressWarnings(fc.igarch.norm <- ugarchforecast(myfit.igarch.norm, 
                                                          n.ahead = 1,
                                                          n.roll = testset.length - 1))
        
        fc.volatility.igarch.norm <- (fc.igarch.norm@forecast$sigmaFor)^2
        fc.mean.igarch.norm <- fc.igarch.norm@forecast$seriesFor         
        
        mse.igarch.norm <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.igarch.norm))^2 )
        



        ####
        myspec.egarch.norm <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "norm")
         
        myfit.egarch.norm <- ugarchfit(myspec.egarch.norm, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")
        
        suppressWarnings(fc.egarch.norm <- ugarchforecast(myfit.egarch.norm, 
                                                          n.ahead = 1,
                                                          n.roll = testset.length - 1))
        
        fc.volatility.egarch.norm <- (fc.egarch.norm@forecast$sigmaFor)^2
        fc.mean.egarch.norm <- fc.egarch.norm@forecast$seriesFor         
        
        mse.egarch.norm <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.egarch.norm))^2 )

        ###########
        myspec.gjrgarch.norm <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(0,0)),
                                           distribution.model = "norm")

        myfit.gjrgarch.norm <- ugarchfit(myspec.gjrgarch.norm, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")
        
        suppressWarnings(fc.gjrgarch.norm <- ugarchforecast(myfit.gjrgarch.norm, 
                                                          n.ahead = 1,
                                                          n.roll = testset.length - 1))
        
        fc.volatility.gjrgarch.norm <- (fc.gjrgarch.norm@forecast$sigmaFor)^2
        fc.mean.gjrgarch.norm <- fc.gjrgarch.norm@forecast$seriesFor         
        
        mse.gjrgarch.norm <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.gjrgarch.norm))^2 )        

        ##################
        myspec.sgarch.snorm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "snorm")

        myfit.sgarch.snorm <- ugarchfit(myspec.sgarch.snorm, 
                                         data = training.set,
                                         out.sample = testset.length,
                                         solver = "hybrid")
        
        suppressWarnings(fc.sgarch.snorm <- ugarchforecast(myfit.sgarch.snorm, 
                                                            n.ahead = 1,
                                                            n.roll = testset.length - 1))
        
        fc.volatility.sgarch.snorm <- (fc.sgarch.snorm@forecast$sigmaFor)^2
        fc.mean.sgarch.snorm <- fc.sgarch.snorm@forecast$seriesFor         
        
        mse.sgarch.snorm <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.sgarch.snorm))^2 )                # 

        ################
        myspec.igarch.snorm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "snorm")

        myfit.igarch.snorm <- ugarchfit(myspec.igarch.snorm, 
                                        data = training.set,
                                        out.sample = testset.length,
                                        solver = "hybrid")
        
        suppressWarnings(fc.igarch.snorm <- ugarchforecast(myfit.igarch.snorm, 
                                                           n.ahead = 1,
                                                           n.roll = testset.length - 1))
        
        fc.volatility.igarch.snorm <- (fc.igarch.snorm@forecast$sigmaFor)^2
        fc.mean.igarch.snorm <- fc.igarch.snorm@forecast$seriesFor         
        
        mse.igarch.snorm <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.igarch.snorm))^2 )                        # 

        ######
        myspec.egarch.snorm <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "snorm")

        myfit.egarch.snorm <- ugarchfit(myspec.egarch.snorm, 
                                        data = training.set,
                                        out.sample = testset.length,
                                        solver = "hybrid")
        
        suppressWarnings(fc.egarch.snorm <- ugarchforecast(myfit.egarch.snorm, 
                                                           n.ahead = 1,
                                                           n.roll = testset.length - 1))
        
        fc.volatility.egarch.snorm <- (fc.egarch.snorm@forecast$sigmaFor)^2
        fc.mean.egarch.snorm <- fc.egarch.snorm@forecast$seriesFor         
        
        mse.egarch.snorm <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.egarch.snorm))^2 )                        #         # 

        ######
        myspec.gjrgarch.snorm <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                            mean.model = list(armaOrder = c(0,0)),
                                            distribution.model = "snorm")

        myfit.gjrgarch.snorm <- ugarchfit(myspec.gjrgarch.snorm, 
                                        data = training.set,
                                        out.sample = testset.length,
                                        solver = "hybrid")
        
        suppressWarnings(fc.gjrgarch.snorm <- ugarchforecast(myfit.gjrgarch.snorm, 
                                                           n.ahead = 1,
                                                           n.roll = testset.length - 1))
        
        fc.volatility.gjrgarch.snorm <- (fc.gjrgarch.snorm@forecast$sigmaFor)^2
        fc.mean.gjrgarch.snorm <- fc.gjrgarch.snorm@forecast$seriesFor         
        
        mse.gjrgarch.snorm <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.gjrgarch.snorm))^2 )                        #         #         # 
        ######

        myspec.sgarch.std <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "std")

        myfit.sgarch.std <- ugarchfit(myspec.sgarch.std, 
                                          data = training.set,
                                          out.sample = testset.length,
                                          solver = "hybrid")
        
        suppressWarnings(fc.sgarch.std <- ugarchforecast(myfit.sgarch.std, 
                                                             n.ahead = 1,
                                                             n.roll = testset.length - 1))
        
        fc.volatility.sgarch.std <- (fc.sgarch.std@forecast$sigmaFor)^2
        fc.mean.sgarch.std <- fc.sgarch.std@forecast$seriesFor         
        
        mse.sgarch.std <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.sgarch.std))^2 )                        #         #         # 


        ######
        myspec.igarch.std <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "std")

        myfit.igarch.std <- ugarchfit(myspec.igarch.std, 
                                      data = training.set,
                                      out.sample = testset.length,
                                      solver = "hybrid")
        
        suppressWarnings(fc.igarch.std <- ugarchforecast(myfit.igarch.std, 
                                                         n.ahead = 1,
                                                         n.roll = testset.length - 1))
        
        fc.volatility.igarch.std <- (fc.igarch.std@forecast$sigmaFor)^2
        fc.mean.igarch.std <- fc.igarch.std@forecast$seriesFor         
        
        mse.igarch.std <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.igarch.std))^2 )                        #         #         # 


        ######
        myspec.egarch.std <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "std")

        myfit.egarch.std <- ugarchfit(myspec.egarch.std, 
                                      data = training.set,
                                      out.sample = testset.length,
                                      solver = "hybrid")
        
        suppressWarnings(fc.egarch.std <- ugarchforecast(myfit.egarch.std, 
                                                         n.ahead = 1,
                                                         n.roll = testset.length - 1))
        
        fc.volatility.egarch.std <- (fc.egarch.std@forecast$sigmaFor)^2
        fc.mean.egarch.std <- fc.egarch.std@forecast$seriesFor         
        
        mse.egarch.std <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.egarch.std))^2 )                        #         #         # 
        
        ######
        myspec.gjrgarch.std <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "std")

        myfit.gjrgarch.std <- ugarchfit(myspec.gjrgarch.std, 
                                      data = training.set,
                                      out.sample = testset.length,
                                      solver = "hybrid")
        
        suppressWarnings(fc.gjrgarch.std <- ugarchforecast(myfit.gjrgarch.std, 
                                                         n.ahead = 1,
                                                         n.roll = testset.length - 1))
        
        fc.volatility.gjrgarch.std <- (fc.gjrgarch.std@forecast$sigmaFor)^2
        fc.mean.gjrgarch.std <- fc.gjrgarch.std@forecast$seriesFor         
        
        mse.gjrgarch.std <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.gjrgarch.std))^2 )                        #         #         # 
        
        ######
        myspec.sgarch.sstd <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sstd")

        myfit.sgarch.sstd <- ugarchfit(myspec.sgarch.sstd, 
                                        data = training.set,
                                        out.sample = testset.length,
                                        solver = "hybrid")
        
        suppressWarnings(fc.sgarch.sstd <- ugarchforecast(myfit.sgarch.sstd, 
                                                           n.ahead = 1,
                                                           n.roll = testset.length - 1))
        
        fc.volatility.sgarch.sstd <- (fc.sgarch.sstd@forecast$sigmaFor)^2
        fc.mean.sgarch.sstd <- fc.sgarch.sstd@forecast$seriesFor         
        
        mse.sgarch.sstd <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.sgarch.sstd))^2 )                        #         #         # 
        
        ######
        myspec.igarch.sstd <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sstd")

        myfit.igarch.sstd <- ugarchfit(myspec.igarch.sstd, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")
        
        suppressWarnings(fc.igarch.sstd <- ugarchforecast(myfit.igarch.sstd, 
                                                          n.ahead = 1,
                                                          n.roll = testset.length - 1))
        
        fc.volatility.igarch.sstd <- (fc.igarch.sstd@forecast$sigmaFor)^2
        fc.mean.igarch.sstd <- fc.igarch.sstd@forecast$seriesFor         
        
        mse.igarch.sstd <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.igarch.sstd))^2 )                        #         #         # 
        
        ######
        myspec.egarch.sstd <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sstd")

        myfit.egarch.sstd <- ugarchfit(myspec.egarch.sstd, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")
        
        suppressWarnings(fc.egarch.sstd <- ugarchforecast(myfit.egarch.sstd, 
                                                          n.ahead = 1,
                                                          n.roll = testset.length - 1))
        
        fc.volatility.egarch.sstd <- (fc.egarch.sstd@forecast$sigmaFor)^2
        fc.mean.egarch.sstd <- fc.egarch.sstd@forecast$seriesFor         
        
        mse.egarch.sstd <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.egarch.sstd))^2 )                        #         #         # 
        
        ######
        myspec.gjrgarch.sstd <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(0,0)),
                                           distribution.model = "sstd")

        myfit.gjrgarch.sstd <- ugarchfit(myspec.gjrgarch.sstd, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")
        
        suppressWarnings(fc.gjrgarch.sstd <- ugarchforecast(myfit.gjrgarch.sstd, 
                                                          n.ahead = 1,
                                                          n.roll = testset.length - 1))
        
        fc.volatility.gjrgarch.sstd <- (fc.gjrgarch.sstd@forecast$sigmaFor)^2
        fc.mean.gjrgarch.sstd <- fc.gjrgarch.sstd@forecast$seriesFor         
        
        mse.gjrgarch.sstd <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.gjrgarch.sstd))^2 )                        #         #         # 
        
        ######
        myspec.sgarch.ged <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "ged")

        myfit.sgarch.ged <- ugarchfit(myspec.sgarch.ged, 
                                         data = training.set,
                                         out.sample = testset.length,
                                         solver = "hybrid")
        
        suppressWarnings(fc.sgarch.ged <- ugarchforecast(myfit.sgarch.ged, 
                                                            n.ahead = 1,
                                                            n.roll = testset.length - 1))
        
        fc.volatility.sgarch.ged <- (fc.sgarch.ged@forecast$sigmaFor)^2
        fc.mean.sgarch.ged <- fc.sgarch.ged@forecast$seriesFor         
        
        mse.sgarch.ged <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.sgarch.ged))^2 )                        #         #         # 

        ######
        myspec.igarch.ged <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "ged")

        myfit.igarch.ged <- ugarchfit(myspec.igarch.ged, 
                                      data = training.set,
                                      out.sample = testset.length,
                                      solver = "hybrid")
        
        suppressWarnings(fc.igarch.ged <- ugarchforecast(myfit.igarch.ged, 
                                                         n.ahead = 1,
                                                         n.roll = testset.length - 1))
        
        fc.volatility.igarch.ged <- (fc.igarch.ged@forecast$sigmaFor)^2
        fc.mean.igarch.ged <- fc.igarch.ged@forecast$seriesFor         
        
        mse.igarch.ged <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.igarch.ged))^2 )                        #         #         # 

        ######
        myspec.gjrgarch.ged <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "ged")

        myfit.gjrgarch.ged <- ugarchfit(myspec.gjrgarch.ged, 
                                      data = training.set,
                                      out.sample = testset.length,
                                      solver = "hybrid")
        
        suppressWarnings(fc.gjrgarch.ged <- ugarchforecast(myfit.gjrgarch.ged, 
                                                         n.ahead = 1,
                                                         n.roll = testset.length - 1))
        
        fc.volatility.gjrgarch.ged <- (fc.gjrgarch.ged@forecast$sigmaFor)^2
        fc.mean.gjrgarch.ged <- fc.gjrgarch.ged@forecast$seriesFor         
        
        mse.gjrgarch.ged <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.gjrgarch.ged))^2 )                        #         #         # 


        ######
        myspec.sgarch.sged <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sged")

        myfit.sgarch.sged <- ugarchfit(myspec.sgarch.sged, 
                                        data = training.set,
                                        out.sample = testset.length,
                                        solver = "hybrid")
        
        suppressWarnings(fc.sgarch.sged <- ugarchforecast(myfit.sgarch.sged, 
                                                           n.ahead = 1,
                                                           n.roll = testset.length - 1))
        
        fc.volatility.sgarch.sged <- (fc.sgarch.sged@forecast$sigmaFor)^2
        fc.mean.sgarch.sged <- fc.sgarch.sged@forecast$seriesFor         
        
        mse.sgarch.sged <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.sgarch.sged))^2 )                        #         #         # 
        
        ######
        myspec.igarch.sged <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sged")

        myfit.igarch.sged <- ugarchfit(myspec.igarch.sged, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")
        
        suppressWarnings(fc.igarch.sged <- ugarchforecast(myfit.igarch.sged, 
                                                          n.ahead = 1,
                                                          n.roll = testset.length - 1))
        
        fc.volatility.igarch.sged <- (fc.igarch.sged@forecast$sigmaFor)^2
        fc.mean.igarch.sged <- fc.igarch.sged@forecast$seriesFor         
        
        mse.igarch.sged <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.igarch.sged))^2 )                        #         #         # 


        ######
        myspec.gjrgarch.sged <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(0,0)),
                                           distribution.model = "sged")

        myfit.gjrgarch.sged <- ugarchfit(myspec.gjrgarch.sged, 
                                       data = training.set,
                                       out.sample = testset.length,
                                       solver = "hybrid")
        
        suppressWarnings(fc.gjrgarch.sged <- ugarchforecast(myfit.gjrgarch.sged, 
                                                          n.ahead = 1,
                                                          n.roll = testset.length - 1))
        
        fc.volatility.gjrgarch.sged <- (fc.gjrgarch.sged@forecast$sigmaFor)^2
        fc.mean.gjrgarch.sged <- fc.gjrgarch.sged@forecast$seriesFor         
        
        mse.gjrgarch.sged <- mean((as.numeric(monthly.RV) - as.numeric(fc.volatility.gjrgarch.sged))^2 )                        #         #         # 
        
        
        ######
        
        mse.name <- c("mse.sgarch.norm" ,
                      "mse.sgarch.snorm",
                      "mse.sgarch.std",
                      "mse.sgarch.sstd",
                      "mse.sgarch.ged",
                      "mse.sgarch.sged",
                      "mse.igarch.norm" ,
                      "mse.igarch.snorm",
                      "mse.igarch.std",
                      "mse.igarch.sstd",
                      "mse.igarch.ged",
                      "mse.igarch.sged",
                      "mse.egarch.norm",
                      "mse.egarch.snorm",
                      "mse.egarch.std",
                      "mse.egarch.sstd",
                      "mse.gjrgarch.norm",
                      "mse.gjrgarch.snorm",
                      "mse.gjrgarch.std",
                      "mse.gjrgarch.sstd",
                      "mse.gjrgarch.ged",
                      "mse.gjrgarch.sged"
                      )
        
        mse.value <- rbind(mse.sgarch.norm ,
                           mse.sgarch.snorm,
                           mse.sgarch.std,
                           mse.sgarch.sstd,
                           mse.sgarch.ged,
                           mse.sgarch.sged,
                           mse.igarch.norm ,
                           mse.igarch.snorm,
                           mse.igarch.std,
                           mse.igarch.sstd,
                           mse.igarch.ged,
                           mse.igarch.sged,
                           mse.egarch.norm,
                           mse.egarch.snorm,
                           mse.egarch.std,
                           mse.egarch.sstd,
                           mse.gjrgarch.norm,
                           mse.gjrgarch.snorm,
                           mse.gjrgarch.std,
                           mse.gjrgarch.sstd,
                           mse.gjrgarch.ged,
                           mse.gjrgarch.sged
                            )
        
        #browser()
        mse.df <- data.frame(name = mse.name, mse = round(mse.value,digits = 2))
        
        mse.df <- (mse.df[order(mse.df$mse),])
   
        row.names(mse.df) <- c(1:length(mse.df$mse))

        print(mse.df)
        
        cat("\n")
        
       
        
}