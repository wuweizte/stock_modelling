
CompareGarchAicARMA00 <- function(arg.training.set, arg.difference){
        
        

        ####
        if(arg.difference == 0){
                training.set <- arg.training.set
        }else{
                training.set <- diff(arg.training.set)
        }
        
        ####
        myspec.sgarch.norm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "norm")
        
        myfit.sgarch.norm <- ugarchfit(myspec.sgarch.norm, data = training.set, solver = "hybrid")
        

        (inf.sgarch.norm <- t(infocriteria(myfit.sgarch.norm)))
        
        
        ####
        myspec.sgarch.norm.archm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0),
                                                           archm = TRUE,
                                                           archpow = 2),
                                         distribution.model = "norm")
        
        myfit.sgarch.norm.archm <- ugarchfit(myspec.sgarch.norm.archm, data = training.set, solver = "hybrid")
        
        
        (inf.sgarch.norm.archm <- t(infocriteria(myfit.sgarch.norm.archm)))
        
        ####
        myspec.igarch.norm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "norm")
        
        myfit.igarch.norm <- ugarchfit(myspec.igarch.norm, data = training.set, solver = "hybrid")

        (inf.igarch.norm <- t(infocriteria(myfit.igarch.norm)))
        
   
        ####
        myspec.igarch.norm.archm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0),
                                                           archm = TRUE,
                                                           archpow = 2),
                                         distribution.model = "norm")

        myfit.igarch.norm.archm <- ugarchfit(myspec.igarch.norm.archm, data = training.set, solver = "hybrid")

        (inf.igarch.norm.archm <- t(infocriteria(myfit.igarch.norm.archm)))
        
        ####
        myspec.egarch.norm <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "norm")
        
        myfit.egarch.norm <- ugarchfit(myspec.egarch.norm, data = training.set, solver = "hybrid")
        

        (inf.egarch.norm <- t(infocriteria(myfit.egarch.norm)))
        
   
        ####
        # myspec.egarch.norm.archm <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
        #                                  mean.model = list(armaOrder = c(0,0),
        #                                                    archm = TRUE,
        #                                                    archpow = 2),
        #                                  distribution.model = "norm")
        # 
        # myfit.egarch.norm.archm <- ugarchfit(myspec.egarch.norm.archm, data = training.set, solver = "hybrid")
        # 

        # (inf.egarch.norm.archm <- t(infocriteria(myfit.egarch.norm.archm)))
        
        ###########
        myspec.gjrgarch.norm <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(0,0)),
                                           distribution.model = "norm")
        
        myfit.gjrgarch.norm <- ugarchfit(myspec.gjrgarch.norm, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.norm <- t(infocriteria(myfit.gjrgarch.norm)))
        
        ###########
        myspec.gjrgarch.norm.archm <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(0,0),
                                                             archm = TRUE,
                                                             archpow = 2),
                                           distribution.model = "norm")
        
        myfit.gjrgarch.norm.archm <- ugarchfit(myspec.gjrgarch.norm.archm, data = training.set, solver = "hybrid")
        
        
        (inf.gjrgarch.norm.archm <- t(infocriteria(myfit.gjrgarch.norm.archm)))
        
        ##################
        myspec.sgarch.snorm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "snorm")
        
        myfit.sgarch.snorm <- ugarchfit(myspec.sgarch.snorm, data = training.set, solver = "hybrid")
        

        (inf.sgarch.snorm <- t(infocriteria(myfit.sgarch.snorm)))
        
        ##################
        myspec.sgarch.snorm.archm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0),
                                                            archm = TRUE,
                                                            archpow = 2),
                                          distribution.model = "snorm")
        
        myfit.sgarch.snorm.archm <- ugarchfit(myspec.sgarch.snorm.archm, data = training.set, solver = "hybrid")
        
        
        (inf.sgarch.snorm.archm <- t(infocriteria(myfit.sgarch.snorm.archm)))
        
        ################
        myspec.igarch.snorm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "snorm")
        
        myfit.igarch.snorm <- ugarchfit(myspec.igarch.snorm, data = training.set, solver = "hybrid")
        

        (inf.igarch.snorm <- t(infocriteria(myfit.igarch.snorm)))
        
        ################
        myspec.igarch.snorm.archm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0),
                                                            archm = TRUE,
                                                            archpow = 2),
                                          distribution.model = "snorm")
        
        myfit.igarch.snorm.archm <- ugarchfit(myspec.igarch.snorm.archm, data = training.set, solver = "hybrid")
        
        
        (inf.igarch.snorm.archm <- t(infocriteria(myfit.igarch.snorm.archm)))
        
        
        ######
        myspec.egarch.snorm <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "snorm")
        
        myfit.egarch.snorm <- ugarchfit(myspec.egarch.snorm, data = training.set, solver = "hybrid")
        

        (inf.egarch.snorm <- t(infocriteria(myfit.egarch.snorm)))
        
        
        ######
        myspec.gjrgarch.snorm <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                            mean.model = list(armaOrder = c(0,0)),
                                            distribution.model = "snorm")
        
        myfit.gjrgarch.snorm <- ugarchfit(myspec.gjrgarch.snorm, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.snorm <- t(infocriteria(myfit.gjrgarch.snorm)))
        
        ######
        
        myspec.sgarch.std <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "std")
        
        myfit.sgarch.std <- ugarchfit(myspec.sgarch.std, data = training.set, solver = "hybrid")
        
        
        (inf.sgarch.std <- t(infocriteria(myfit.sgarch.std)))
        

        ######
        myspec.igarch.std <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "std")
        
        myfit.igarch.std <- ugarchfit(myspec.igarch.std, data = training.set, solver = "hybrid")
        

        (inf.igarch.std <- t(infocriteria(myfit.igarch.std)))
        

        ######
        myspec.igarch.std.archm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0),
                                                          archm = TRUE,
                                                          archpow = 2),
                                        distribution.model = "std")
        
        myfit.igarch.std.archm <- ugarchfit(myspec.igarch.std.archm, data = training.set, solver = "hybrid")
        
        
        (inf.igarch.std.archm <- t(infocriteria(myfit.igarch.std.archm)))
        
        ######
        myspec.egarch.std <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "std")
        
        myfit.egarch.std <- ugarchfit(myspec.egarch.std, data = training.set, solver = "hybrid")
        

        (inf.egarch.std <- t(infocriteria(myfit.egarch.std)))
        

        ######
        myspec.gjrgarch.std <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "std")
        
        myfit.gjrgarch.std <- ugarchfit(myspec.gjrgarch.std, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.std <- t(infocriteria(myfit.gjrgarch.std)))
        

        
        ######
        myspec.sgarch.sstd <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sstd")
        
        myfit.sgarch.sstd <- ugarchfit(myspec.sgarch.sstd, data = training.set, solver = "hybrid")
        

        (inf.sgarch.sstd <- t(infocriteria(myfit.sgarch.sstd)))
        

        ######
        myspec.igarch.sstd <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sstd")
        
        myfit.igarch.sstd <- ugarchfit(myspec.igarch.sstd, data = training.set, solver = "hybrid")
        

        (inf.igarch.sstd <- t(infocriteria(myfit.igarch.sstd)))
        

        ######
        myspec.egarch.sstd <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sstd")
        
        myfit.egarch.sstd <- ugarchfit(myspec.egarch.sstd, data = training.set, solver = "hybrid")
        

        (inf.egarch.sstd <- t(infocriteria(myfit.egarch.sstd)))
        

        ######
        myspec.gjrgarch.sstd <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(0,0)),
                                           distribution.model = "sstd")
        
        myfit.gjrgarch.sstd <- ugarchfit(myspec.gjrgarch.sstd, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.sstd <- t(infocriteria(myfit.gjrgarch.sstd)))
        

        ######
        myspec.sgarch.ged <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "ged")
        
        myfit.sgarch.ged <- ugarchfit(myspec.sgarch.ged, data = training.set, solver = "hybrid")
        

        (inf.sgarch.ged <- t(infocriteria(myfit.sgarch.ged)))
        

        ######
        myspec.igarch.ged <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0)),
                                        distribution.model = "ged")
        
        myfit.igarch.ged <- ugarchfit(myspec.igarch.ged, data = training.set, solver = "hybrid")
        

        (inf.igarch.ged <- t(infocriteria(myfit.igarch.ged)))
        

        ######
        myspec.igarch.ged.archm <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                        mean.model = list(armaOrder = c(0,0),
                                                          archm = TRUE,
                                                          archpow = 2),
                                        distribution.model = "ged")
        
        myfit.igarch.ged.archm <- ugarchfit(myspec.igarch.ged.archm, data = training.set, solver = "hybrid")
        
        
        (inf.igarch.ged.archm <- t(infocriteria(myfit.igarch.ged.archm)))
        
        ######
        myspec.gjrgarch.ged <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                          mean.model = list(armaOrder = c(0,0)),
                                          distribution.model = "ged")
        
        myfit.gjrgarch.ged <- ugarchfit(myspec.gjrgarch.ged, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.ged <- t(infocriteria(myfit.gjrgarch.ged)))
        

        
        ######
        myspec.sgarch.sged <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sged")
        
        myfit.sgarch.sged <- ugarchfit(myspec.sgarch.sged, data = training.set, solver = "hybrid")
        

        (inf.sgarch.sged <- t(infocriteria(myfit.sgarch.sged)))
        

        ######
        myspec.igarch.sged <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1,1)),
                                         mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "sged")
        
        myfit.igarch.sged <- ugarchfit(myspec.igarch.sged, data = training.set, solver = "hybrid")
        

        (inf.igarch.sged <- t(infocriteria(myfit.igarch.sged)))
        

        ######
        myspec.gjrgarch.sged <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(0,0)),
                                           distribution.model = "sged")
        
        myfit.gjrgarch.sged <- ugarchfit(myspec.gjrgarch.sged, data = training.set, solver = "hybrid")
        

        (inf.gjrgarch.sged <- t(infocriteria(myfit.gjrgarch.sged)))
        

        
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
                      "inf.sgarch.norm.archm",
                      "inf.igarch.norm.archm",
                      "inf.gjrgarch.norm.archm",
                      "inf.sgarch.snorm.archm",
                      "inf.igarch.snorm.archm",
                      "inf.igarch.std.archm",
                      "inf.igarch.ged.archm"
                      )
        
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
                           inf.sgarch.norm.archm,
                           inf.igarch.norm.archm,
                           inf.gjrgarch.norm.archm,
                           inf.sgarch.snorm.archm,
                           inf.igarch.snorm.archm,
                           inf.igarch.std.archm,
                           inf.igarch.ged.archm
                            )
        
        #browser()
        aic.df <- data.frame(name = aic.name, round(as.data.frame(aic.value),digits = 2))
        
        print(aic.df[order(aic.df[[3]], aic.df[[2]]),])
        
        cat("\n")
        
       
        
}