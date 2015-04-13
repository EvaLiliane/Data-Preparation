rm(list=ls(all=T))
library(zoo)
library(longitudinal)
library(foreign)
library(nlme) 
library(plyr) 
library(lattice)
#library(lme4)

library(mgcv) 
library(ggplot2) 
library(reshape2)

setwd("/home/evaliliane/Documents/PhD/Codes")

# Read in the two datasets
addata <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/CD4Cat_Adults2015-03-11.csv")
#chdata <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/CD4Cat_Children2015-03-11.csv") 
modchdata <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/CD4Cat_Children_Modified2015-03-11.csv")

# Replacing small neg zscores by -12
modchdata$zscore[modchdata$zscore < -10] <- -10

# Removing row with TOFU > 15
addata <- addata[addata$diff < 5476,]
modchdata <- modchdata[modchdata$diff < 5476,]

currentDate <- Sys.Date()
mypath <- file.path("/home/evaliliane/Documents/PhD/Codes/Figures/")
# Select a subgroup of people
subselect <- function(addata,n){ 
  num.to.do <- n ## to save computing time when playing with analyses
  pids.to.run1 <- sample(unique(addata$patient), num.to.do)
  addata <- addata[addata$patient %in% pids.to.run1,] 
  return(addata)
}
col.vec <- 1:10
# png("/home/evaliliane/Documents/PhD/Codes/Output/Ch",ii,"Hist.png",w =480, h = 480)
# hist(modchdata$zscore)
# dev.off()
# ##############################################################################################
# ################         NLME FIGURES        #################################################
# ##############################################################################################

# # ln.fxn <- function(xx, yy, pid, order.by.xx=F) {
# #   ##  browser()
# #   missing.data <- is.na(xx) | is.na(yy)
# #   xx <- xx[!missing.data]
# #   yy <- yy[!missing.data]
# #     ord <- order(xx)
# #     xx <- xx[ord]
# #     yy <- yy[ord]
# #   }
# #   lines(xx, yy, col = pid[1])
# # }

# ############ Tried non-linear mixed effects models

#testaddata <- testaddata[!(testaddata$base == 1 & testaddata$lab_v > 1500),]
# testaddata <- addata #[addata$suppress == 1,]      #subselect(addata,2000)
# #testaddata <- groupedData(lab_v ~ diff | patient, data = testaddata,order.groups=F)
# 
# nn <- nlevels(testaddata$cd4a.categ)
# for(ii in 4:4) {  # For (0,100] category
# 
#     curr.cat <- levels(testaddata$cd4a.categ)[ii]
#     test <- testaddata[testaddata$cd4a.categ == curr.cat,] 
#     mydataa <- groupedData(lab_v ~ diff | patient, data = test,order.groups=F) # Only On-ART  period
#     mydataa <- mydataa[order(mydataa$patient,mydataa$diff),]  
#     #print(paste0(length(unique(mydataa$patient)),'  ',sd(mydataa$lab_v), " patients count, std of CD4 counts, used for LME models"))
#     if (ii == 1){
#       model1<-nls(sqrt(lab_v) ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
#                 start=c(Asym=19,R0=7,lrc=-6), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/2048,
#                                                                        printEval = FALSE, warnOnly = FALSE))}
#     if (ii == 2){
#       model1<-nls(sqrt(lab_v) ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
#                   start=c(Asym=19,R0=12,lrc=-6), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/4096,
#                                                                        printEval = FALSE, warnOnly = FALSE))}
#     if (ii == 3){
#       model1<-nls(sqrt(lab_v) ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
#                   start=c(Asym=20,R0=15,lrc=-6.5), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/4096,
#                                                                        printEval = FALSE, warnOnly = FALSE))}    
#     if (ii == 4){
#       model1<-nls(sqrt(lab_v) ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
#                   start=c(Asym=23,R0=18,lrc=-6), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/4096,
#                                                                        printEval = FALSE, warnOnly = FALSE))}
#     cof <- coef(summary(model1))
#     print(paste("Starting with category : ", curr.cat))
#     print(cof[1:3])
#     init = cof[1:3] #c(Asym = 450, R0 = 380, lrc = -6)
#     
#     model3 <- nlme(sqrt(lab_v) ~ SSasymp(diff,Asym,R0,lrc),
#                   data = mydataa, #we want this to be mydataa right?  not mydata?
#                    na.action="na.omit",
#                    random = Asym + R0 ~ 1,
#                    fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
#                    start= init, verbose = FALSE) 
#     
#     print("Saving Figures")    
#     FigureName <- paste("/home/evaliliane/Documents/PhD/Codes/Output/AdCat",ii,"_Residual",currentDate,".pdf",sep="") 
#     pdf(FigureName, w =480, h = 480)
#     plot(model3)
#     dev.off()
#     
#     print("Saving Output")
#     FileName <- paste("/home/evaliliane/Documents/PhD/Codes/Output/AdCat",ii,"_Outputs",currentDate,".txt",sep="")    
#     res <- capture.output(summary(model3))
#     cat(res, file = FileName, sep="\n", append = FALSE)
#     ci <- capture.output(intervals(model3))
#     #cat(ci, file = FileName, sep="\n", append = TRUE)
#    
# }


# ===================== Children ==========================

 testchdata <- modchdata #[addata$suppress == 1,]      #subselect(addata,2000)
# #testchdata <- groupedData(zscore ~ diff | patient, data = testchdata,order.groups=F)
# 
# 
nn <- nlevels(testchdata$cd4a.categ)

png('Ch_Model-predictions.png', width = 480, height = 480)      
par('ps' = 16, mfrow = c(1,1)) ## graphical parameters
plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 z-scores',xaxt='n', yaxt='n', bty = 'n', 
     xlim = c(0,6000), ylim = c(-8,2), cex.main =0.9) # , main = "Suppressed viral load")
axis(2, at = seq(-8,2, by = 2), las = 2) ## x axis
axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
## line for each pid
print("Start Figure")

for(ii in 1:5) {  # For (0,100] category
  
  curr.cat <- levels(testchdata$cd4a.categ)[ii]
  #print(curr.cat)
  test <- testchdata[testchdata$cd4a.categ == curr.cat,] 
  test <- test[!is.na(test$zscore),]
  
  #print(dim(test))
  mydataa <- groupedData(zscore ~ diff | patient, data = test,order.groups=F) # Only On-ART  period
  mydataa <- mydataa[order(mydataa$patient,mydataa$diff),]  
  #print(paste0(length(unique(mydataa$patient)),'  ',sd(mydataa$lab_v), " patients count, std of CD4 counts, used for LME models"))
  if (ii == 1){
    model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
                start=c(Asym=-2.5,R0=-8,lrc=-5), control = nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/4096,
                                                                       printEval = FALSE, warnOnly = FALSE))}
  if (ii == 2){ # For 2 : start=c(Asym=-1,R0=-1.5,lrc=-4)
    model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
                start=c(Asym=-1,R0=-0.5,lrc=-3), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/4096,
                                                                       printEval = FALSE, warnOnly = FALSE))}
  if (ii %in% 3:5){
    model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
                start=c(Asym=-2,R0=-4,lrc=-5), control = nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/4096,
                                                                     printEval = FALSE, warnOnly = FALSE))}
  if (ii == 6){ # For 2 : start=c(Asym=-1,R0=-1.5,lrc=-4)
    model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
                start=c(Asym=-1.3,R0=-1.5,lrc=-4.5), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/4096,
                                                                           printEval = FALSE, warnOnly = FALSE))}
  cof <- coef(summary(model1))
  print(paste("Starting with category : ", curr.cat))
  #print(cof)
  init = cof[1:3] #c(Asym = 450, R0 = 380, lrc = -6)
  
  model3 <- nlme(zscore ~ SSasymp(diff,Asym,R0,lrc),
                 data = mydataa, #we want this to be mydataa right?  not mydata?
                 na.action="na.omit",
                 random = Asym + R0 ~ 1|patient,
                 fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
                 start= init, verbose = FALSE ) 
  
  f <- fixef(model3)
  print(f)
  t <- seq(0,5000,25)
  out <- f[1] + (f[2] - f[1]) * exp(-exp(-6) * t)
  lines(t,out,col = col.vec[ii])
  #plot(r$R0,r$Asym, xlab = "Baseline z-score", ylab = "Long term z-score", main = "Relation between Asy & Int")
  #legend("toplef", rownames(r), lty = 1, cex=0.7)
  
  
  ## Plot Model results - working
#   modelpred.c1 <-as.numeric(predict(model3))
#   print(head(predict(model3)))
#   modelpred.c2 <-labels(predict(model3))
#   modelpred.c3 <-seq(0,5000,25)
#   modelpred.c4 <- rep(curr.cat, length(modelpred.c3))
#   modelpred <- data.frame(modelpred.c1,modelpred.c2, modelpred.c3, modelpred.c4)
#   
#   nam <- c("predictions","Ids","years", "cd4.categ")
#   names(modelpred) <- nam
# 
#   #test <- ddply(modelpred, .(Ids), with, ln.fxn(years, predictions, Ids, order.by.xx=T)) 
#   test <- modelpred
#   test <- test[order(test$years),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,3])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   print("Build longitudinal object")
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,3]))), unique(as.numeric(dat2[,3])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 1, median)
# #   confIntu <- condense.longitudinal(dat, 1, myCI_u) #CI(dat[,33],ci = 0.95)
# #   confIntl <- condense.longitudinal(dat, 1, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
  print("Done plotting")
}
title("CD4 z-scores predictions" ) #, outer=FALSE)
legend("topleft", levels(testchdata$cd4a.categ)[-6], col = 1:5, lty = 1, cex = 0.8)
dev.off()




#   
# FigureName <- paste("/home/evaliliane/Documents/PhD/Codes/Output/Ch_All-Residual2",currentDate,".png")
# png(FigureName, w =480, h = 480)
# par('ps' = 16, mfrow = c(2,3)) ## graphical parameters
# #qqnorm(model3, col=mydataa$fhv_stage_who)
# #plot(model3, gender ~ resid(.))
# for(ii in 1:5) {  # For (0,100] category
#   
#   curr.cat <- levels(testchdata$cd4a.categ)[ii]
#   print(curr.cat)
#   test <- testchdata[testchdata$cd4a.categ == curr.cat,] 
#   test <- test[!is.na(test$zscore),]
#   
#   print(dim(test))
#   mydataa <- groupedData(zscore ~ diff | patient, data = test,order.groups=F) # Only On-ART  period
#   mydataa <- mydataa[order(mydataa$patient,mydataa$diff),]  
#   #print(paste0(length(unique(mydataa$patient)),'  ',sd(mydataa$lab_v), " patients count, std of CD4 counts, used for LME models"))
#   if (ii == 1){
#     model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
#                 start=c(Asym=-2.5,R0=-8,lrc=-5), control = nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/4096,
#                                                                        printEval = FALSE, warnOnly = FALSE))}
#   if (ii == 2){ # For 2 : start=c(Asym=-1,R0=-1.5,lrc=-4)
#     model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
#                 start=c(Asym=-1,R0=-0.5,lrc=-3), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/4096,
#                                                                        printEval = FALSE, warnOnly = FALSE))}
#   if (ii %in% 3:5){
#     model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
#                 start=c(Asym=-2,R0=-4,lrc=-5), control = nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/4096,
#                                                                      printEval = FALSE, warnOnly = FALSE))}
#   if (ii == 6){ # For 2 : start=c(Asym=-1,R0=-1.5,lrc=-4)
#     model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
#                 start=c(Asym=-1.3,R0=-1.5,lrc=-4.5), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/4096,
#                                                                            printEval = FALSE, warnOnly = FALSE))}
#   cof <- coef(summary(model1))
#   print(paste("Starting with category : ", curr.cat))
#   #print(cof)
#   init = cof[1:3] #c(Asym = 450, R0 = 380, lrc = -6)
#   
#   model3 <- nlme(zscore ~ SSasymp(diff,Asym,R0,lrc),
#                  data = mydataa, #we want this to be mydataa right?  not mydata?
#                  na.action="na.omit",
#                  weights = varPower(0.2, ~ diff),
#                  random = Asym + R0 ~ 1|patient,
#                  fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
#                  start= init, verbose = FALSE )   
#   
#   plot(fitted(model3),residuals(model3),main="Residuals vs Fitted", cex=0.5, xlab = "Predictions", ylab= "Standardized Residuals")
#     
#   print("Saving Output")
#   FileName <- paste("/home/evaliliane/Documents/PhD/Codes/Output/ChCat",ii,"_Outputs",currentDate,".txt",sep="")  
#   cat(curr.cat, append = FALSE)
#   res <- capture.output(summary(model3))
#   cat(res, file = FileName, sep="\n", append = TRUE)
#   ci <- capture.output(intervals(model3))
#   cat(ci, file = FileName, sep="\n", append = TRUE)
# }
# 
# dev.off()
# print("Done")



# 
# #qqnorm(lme2, col= dataf$roostsitu)
# ns <- nn + 1
# n <- nlevels(testaddata$cd4a.categ)- 1
# for(ii in ns:n) {  # For (100,200] category
#   
#   curr.cat <- levels(testaddata$cd4a.categ)[ii]
#   test <- testaddata[testaddata$cd4a.categ == curr.cat,]
#   limits <- c(1095,2190,3650 )
#   for(j in 1:3) {
#     pat.lon <- unique(test$patient[which(test$diff > limits[j])])
#     test <- test[test$patient %in% pat.lon,] 
#     mydataa <- groupedData(lab_v ~ diff | patient, data = test,order.groups=F) # Only On-ART  period
#     mydataa <- mydataa[order(mydataa$patient,mydataa$lab_dmy),]  
#     print(paste0(length(mydataa$lab_v), " rows used for LME models"))
#     model3 <- nlme(lab_v ~ SSasymp(diff,Asym,R0,lrc),
#                    data = mydataa, #we want this to be mydataa right?  not mydata?
#                    na.action="na.omit",
#                    random = Asym + R0 ~ 1,
#                    fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
#                    start=c(Asym = 420, R0 = 130, lrc = -6)) 
#     
#     res <- capture.output(summary(model3))
#     FileName <- paste("/home/evaliliane/Documents/PhD/Codes/Figures/Cat",ii,"_Outputs",j,".txt",sep="")
#     cat(res, file = FileName, sep="\n", append = FALSE)
#     ci <- capture.output(intervals(model3))
#     cat(ci, file = FileName, sep="\n", append = TRUE)
#     
#     FigureName <- paste("/home/evaliliane/Documents/PhD/Codes/Figures/Cat",ii,"_Residual",j,".png",sep="")
#     
#     png(paste("home/evaliliane/Documents/PhD/Codes/Figures/cat",ii,"_Residuals",j,".png",sep=""), w =480, h = 480)
#     plot(model3)
#     dev.off() 
#     
#     png(FigureName, w =480, h = 480)
#     plot(model3)
#     dev.off()
#   }
# }
# 
# control = nlmeControl(pnlsTol = 0.01, msVerbose = TRUE)
# 
# nt <- n + 1
# for(ii in nt:nt) {  # For (200,300] category
#   curr.cat <- levels(testaddata$cd4a.categ)[ii]
#   test <- testaddata[testaddata$cd4a.categ == curr.cat,]  # & testaddata$lab_v < 2000,]
#   limits <- c(1095,2190,3650 )
#   for(j in 1:3) {
#     pat.lon <- unique(test$patient[which(test$diff > limits[j])])
#     test <- test[test$patient %in% pat.lon,] 
#     mydataa <- groupedData(lab_v ~ diff | patient, data = test,order.groups=F) # Only On-ART  period
#     mydataa <- mydataa[order(mydataa$patient,mydataa$lab_dmy),]  
#     print(paste0(length(mydataa$lab_v), " rows used for LME models"))
#     model3 <- nlme(sqrt(lab_v) ~ SSasymp(diff,Asym,R0,lrc),
#                    data = mydataa, #we want this to be mydataa right?  not mydata?
#                    na.action="na.omit", 
#                    #re.paramtr = "cholesky",
#                    random = Asym + R0 ~ 1|patient,
#                    fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
#                    start=c(Asym = 450, R0 = 250, lrc = -6)) 
#     
#     res <- capture.output(summary(model3))
#     FileName <- paste("/home/evaliliane/Documents/PhD/Codes/Figures/Cat",ii,"_Outputs",j,".txt",sep="")
#     cat(res, file = FileName, sep="\n", append = FALSE)
#     ci <- capture.output(intervals(model3))
#     cat(ci, file = FileName, sep="\n", append = TRUE)
#     
#     FigureName <- paste("/home/evaliliane/Documents/PhD/Codes/Figures/Cat",ii,"_Residual",j,".png",sep="")
#     
#     png(paste("home/evaliliane/Documents/PhD/Codes/Figures/cat",ii,"_Residuals",j,".png",sep=""), w =480, h = 480)
#     plot(model3)
#     dev.off() 
#     
#     png(FigureName, w =480, h = 480)
#     plot(model3)
#     dev.off()
#   }
# }
# 