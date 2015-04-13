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

# Read in the two children dataset
modchdat <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/CD4Cat_Children_Modified2015-03-11.csv")
useSubset <- T
# Select a subgroup of people
subselect <- function(addata,n){ 
  num.to.do <- n ## to save computing time when playing with analyses
  pids.to.run1 <- sample(unique(addata$patient), num.to.do)
  addata <- addata[addata$patient %in% pids.to.run1,] 
  return(addata)
}
dat <- subselect(modchdat, 500)
modchdata <- dat
# Replacing small neg zscores by -12
modchdata$zscore[modchdata$zscore < -10] <- -10

# Removing row with TOFU > 15
modchdata <- modchdata[modchdata$diff < 5476,]

currentDate <- Sys.Date()

# ##############################################################################################
# ################      Model Building         #################################################
# ##############################################################################################


# ===================== Children per CD4 Categories ==========================

testchdata <- modchdata #[addata$suppress == 1,]      #subselect(addata,2000)
nn <- nlevels(testchdata$cd4a.categ)
mydataa <- groupedData(zscore ~ diff | cd4a.categ/patient, data = testchdata,order.groups=F) # Only On-ART  period

# plot(mydataa, display = 1, collapse =1, )

# Order the data and get intial values for the parameter estimates
mydataa <- mydataa[order(mydataa$cd4a.categ, mydataa$patient,mydataa$diff),]  
model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
              start=c(Asym=-2,R0=-4,lrc=-5), control = nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/4096,
                                                                   printEval = FALSE, warnOnly = FALSE))
# model.lis <- nlsList(SSasymp,mydataa, na.action = "na.omit")
# model1.lis <- nlsList(zscore ~ SSasymp(diff,Asym,R0,lrc)|patient, data=mydataa,na.action="na.omit")#,
#                start=c(Asym=-2,R0=-4,lrc=-5))
cof <- coef(summary(model1))
init = cof[1:3] #c(Asym = 450, R0 = 380, lrc = -6)

# # ======================================================================
# # Single level models
# #Random effect defined for CD4 categories level
# model2a <- nlme(zscore ~ SSasymp(diff,Asym,R0,lrc),
#                data = mydataa, 
#                na.action="na.omit",
#                random = Asym + R0 ~ 1|cd4a.categ, 
#                fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
#                start= init, verbose = FALSE )
# 
# #Random effect defined for patient level
# model2b <- nlme(zscore ~ SSasymp(diff,Asym,R0,lrc),
#                data = mydataa, 
#                na.action="na.omit",
#                random = Asym + R0 ~ 1|patient, 
#                fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
#                start= init, verbose = FALSE )
# 
# # Look at both outputs.
# summary(model2a)
# #          StdDev    Corr 
# # Asym     0.5431035 Asym 
# # R0       2.4207631 0.989
# # Residual 1.9394735 
# summary(model2b)
# #          StdDev   Corr 
# # Asym     1.748278 Asym 
# # R0       2.847446 0.482
# # Residual 1.369344

# ======================================================================
# Multiple levels models
# Random effect defined for CD4 categories and patient levels
model3 <- nlme(zscore ~ SSasymp(diff,Asym,R0,lrc),
               data = mydataa, 
               na.action="na.omit",
               random = Asym + R0 ~ 1|cd4a.categ/patient,
               fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
               start= init, verbose = FALSE ) 

# Random effects:
#   Formula: list(Asym ~ 1, R0 ~ 1)
# Level: cd4a.categ
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr 
# Asym 0.6416563 Asym 
# R0   2.5075151 0.993
# 
# Formula: list(Asym ~ 1, R0 ~ 1)
# Level: patient %in% cd4a.categ
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr 
# Asym     1.6152819 Asym 
# R0       0.8913627 0.488
# Residual 1.3534584
# Fixed effects: list(Asym ~ 1, R0 ~ 1, lrc ~ 1) 
# Value Std.Error     DF   t-value p-value
# Asym -1.842215 0.2624473 106206   -7.0194   0e+00
# R0   -3.448519 1.0237622 106206   -3.3685   8e-04
# lrc  -5.165131 0.0114065 106206 -452.8233   0e+00
# Correlation: 
#   Asym   R0    
# R0   0.991       
# lrc -0.012 -0.001
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -7.28166928 -0.33451091  0.02695033  0.44571046  5.93587097 
print("Model 3 with  fixed effect and a two levels random effect.")
summary(model3)

# --------------------------------------
# Model 3 corrected for independant random effects on both levels
model4 <- update(model3, random = list(cd4a.categ = pdDiag(Asym + R0 ~ 1),patient = pdDiag(Asym + R0 ~ 1)))
print("Model3 corrected for independant random effects on both levels.")
summary(model4)
# Random effects:
#   Formula: list(Asym ~ 1, R0 ~ 1)
# Level: cd4a.categ
# Structure: Diagonal
# Asym       R0
# StdDev: 0.6691076 2.512659
# 
# Formula: list(Asym ~ 1, R0 ~ 1)
# Level: patient %in% cd4a.categ
# Structure: Diagonal
# Asym        R0 Residual
# StdDev: 1.626208 0.8844076 1.357189
# Fixed effects: list(Asym ~ 1, R0 ~ 1, lrc ~ 1) 
# Value Std.Error     DF   t-value p-value
# Asym -1.851356 0.2736561 106206   -6.7653   0e+00
# R0   -3.451611 1.0258625 106206   -3.3646   8e-04
# lrc  -5.120753 0.0112202 106206 -456.3880   0e+00
# Correlation: 
#   Asym   R0    
# R0   0.000       
# lrc -0.012 -0.001
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -7.2437852 -0.3312831  0.0259728  0.4409720  6.0682170
anova(model3, model4)

# ----------------------------
# Model 4 corrected for 
model5 <- update(model4, weights = varIdent( 0.2, ~ 1|cd4a.categ))
print("Model3 corrected for heteroscedasticity.")
summary(model5)
# Random effects:
#   Formula: list(Asym ~ 1, R0 ~ 1)
# Level: cd4a.categ
# Structure: Diagonal
# Asym       R0
# StdDev: 0.7056194 2.515581
# 
# Formula: list(Asym ~ 1, R0 ~ 1)
# Level: patient %in% cd4a.categ
# Structure: Diagonal
# Asym        R0 Residual
# StdDev: 1.519648 0.7164818 1.841432
# 
# Variance function:
#   Structure: Different standard deviations per stratum
# Formula: ~1 | cd4a.categ 
# Parameter estimates:
#   (0,200] (1e+03,5e+03]     (200,400]     (400,600]     (600,800]   (800,1e+03] 
# 1.0000000     0.5980873     0.6800324     0.6797960     0.6396125     0.6930978 
# Fixed effects: list(Asym ~ 1, R0 ~ 1, lrc ~ 1) 
# Value Std.Error     DF   t-value p-value
# Asym -1.885244 0.2884773 106206   -6.5352   0e+00
# R0   -3.462118 1.0270442 106206   -3.3710   7e-04
# lrc  -4.969925 0.0139871 106206 -355.3233   0e+00
# Correlation: 
#   Asym   R0    
# R0   0.000       
# lrc -0.011 -0.001
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -8.6835304 -0.3597126  0.0378315  0.4753506  5.5984552 
anova(model4, model5)

# -------------------------------------------------------
# Univariate models for different covariates
model6age <- update(model3, random = Asym + R0 ~ age )

model6who <- update(model3, random = Asym + R0 ~ fhv_stage_who |cd4a.categ/patient)
model6viral <- update(model3, random = Asym + R0 ~ viral |cd4a.categ/patient)

summary(model5)
anova(model4,model5)

summary(model6)
par(mfrow=c(2,1))
plot(model6)
plot(model5)
anova(model3,model4)

model6 <- update(model4, weights = varConstPower(power = 0.1))

plot(model3, resid(.) ~ diff | cd4a.categ,
     panel = function (x,y,...){
       panel.grid()
       panel.xyplot(x,y)
       panel.loess(x,y,lty = 2)
       panel.abline(0,0)
     })
intervals(model3, which = "var-cov")
anova(model2,model3)
mod <- update(model3, method = "ML")
intervals(model2, which = "var-cov")
model4 <- update(model3, random = list(cd4a.categ = pdDiag(Asym + R0 ~ 1),patient = pdDiag(Asym + R0 ~ 1)))
print("Removed correlation within the parameters")
summary(model4)

model5 <- update(model3,random = list(cd4a.categ = pdDiag(Asym + R0 + lrc ~ 1|cd4a.categ )))
anova(model3, model5)

  zscore ~ SSasymp(diff,Asym,R0,lrc),
               data = mydataa, #we want this to be mydataa right?  not mydata?
               na.action="na.omit",
               random = Asym + R0 ~ 1|cd4a.categ/patient,
               fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),                        
               start= init, verbose = FALSE ) 





 model33 <- nlme(zscore ~ SSasymp(diff,Asym,R0,lrc),
               data = mydataa, #we want this to be mydataa right?  not mydata?
               na.action="na.omit",
               random = Asym + R0 ~ 1|cd4a.categ,
               fixed = list(Asym ~ age + gender, R0 ~ age + gender, lrc ~ 1),#,+ fhv_stage_who + gender + suppress + weight + height, R0 ~ 1, lrc ~ 1),                        
               start= c(Asym=-2,5,1,R0=-4,5,1,lrc=-5), verbose = FALSE )
 
# ================================ Asy .vs. Int ===============================
r <- ranef(model3)
f <- fixef(model3)
r<- c( -2.840674, -1.049151, -2.377164, -1.908371, -1.602952)
i <- c( -8.298243, -0.790337, -4.767319, -3.075800, -2.160361)

png("Output/Asym_Int.png", w=480,h = 480)
plot(i,r, xlab = "Baseline z-score",pch = 15, ylab = "Long term z-score", col=1:6, main = "Relation between Asy & Int")
#legend("toplef", levels(testchdata)[-6], col = 1:6, lty = 1, cex=0.7)
dev.off()

# =============================================================================

barplot(r$Asym, names.arg = rownames(r))
barplot(r$R0, names.arg = rownames(r))
png("Output/Rebound_Int-Asym.png", w=480,h = 480)
barplot(r$Asym - r$R0, names.arg = rownames(r))
dev.off
# =============================================================================

# Plot residualss
png("Output/Residuals.png", w=480,h = 480)
plot(fitted(model33),residuals(model33),main="Residuals vs Fitted", cex=0.5, xlab = "Predictions", ylab= "Standardized Residuals")
dev.off()

# ##############################################################################


# ===================== Children per CD4 Categories ==========================

testchdata <- modchdata #[addata$suppress == 1,]      #subselect(addata,2000)
# #testchdata <- groupedData(zscore ~ diff | patient, data = testchdata,order.groups=F)
# 
# 
nn <- nlevels(testchdata$cd4a.categ)
mydataa <- groupedData(zscore ~ diff |patient, data = testchdata,order.groups=F) # Only On-ART  period
mydataa <- mydataa[order(mydataa$patient,mydataa$diff),]  
#print(paste0(length(unique(mydataa$patient)),'  ',sd(mydataa$lab_v), " patients count, std of CD4 counts, used for LME models"))
model1<-nls(zscore ~ SSasymp(diff,Asym,R0,lrc), data=mydataa,na.action="na.omit",
            start=c(Asym=-2,R0=-4,lrc=-5), control = nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/4096,
                                                                 printEval = FALSE, warnOnly = FALSE))
cof <- coef(summary(model1))
#print(paste("Starting with category : ", curr.cat))
print(cof)
init = cof[1:3] #c(Asym = 450, R0 = 380, lrc = -6)

model1 <- nlme(zscore ~ SSasymp(diff,Asym,R0,lrc),
               data = mydataa, #we want this to be mydataa right?  not mydata?
               na.action="na.omit",
               random = Asym + R0 ~ 1,
               fixed = list(Asym ~ 1, R0 ~ 1, lrc ~ 1),#,+ fhv_stage_who + gender + suppress + weight + height, R0 ~ 1, lrc ~ 1),                        
               start= c(Asym=-2,R0=-4,lrc=-5), verbose = FALSE ) 


model2 <- nlme(zscore ~ SSasymp(diff,Asym,R0,lrc),
               data = mydataa, #we want this to be mydataa right?  not mydata?
               na.action="na.omit",
               random = Asym + R0 ~ 1,
               fixed = list(Asym ~ age + gender, R0 ~ age + gender, lrc ~ 1),#,+ fhv_stage_who + gender + suppress + weight + height, R0 ~ 1, lrc ~ 1),                        
               start= c(Asym=-2,5,1,R0=-4,5,1,lrc=-5), verbose = FALSE ) 

# ================================ Asy .vs. Int ===============================
r <- ranef(model2)
f <- fixef(model2)
plot(r$R0,r$Asym, xlab = "Baseline z-score", ylab = "Long term z-score", main = "Relation between Asy & Int")
legend("toplef", rownames(r), lty = 1, cex=0.7)

# =============================================================================

barplot(r$Asym, names.arg = rownames(r))
barplot(r$R0, names.arg = rownames(r))
barplot(r$Asym - r$R0, names.arg = rownames(r))

# =============================================================================
#plot(model33, resid(., type = "p") ~ fitted(.) | cd4a.categ, abline = 0)
plot(fitted(model33),residuals(model33),main="Residuals vs Fitted", cex=0.5)
plot(model33) # Same as above. This looks better

####################################################
# COMMENTS:
# By comparing all nlme models, model5 sems to be the best fit to our data.
# For the model formula, see refs Beaudrap (2008) and Lewis et al. (2011) in our Mendeley folder
# KR: this formula is the same as the default SSasym. The only difference between model 4 and 5 is that
# model 5 does not have a random R0 (intercept) which seems like it would be important in our
# model since people start ART at many different starting CD4 counts and this has been shown to be
# fairly important in determining what CD4 count people level off at


## Plot Model results - working
modelpred.c1 <-as.numeric(predict(model2))
modelpred.c2 <-labels(predict(model2))
modelpred.c3 <-mydataa$diff
modelpred <- data.frame(modelpred.c1,modelpred.c2, modelpred.c3)

nam <- c("predictions","Ids","years")
names(modelpred) <- nam

# ====================================================== Functions

# Defined functions
med_CI <- function (x, ci = 0.90) {
  a <- median(x)
  s <- sd(x)
  n <- length(x)
  if (n == 1){
    s <- 0
    n <- 2
  }
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  return(c(upper = a + error, med = a, lower = a - error))
}

myCI_u <- function(x){
  #print(length(x))
  # x <- x[!is.na(x)]
  bb <- med_CI(x, ci = 0.90)
  return(as.numeric(bb[1]))
}

myCI_l <- function(x){
  #print(length(x))
  # x <- x[!is.na(x)]
  bb <- med_CI(x, ci = 0.90)
  return(as.numeric(bb[3]))
}


ln.fxn <- function(xx, yy, pid, order.by.xx=T) {
  #browser()
  missing.data <- is.na(xx) | is.na(yy)
  xx <- xx[!missing.data]
  yy <- yy[!missing.data]
  if(order.by.xx) {
    ord <- order(xx)
    xx <- xx[ord]
    yy <- yy[ord]
  }
  #lines(xx, yy, col = pid[1])
}

# =================================================================================

## Figures 1 - Model Output
pdf('Ch_Modelpredictions.pdf', w =10, h = 7)
xs <- seq(0, 6, by = .1) 
xlim <- range(xs)        
par('ps' = 16, mfrow = c(1,1)) ## graphical parameters
plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 z-scores',xaxt='n', yaxt='n', bty = 'n', 
     xlim = c(0,6000), ylim = c(-8,2), cex.main =0.9) # , main = "Suppressed viral load")
axis(2, at = seq(-8,2, by = 2), las = 2) ## x axis
axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
## line for each pid
print("Start ddply")
#test <- ddply(modelpred, .(Ids), with, ln.fxn(years, predictions, Ids, order.by.xx=T)) 
test <- modelpred
test <- test[order(test$years),]                    # Order with resepct to time since HAART initiation
dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
length(test$lab_v) == length(dat2[,3])                 # (diff variable)

# Transform matrix into longitudinal object's class
print("Build longitudinal object")
dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,3]))), unique(as.numeric(dat2[,3])))
#is.longitudinal(dat)  

# Calculate the medians
med <- condense.longitudinal(dat, 1, median)
confIntu <- condense.longitudinal(dat, 1, myCI_u) #CI(dat[,33],ci = 0.95)
confIntl <- condense.longitudinal(dat, 1, myCI_l) #CI(dat[,33],ci = 0.95)  
tim <- get.time.repeats(dat)
#   sp <- smooth.spline(tim$time, med, spar=0.35)
#   lines(sp, col = col.vec[ii])
lines(tim$time[180:length(tim$time)], rollmean(med, 180))#, col = col.vec[ii])
# if (ii == nn){
#   lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
# }
# if (ii == 1){
#   lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
# }
# }
title("CD4 z-scores medians' trajectory" ) #, outer=FALSE)
#legend("topright", levels(testchdata$cd4a.categ), col = 1:nn, lty = 1)
dev.off()


# AIC
anova(model1)
anova(model1,model2)
anova(model33,model3)   # Not comparable
qqnorm(model1)
qqnorm(model2)
qqnorm(model3)
qqnorm(model33)
# ============================================================================

