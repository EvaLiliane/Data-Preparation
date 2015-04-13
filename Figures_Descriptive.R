rm(list=ls(all=T))
if(Sys.info()['nodename']=="evaliliane-laptop") setwd("/home/evaliliane/Documents/PhD/Codes") 
# LIBRARIES

library(zoo)
library(longitudinal); library(foreign); library(nlme); library(Rmisc)
library(geepack); library(plyr); library(base); library(ggplot2); library(mgcv) # ;  library(caTools);

# ###############################################################################################
# ###########################  BASELINE FIGURES   ##########################################
# ###############################################################################################
# 
#library(geepack); library(plyr); library(caTools); library(base); library(ggplot2); library(mgcv)

#load(file='Rdata Files/cohproc.Rdata')
addata <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/CD4Cat_Adults2015-03-11.csv")
#chdata <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/CD4Cat_Children2015-03-11.csv") 
chdata <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/CD4Cat_Children_Modified2015-03-11.csv")

addata$fhv_stage_who[addata$fhv_stage_who > 4] <-  NA
adtest <- addata[addata$base == 0,]
adtest <- adtest[!duplicated(addata$patient),]


chdata$fhv_stage_who[chdata$fhv_stage_who > 4] <-  NA
chtest <- chdata[chdata$base == 0,]
chtest <- chtest[!duplicated(chdata$patient),]
#length(adtest) == length(unique(adtest))
#chtest <- modchdata[modchdata$base == 0 & !duplicated(modchdata$patient),]

# oi.nms <- c("oropharyngealcandidiasis", "toxoplasmosisofthebrain", 
#             "unexplainedchronicdiarrhoea", "severbacterialpneumonia", 
#             "extrapulmonarytb", "pulmonarytb", 
#             "herpeszoster", "pcppneumonia", 
#             "cryptococcalmeningitis", "kaposissarcoma", 
#             "hsvinfection", "cmvretinitis", 
#             "malaria", "hepatitis", 
#             "oesophagealcandidiasis", "prurigo", 
#             "lymphoma", "guilianbarresyndrome", 
#             "hivencepalopathy")
# 
# ####################################################################################################
## Baseline characteristics For Sacema Research Day
#Adults

#pdf(file.path('Figures','Adults age baseline characteristics.png'), w = 11, h = 8) # Initialize PDF

#par(mfrow=c(2,3))
png(file.path('Figures','Adults age baseline characteristics.png'),width = 480, height = 480) # Initialize PDF
hist(ddply(adtest, .(patient), summarise, age)[,2], xlab = 'years', main = 'Age at baseline (years)', bty = 'n', col = 'blue')
dev.off()

# png(file.path('Figures','Adults CD4 categories at baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
# hist(ddply(adtest, .(patient), summarise, cd4a.categ)[,2], breaks= seq(.5,4.5, by = 1), xlab = 'CD4 categories', main = 'CD4 categories', bty = 'n', col = 'blue')
# dev.off()

png(file.path('Figures','Adults CD4 baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
hist(ddply(addata, .(patient), summarise, lab_v[1])[,2],xlim = c(0, 1500), xlab = 'CD4 count', main = 'CD4 at baseline', bty = 'n', col = 'blue')
dev.off()

png(file.path('Figures','Adults viral load baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
hist(ddply(addata, .(patient), summarise, viral[1])[,2],  xlab = '', main = 'Viral load at baseline', xlim = c(0, 1100000),bty = 'n', col = 'blue')
dev.off()

png(file.path('Figures','Adults total time of follow up.png'), width = 480, height = 480) # Initialize PDF
hist(ddply(addata, .(patient), summarise, max(diff/365))[,2], main = 'Total time followed-up',xlim = c(0, 14), bty = 'n', col = 'blue', xlab = 'years')
dev.off()

png(file.path('Figures','Adults WHO stage at baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
hist(ddply(adtest, .(patient), summarise, fhv_stage_who)[,2], breaks= seq(.5,4.5, by = 1), xlab = '', main = 'WHO stage at baseline', bty = 'n', col = 'blue')
dev.off()

# -----------------------------------------------------------------
# Children
png(file.path('Figures','Children age baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
#par(mfrow=c(2,3))
hist(ddply(chtest, .(patient), summarise, age)[,2], xlab = 'years', main = 'Age at baseline (years)', bty = 'n', col = 'blue')
dev.off()

# png(file.path('Figures','Children CD4 categories at baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
# hist(ddply(chtest, .(patient), summarise, cd4a.categ)[,2], breaks= seq(.5,4.5, by = 1), xlab = 'CD4 categories', main = 'CD4 categories', bty = 'n', col = 'blue')
# dev.off()

png(file.path('Figures','Children CD4 baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
hist(ddply(chdata, .(patient), summarise, lab_v[1])[,2],xlim = c(0, 3000), xlab = 'CD4 count', main = 'CD4 at baseline', bty = 'n', col = 'blue')
dev.off()

png(file.path('Figures','Children viral load baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
hist(ddply(chdata, .(patient), summarise, viral[1])[,2],  xlab = '', main = 'Viral load at baseline', xlim = c(0, 1500000),bty = 'n', col = 'blue')
dev.off()

png(file.path('Figures','Children total time of follow up.png'), width = 480, height = 480) # Initialize PDF
hist(ddply(chdata, .(patient), summarise, max(diff/365))[,2], main = 'Total time followed-up',xlim = c(0, 14), bty = 'n', col = 'blue', xlab = 'years')
dev.off()

png(file.path('Figures','Children WHO stage at baseline characteristics.png'), width = 480, height = 480) # Initialize PDF
hist(ddply(chtest, .(patient), summarise, fhv_stage_who)[,2], breaks= seq(.5,4.5, by = 1), xlab = '', main = 'WHO stage at baseline', bty = 'n', col = 'blue')
dev.off()

# # =============================================== TO DO
# # Baseline characteristic for paper
# # png(file.path('Figures','Adults baseline characteristics.png'), w = 11, h = 8) # Initialize PDF
# # par(mfrow=c(2,3))
# # hist(ddply(adtest, .(patient), summarise, age, xlab = '', main = 'Age at baseline (years)', bty = 'n', col = 'black'))
# # hist(ddply(adtest, .(patient), summarise, cd4a.categ, xlab = '', main = 'CD4 categories', bty = 'n', col = 'black'))
# # hist(ddply(adtest, .(patient), summarise, lab_v, xlab = '', main = 'CD4 at baseline', bty = 'n', col = 'black'))
# # hist(ddply(adtest, .(patient), summarise, viral,  xlab = '', main = 'Viral load at baseline', bty = 'n', col = 'black'))
# # hist(ddply(addata, .(patient), summarise, max(diff/365), main = 'Total time followed-up', bty = 'n', col = 'black', xlab = 'years'))
# # hist(ddply(adtest, .(patient), summarise, fhv_stage_who, breaks= seq(.5,4.5, by = 1), xlab = '', main = 'WHO stage at baseline', bty = 'n', col = 'black'))
# # dev.off()
#      
# ## OI breakdown
# par(mfrow=c(1,1), mar = c(15, 4, 3,1))
# barplot(colSums(coh[,oi.nms], na.rm=T), las = 2, main = 'distribution of OI-visits by OI class')

###############################################################################################
###########################   CD4 MEDIANS FIGURES   ##########################################
###############################################################################################

# addata <- read.csv("/home/evaliliane/Documents/PhD/Python/AdultData_Time_Slope_Base.csv")
# chdata <- read.csv("/home/evaliliane/Documents/PhD/Python/ChildrenZscores.csv") 

# Replacing small neg zscores by -12
chdata <- chdata[!(is.na(chdata$zscore)),]
chdata$zscore[chdata$zscore < -10] <- -10

# Removing row with TOFU > 15
addata <- addata[addata$diff < 5476,]
chdata <- chdata[chdata$diff < 5476,]

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


###### ADULTS ############################################
# Descriptive tables and figures
# addatabase <- addata[addata$base == 1,]
# length(addatabase[addatabase$diff != 0])
# length(addatabase[addatabase$base == 0])
# #summary(addatabase$lab_v)
# 
# 
# ##Create a variable for CD4 categories at ART initiation
# brks <- c(0,100,200,300,5000)                                             # Categories' limits
# addatabase$cd4a.categ <- cut(addatabase$lab_v, breaks = brks)             # Add categories variable to the baseline dataset
# sub.addata <- subset(addatabase, select = c("patient", "cd4a.categ"))     # Subset category & patient variables
# testaddata <- merge(addata, sub.addata, by = "patient", all = FALSE )     # Add categories variable to the entire dataset
testaddata <- addata[order(addata$patient,addata$lab_dmy),]   # Order with respect to individual lab dates
testaddata$diff2 <- testaddata$diff / 365                                 # Add a yearly time Diff2 PS: Diff is in DAYS
testaddata$rounddiff <- testaddata$diff2
col.vec <- 1:10
# -----------------------------------------------------
# Plot all together
# Initiate plot for the medians
png(file.path('Figures','CD4_All-Adult.png'), width = 480, height = 480)
#png('CD4_All-Adult.png')
xs <- seq(0, 5110, by = 730)
yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
     xlim = c(0,6000), ylim = c(100,1100), cex.main =0.9) # , main = "Suppressed viral load")
axis(2, at = seq(100,1100, by = 200), las = 2) ## x axis
axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
nn <- nlevels(testaddata$cd4a.categ) #- 1                   # Number of Categories
for(ii in 1:nn) {  
  
  curr.cat <- levels(testaddata$cd4a.categ)[ii]           # Choose range of CD4 counts 
  test <- testaddata[testaddata$cd4a.categ == curr.cat,]  # Define subdataset to be used
  test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
  test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
  dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
  length(test$lab_v) == length(dat2[,42])                 # (diff variable)
  
  # Transform matrix into longitudinal object's class
  dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,42]))), unique(as.numeric(dat2[,42])))
  #is.longitudinal(dat)  
  
  # Calculate the medians
  med <- condense.longitudinal(dat, 34, median)
  tim <- get.time.repeats(dat)
  #   sp <- smooth.spline(tim$time, med, spar=0.35)
  #   lines(sp, col = col.vec[ii])
  lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
  if (ii == nn){ # Add upper CI only for the last CD4 categ
    confIntu <- condense.longitudinal(dat, 34, myCI_u) #CI(dat[,33],ci = 0.95)
    lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
  }
  if (ii == 1){ # Add lower CI only for the first CD4 categ
    confIntl <- condense.longitudinal(dat, 34, myCI_l) #CI(dat[,33],ci = 0.95)
    lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
  } 
}
# xx <- c(tim$time[180:length(tim$time)],rev(tim$time[180:length(tim$time)]))
# yy <- c(rollmean(confIntu[1:length(tim$time)], 180), rev(rollmean(confIntl[1:length(tim$time)],180))) #, col =  gray(0.7), lty = 2)
# polygon(xx,yy, col='lightgrey')
legend("topright", levels(testaddata$cd4a.categ), col = 1:nn, lty = 1)
title("CD4 count medians' trajectory")
dev.off()


# #----------------------------------------------------------
# # Suppressed .vs. Not suppressed
# # Initiate plot for the medians
# png(file.path('Figures','CD4_Adult.png'), width = 400, height = 800)
# par(mfrow=c(2,1))
# xs <- seq(0, 5110, by = 730)
# yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
# plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
#      xlim = c(0,6000), ylim = c(100,1100), cex.main =0.9) # , main = "Suppressed viral load")
# axis(2, at = seq(100,1100, by = 200), las = 2) ## x axis
# axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
# nn <- nlevels(testaddata$cd4a.categ) #- 1                   # Number of Categories
# for(ii in 1:nn) {  
#   
#   curr.cat <- levels(testaddata$cd4a.categ)[ii]           # Choose range of CD4 counts 
#   test <- testaddata[testaddata$cd4a.categ == curr.cat  & testaddata$suppress == 1,]  # Define subdataset to be used
#   test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
#   test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,42])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,42]))), unique(as.numeric(dat2[,42])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 34, median)
#   confIntu <- condense.longitudinal(dat, 34, myCI_u) #CI(dat[,33],ci = 0.95)
#   confIntl <- condense.longitudinal(dat, 34, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
#   if (ii == nn){ # Add upper CI only for the last CD4 categ
#     lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
#   }
#   if (ii == 1){ # Add lower CI only for the first CD4 categ
#     lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
#   }
#   
# }
# 
# # Non suppressed
# xs <- seq(0, 5110, by = 730)
# yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
# plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
#      xlim = c(0,6000), ylim = c(100,1100), cex.main =0.9) # , main = " Viral load not suppressed")
# axis(2, at = seq(100,1100, by = 200), las = 2) ## x axis
# axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
# nn <- nlevels(testaddata$cd4a.categ) #- 1                   # Number of Categories
# for(ii in 1:nn) {  
#   
#   curr.cat <- levels(testaddata$cd4a.categ)[ii]           # Choose range of CD4 counts 
#   test <- testaddata[testaddata$cd4a.categ == curr.cat & testaddata$suppress == 0,]  # Define subdataset to be used
#   test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
#   test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,42])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,42]))), unique(as.numeric(dat2[,42])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 34, median)
#   confIntu <- condense.longitudinal(dat, 34, myCI_u) #CI(dat[,33],ci = 0.95)
#   confIntl <- condense.longitudinal(dat, 34, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
#   if (ii == nn){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
#   }
#   if (ii == 1){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
#   }
# }
# title("CD4 count medians' trajectory")
# dev.off()
# 
# #  ============= Square root CD4 for Adults  ======================================================================
# # Plot all together
png(file.path('Figures','CD4_AllSQRT-Adult.png'), width = 480, height = 480)
xs <- seq(0, 5110, by = 730)
yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
     xlim = c(0,6000), ylim = c(5,30), cex.main =0.9) # , main = " Suppressed viral load")
axis(2, at = seq(5,30, by = 5), las = 2) ## x axis
axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
nn <- nlevels(testaddata$cd4a.categ) #- 1                   # Number of Categories
for(ii in 1:nn) {  
  
  curr.cat <- levels(testaddata$cd4a.categ)[ii]           # Choose range of CD4 counts 
  test <- testaddata[testaddata$cd4a.categ == curr.cat,]  # Define subdataset to be used
  #test$sqrtCD4 
  test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
  test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
  dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
  length(test$lab_v) == length(dat2[,42])                 # (diff variable)
  
  # Transform matrix into longitudinal object's class
  dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,42]))), unique(as.numeric(dat2[,42])))
  #is.longitudinal(dat)  
  
  # Calculate the medians
  med <- condense.longitudinal(dat, 34, median)
  confIntu <- condense.longitudinal(dat, 34, myCI_u) #CI(dat[,33],ci = 0.95)
  confIntl <- condense.longitudinal(dat, 34, myCI_l) #CI(dat[,33],ci = 0.95)  
  tim <- get.time.repeats(dat)
  #   sp <- smooth.spline(tim$time, med, spar=0.35)
  #   lines(sp, col = col.vec[ii])
  lines(tim$time[180:length(tim$time)], rollmean(sqrt(med), 180), col = col.vec[ii])
  if (ii == nn){
    lines(tim$time[180:length(tim$time)], rollmean(sqrt(confIntu),180), col =  gray(0.7), lty = 2)
  }
  if (ii == 1){
    confIntl <- ifelse(confIntl < 0,0,confIntl)
    lines(tim$time[180:length(tim$time)], rollmean(sqrt(confIntl),180), col =  gray(0.7), lty = 2)
  }
  
}
legend("topleft", levels(testaddata$cd4a.categ), col = 1:nn, lty = 1)
title("Squared root CD4 medians' trajectory" ) #, outer=FALSE)
dev.off()
# 
# # --------------------------------------------------
# # Suppressed .vs. Not suppressed
# # Initiate plot for the medians
# png(file.path('Figures','CD4_SQRT-Adult.png'), width = 400, height = 800)
# par(mfrow=c(2,1))
# xs <- seq(0, 5110, by = 730)
# yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
# plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
#      xlim = c(0,6000), ylim = c(5,30), cex.main =0.9) # , main = " Suppressed viral load")
# axis(2, at = seq(5,30, by = 5), las = 2) ## x axis
# axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
# nn <- nlevels(testaddata$cd4a.categ) #- 1                   # Number of Categories
# for(ii in 1:nn) {  
#   
#   curr.cat <- levels(testaddata$cd4a.categ)[ii]           # Choose range of CD4 counts 
#   test <- testaddata[testaddata$cd4a.categ == curr.cat  & testaddata$suppress == 1,]  # Define subdataset to be used
#   #test$sqrtCD4 
#   test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
#   test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,42])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,42]))), unique(as.numeric(dat2[,42])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 34, median)
#   confIntu <- condense.longitudinal(dat, 34, myCI_u) #CI(dat[,33],ci = 0.95)
#   confIntl <- condense.longitudinal(dat, 34, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(sqrt(med), 180), col = col.vec[ii])
#   if (ii == nn){
#     lines(tim$time[180:length(tim$time)], rollmean(sqrt(confIntu),180), col =  gray(0.7), lty = 2)
#   }
#   if (ii == 1){
#     confIntl <- ifelse(confIntl < 0,0,confIntl)
#     lines(tim$time[180:length(tim$time)], rollmean(sqrt(confIntl),180), col =  gray(0.7), lty = 2)
#   }
#   
# }
# 
# # Non suppressed
# xs <- seq(0, 5110, by = 730)
# yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
# plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
#      xlim = c(0,6000), ylim = c(5,30), cex.main =0.9) # , main = "Viral load not suppressed")
# axis(2, at = seq(5,30, by = 5 ), las = 2) ## x axis
# axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
# nn <- nlevels(testaddata$cd4a.categ) #- 1                   # Number of Categories
# for(ii in 1:nn) {  
#   
#   curr.cat <- levels(testaddata$cd4a.categ)[ii]           # Choose range of CD4 counts 
#   test <- testaddata[testaddata$cd4a.categ == curr.cat & testaddata$suppress == 0,]  # Define subdataset to be used
#   test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
#   test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,42])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,42]))), unique(as.numeric(dat2[,42])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 34, median)
#   confIntu <- condense.longitudinal(dat, 34, myCI_u) #CI(dat[,33],ci = 0.95)
#   confIntl <- condense.longitudinal(dat, 34, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(sqrt(med), 180), col = col.vec[ii])
#   if (ii == nn){
#     lines(tim$time[180:length(tim$time)], rollmean(sqrt(confIntu),180), col =  gray(0.7), lty = 2)
#   }
#   if (ii == 1){
#     confIntl <- ifelse(confIntl < 0,0,confIntl)
#     lines(tim$time[180:length(tim$time)], rollmean(sqrt(confIntl),180), col =  gray(0.7), lty = 2)
#   }
# }
# title("Squared root CD4 medians' trajectory" ) #, outer=FALSE)
# dev.off()
# 
# 
# ##################  CHILDREN  ######################################
# 
# # Descriptive tables and figures
chdatabase <- chdata[chdata$base == 1,]
length(chdatabase[chdatabase$diff != 0])
length(chdatabase[chdatabase$base == 0])
#summary(chdatabase$lab_v)


# ##Create a variable for CD4 categories at ART initiation
# cbrks <- c(0,200,400,600,800,1000,5000)                                             # Categories' limits
# chdatabase$cd4a.categ <- cut(chdatabase$lab_v, breaks = cbrks)             # chd categories variable to the baseline dataset
# sub.chdata <- subset(chdatabase, select = c("patient", "cd4a.categ"))     # Subset category & patient variables
# testchdata <- merge(chdata, sub.chdata, by = "patient", all = FALSE )     # chd categories variable to the entire dataset
testchdata <- chdata[order(chdata$patient,chdata$lab_dmy),]   # Order with respect to individual lab dates
testchdata$diff2 <- testchdata$diff / 365                                 # chd a yearly time Diff2 PS: Diff is in DAYS
testchdata$rounddiff <- testchdata$diff2
# 
# # ----------------------------------------------------------
# # All children altogether
# # Initiate plot for the medians
png(file.path('Figures','CD4_All-Children.png'), width = 480, height = 480)
xs <- seq(0, 5110, by = 730)
yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
     xlim = c(0,6000), ylim = c(0,2800), cex.main =0.9) # , main = "Suppressed viral load")
axis(2, at = seq(0,2800, by = 400), las = 2) ## x axis
axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis

nn <- nlevels(testchdata$cd4a.categ) #- 1                   # Number of Categories
for(ii in 1:nn) {  
  
  curr.cat <- levels(testchdata$cd4a.categ)[ii]           # Choose range of CD4 counts 
  test <- testchdata[testchdata$cd4a.categ == curr.cat,]  # Define subdataset to be used
  test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
  test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
  dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
  length(test$lab_v) == length(dat2[,44])                 # (diff variable)
  
  # Transform matrix into longitudinal object's class
  dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,44]))), unique(as.numeric(dat2[,44])))
  #is.longitudinal(dat)  
  
  # Calculate the medians
  med <- condense.longitudinal(dat, 35, median)
  confIntu <- condense.longitudinal(dat, 35, myCI_u) #CI(dat[,33],ci = 0.95)
  confIntl <- condense.longitudinal(dat, 35, myCI_l) #CI(dat[,33],ci = 0.95)  
  tim <- get.time.repeats(dat)
  #   sp <- smooth.spline(tim$time, med, spar=0.35)
  #   lines(sp, col = col.vec[ii])
  lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
  if (ii == nn){
    lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
  }
  if (ii == 1){
    lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
  }  
}
title("CD4 count medians' trajectory" ) #, outer=FALSE)
legend("topright", levels(testchdata$cd4a.categ), col = 1:nn, lty = 1)
dev.off()

# # ---------------------------------------------------------------------------
# # Suppressed .vs. Not suppressed
# png(file.path('Figures','CD4_Children.png'), width = 400, height = 800)
# par(mfrow=c(2,1))
# xs <- seq(0, 5110, by = 730)
# yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
# plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
#      xlim = c(0,6000), ylim = c(0,2800), cex.main =0.9) # , main = "Suppressed viral load")
# axis(2, at = seq(0,2800, by = 400), las = 2) ## x axis
# axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
# 
# nn <- nlevels(testchdata$cd4a.categ) #- 1                   # Number of Categories
# for(ii in 1:nn) {  
#   
#   curr.cat <- levels(testchdata$cd4a.categ)[ii]           # Choose range of CD4 counts 
#   test <- testchdata[testchdata$cd4a.categ == curr.cat & testchdata$suppress == 1,]  # Define subdataset to be used
#   test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
#   test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,44])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,44]))), unique(as.numeric(dat2[,44])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 35, median)
#   confIntu <- condense.longitudinal(dat, 35, myCI_u) #CI(dat[,33],ci = 0.95)
#   confIntl <- condense.longitudinal(dat, 35, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
#   if (ii == nn){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
#   }
#   if (ii == 1){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
#   }
#   
# }
# 
# # Not suppressed
# xs <- seq(0, 5110, by = 730)
# yfu.xs <- seq(0, 10, by = 1) ## predict model at these ayrs.fu
# plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 count',xaxt='n', yaxt='n', bty = 'n', 
#      xlim = c(0,6000), ylim = c(0,2800), cex.main =0.9) # , main = "Viral load not suppressed")
# axis(2, at = seq(0,2800, by = 400), las = 2) ## x axis
# axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
# 
# nn <- nlevels(testchdata$cd4a.categ) #- 1                   # Number of Categories
# for(ii in 1:nn) {  
#   
#   curr.cat <- levels(testchdata$cd4a.categ)[ii]           # Choose range of CD4 counts 
#   test <- testchdata[testchdata$cd4a.categ == curr.cat   & testchdata$suppress == 0,]  # Define subdataset to be used
#   test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
#   test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,44])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,44]))), unique(as.numeric(dat2[,44])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 35, median)
#   confIntu <- condense.longitudinal(dat, 35, myCI_u) #CI(dat[,33],ci = 0.95)
#   confIntl <- condense.longitudinal(dat, 35, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
#   if (ii == nn){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
#   }
#   if (ii == 1){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
#   }
# }
# title("CD4 count medians' trajectory" ) #, outer=FALSE)
# dev.off()
# 
# # =========== CD4 Z-scores ===================================================================================
# # All children
# # Initiate plot for the medians
png(file.path('Figures','CD4_AllZscores-Children.png'), width = 480, height = 480)
#png('CD4-Zscores_Children.png')
xs <- seq(0, 5110, by = 730)
yfu.xs <- seq(-8, 2, by = 2) ## predict model at these ayrs.fu
plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 z-scores',xaxt='n', yaxt='n', bty = 'n', 
     xlim = c(0,6000), ylim = c(-8,2), cex.main =0.9) # , main = "Suppressed viral load")
axis(2, at = seq(-8,2, by = 2), las = 2) ## x axis
axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis

# Suppressed
nn <- nlevels(testchdata$cd4a.categ) #- 1                   # Number of Categories
for(ii in 1:nn) {  
  
  curr.cat <- levels(testchdata$cd4a.categ)[ii]           # Choose range of CD4 counts 
  test <- testchdata[testchdata$cd4a.categ == curr.cat,]  # Define subdataset to be used
  test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
  test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
  dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
  length(test$lab_v) == length(dat2[,44])                 # (diff variable)
  
  # Transform matrix into longitudinal object's class
  dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,44]))), unique(as.numeric(dat2[,44])))
  #is.longitudinal(dat)  
  
  # Calculate the medians
  med <- condense.longitudinal(dat, 48, median)
  confIntu <- condense.longitudinal(dat, 48, myCI_u) #CI(dat[,33],ci = 0.95)
  confIntl <- condense.longitudinal(dat, 48, myCI_l) #CI(dat[,33],ci = 0.95)  
  tim <- get.time.repeats(dat)
  #   sp <- smooth.spline(tim$time, med, spar=0.35)
  #   lines(sp, col = col.vec[ii])
  lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
  if (ii == nn){
    lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
  }
  if (ii == 1){
    lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
  }
}
title("CD4 z-scores medians' trajectory" ) #, outer=FALSE)
legend("topright", levels(testchdata$cd4a.categ), col = 1:nn, lty = 1)
dev.off()

# 
# png(file.path('Figures','CD4_Zscores-Children.png'), width = 400, height = 800)
# par(mfrow=c(2,1))
# 
# xs <- seq(0, 5110, by = 730)
# yfu.xs <- seq(-8, 2, by = 2) ## predict model at these ayrs.fu
# plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 z-scores',xaxt='n', yaxt='n', bty = 'n', 
#      xlim = c(0,6000), ylim = c(-8,2), cex.main =0.9) # , main = "Suppressed viral load")
# axis(2, at = seq(-8,2, by = 2), las = 2) ## x axis
# axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
# 
# # Suppressed
# nn <- nlevels(testchdata$cd4a.categ) #- 1                   # Number of Categories
# for(ii in 1:nn) {  
#   
#   curr.cat <- levels(testchdata$cd4a.categ)[ii]           # Choose range of CD4 counts 
#   test <- testchdata[testchdata$cd4a.categ == curr.cat  & testchdata$suppress == 1,]  # Define subdataset to be used
#   test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
#   test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,44])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,44]))), unique(as.numeric(dat2[,44])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 48, median)
#   confIntu <- condense.longitudinal(dat, 48, myCI_u) #CI(dat[,33],ci = 0.95)
#   confIntl <- condense.longitudinal(dat, 48, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
#   if (ii == nn){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
#   }
#   if (ii == 1){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
#   }
# }
# 
# # Not suppressed
# xs <- seq(0, 5110, by = 730)
# yfu.xs <- seq(-8, 2, by = 2) ## predict model at these ayrs.fu
# plot(0,0, type='n', xlab = 'Years since ART initiation', ylab = 'CD4 z-scores',xaxt='n', yaxt='n', bty = 'n', 
#      xlim = c(0,6000), ylim = c(-8,2), cex.main =0.9) # , main = "Viral load not suppressed")
# axis(2, at = seq(-8,2, by = 2), las = 2) ## x axis
# axis(1, at = seq(0,5110, by = 730), labels = seq(0,14, by= 2) ) #,las = 2) ## x axis
# 
# 
# nn <- nlevels(testchdata$cd4a.categ) #- 1                   # Number of Categories
# for(ii in 1:nn) {  
#   
#   curr.cat <- levels(testchdata$cd4a.categ)[ii]           # Choose range of CD4 counts 
#   test <- testchdata[testchdata$cd4a.categ == curr.cat  & testchdata$suppress == 0,]  # Define subdataset to be used
#   test <- test[!(is.na(test$lab_v)),]                     # Remove NAs
#   test <- test[order(test$diff),]                    # Order with resepct to time since HAART initiation
#   dat2 <- data.matrix(test)                               # Transform dataframe into a Matrix
#   length(test$lab_v) == length(dat2[,44])                 # (diff variable)
#   
#   # Transform matrix into longitudinal object's class
#   dat <- as.longitudinal(dat2 , repeats = as.numeric(table(as.numeric(dat2[,44]))), unique(as.numeric(dat2[,44])))
#   #is.longitudinal(dat)  
#   
#   # Calculate the medians
#   med <- condense.longitudinal(dat, 48, median)
#   confIntu <- condense.longitudinal(dat, 48, myCI_u) #CI(dat[,33],ci = 0.95)
#   confIntl <- condense.longitudinal(dat, 48, myCI_l) #CI(dat[,33],ci = 0.95)  
#   tim <- get.time.repeats(dat)
#   #   sp <- smooth.spline(tim$time, med, spar=0.35)
#   #   lines(sp, col = col.vec[ii])
#   lines(tim$time[180:length(tim$time)], rollmean(med, 180), col = col.vec[ii])
#   if (ii == nn){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntu,180), col =  gray(0.7), lty = 2)
#   }
#   if (ii == 1){
#     lines(tim$time[180:length(tim$time)], rollmean(confIntl,180), col =  gray(0.7), lty = 2)
#   }
# }
# title("CD4 z-scores medians' trajectory") #, outer=FALSE)
# dev.off()
# 
# # ===================================================================================