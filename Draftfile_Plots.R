# Clear the workspace
rm(list=ls())

# Load libraries to be used
library(foreign)
library(nlme)
library(plyr)
# The below library is not available for R 3.0.2
library(gdata)

# Set working directory
setwd("/home/evaliliane/Documents/PhD/Codes")

#################### READ DATA ############################################
# Set the most recent date on which datasets were generated.
#Sys.Date()
currentDate <- "2014-08-22"
# Read Adult data
print("Read adult data")
LabFileName <- paste("NewData/adult_labdata",currentDate,".csv",sep="")
PatFileName <- paste("NewData/adult_patdata",currentDate,".csv",sep="")
VisFileName <- paste("NewData/adult_visdata",currentDate,".csv",sep="")
OiFileName <- paste("NewData/adult_oidata",currentDate,".csv",sep="")
ArtFileName <- paste("NewData/adult_artdata",currentDate,".csv",sep="")

print(PatFileName)

adult.patdata <- read.csv(file=PatFileName)
adult.labdata <- read.csv(file=LabFileName)
adult.artdata <- read.csv(file=ArtFileName)
adult.oidata <- read.csv(file=OiFileName)
adult.visdata <- read.csv(file=VisFileName)


##################################################################################
#             DESCRIPTIVE ANALYSIS
##################################################################################

##########################  PAT DATASET  #######################################
## Histogram of age at ART Start
pdf('Age at HAART initiation.pdf', w = 10, h = 7)
hist(adult.patdata$age.haart.init, col = 'blue', xlab = 'Age at HAART initiation')
dev.off()
summary (adult.patdata$age.haart.init)

# Number of patients per cohort
tab.per.cohort <- ddply(adult.patdata,.(cohort) , summarize, Num.patients = length(unique(patient)), Female = sum(gender == 1), Male = sum(gender == 2) )
tab.per.cohort2 <- table(adult.patdata$cohort, adult.patdata$gender)
# Remove the 1 NA for COHORT

# Number of patients per HIV type
#tab.per.hivtype <- ddply(adult.patdata,.(hiv_type) , summarize, Num.patients = length(unique(patient)))

# Number of patients deceased per cohort
tab.death.per.cohort <- ddply(adult.patdata,.(cohort,gender) , summarize, Num.patients = length(unique(patient)), Death.HIV = sum(outcome == 10), Death.Unknown = sum(outcome == 11), Death.not.HIV = sum(outcome == 12))

# Number of patient with FIV Stage WHO per cohort
tab.fhv.per.cohort <- ddply(adult.patdata,.(cohort,gender) , summarize, Num.patients = length(unique(patient)), Stage.I = sum(fhv_stage_who == 1), Stage.II = sum(fhv_stage_who == 2), Stage.III = sum(fhv_stage_who == 3),Stage.IV = sum(fhv_stage_who == 4))

##########################  LAB DATASET  #######################################
## Individual CD4 trajectories over time

# Number of LAB measurements per cohort
tab.lab.per.cohort <- ddply(adult.labdata, .(cohort), summarize, Num.measurement = length(patient), RNA = sum(lab_id == "RNA"), WBC = sum(lab_id == "WBC"), CD4A = sum(lab_id == "CD4A"), CD4P= sum(lab_id == "CD4P"), LYPM = sum(lab_id == "LYMP"))

# Statistics for the different LAB measurements' variables
tab.lab.per.var <- ddply(adult.labdata, .(lab_id), summarize, Num.measurement = length(unique(patient)), TOP = max(lab_v), BOTTOM = min(lab_v), MIDDLE = mean(lab_v), STD = sd(lab_v))

#system.time(adult.labdata.rna <- ddply(adult.labdata.rna, .(patient, lab_id), transform, tim.vis = get.tfu(lab_dmy,patient, browse=T)))

########################### SUBSET DATA ##########################################
## Only work with a subset of this data to make things fast while debugging
set.seed(3) ## set random number generator seed
num.to.do <- 2500 ## to save computing time when playing with analyses
pids.to.run <- sample(unique(adult.patdata$patient), num.to.do) ## sample some patient ids to work with 

pattest1 <-adult.patdata[adult.patdata$patient %in% pids.to.run,] ## in pids.to.run?
labtest1 <- adult.labdata[adult.labdata$patient %in% pids.to.run,] ## in pids.to.run?
oitest1 <- adult.oidata[adult.oidata$patient %in% pids.to.run,] ## in pids.to.run?
vistest1 <- adult.visdata[adult.visdata$patient %in% pids.to.run,] ## in pids.to.run?
arttest1 <- adult.artdata[adult.artdata$patient %in% pids.to.run,] ## in pids.to.run?

# test <- merge(test.m3, arttest,by = c("patient","cohort"), all = TRUE)
#mytest <- rbind.fill(pattest,labtest,artdata,visdata, oidata )
length(unique(pattest1$patient)) ## should equal num.to.do

# Create a dataframe of only needed columns in LAB dataset.
labtest <- data.frame(labtest1$patient, labtest1$lab_id, labtest1$lab_v, labtest1$lab_dmy)
names(labtest) <- c("patient", "lab_id", "lab_v", "lab_dmy")

# Compute time since first measurement and add a column to the dataset
get.tfu <- function(vd, pid, browse=F) {
  ##if(browse) browser()
  #print(vd)
  return(as.numeric(difftime(vd, vd[1], units = 'days'))/365.25)
}


# Add column ffor time since first visit
print("Start ddply get.tfu")
labtest <- labtest[order(labtest$patient, labtest$lab_id, labtest$lab_dmy),]
# labtest <- ddply(labtest, .(patient, lab_id), transform, tim.vis = get.tfu(lab_dmy,patient, browse=T))

# Order each dataset by date of measurement / patient
# test1.rna <- ddply(test1.cd4p,.(patient, lab_dmy))
# test1.cd4a <- ddply(test1.cd4a,.(patient, lab_dmy))
# test1.lymp <- ddply(test1.lymp,.(patient, lab_dmy))
# test1.rna <- ddply(test1.rna,.(patient, lab_dmy))
# test1.wbc <- ddply(test1.wbc,.(patient, lab_dmy))

# Choose patients who supresses viral load.
# This will be done by looking at their slope.
adult.labdata.rna <- labtest[labtest$lab_id == "RNA",]
adult.labdata.rna <- adult.labdata.rna[order(adult.labdata.rna$patient, adult.labdata.rna$lab_id, adult.labdata.rna$lab_dmy),]
print(length(labtest$lab_v))
print(length(adult.labdata.rna$lab_v))
system.time(adult.labdata.rna <- ddply(adult.labdata.rna, .(patient, lab_id), transform, tim.vis = get.tfu(lab_dmy,patient, browse=T)))

fun.lmtest <- function(lab,tim){
  mm <- lm(lab ~ tim)
  m <- summary(mm)
  return(as.numeric(m$coefficients[,1][2]))
}
lmtest <- ddply(adult.labdata.rna, .(patient), summarize, slope = fun.lmtest(log(lab_v),tim.vis) )
pat.to.keep <- lmtest$patient[which(lmtest$slope < -0.5 & lmtest$slope > - 5)]
#mm <- lm(log(labtest.rna$lab_v) ~ labtest.rna$tim.vis)

pattest <-adult.patdata[adult.patdata$patient %in% pat.to.keep,] ## in pids.to.run?
labtest <- adult.labdata[adult.labdata$patient %in% pat.to.keep,] ## in pids.to.run?
print(paste("Patients for RNA",length(unique(labtest$patient[labtest$lab_id == "RNA"]))))
print(paste("Number to keep",length(pat.to.keep)))

# Separate the different entiy measured
test1.cd4p <-labtest[labtest$lab_id == "CD4P",]
test1.cd4a <- labtest[labtest$lab_id == "CD4A",]
test1.lymp <- labtest[labtest$lab_id == "LYMP",]
test1.rna <- labtest[labtest$lab_id == "RNA",]
test1.wbc <- labtest[labtest$lab_id == "WBC",]

print(paste("Total patients for CD4 counts",length(unique(test1.cd4a$patient)))) ## should equal pat.to.keep))

# print("Order datasets")
test1.cd4a <- test1.cd4a[order(test1.cd4a$patient, test1.cd4a$lab_dmy),]
test1.cd4p <- test1.cd4p[order(test1.cd4p$patient, test1.cd4p$lab_dmy),]
test1.lymp <- test1.lymp[order(test1.lymp$patient, test1.lymp$lab_dmy),]
test1.rna <- test1.rna[order(test1.rna$patient, test1.rna$lab_dmy),]
test1.wbc <- test1.wbc[order(test1.wbc$patient, test1.wbc$lab_dmy),]            
            
# tim.vis <- daply(test1.cd4a, .(patient), transform, get.tfu(lab_dmy,patient, browse=T))
system.time(test1.cd4a <- ddply(test1.cd4a, .(patient), transform, tim.vis = get.tfu(lab_dmy,patient, browse=T)))
system.time(test1.cd4p <- ddply(test1.cd4p, .(patient), transform, tim.vis = get.tfu(lab_dmy,patient, browse=T)))
system.time(test1.rna <- ddply(test1.rna, .(patient), transform, tim.vis = get.tfu(lab_dmy,patient, browse=T)))
system.time(test1.wbc <- ddply(test1.wbc, .(patient), transform, tim.vis = get.tfu(lab_dmy,patient, browse=T)))
system.time(test1.lymp <- ddply(test1.lymp, .(patient), transform, tim.vis = get.tfu(lab_dmy,patient, browse=T)))

print("Start to work on the figures")
## Figures 1 - on-ART CD4 counts
# Order before plotting (Is this necesary anymore ?)
ln.fxn <- function(xx, yy, pid, order.by.xx=F) {
  ##  browser()
  missing.data <- is.na(xx) | is.na(yy)
  xx <- xx[!missing.data]
  yy <- yy[!missing.data]
  if(order.by.xx) {
    ord <- order(xx)
    xx <- xx[ord]
    yy <- yy[ord]
  }
  lines(xx, yy, col = pid[1])
}

###############################################################################
#================  CD4 =======================================================
################################################################################

pdf('Post HAART CD4 trajectories.pdf', w = 10, h = 7)
xs <- seq(0, 10, by = .1) ## predict model at these yfu (years followed-up
xlim <- range(xs)        ## yfu plot limits
par('ps' = 16, mfrow = c(1,1)) ## graphical parameters
plot(0,0, type='n', xlab = 'Time on HAART (years)', ylab = 'CD4 count', yaxt='n',
     xlim = xlim, ylim = c(0,1600), main = 'HAART CD4 trajectories') ## initialize plot
axis(2, at = seq(0,1600, by = 200), las = 2) ## create x-axis at these tick marks, las=2 rotates text to horizontal

## line for each pid
ddply(test1.cd4a, .(patient), with, ln.fxn(tim.vis, lab_v, patient, order.by.xx=T))
modc0 <-  loess(lab_v ~ tim.vis, data = test1.cd4a)  ## get loess model
lines(xs, predict(modc0, data.frame(tim.vis = xs, lab_v=NA)), lwd = 5) ## plot LOESS
dev.off()

# ===================================================================

pdf('Post HAART CD4 trajectories all range.pdf', w =8, h = 6)
par('ps' = 16, mfrow = c(2,2))
brks <- c(0,10,50,100,200,350,4000)
brks <- c(0,150,300,450,600,4000) ##Create a variable for CD4 categories at ART initiation
test1.cd4a$cd4a.categ <- cut(test1.cd4a$lab_v, breaks = brks)
yfu.xs <- seq(0, 12, by = .1) ## predict model at these ayrs.fu

for(ii in 1:nlevels(test1.cd4a$cd4a.categ)) {
  curr.cat <- levels(test1.cd4a$cd4a.categ)[ii]
  plot(0,0, type='n', xlab = 'years since ART', ylab = 'CD4 count', yaxt='n',
       xlim = c(0,10), ylim = c(0,1600), main = paste('CD4 at ART start:', curr.cat))
  axis(2, at = seq(0,1600, by = 200), las = 2)
  temp <- test1.cd4a[test1.cd4a$cd4a.categ == curr.cat,]
  ddply(temp, .(patient), with, ln.fxn(lab_v, tim.vis, patient))
  modc <-  loess(lab_v ~  tim.vis, data = temp)
  assign(paste0('mod',ii), modc)
  lines(yfu.xs, predict(modc, data.frame(tim.vis = yfu.xs, lab_v=NA)), lwd = 5,
        col = ii) ## plot LOESS
}
dev.off()

#Figure to summarize above#
pdf('CD4 trajectory by CD4 at ART initiation by range.pdf', w =8, h = 6)
plot(0,0, type='n', xlab = 'years since ART', ylab = 'CD4 count', yaxt='n', bty = 'n', 
     xlim = c(0,10), ylim = c(0,1200), main = 'average CD4 trajectory')
axis(2, at = seq(0,1200, by = 300), las = 2) ## x axis
for(ii in 1:nlevels(test1.cd4a$cd4a.categ)) { ## for each CD4 category
  modc <- get(paste0('mod',ii)) ## get the loess model we assigned in the above for loop
  lines(yfu.xs, predict(modc, data.frame(tim.vis = yfu.xs, lab_v=NA)), lwd = 5, col = ii) ## plot that LOESS model
}
legend('topright', leg = levels(test1.cd4a$cd4a.categ), ## add a legend with levels of the CD4 categories as the text
       col = 1:nlevels(test1.cd4a$cd4a.categ), lwd = 2,  ## and the same colors as above
       title = 'CD4 at ART initiation')
dev.off()

##########################################################################################
# ============================ CD4 percentage ===========================================
###########################################################################################

pdf('Post HAART CD4 percentage trajectories.pdf', w = 10, h = 7)
xs <- seq(0, 10, by = .1) ## predict model at these yfu (years followed-up
xlim <- range(xs)        ## yfu plot limits
par('ps' = 16, mfrow = c(1,1)) ## graphical parameters
plot(0,0, type='n', xlab = 'Time on HAART (years)', ylab = 'CD4 percentage', yaxt='n',
     xlim = xlim, ylim = c(0,60), main = 'HAART CD4 percentage trajectories') ## initialize plot
axis(2, at = seq(0,60, by = 10), las = 2) ## create x-axis at these tick marks, las=2 rotates text to horizontal

## line for each pid
ddply(test1.cd4p, .(patient), with, ln.fxn(tim.vis, lab_v, patient, order.by.xx=T))
modc0 <-  loess(lab_v ~ tim.vis, data = test1.cd4p)  ## get loess model
lines(xs, predict(modc0, data.frame(tim.vis = xs, lab_v=NA)), lwd = 5) ## plot LOESS
dev.off()

# ===================================================================

pdf('Post HAART CD4 percentage trajectories all range.pdf', w =8, h = 6)
par('ps' = 16, mfrow = c(2,2))
#brks <- c(0,10,50,100,200,350,5000)
brks <- c(0,10,20,30,60) ##Create a variable for CD4 categories at ART initiation
test1.cd4p$cd4p.categ <- cut(test1.cd4p$lab_v, breaks = brks)
yfu.xs <- seq(0, 12, by = .1) ## predict model at these ayrs.fu

for(ii in 1:nlevels(test1.cd4p$cd4p.categ)) {
  curr.cat <- levels(test1.cd4p$cd4p.categ)[ii]
  plot(0,0, type='n', xlab = 'years since ART', ylab = 'CD4 percentage', yaxt='n',
       xlim = c(0,10), ylim = c(0,60), main = paste('CD4 percentage at ART start:', curr.cat))
  axis(2, at = seq(0,60, by = 10), las = 2)
  temp <- test1.cd4p[test1.cd4p$cd4p.categ == curr.cat,]
  ddply(temp, .(patient), with, ln.fxn(lab_v, tim.vis, patient))
  modc <-  loess(lab_v ~  tim.vis, data = temp)
  assign(paste0('mod',ii), modc)
  lines(yfu.xs, predict(modc, data.frame(tim.vis = yfu.xs, lab_v=NA)), lwd = 5,
        col = ii) ## plot LOESS
}
dev.off()

#Figure to summarize above#
pdf('CD4 trajectory by CD4 percentage at ART initiation by range.pdf', w =8, h = 6)
plot(0,0, type='n', xlab = 'years since ART', ylab = 'CD4 percentage', yaxt='n', bty = 'n', 
     xlim = c(0,10), ylim = c(0,60), main = 'Average CD4 percentage trajectory')
axis(2, at = seq(0,60, by = 10), las = 2) ## x axis
for(ii in 1:nlevels(test1.cd4p$cd4p.categ)) { ## for each CD4 category
  modc <- get(paste0('mod',ii)) ## get the loess model we assigned in the above for loop
  lines(yfu.xs, predict(modc, data.frame(tim.vis = yfu.xs, lab_v=NA)), lwd = 5, col = ii) ## plot that LOESS model
}
legend('topright', leg = levels(test1.cd4p$cd4p.categ), ## add a legend with levels of the CD4 categories as the text
       col = 1:nlevels(test1.cd4p$cd4p.categ), lwd = 2,  ## and the same colors as above
       title = 'CD4 percentage at ART initiation')
dev.off()

##########################################################################################
# ============================ RNA Viral loads ===========================================
###########################################################################################

pdf('Post HAART RNA trajectories.pdf', w = 10, h = 7)
xs <- seq(0, 10, by = .1) ## predict model at these yfu (years followed-up
xlim <- range(xs)        ## yfu plot limits
par('ps' = 16, mfrow = c(1,1)) ## graphical parameters
plot(0,0, type='n', xlab = 'Time on HAART (years)', ylab = ' RNA percentage', yaxt='n',
     xlim = xlim, ylim = c(0,16), main = 'HAART RNA percentage trajectories') ## initialize plot
axis(2, at = seq(0,16, by = 4), las = 2) ## create x-axis at these tick marks, las=2 rotates text to horizontal

## line for each pid
ddply(test1.rna, .(patient), with, ln.fxn(tim.vis, log(lab_v), patient, order.by.xx=T))
modc0 <-  loess(log(lab_v) ~ tim.vis, data = test1.rna)  ## get loess model
lines(xs, predict(modc0, data.frame(tim.vis = xs, lab_v=NA)), lwd = 5) ## plot LOESS
dev.off()

# ===================================================================

####### ADULTS ########################## CHILDREN #######################
# Limit for CD4A 4000 okay          # 6000 okay
# Limit for CD4P 70 okay            # 
# Limit for LYMP 10000 okay         # 13000 okay
# Limit for RNA 1500000 ?           # 
# Limit for WBC 15  okay            # 20 okay
# Limit for NEUT 10 okay            #

pdf('Post HAART RNA percentage trajectories all range.pdf', w =8, h = 6)
par('ps' = 16, mfrow = c(2,2))
#brks <- c(0,10,50,100,200,350,5000)
brks <- c(0,30,50,200,1500000) ##Create a variable for RNA categories at ART initiation
test1.rna$cd4p.categ <- cut(test1.rna$lab_v, breaks = brks)
yfu.xs <- seq(0, 12, by = .1) ## predict model at these ayrs.fu

for(ii in 1:nlevels(test1.rna$cd4p.categ)) {
  curr.cat <- levels(test1.rna$cd4p.categ)[ii]
  plot(0,0, type='n', xlab = 'years since ART', ylab = ' RNA percentage', yaxt='n',
       xlim = c(0,10), ylim = c(0,16), main = paste(' RNA percentage at ART start:', curr.cat))
  axis(2, at = seq(0,16, by = 4), las = 2)
  temp <- test1.rna[test1.rna$cd4p.categ == curr.cat,]
  ddply(temp, .(patient), with, ln.fxn(log(lab_v), tim.vis, patient))
  modc <-  loess(log(lab_v) ~  tim.vis, data = temp)
  assign(paste0('mod',ii), modc)
  lines(yfu.xs, predict(modc, data.frame(tim.vis = yfu.xs, lab_v=NA)), lwd = 5,
        col = ii) ## plot LOESS
}
dev.off()

#Figure to summarize above#
pdf(' RNA trajectory by RNA percentage at ART initiation by range.pdf', w =8, h = 6)
plot(0,0, type='n', xlab = 'years since ART', ylab = ' RNA percentage', yaxt='n', bty = 'n', 
     xlim = c(0,10), ylim = c(0,16), main = 'Average RNA percentage trajectory')
axis(2, at = seq(0,16, by = 4), las = 2) ## x axis
for(ii in 1:nlevels(test1.rna$cd4p.categ)) { ## for each CD4 category
  modc <- get(paste0('mod',ii)) ## get the loess model we assigned in the above for loop
  lines(yfu.xs, predict(modc, data.frame(tim.vis = yfu.xs, lab_v=NA)), lwd = 5, col = ii) ## plot that LOESS model
}
legend('topright', leg = levels(test1.rna$cd4p.categ), ## add a legend with levels of the CD4 categories as the text
       col = 1:nlevels(test1.rna$cd4p.categ), lwd = 2,  ## and the same colors as above
       title = ' RNA percentage at ART initiation')
dev.off()


# # head(test)
# # 
# # ##how many people in sample
# # length(unique(test$patient)) #there are 1000 unique patients in the sample
# # 
# # ## Get only baseline visit data
# # baselinedat <- test[test$yfu==0,] ## select only rows where they are at 0 years of follow-up (first visit)
# # 
# # summary(baselinedat) ## R's built in summary command will summarize all the variables
# # summary(baselinedat$sex)
# # 
# # ## You can also tabulate with xtabs
# # xtabs(~sex, baselinedat)
# # xtabs(~sex + NowPregnant, baselinedat) ## for instance you can see 655 of the men were coded as " " for Now pregnant and 50 were No
# # 
# # ## Histogram of age at entry into the clinic electronic records data base
# # age.in.days <- difftime(baselinedat$date, baselinedat$dob, units = 'days') # difftime takes difference between date objects
# # head(age.in.days)
# # age.in.years <- as.numeric(age.in.days/365.25)
# # baselinedat$age <- age.in.years
# # head(baselinedat$age)
# # hist(baselinedat$age, col = 'black', xlab = 'age at first observation')
# # summary(baselinedat$age)
# # 
# # 
# # 
# # ## Identifying the time between HIV diagnosis and ART initiation
# # days.to.ART.start <- difftime(baselinedat$arts_first_date, baselinedat$dfirsthivpos, units = 'days') 
# # head(days.to.ART.start)
# # days.since.start <- as.numeric(days.to.ART.start)
# # head(days.since.start)
# # summary(days.since.start)# not very interesting, most people start right away (median=35 days, mean=165 days)
# # 
# # 
# # ##CD4 trajectories over time##
# # #(copied from visCD4.R file writtend during MMED)#
# # ln.fxn <- function(xx, yy, pid, order.by.xx=F) {
# #   ##  browser()
# #   missing.data <- is.na(xx) | is.na(yy)
# #   xx <- xx[!missing.data]
# #   yy <- yy[!missing.data]
# #   if(order.by.xx) {
# #     ord <- order(xx)
# #     xx <- xx[ord]
# #     yy <- yy[ord]
# #   }
# #   lines(xx, yy, col = pid[1])
# # }
# # 
# # pdf('CD4 trajectory by CD4 at ART initiation.pdf', w =8, h = 6)
# # par('ps' = 16, mfrow = c(2,2))
# # brks <- c(0,10,50,100,200,350,5000)
# # brks <- c(0,50,200,350,5000) ##Create a variable for CD4 categories at ART initiation
# # test$CD4ARTStartcat <- cut(test$CD4ARTStart, breaks = brks)
# # yfu.xs <- seq(0, 12, by = .1) ## predict model at these ayrs.fu
# # #Visualize CD4 trajectories by CD4 at ART initiation
# # for(ii in 1:nlevels(test$CD4ARTStartcat)) {
# #   curr.cat <- levels(test$CD4ARTStartcat)[ii]
# #   plot(0,0, type='n', xlab = 'years since ART', ylab = 'CD4 count', yaxt='n',
# #        xlim = c(0,8), ylim = c(0,1000), main = paste('CD4 at ART start:', curr.cat))
# #   axis(2, at = seq(0,1000, by = 200), las = 2)
# #   temp <- test[test$CD4ARTStartcat == curr.cat,]
# #   ddply(temp, .(pid), with, ln.fxn(CD4, ayfu, pid))
# #   modc <-  loess(CD4 ~ ayfu, data = temp)
# #   assign(paste0('mod',ii), modc)
# #   lines(yfu.xs, predict(modc, data.frame(ayfu = yfu.xs, CD4=NA)), lwd = 5,
# #         col = ii) ## plot LOESS
# # }
# # 
# # #Figure to summarize above#
# # pdf('CD4 trajectory by CD4 at ART initiation (summary).pdf', w =8, h = 6)
# # plot(0,0, type='n', xlab = 'years since ART', ylab = 'CD4 count', yaxt='n', bty = 'n', 
# #      xlim = c(0,8), ylim = c(0,1000), main = 'average CD4 trajectory')
# # axis(2, at = seq(0,1000, by = 200), las = 2) ## x axis
# # for(ii in 1:nlevels(test$CD4ARTStartcat)) { ## for each CD4 category
# #   modc <- get(paste0('mod',ii)) ## get the loess model we assigned in the above for loop
# #   lines(yfu.xs, predict(modc, data.frame(ayfu = yfu.xs, CD4=NA)), lwd = 5, col = ii) ## plot that LOESS model
# # }
# # legend('topright', leg = levels(test$CD4ARTStartcat), ## add a legend with levels of the CD4 categories as the text
# #        col = 1:nlevels(test$CD4ARTStartcat), lwd = 2,  ## and the same colors as above
# #        title = 'CD4 at ART initiation')
# # dev.off()
# # 
# # 
# # ## Prevalence of OIs by time followed up
# # # Tabulate the number of OIs observed for KS, CM
# # table(test$KS)
# # table(test$CM)
# # table(test$PCP)
# # table(test$KS[test$ARVStatusCode>1])
# # table(test$CM[test$ARVStatusCode>1])
# # 
# # #Sum total follow-up time - didn't censor at first event since people can get KS/CM more than once?
# # get.maxfollow <- function(ayfu,pid){
# #   print(pid)
# #   return(max(ayfu,na.rm=T))
# # }
# # 
# # test <- ddply(test, .(pid), transform, maxfollow = get.maxfollow(ayfu,pid))
# # test <- ddply(test, .(pid), transform, maxfollowall = get.maxfollow(yfu,pid))
# # 
# # 
# # sum(test$KS[test$ARVStatusCode>1],na.rm=T)/sum(test$maxfollow[test$yfu==0])
# # sum(test$CM[test$ARVStatusCode>1],na.rm=T)/sum(test$maxfollow[test$yfu==0])
# # sum(test$KS,na.rm=T)/sum(test$maxfollowall[test$yfu==0])
# # sum(test$CM,na.rm=T)/sum(test$maxfollowall[test$yfu==0])
# # 
# # #Median CD4 at KS/CM diagnosis
# # summary(test$CD4[test$KS==1],na.rm=T)
# # summary(test$CD4[test$CM==1],na.rm=T)
# # 
# # ###############################################################################
# # 
# # # ## example of using 'incomparables'
# # # x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
# # # y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
# # # merge(x, y, by = c("k1","k2")) # NA's match
# # # # merge(x, y, by = c("k1","k2"), incomparables = NA)
# # # merge(x, y, by = "k1") # NA's match, so 6 rows
# # # merge(x, y, by = "k2", incomparables = NA) # 2 rows
# # 
# # 
# # # To plot your data and add a smoothed line on them.
# # # var3 - factor
# # # var1 & var2 - numerics
# # # p <- ggplot(data = namedataset,
# # #             aes(x = var1, y = var2, col = var3)) +
# # #   geom_point() + geom_smooth(method = "lm")
# # # print(p)