#######################
# This code produces the values needed for the descriptive part of the NLME paper.
# Written: Jan 29th, 2015 by Eva Liliane Ujeneza  
# Contains pieces of Arianna's codes written for the OI MMED group project.
#######################

## Read in the data and load libraries
library(foreign)
library(plyr) 

#library(reshape2)

setwd("/home/evaliliane/Documents/PhD/Codes")
#setwd("/home/evaliliane/Documents/SACEMA/Nov Seminar")
currentDate <- Sys.Date()

for(i in 1:3){
  if(i == 1){
    addata <- read.csv("/home/evaliliane/Documents/PhD/Python/ChildrenZscores.csv")
    ad <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/children_oidata2014-08-22.csv")
    message <- "This is the Children output for Table 1 "
    brks <- c(0,200,400,600,800,1000,5000)
    filename <- paste("NewData/CD4Cat_Children_Modified",currentDate,".csv",sep="")
    sink("Table1_Children-Modified_output.txt")
    print(message)
    
  }
  if(i == 2){
    addata <- read.csv("/home/evaliliane/Documents/PhD/Python/AdultData_Time_Slope_Base.csv")
    ad <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/adult_oidata2014-08-22.csv")
    message <- "This is the Adults output for Table 1 "
    brks <- c(0,100,200,300,5000)
    sink("Table1_Adults_output.txt")
    filename <- paste("NewData/CD4Cat_Adults",currentDate,".csv",sep="")
    print(message)
  }   
  if(i == 3){
    addata <- read.csv("/home/evaliliane/Documents/PhD/Python/ChildrenZscores.csv")
    ad <- read.csv("/home/evaliliane/Documents/PhD/Codes/NewData/children_oidata2014-08-22.csv")
    message <- "This is the Children output for Table 1 "
    brks <- c(0,100,200,300,5000)
    sink("Table1_Children_output.txt")
    filename <- paste("NewData/CD4Cat_Children",currentDate,".csv",sep="")
    print(message)
    
  }

ad$patient <- as.factor(ad$patient)
ad$oi_id <- as.factor(ad$oi_id)
ad$oi_sd_dmy <- as.Date(ad$oi_sd_dmy)

# ch$patient <- as.factor(ch$patient)
# ch$oi_id <- as.factor(ch$oi_id)
# ch$oi_sd_dmy <- as.Date(ch$oi_sd_dmy)
# 
# addata <- read.csv("/home/evaliliane/Documents/PhD/Python/AdultData_Time_Slope_Base.csv")
# chdata <- read.csv("/home/evaliliane/Documents/PhD/Python/ChildrenZscores.csv")

str(addata)
addata <- addata[!(is.na(addata$diff)),]
#head(addata)

# str(chdata)
# head(chdata)

##Create a variable for CD4 categories at ART initiation
addatabase <-addata[addata$base == 1,]                                             # Categories' limits
addatabase$cd4a.categ <- cut(addatabase$lab_v, breaks = brks)             # Add categories variable to the baseline dataset
sub.addata <- subset(addatabase, select = c("patient", "cd4a.categ"))     # Subset category & patient variables
ddat <- merge(addata, sub.addata, by = "patient", all = FALSE, incomparables = NA )     # Add categories variable to the entire dataset
ddat <- ddat[!(is.na(ddat$diff)),]
ddat <- ddat[order(ddat$patient, ddat$diff),]

print("Number of rows")
print(dim(addata))
print(dim(ddat))

######################### DEMOGRAPHIC INFOS ##########################

#ddply(ddat,.(patient),transform, maxtofu = max(diff))
test <-ddat[!duplicated(ddat$patient),]

print(table(test$cd4a.categ,test$suppress))

print('##How many people in sample?')
print(length(test$patient))
print(length(unique(addata$patient)))
print(length(unique(ddat$patient)))
print(length(unique(addatabase$patient)))


print('## How many per gender')
print(table(test$gender, useNA = "ifany"))


print('## Patients age quantiles & categories')
print(summary(test$age))
print('compare')
print(summary(ddat$age[unique(ddat$patient)]))

print(length(test$patient[0 <=  test$age & test$age < 1]))  
print(length(test$patient[1 <= test$age & test$age < 3]))
print(length(test$patient[3 <= test$age & test$age < 6]))
print(length(test$patient[6 <= test$age & test$age < 10]))
print(length(test$patient[10 <= test$age]))

print(length(test$patient[17 <=  test$age & test$age < 25]))   
print(length(test$patient[25 <= test$age & test$age < 35]))
print(length(test$patient[35 <= test$age & test$age < 50]))
print(length(test$patient[50 <= test$age]))


print('## Patients bmi quantiles')

#print(summary(test$bmi))   
# Still need to be fixed. Only NAs now.
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   
# Interquantile 


print('## Time of follow up')
#print(summary(test$maxtofu))
print("# compare")
print(length(unique(ddat$patient[0 <=  ddat$diff/365 & ddat$diff/365 < 3])))   
print(length(unique(ddat$patient[3 <= ddat$diff/365 & ddat$diff/365 < 6])))
print(length(unique(ddat$patient[6 <= ddat$diff/365 & ddat$diff/365 < 9])))
print(length(unique(ddat$patient[9 <= ddat$diff/365 & ddat$diff/365 < 12])))
print(length(unique(ddat$patient[12 <= ddat$diff/365])))

print('## Time of follow up greater than 10 years')
print(length(unique(ddat$patient[10 <= ddat$diff/365])))

print('## Number of people per time of follow up & cd4 category')
#print(ddply(test,.(cd4a.categ),summarize, M = mean(maxtofu), S=sd(maxtofu), C = length(maxtofu))) 


print('## WHO stages')
print(length(unique(ddat$patient[ddat$fhv_stage_who == 1])))  
print(length(unique(ddat$patient[ddat$fhv_stage_who == 2]))) 
print(length(unique(ddat$patient[ddat$fhv_stage_who == 3]))) 
print(length(unique(ddat$patient[ddat$fhv_stage_who == 4]))) 
print(length(unique(ddat$patient[ddat$fhv_stage_who == 95])))
print(length(unique(ddat$patient[is.na(ddat$fhv_stage_who)])))
print("# compare")
print(table(test$fhv_stage_who, useNA = "ifany"))


print('## Outcomes')
print(length(unique(ddat$patient[ddat$outcome == 20])))  
print(length(unique(ddat$patient[ddat$outcome == 11])) + length(unique(ddat$patient[ddat$outcome == 12])) + length(unique(ddat$patient[ddat$outcome == 10]))) 
print(length(unique(ddat$patient[ddat$outcome == 41])) + length(unique(ddat$patient[ddat$outcome == 40])))
print("# compare")
print(table(test$outcome), useNA = "ifany")


######################### CLINICAL INFORMATIONS ##########################

print('## How many unique patients per CD4 categories')
print(table(test$cd4a.categ, useNA = "ifany"))


print("Viral suppression within 12 mths per CD4 categories")
print(table(test$suppress, test$cd4a.categ, useNA = "ifany"))


print("Viral load at baseline per CD4 categories")
subno_nas <- ddat[ddat$base == 1,]
print(ddply(subno_nas, .(cd4a.categ),summarize,meanval = mean(viral), stdval = sd(viral), countval = length(!duplicated(patient))))
print("check")
print(ddply(subno_nas, .(cd4a.categ),summarize,meanval = mean(viral), stdval = sd(viral), countval = length(patient), countNAs = length(is.na(viral))))


print('## Interquantile range of OIs number per patient')
# Functions
count2 <- function(pid,nx,browse=F){    # Sums number of OIs per pid.
  if(browse) browser()
  return(sum(nx))
}

count1 <- function(pid,oi_id,nx,browse=F){   # Remove OI repetitions per pid (Put 1 for new OI type and 0 for repetion).
  if(browse) browser()
  nt <- sum(nx)
  n <- length(nx)
  if(n == 1)
    return(1)
  else
    res <- rep(0, n)
    res[1] <- 1
    return(res)  
}

ad$nx <- 1  
#ad1 <- ad[1:2500,]
# ad <- ddply(ad, .(patient,oi_id), transform, tes1 = count1(patient,oi_id,nx))
# ad <- ddply(ad, .(patient), transform, tes2 = count2(patient,tes1))  
# 
# print(summary(ad$tes2))

print('# Number of occurence per OI Id.')
print(sort(summary(ad$oi_id)))    # Number of cases per OI Id.
print('# Number of occurence per unique patient for each OI Id.')
print(sort(summary(ad$oi_id[!duplicated(ad$patient)])))    # Number of cases per OI Id.


print( "Check pregnancies & children ages per CD4 categories")
print(ddply(test,.(cd4a.categ, gender), summarize, M = mean(age), S = sd(age), C = length(patient)))

print("# Check time of follow up for suppress .vs. non suppressed group")
datbase <- ddat[ddat$diff > 3650,]
print(ddply(datbase,.(cd4a.categ, suppress), summarize, M = mean(diff/365), S = sd(diff/365), BIG = max(diff/365), C = length(!duplicated(patient))))
sink()


write.csv(ddat,file = filename)
}


