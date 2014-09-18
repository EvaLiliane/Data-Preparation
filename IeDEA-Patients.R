# Clear the workspace
rm(list=ls())

# CHECK THE MCCORD COHORT DATA.
# patdata[which(patdata1$cohort == "MCCORD"),])

# Load libraries to be used
library(foreign)
library(nlme)
library(plyr)
# The below library is not available for R 3.0.2
library(gdata)

setwd("/home/evaliliane/Documents/PhD/Codes")

# Set the most recent date on which PAT and LAB datasets have been generated.
#Sys.Date()
currentDate <- "2014-07-10"
patFile <- paste("NewData/patdata_",currentDate,".csv",sep="")
labFile <- paste("NewData/labdata_",currentDate,".csv",sep="")

############# READ PAT DATSET  ############################################################
# Load PAT data
patdata <- read.csv(file = patFile)

############# CLEANING PATDATA  ############################################################
# Define Factors
patdata$cohort <- as.factor(patdata$cohort)
patdata$patient <- as.factor(patdata$patient)

# Define Date
patdata$last_contact_dmy <- as.Date(patdata$last_contact_dmy)
print("Done 1")
patdata$outcome_dmy <- as.Date(patdata$outcome_dmy)
print("Done 2")
patdata$birth_dmy <- as.Date(patdata$birth_dmy)
print("Done 3")
patdata$haart_dmy <- as.Date(patdata$haart_dmy)
print("Done 4")
patdata$frsvis_dmy <- as.Date(patdata$frsvis_dmy)
print("Done 5")

# Define Characters
patdata$entry_other <- as.character(patdata$entry_other)

# Check that all patients in this file are on haart (haart == 1) and have HIV-1
length(unique(patdata$patient)) == length(which(patdata$haart == 1))
#length(unique(patdata$patient)) == length(patdata$hiv_type[patdata$hiv_type == 1])
npat <- length(unique(patdata$patient))

#DROP OUT People whose last contact data is greater than "2014-04-30" and NA. Error in entering last contact date.
rm.bad.contact.date <- patdata$patient[patdata$last_contact_dmy > "2014-04-30"]
print(paste("Patients whose last contact data is after 30.04.2014 are" ,length(rm.bad.contact.date)))
patdata <- patdata[!(patdata$patient %in% rm.bad.contact.date),]
#patdata <- patdata[which(!(is.na(patdata$last_contact_dmy))),]
#print(paste("Patients without contact date error" , length(patdata$patient)))

# DROP OUT People whose HAART initial date is before "2001-01-01" and NA. 
# They were probably on other treatments than HAART.
rm.bad.haart <- patdata$patient[patdata$haart_dmy < "2001-01-01"]
print(paste("Patients with init date before 2001 are" ,length(rm.bad.haart)))
rm.bad.haart.date <- patdata$patient[is.na(patdata$haart_dmy)]
print(paste("Patients with no haart.dmy are" ,length(rm.bad.haart.date)))
patdata <- patdata[!(patdata$patient %in% rm.bad.haart),] 
patdata <- patdata[which(!(is.na(patdata$haart_dmy))),]

# DROP OUT People whose gender is neither 1 or 2. 
rm.bad.gender <- patdata$patient[patdata$gender != 1 & patdata$gender != 2]
print(paste("Patients with weird gender" ,length(rm.bad.gender)))
patdata <- patdata[!(patdata$patient %in% rm.bad.gender),] 

# ADD AN AGE AT HAART INITIATION COLUMN ON THE PATDATASET
# Computing patients age at HAART initiation"
calc.age <- function (haart,birth) {
  age.haart.init2 <- as.numeric(difftime(haart,birth, units =  "days")/365) 
  return (age.haart.init2)
}
patdata$age.haart.init <- calc.age(patdata$haart_dmy,patdata$birth_dmy)
patdata$age.haart.init[is.na(patdata$age.haart.init)] <- as.numeric(format(patdata$haart_dmy[is.na(patdata$age.haart.init)],format = "%Y")) - patdata$birth_y[is.na(patdata$age.haart.init)]

#DROP OUT People with negative or NA age.
rm.bad.age <- patdata$patient[which(patdata$age.haart.init < 0)]
rm.bad.age1 <- patdata$patient[which(patdata$age.haart.init > 100)]
print(paste("Patients with neg age are" ,length(rm.bad.age)))
print(paste("Patients older than 100 years age are" ,length(rm.bad.age1)))
print(paste("Patients whose age is NA are ",length(patdata$age.haart.init[is.na(patdata$age.haart.init)])))

patdata <- patdata[!(patdata$patient %in% rm.bad.age),]
patdata <- patdata[which(!(is.na(patdata$age.haart.init))),]

# Alternative way
# patdata <- ddply(patdata,.(patient), mutate, calc.age(haart_dmy,birth_dmy))
# age.haart.init <- as.numeric(format(patdata$haart_dmy,format = "%Y")) - patdata$birth_y
# age.haart.init2 <- as.numeric(difftime(patdata$haart_dmy,patdata$birth_dmy, units =  "days")/365) 

# Print total number of patient
print(paste(" Initial number of Patients in pat dataset" , npat))
print(paste("Patients removed from pat dataset" , npat - length(patdata$patient)))
print(paste("Patients in cleaned patdataset" , length(unique(patdata$patient))))

# Separate children from adults 
child.patdata <- patdata[patdata$age.haart.init < 17,]
adult.patdata <- patdata[patdata$age.haart.init >= 17,]


################### LAB DATASET ###########################################################
# Load LAB dataset
labdata <- read.csv(file = labFile)
#labdata <- read.csv("NewData/labdata_20140702.csv")

# Define factors
labdata$cohort <- as.factor(labdata$cohort)
labdata$patient <- as.factor(labdata$patient)
labdata$lab_id <- as.factor(labdata$lab_id)

# Define Date
print("Prob is LAB ?")
labdata$lab_dmy <- as.Date(labdata$lab_dmy)
#str(labdata) 

############## Clean labdata datasets
print("Initial length")
print(paste("Number of patients in labdata", length(unique(labdata$patient))))
print(length(labdata$lab_v))

# Remove all rows with a negative or null value for "CD4A", "CD4P", "LYMP", "RNA", "WBC" and "NEUT" measurements.
print(paste("Initial values of patient in labdataset", length(unique(labdata$patient))))
print(paste("Total rows number", length(labdata$lab_v)))
print(paste("Rows with negative lab_v values", length(labdata$lab_v[labdata$lab_v < 0])))
print(paste("Rows with NA lab_v values", length(labdata$lab_v[is.na(labdata$lab_v)])))

labdata <- labdata[which(labdata$lab_v > 0 ),]
labdata <- labdata[!(is.na(labdata$lab_v)),]
print(paste("Remaining rows number after rm neg and NA", length(labdata$lab_v)))
print(paste("Remaining patients number after rm neg and NA", length(unique(labdata$patient))))

# Remove rows without date of measurement for lab_v
print("Removed non date")
labdata <- labdata[!(is.na(labdata$lab_dmy)),]
print(paste("After rm missing date for measurement ", length(unique(labdata$patient))))
print(paste("After rm missing date for measurement ", length(labdata$lab_v)))

#===========================================================================================================
# Remove all rows with an improbable value for "CD4A", "CD4P", "LYMP", "RNA", "WBC" and "NEUT" measurements
#===========================================================================================================

# First separate children from adults
child.labdata <- labdata[labdata$patient %in% child.patdata$patient,]
adult.labdata <- labdata[labdata$patient %in% adult.patdata$patient,]

####### ADULTS ########################## CHILDREN #######################
# Limit for CD4A 4000 okay          # 6000 okay
# Limit for CD4P 70 okay            # 
# Limit for LYMP 10000 okay         # 13000 okay
# Limit for RNA 1500000 ?           # 
# Limit for WBC 15  okay            # 20 okay
# Limit for NEUT 10 okay            #

# Adults
print(paste("Adults patient with bad CD4A values",length(unique(adult.labdata$patient[which((adult.labdata$lab_id == "CD4A") & (adult.labdata$lab_v > 4000))]))))
print(paste("Adults patient with bad CD4P values",length(unique(adult.labdata$patient[which((adult.labdata$lab_id == "CD4P") & (adult.labdata$lab_v > 70))] ))))
print(paste("Adults patient with bad LYMP values",length(unique(adult.labdata$patient[which((adult.labdata$lab_id == "LYMP") & (adult.labdata$lab_v > 10000))]))))
print(paste("Adults patient with bad RNA values",length(unique(adult.labdata$patient[which((adult.labdata$lab_id == "RNA") & (adult.labdata$lab_v > 1500000))]))))
print(paste("Adults patient with bad WBC values",length(unique(adult.labdata$patient[which((adult.labdata$lab_id == "WBC") & (adult.labdata$lab_v > 15))]))))
print(paste("Adults patient with bad NEUT values",length(unique(adult.labdata$patient[which((adult.labdata$lab_id == "NEUT") & (adult.labdata$lab_v > 10))]))))

adult.labdata <- adult.labdata[which(!((adult.labdata$lab_id == "CD4A") & (adult.labdata$lab_v > 4000))),]
adult.labdata <- adult.labdata[which(!((adult.labdata$lab_id == "CD4P") & (adult.labdata$lab_v > 70))),]
adult.labdata <- adult.labdata[which(!((adult.labdata$lab_id == "LYMP") & (adult.labdata$lab_v > 10000))),]
adult.labdata <- adult.labdata[which(!((adult.labdata$lab_id == "RNA") & (adult.labdata$lab_v > 1500000))),]
adult.labdata <- adult.labdata[which(!((adult.labdata$lab_id == "WBC") & (adult.labdata$lab_v > 15))),]
adult.labdata <- adult.labdata[which(!((adult.labdata$lab_id == "NEUT") & (adult.labdata$lab_v > 10))),]

# Children
print(paste("Children patient with bad CD4A values",length(unique(child.labdata$patient[which((child.labdata$lab_id == "CD4A") & (child.labdata$lab_v > 6000))]))))
print(paste("Children patient with bad CD4P values",length(unique(child.labdata$patient[which((child.labdata$lab_id == "CD4P") & (child.labdata$lab_v > 70))] ))))
print(paste("Children patient with bad LYMP values",length(unique(child.labdata$patient[which((child.labdata$lab_id == "LYMP") & (child.labdata$lab_v > 13000))]))))
print(paste("Children patient with bad RNA values",length(unique(child.labdata$patient[which((child.labdata$lab_id == "RNA") & (child.labdata$lab_v > 1500000))]))))
print(paste("Children patient with bad WBC values",length(unique(child.labdata$patient[which((child.labdata$lab_id == "WBC") & (child.labdata$lab_v > 20))]))))
print(paste("Children patient with bad NEUT values",length(unique(child.labdata$patient[which((child.labdata$lab_id == "NEUT") & (child.labdata$lab_v > 10))]))))

child.labdata <- child.labdata[which(!((child.labdata$lab_id == "CD4A") & (child.labdata$lab_v > 6000))),]
child.labdata <- child.labdata[which(!((child.labdata$lab_id == "CD4P") & (child.labdata$lab_v > 70))),]
child.labdata <- child.labdata[which(!((child.labdata$lab_id == "LYMP") & (child.labdata$lab_v > 13000))),]
child.labdata <- child.labdata[which(!((child.labdata$lab_id == "RNA") & (child.labdata$lab_v > 1500000))),]
child.labdata <- child.labdata[which(!((child.labdata$lab_id == "WBC") & (child.labdata$lab_v > 20))),]
child.labdata <- child.labdata[which(!((child.labdata$lab_id == "NEUT") & (child.labdata$lab_v > 10))),]

# print("Removed improbable values 1")
#print(length(labdata$lab_v))
# print("Removed improbable values 2")
#print(length(unique(labdata$patient)))
# print("Removed improbable values 3")
# print(length(unique(cond3$patient)))
# print("Removed improbable values 4")
# print(length(unique(cond4$patient)))
# print("Removed improbable values 5")
# print(length(unique(cond5$patient)))
# print("Removed improbable values 6")
# print(length(unique(cond6$patient)))

################### ART DATASETS ###########################################################
# LOAD ART DATA
artdata <- read.dta("/home/evaliliane/Documents/PhD/IeDEA/Data/c5_14_Final Data_vs3_1/c5_14_art_20140505.dta")

# Define factors
artdata$cohort <- as.factor(artdata$cohort)
artdata$patient <- as.factor(artdata$patient)
#str(artdata)

# Separate children from adults
child.artdata <- artdata[artdata$patient %in% child.patdata$patient,]
adult.artdata <- artdata[artdata$patient %in% adult.patdata$patient,]

################### VIS DATASETS ###########################################################
# LOAD VIS DATA
visdata <- read.dta("/home/evaliliane/Documents/PhD/IeDEA/Data/c5_14_Final Data_vs3_1/c5_14_vis_20140505.dta")

# Define factors
visdata$cohort <- as.factor(visdata$cohort)
visdata$patient <- as.factor(visdata$patient)
#str(visdata)

# Separate adults from children
child.visdata <- visdata[visdata$patient %in% child.patdata$patient,]
adult.visdata <- visdata[visdata$patient %in% adult.patdata$patient,]

################### OI DATASETS ###########################################################
# LOAD OI DATA
oidata <- read.dta("/home/evaliliane/Documents/PhD/IeDEA/Data/c5_14_Final Data_vs3_1/c5_14_oi_20140505.dta")

# Define factors
oidata$cohort <- as.factor(oidata$cohort)
oidata$patient <- as.factor(oidata$patient)
#str(oidata)

# Separate children from adults
child.oidata <- oidata[oidata$patient %in% child.patdata$patient,]
adult.oidata <- oidata[oidata$patient %in% adult.patdata$patient,]

########################## CREATE DATASETS MERGED DATASETS FOR MODELLING ADULTS & CHILDREN SEPARETELY ##########################################

dat.child.patdata <- data.frame(child.patdata$patient,child.patdata$cohort, child.patdata$age.haart.init, child.patdata$gender) 
names(dat.child.patdata) <- c("patient", "cohort", "age.haart.init", "gender")
dat.child.labdata <- data.frame(child.labdata$patient, child.labdata$lab_v, child.labdata$lab_id, child.labdata$rna_l) # , child.labdata$tim.vis
names(dat.child.labdata) <- c("patient", "lab_v", "lab_id", "rna_l")
dat.child.visdata <- data.frame(child.visdata$patient, child.visdata$weight, child.visdata$height)
names(dat.child.visdata) <- c("patient", "weight", "height")

system.time(child.data1 <- merge(dat.child.visdata, dat.child.patdata,by = "patient", all = TRUE))
system.time(child.data <- merge(child.data1, dat.child.labdata,by = "patient", all = TRUE))

dat.adult.patdata <- data.frame(adult.patdata$patient,adult.patdata$cohort, adult.patdata$age.haart.init, adult.patdata$gender) 
names(dat.adult.patdata) <- c("patient", "cohort", "age.haart.init", "gender")
dat.adult.labdata <- data.frame(adult.labdata$patient, adult.labdata$lab_v, adult.labdata$lab_id, adult.labdata$rna_l) # , adult.labdata$tim.vis
names(dat.adult.labdata) <- c("patient", "lab_v", "lab_id", "rna_l")
dat.adult.visdata <- data.frame(adult.visdata$patient, adult.visdata$weight, adult.visdata$height)
names(dat.adult.visdata) <- c("patient", "weight", "height")

system.time(adult.data1 <- merge(dat.adult.visdata, dat.adult.patdata,by = "patient", all = TRUE))

print("The below line broke the code. To be checked.")
system.time(adult.data <- merge(adult.data1, dat.adult.labdata,by = "patient", all = TRUE))
print("Well done !")

#adult.data <- merge()

# ########################## SAVING ALL DATASET INTO ADULTS & CHILDREN SEPARETELY ##########################################
# # Merging creates file that are too big and heaving. Keep all files separetly for adults and children.
# # CHILDREN
# print("Start children")
# #child.data <- rbind.fill(child.patdata,child.labdata,child.artdata,child.visdata, child.oidata)
# 
# print("Save child data")
# currentDate <- Sys.Date()
# labFileName <- paste("NewData/children_labdata",currentDate,".csv",sep="")
# patFileName <- paste("NewData/children_patdata",currentDate,".csv",sep="")
# visFileName <- paste("NewData/children_visdata",currentDate,".csv",sep="")
# oiFileName <- paste("NewData/children_oidata",currentDate,".csv",sep="")
# artFileName <- paste("NewData/children_artdata",currentDate,".csv",sep="")
# 
# write.csv(child.patdata,file=patFileName)
# write.csv(child.labdata,file=labFileName)
# write.csv(child.artdata,file=artFileName)
# write.csv(child.oidata,file=oiFileName)
# write.csv(child.visdata,file=visFileName)
# 
# print(paste("Number of children in patdata", length(unique(child.patdata$patient))))
# print(paste("Number of children in labdata", length(unique(child.labdata$patient))))
# print(paste("Number of children in visdata", length(unique(child.visdata$patient))))
# print(paste("Number of children in oidata", length(unique(child.oidata$patient))))
# print(paste("Number of children in artdata", length(unique(child.artdata$patient))))
# 
# FileName1 <- paste("NewData/cleaned_patdata",currentDate,".csv",sep="")
# write.csv(patdata,file=FileName1)
# FileName2 <- paste("NewData/cleaned_labdata",currentDate,".csv",sep="")
# labdata2 <- merge(adult.labdata,child.labdata, by = intersect(names(adult.labdata), names(child.labdata)), all = TRUE)
# write.csv(labdata2,file=FileName2)
# 
# rm(child.patdata)
# rm(child.labdata)
# rm(child.visdata)
# rm(child.oidata)
# rm(child.artdata)
# 
# # ADULTS
# print("Start adults")
# #adult.data <- rbind.fill(adult.patdata,adult.labdata,adult.artdata,adult.visdata, adult.oidata)
# 
# print("Save adult data")
# LabFileName <- paste("NewData/adult_labdata",currentDate,".csv",sep="")
# PatFileName <- paste("NewData/adult_patdata",currentDate,".csv",sep="")
# VisFileName <- paste("NewData/adult_visdata",currentDate,".csv",sep="")
# OiFileName <- paste("NewData/adult_oidata",currentDate,".csv",sep="")
# ArtFileName <- paste("NewData/adult_artdata",currentDate,".csv",sep="")
# 
# print(paste("Number of adults in patdata", length(unique(adult.patdata$patient))))
# print(paste("Number of adults in labdata", length(unique(adult.labdata$patient))))
# print(paste("Number of adults in visdata", length(unique(adult.visdata$patient))))
# print(paste("Number of adults in oidata", length(unique(adult.oidata$patient))))
# print(paste("Number of adults in artdata", length(unique(adult.artdata$patient))))
# 
# write.csv(adult.patdata,file=PatFileName)
# write.csv(adult.labdata,file=LabFileName)
# write.csv(adult.artdata,file=ArtFileName)
# write.csv(adult.oidata,file=OiFileName)
# write.csv(adult.visdata,file=VisFileName)
# 
