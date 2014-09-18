# Clear the workspace
rm(list=ls())

# Load libraries to be used
library(foreign)

setwd("/home/evaliliane/Documents/PhD/Codes")

####################  PAT DATASET ###########################################################
# Read data
patdata1 <- read.dta("/home/evaliliane/Documents/PhD/IeDEA/Data/c5_14_Final Data_vs3_1/c5_14_pat_20140505.dta")
# Select needed variables
patdata <- data.frame(patdata1$patient , patdata1$cohort, patdata1$birth_dmy, patdata1$birth_y, patdata1$birth_m, patdata1$gender, patdata1$frsvis_dmy, patdata1$entry, patdata1$entry_other, patdata1$mode, patdata1$haart, patdata1$haart_dmy, patdata1$fhv_stage_who, patdata1$exp_y, patdata1$mtct_y, patdata1$pep_y, patdata1$tb_fhv, patdata1$wks_tb_fhv, patdata1$preg_fhv, patdata1$last_contact_dmy, patdata1$last_contact_t, patdata1$outcome, patdata1$outcome_dmy, patdata1$outcome_y, patdata1$outcome_m, patdata1$death_txt, patdata1$deliv_m, patdata1$weight_birth)
# Define columns names
names(patdata) <- c("patient" , "cohort", "birth_dmy", "birth_y", "birth_m", "gender", "frsvis_dmy", "entry", "entry_other", "mode", "haart", "haart_dmy", "fhv_stage_who", "exp_y", "mtct_y", "pep_y", "tb_fhv", "wks_tb_fhv", "preg_fhv", "last_contact_dmy", "last_contact_t", "outcome", "outcome_dmy", "outcome_y", "outcome_m", "death_txt", "deliv_m", "weight_birth")

# save the dataset
currentDate <- Sys.Date()
patFileName <- paste("NewData/patdata_",currentDate,".csv",sep="")
write.csv(patdata,file=patFileName)

################### LAB DATASET ###########################################################
# Load LAB dataset
labdata1 <- read.dta("/home/evaliliane/Documents/PhD/IeDEA/Data/c5_14_Final Data_vs3_1/c5_14_lab_20140505.dta")
labdata1$lab_id <- as.factor(labdata1$lab_id)
# Select variables
labdata <- labdata1[which(labdata1$lab_id == "CD4A" | labdata1$lab_id == "CD4P" | labdata1$lab_id == "NEUT" | labdata1$lab_id == "LYMP" | labdata1$lab_id == "RNA" | labdata1$lab_id == "WBC"),]

# Save dataset
labFileName <- paste("NewData/labdata_",currentDate,".csv",sep="")
write.csv(labdata,file=labFileName)
