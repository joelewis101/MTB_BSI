##########################################################
#####   MTBBSI IPDMA primary data processing script   ####
#####          **** CRUMP 2012****                ######
##########################################################

##### NOTES
### need to add in CXR stuff
### I've made some assumptions, below
### Christ this is complicated


####

#### 0. Set up

### 0.1 set working directory
#set constant for path, in case of change (e.g. different machine)
wdpath = "/Users/joelewis/Documents/Projects/MTB_BSI/data"


### 0.2 required packages
#library(tidyr)
#library(ggplot2)
#library(dplyr)

#### 1. Load raw data ####
# Expects a number of .csvs - below

adm <- read.csv(paste0(wdpath , "/crump/Crump_admission.csv"), header=TRUE)
dc <- read.csv(paste0(wdpath , "/crump/Crump_discharge.csv"), header=TRUE)
Crump_BCs <- read.csv(paste0(wdpath , "/crump/Crump_BCs.csv"), header=TRUE)
r_wide <- read.csv(paste0(wdpath , "/crump/Crump_results_wide.csv"), header=TRUE)
r_other <- read.csv(paste0(wdpath , "/crump/Crump_results_other.csv"), header=TRUE)
CXR <- read.csv(paste0(wdpath , "/crump/Crump_CXR.csv"), header=TRUE)

Crump_BCs_lookup <- read.csv(paste0(wdpath , "/crump/Duke_OrganismCodes.csv"), header=TRUE)

lookup <- Crump_BCs_lookup[c(1,2)]

## one patient admitted twice
# delete second admission
adm <- adm[-276,]


## the df crump_BCs contains ALL the blood cultures for the study patients (plus some more)
## three types of BC - MV, MycoF and ISOL. A variable number of each for each patient

## Plan: generate 3 data frames, one for each blood culture type with one row for each PID 
## and data on number of that type of BC etc. Then merge on PID

## BCs - MB looks like a MB BC was sent
## MB Res  is 1/0 pos/neg
## Can ignore MBOrg 2 - no TB has 2 orgs rest are contaminent

BCs <- Crump_BCs[c("PID","CollD","RecD","MB","MBWtUninoc","MBWtInoc","MBLoadD","MBRes","MBPosD","MBOrg1","MBOrg2")]

BCs$ORG.NO <- BCs$MBOrg1
BCs$ORG.NO[is.na(BCs$ORG.NO)] <- 0
BCs$MBOrg1[is.na(BCs$MBOrg1)] <- 0
BCs <- merge(BCs, lookup)
BCs <- BCs [,-1]
names(BCs)[names(BCs) == "ORG.NAME"] <- "MBOrgName"

# choose only those rows where MBC were sent

BCs <- (subset(BCs, (PID %in% adm$PID)))
BCs <- subset(BCs, !is.na(BCs$MB))
BCs <- subset(BCs, MB == 1)

# two patients had two MB blood cultures sent
# 1291863K 1291931K
# On had contaminents in both, one contaminents in one (the first)
# take the first culture for each for TTP etc
# isn't really going to affect analysis

p<- as.data.frame(table(BCs$PID))
two_BCs <- p$Var1[p$Freq > 1] 

# set dates as dates

BCs$CollD <- as.Date(as.character(BCs$CollD), "%m/%d/%Y")
BCs$RecD <- as.Date(as.character(BCs$RecD), "%m/%d/%Y")
BCs$MBLoadD <- as.Date(as.character(BCs$MBLoadD), "%m/%d/%Y")
BCs$MBPosD <- as.Date(as.character(BCs$MBPosD), "%m/%d/%Y")

## generate a new table; each PID will have 1 row with number of MB BCs

n_pids <- as.character(levels(BCs$PID))
tempout <- BCs[1,]

for  (i in 1:length(n_pids)) {
  temp <- subset(BCs,PID== n_pids[i])
  if (nrow(temp) > 1) { 
    temp <- temp[order(temp$CollD),]
    temp <- temp[1,]
  }
  tempout <- rbind(tempout,temp)
}

tempout <- tempout[-1,]
BCs <- tempout

# set number of ALL mycobac BCs (total OSOL, MB, MyoF) as n_MBC

#BCs$n_MBC <- 1
BCs$MB[BCs$PID %in% two_BCs] <- 2

MB <- BCs

## do the same for ISOL

MBIsolBCs <- Crump_BCs[c("PID","CollD","RecD","ISOL","IsoVol","IsolProcD","IsolPosD","IsolOrg1","IsolOrg2")]
MBIsolBCs <- subset(MBIsolBCs, (PID %in% adm$PID))
MBIsolBCs <- subset(MBIsolBCs, !is.na(ISOL))
MBIsolBCs <- MBIsolBCs[-c(2,3)]

MBIsolBCs$ORG.NO <- MBIsolBCs$IsolOrg1
MBIsolBCs$ORG.NO[is.na(MBIsolBCs$ORG.NO)] <- 0
MBIsolBCs$IsolOrg1[is.na(MBIsolBCs$IsolOrg1)] <- 0
MBIsolBCs <- merge(MBIsolBCs, lookup)
MBIsolBCs <- MBIsolBCs [,-1]
names(MBIsolBCs)[names(MBIsolBCs) == "ORG.NAME"] <- "ISOLOrgName"


BCs <- merge (MB,MBIsolBCs, by = "PID", all = TRUE)
BCs$ISOL[is.na(BCs$ISOL)] <- 0 

### these were all done on patients that had an MB sample sent - no new TBs or NTMs
#### so will just add 1 to n_MBC for all of these

BCs$n_MBC[BCs$ISOL == 1] <- BCs$n_MBC[BCs$ISOL == 1] + 1



### there is also MycoF variable for the THIRD culture system they used 
MycoFBCs <- Crump_BCs[c("PID","CollD","RecD","MycoF","MycFWtUninoc","MycFWtInoc","MycFIncD","MycFRes","MycFPosD", "MycFOrg1","MycFOrg2")]
MycoFBCs <- (subset(MycoFBCs, (PID %in% adm$PID)))
MycoFBCs <- subset(MycoFBCs, !is.na(MycoFBCs$MycoF))
MycoFBCs <- subset(MycoFBCs, MycoFBCs$MycoF == 1)

# There is only one sample for each patient- I checked
# Also all sent same date as other MB BCs so lose dates

MycoFBCs <- MycoFBCs[-c(2,3)]
MycoFBCs$ORG.NO <- MycoFBCs$MycFOrg1

MycoFBCs$ORG.NO[is.na(MycoFBCs$ORG.NO)] <- 0
MycoFBCs$MycFOrg1[is.na(MycoFBCs$MycFOrg1)] <-0
MycoFBCs <- merge(MycoFBCs, lookup)
MycoFBCs <- MycoFBCs [,-1]
names(MycoFBCs)[names(MycoFBCs) == "ORG.NAME"] <- "MycoFOrgName"

BCs <- merge (BCs,MycoFBCs, by = "PID", all = TRUE)

### there are no patients with two samples - I guess this is probably the complete data set of BCs?
### n = 12 MTB, which is right
### I'm assuming the organism coded 1403 is the MOTT they mention in the manuscript (the data dictionary doesn't go this far)
### So - assume everything except TB is a contaminent 
### (because it would have grown in these bottles potentially overgrowing TB)

### add 1 sample for all those that have a MycoF

BCs$ISOL[is.na(BCs$ISOL)] <- 0
BCs$MB[is.na(BCs$MB)] <-0
BCs$MycoF[is.na(BCs$MycoF)] <-0

BCs$n_MBC <- 0
BCs$n_MBC <- BCs$ISOL + BCs$MB + BCs$MycoF

### make a new variable - MTB_grew {1,0} for ANY OF MycoF or MB or ISOL that grew MTB
## MTB is coded as 1397



BCs$contam_MycoF <- 0
BCs$contam_MycoF[!is.na(BCs$MycFOrg1) & (BCs$MycFOrg1 == 409 | BCs$MycFOrg1 == 413 | BCs$MycFOrg1 == 1093 | BCs$MycFOrg1 == 404 | BCs$MycFOrg1 == 1298)] <-1

BCs$other_path_MycoF <- ""
BCs$other_path_MycoF[!is.na(BCs$MycFOrg1) & (BCs$MycFOrg1) != 1397 & BCs$MycFOrg1 != 409 & BCs$MycFOrg1 != 413 & BCs$MycFOrg1 != 1093 & BCs$MycFOrg1 != 404 & BCs$MycFOrg1 != 1298] <- as.character(BCs$MycoFOrgName[!is.na(BCs$MycFOrg1) & (BCs$MycFOrg1) != 1397 & BCs$MycFOrg1 != 409 & BCs$MycFOrg1 != 413 & BCs$MycFOrg1 != 1093 & BCs$MycFOrg1 != 404 & BCs$MycFOrg1 != 1298])

BCs$other_path_MycoF[BCs$other_path_MycoF == "NONE"] <- ""

BCs$contam_ISOL <- 0
BCs$contam_ISOL[!is.na(BCs$IsolOrg1) & (BCs$IsolOrg1 == 409 | BCs$IsolOrg1 == 413 | BCs$IsolOrg1 == 1093 | BCs$IsolOrg1 == 404 | BCs$IsolOrg1 == 1298)] <-1


BCs$other_path_ISOL <- ""
BCs$other_path_ISOL[!is.na(BCs$IsolOrg1) & (BCs$IsolOrg1) != 1397 & BCs$IsolOrg1 != 409 & BCs$IsolOrg1 != 413 & BCs$IsolOrg1 != 1093 & BCs$IsolOrg1 != 404 & BCs$IsolOrg1 != 1298] <- as.character(BCs$ISOLOrgName[!is.na(BCs$IsolOrg1) & (BCs$IsolOrg1) != 1397 & BCs$IsolOrg1 != 409 & BCs$IsolOrg1 != 413 & BCs$IsolOrg1 != 1093 & BCs$IsolOrg1 != 404 & BCs$IsolOrg1 != 1298])
BCs$other_path_ISOL[BCs$other_path_ISOL == "NONE"] <- ""

BCs$contam_MB <- 0
BCs$contam_MB[!is.na(BCs$MBOrg1) & (BCs$MBOrg1 == 409 | BCs$MBOrg1 == 413 | BCs$MBOrg1 == 1093 | BCs$MBOrg1 == 404 | BCs$MBOrg1 == 1298)] <-1

BCs$other_path_MB <- ""
BCs$other_path_MB[!is.na(BCs$MBOrg1) & (BCs$MBOrg1) != 1397 & BCs$MBOrg1 != 409 & BCs$MBOrg1 != 413 & BCs$MBOrg1 != 1093 & BCs$MBOrg1 != 404 & BCs$MBOrg1 != 1298] <- as.character(BCs$MBOrgName[!is.na(BCs$MBOrg1) & (BCs$MBOrg1) != 1397 & BCs$MBOrg1 != 409 & BCs$MBOrg1 != 413 & BCs$MBOrg1 != 1093 & BCs$MBOrg1 != 404 & BCs$MBOrg1 != 1298])
BCs$other_path_MB[BCs$other_path_MB == "NONE"] <- ""

BCs$MTB_grew <- 0
BCs$MTB_grew[BCs$MBOrg1 == 1397 | BCs$IsolOrg1 == 1397 | BCs$MycFOrg1 == 1397 ] <- 1


BCs$n_failed_BC <- 0

# generate number of available BCs

BCs$n_contam_BC <- BCs$contam_MB + BCs$contam_ISOL + BCs$contam_MycoF
BCs$n_avail_BC <- BCs$n_MBC - BCs$n_contam_BC

# there are no BCs where 2 different pathogens are grown

BCs$other_path <- ""
BCs$other_path[BCs$other_path_MB != ""] <- BCs$other_path_MB[BCs$other_path_MB != ""]
BCs$other_path[BCs$other_path_ISOL != "" & BCs$other_path == ""] <- BCs$other_path_ISOL[BCs$other_path_ISOL != "" & BCs$other_path == ""] 
BCs$other_path[BCs$other_path_MycoF != "" & BCs$other_path == "" ] <- BCs$other_path_MycoF[BCs$other_path_MycoF != "" & BCs$other_path == "" ]

### set volume BC as highest vol of any avaiable

# make MBvol and remove impossible values
BCs$MBVol <- BCs$MBWtInoc - BCs$MBWtUninoc 
BCs$MBVol[BCs$MBVol < 0 ] <- NA
BCs$MBVol[BCs$MBVol > 10 ] <- NA

# Make MycoFvol and remove impossible values
BCs$MycoFVol <- as.numeric(BCs$MycFWtInoc) - as.numeric(BCs$MycFWtUninoc)
BCs$MycoFVol[BCs$MycoFVol > 10 ] <- NA 

# Make ISOVOLFvol and remove impossible values

BCs$IsoVol <- as.numeric(BCs$IsoVol)
BCs$IsoVol[BCs$IsoVol > 10] <- NA

# put the highest of the three intoBC_vol:

BCs$BC_vol <- BCs$MBVol
# IF BC_vol is NA add in mycoF instead
BCs$BC_vol[is.na(BCs$BC_vol) & !is.na(BCs$MycoFVol)] <- BCs$MycoFVol[is.na(BCs$BC_vol) & !is.na(BCs$MycoFVol)]
# if MycoFvol is higher, add that
BCs$BC_vol[!is.na(BCs$BC_vol < BCs$MycoFVol)] <- BCs$MycoFVol[!is.na(BCs$BC_vol < BCs$MycoFVol)] 
# If BC-vol is NA add in Isovol
BCs$BC_vol[is.na(BCs$BC_vol) & !is.na(BCs$IsoVol)] <- BCs$IsoVol[is.na(BCs$BC_vol) & !is.na(BCs$IsoVol)]
#if isovol is higher add in
BCs$BC_vol[!is.na(BCs$BC_vol < BCs$IsoVol)] <- BCs$IsoVol[!is.na(BCs$BC_vol < BCs$IsoVol)] 



# positive culture date; if there's a positive TB culture, choose that. If there's two, choose the earliest

BCs$MBTTP <- as.numeric(BCs$MBPosD - BCs$MBLoadD)
# 1 neg one - assume they got dates wrong way round and reverse
BCs$MBTTP[BCs$MBTTP < 0 & !is.na(BCs$MBTTP < 0)] <- - BCs$MBTTP[BCs$MBTTP < 0  & !is.na(BCs$MBTTP < 0)]

BCs$MBTTP[BCs$MBTTP < 0 & !is.na(BCs$MBTTP < 0)] <- - BCs$MBTTP[BCs$MBTTP < 0 & !is.na(BCs$MBTTP < 0)]

BCs$IsolProcD <- as.Date(as.character(BCs$IsolProcD), "%m/%d/%Y")
BCs$IsolPosD <- as.Date(as.character(BCs$IsolPosD), "%m/%d/%Y")
BCs$IsolTTP <- as.numeric(BCs$IsolPosD - BCs$IsolProcD)

BCs$MycFIncD <- as.Date(as.character(BCs$MycFIncD), "%m/%d/%Y")
BCs$MycFPosD <- as.Date(as.character(BCs$MycFPosD), "%m/%d/%Y")

BCs$MycFTTP <- as.numeric(BCs$MycFPosD - BCs$MycFIncD)

# first do MTB TTPs
# For MTB TTPS ONLY
## make new variable

BCs$fastest_ttp <- NA

# set it to the ttp for the TBs for MB
BCs$fastest_ttp[!is.na(BCs$MBTTP) & BCs$MBOrg1 == 1397] <- BCs$MBTTP[!is.na(BCs$MBTTP) & BCs$MBOrg1 == 1397]

# if isol is not NA fastest_ttp is, set to isl_ttp
BCs$fastest_ttp[is.na(BCs$fastest_ttp) & !is.na(BCs$IsolTTP) & BCs$IsolOrg1 == 1397] <- BCs$isolTTP[is.na(BCs$fastest_ttp) & !is.na(BCs$IsolTTP) & BCs$IsolOrg1 == 1397]
BCs$fastest_ttp[is.na(BCs$fastest_ttp) & !is.na(BCs$IsolTTP) & BCs$IsolOrg1 == 1397 & (BCs$fastest_ttp > BCs$IsolTTP)] <- BCs$isolTTP[is.na(BCs$fastest_ttp) & !is.na(BCs$IsolTTP) & BCs$IsolOrg1 == 1397 & (BCs$fastest_ttp > BCs$IsolTTP)]

# if fastest_ttp still NA and MycoF_ttp is not, set to the latter
BCs$fastest_ttp[is.na(BCs$fastest_ttp) & !is.na(BCs$MycFTTP ) & BCs$MycFOrg1 == 1397] <- BCs$MycFTTP [is.na(BCs$fastest_ttp) & !is.na(BCs$MycFTTP) & BCs$MycFOrg1 == 1397]

# if mycof is faster than fastes, set to myco f
BCs$fastest_ttp[is.na(BCs$fastest_ttp) & !is.na(BCs$MycFTTP ) & BCs$MycFOrg1 == 1397 & (BCs$fastest_ttp > BCs$MycFTTP) ] <- BCs$MycFTTP[is.na(BCs$fastest_ttp) & !is.na(BCs$MycFTTP) & BCs$MycFOrg1 == 1397 & (BCs$fastest_ttp > BCs$MycFTTP)]


## now do same for non -tbs
# set it to the ttp for the TBs for MB
BCs$fastest_ttp[!is.na(BCs$MBTTP) & BCs$MBOrg1 != 1397 & BCs$MBOrg1 != 0 ] <- BCs$MBTTP[!is.na(BCs$MBTTP) & (BCs$MBOrg1 != 1397 & BCs$MBOrg1 != 0)]

# if isol is not NA fastest_ttp is, set to isl_ttp
BCs$fastest_ttp [is.na(BCs$fastest_ttp) & !is.na(BCs$IsolTTP) & (BCs$IsolOrg1 != 1397 & BCs$IsolOrg1 != 0)] <- BCs$IsolTTP[is.na(BCs$fastest_ttp) & !is.na(BCs$IsolTTP) & (BCs$IsolOrg1 != 1397 & BCs$IsolOrg1 != 0)]

BCs$fastest_ttp[is.na(BCs$fastest_ttp) & !is.na(BCs$IsolTTP) & (BCs$MBOrg1 != 1397 & BCs$MBOrg1 != 0) & (BCs$fastest_ttp > BCs$IsolTTP)] <- BCs$IsolTTP[is.na(BCs$fastest_ttp) & !is.na(BCs$IsolTTP) & (BCs$MBOrg1 != 1397 & BCs$MBOrg1 != 0) & (BCs$fastest_ttp > BCs$IsolTTP)]

# if fastest_ttp still NA and MycoF_ttp is not, set to the latter
BCs$fastest_ttp[!is.na(BCs$fastest_ttp) & !is.na(BCs$MycFTTP ) & (BCs$MBOrg1 != 1397 & BCs$MBOrg1 != 0)] <- BCs$MycFTTP [!is.na(BCs$fastest_ttp) & !is.na(BCs$MycFTTP) & (BCs$MBOrg1 != 1397 & BCs$MBOrg1 != 0)]

# if mycof is faster than fastes, set to myco f
BCs$fastest_ttp[!is.na(BCs$fastest_ttp) & !is.na(BCs$MycFTTP ) & (BCs$MBOrg1 != 1397 & BCs$MBOrg1 != 0) & (BCs$fastest_ttp > BCs$MycFTTP) ] <- BCs$MycFTTP[!is.na(BCs$fastest_ttp) & !is.na(BCs$MycFTTP) & (BCs$MBOrg1 != 1397 & BCs$MBOrg1 != 0) & (BCs$fastest_ttp > BCs$MycFTTP)]





df<- merge(adm,BCs, by= "PID", all= TRUE)
dc<- dc[c("PID","Died","DischargeDate")]
dc <- subset(dc,PID %in% df$PID)
df<- merge(df,dc, by= "PID", all = TRUE)




### The r_other df contains ALL INVESTIGATIONS for all study patients and others not in the
### study, linked by PID to the patient
### some patients have multiple investigations eg WBC done lots
### so for each investigation that we need, plan to extract the FIRST AVAILABLE to a new
### df with one row for each PID, and then merge to main df by PID

### WBC
## subset 

WBC <- r_other[c("PID","WBC","CollD")]
WBC <- subset(WBC,as.character(WBC) != "  .")
WBC <- (subset(WBC, (PID %in% df$PID)))
WBC$CollD <- as.Date(as.character(WBC$CollD),"%m/%d/%Y")

## remove all but first WBC
n_pids <- as.character(levels(WBC$PID))
tempout <- WBC[1,]

for  (i in 1:length(n_pids)) {
  temp <- subset(WBC,PID== n_pids[i])
  if (nrow(temp) > 1) { 
    temp <- temp[order(temp$CollD),]
    temp <- temp[1,]
  }
    tempout <- rbind(tempout,temp)
}
  
tempout <- tempout[-1,]
WBC <- tempout  
WBC <- WBC[c("PID","WBC")]
WBC$PID <- as.factor(as.character(WBC$PID))
# merge

df <- merge(df,WBC, by= "PID", all= TRUE)

### HIV status is fuck complicated.
### Patients were testes with rapid test -if pos (1) -> POS, if neg (0) -> NEG
### If discordant (2) -> ELISA
### If ELISA NEG - > NEG
### If ELISA POS -> WB
#### If WB POS -> POS
### This is what it says in the manuscript HOWEVER it doesnt look like any WBs were done
### So accept the result of ELISA for discordant RDTs
### Also some patients seemed to hav ONLY ELISA - so accept ELISA results
### Also some patients were tested twice
### For the purposes of this, use LATER test (ie if seroconverting and missed by rapid
### test but picked up later should consider as pos.
### all of these tests are in r_other linked to patient by PID

# subset
HIV <- r_other[c("PID","HIVRap","TDHIVRap","ELISARes","WBRes", "TDELISA")]
HIV <- (subset(HIV, (PID %in% df$PID)))

# make data frame of rapid tests

HIV_rapid <- HIV[c("PID","HIVRap","TDHIVRap")]
HIV_rapid <- subset(HIV_rapid,!is.na(HIVRap))

# identify patients with more than one rapid test and remove the earlier test 

HIV_rapid$PID <- as.factor(as.character(HIV_rapid$PID))

n_pids <- as.character(levels(HIV_rapid$PID))
tempout <- HIV_rapid[1,]

for  (i in 1:length(n_pids)) {
  temp <- subset(HIV_rapid,PID== n_pids[i])
  if (nrow(temp) > 1) { 
    temp <- temp[order(temp$TDHIVRap, decreasing = TRUE),]
    temp <- temp[1,]
  }
  tempout <- rbind(tempout,temp)
}

tempout <- tempout[-1,]
HIV_rapid <- tempout

# make frame of elisas (there are no WB results) - no one had 2 ELISAs

HIV_ELISA <- HIV[c("PID","ELISARes", "TDELISA")]
HIV_ELISA <- subset(HIV_ELISA, !(is.na(ELISARes)))
HIV_ELISA <- HIV_ELISA[c("PID", "ELISARes")]

# merge dataframes
HIV <- merge(HIV_rapid, HIV_ELISA, by = "PID", all = TRUE)
#ELISA trumps RDT
HIV$HIV_test <- HIV$HIVRap
HIV$HIV_test[!is.na(HIV$ELISARes)] <- HIV$ELISARes[!is.na(HIV$ELISARes)]

HIV <- HIV[c("PID","HIV_test")]
df <- merge(df,HIV, by = "PID", all = TRUE)




# Add CD4; if two, choose lowest

CD4 <- r_other[c("PID","CD4Ct")]
CD4 <- (subset(CD4, (PID %in% df$PID)))
CD4 <- subset(CD4, !is.na(CD4Ct))

n_pids <- as.character(levels(CD4$PID))
tempout <- CD4[1,]

for  (i in 1:length(n_pids)) {
  
  temp <- subset(CD4,PID== n_pids[i])
  
  if (nrow(temp) > 1) { 
    temp <- temp[order(temp$CD4),]
    temp <- temp[1,]
  }
  tempout <- rbind(tempout,temp)
}

tempout <- tempout[-1,]

CD4 <- tempout  
df <-merge(df,CD4, by = "PID", all= TRUE)



## if two, choose first (n=11)

CXR <- (subset(CXR, (PID %in% df$PID)))
CXR$DateRadio <- as.Date(as.character(CXR$DateRadio), "%m/%d/%Y")

n_pids <- as.character(levels(CXR$PID))
tempout <- CXR[1,]


for  (i in 1:length(n_pids)) {
temp <- subset(CXR,PID== n_pids[i])
  if (nrow(temp) > 1) { 
    temp <- temp[order(temp$DateRadio),]
    temp <- temp[1,]
  }
tempout <- rbind(tempout,temp)
}

CXR <- tempout[-1,]

# CXR - need meta analysis definitions
# harmonised reclassification {"suggestsTB"=cavitation, adenopathy, predominant UL infiltrates, unilateral pleural effusion, pericardial effusion, miliary; 
# "possibleTB"=interstitial (asymetrical nodular +/- reticular) changes or other non-specific asymetrical infiltrates, bilateral effusion;
# "undiagnosticTB"=other findings including consolidation, symetrical interstitial changes other than miliary; "
# "normal"=NAD}.

CXR$meta <- ""
## cavities
CXR$meta[CXR$CavityPresent ==1 ] <- "suggestsTB"
## predominent UL bilateral infiltrates
CXR$meta[(CXR$InflitratesRUL == 1 & CXR$InflitratesLUL) & (CXR$InfiltratesRLL == 0 | is.na(CXR$InfiltratesRLL)) & (CXR$InfiltratesLLL == 0 | is.na(CXR$InfiltratesLLL))] <- "suggestsTB"
## unilateral pleural effison
CXR$meta[CXR$PleuralEffusionR == 1 & (CXR$PleuralEffusionL == 0 | !is.na(CXR$PleuralEffusionL))] <- "suggestsTB"
CXR$meta[CXR$PleuralEffusionL == 1 & (CXR$PleuralEffusionR == 0 | !is.na(CXR$PleuralEffusionR))] <- "suggestsTB"
## pericardial effusion
CXR$meta[CXR$PericardialEffusion == 1] <- "suggestsTB"
## hilar LNs is not coded

## define miliay changes as >= 3 zones full of micronodules (this tallys with report)
CXR$meta[CXR$NodulesFineZonesR >= 3 | CXR$NodulesFineZonesL >= 3] <- "suggestsTB"
CXR$meta[CXR$NodulesFineZonesR  + CXR$NodulesFineZonesL >= 3] <- "suggestsTB"

## multilobar infiltrates is possible TB

CXR$meta[(CXR$InfiltratesZonesR > 1  | CXR$InfiltrateZonesL > 1) & CXR$meta == ""] <- "possibleTB"

## multilobar nodular changes is possible TB

CXR$meta[(CXR$NoduleZonesR > 1 | CXR$NoduleZonesL > 1) & CXR$meta == ""] <- "possibleTB"

# bilateral pleural effusion is possible TB

CXR$meta[CXR$PleuralEffusion == 1 & CXR$meta == ""] <- "possibleTB"

# Any other abnormailty is undiagnostic TB

CXR$meta[CXR$Normal == 0 & CXR$meta == ""] <- "undiagnosticTB"
CXR$meta[CXR$Normal == 1 & CXR$meta == ""] <- "normal"
CXR$meta[CXR$meta == ""] <- NA

CXR <- CXR[c("PID","meta")]
df <- merge(df, CXR, by = "PID", all = TRUE)

#### 2. Apply primary study specific inclusion criteria if needed ####




#### 3. Set up standardise df to populate with data from primary df ####

# get standardised variable names
newvars <- read.csv(paste0(wdpath , "/vardefs.csv"), header=T, na.strings=c(".", "NA"))
newvars <- as.character(newvars[ ,1])

# make a new df for standardised data #should be nrows of df, ncols=newvars
crump <- as.data.frame(t(matrix(data=rep(0, (nrow(df)*length(newvars))), nrow=length(newvars))))
# name the variables
names(crump) <- newvars





#### 4. populate the variables in standardised data frame from primary data frame ####



crump$primary.study <- rep("Crump2012", nrow(crump))

# There are study site codes but no data dictionary

crump$primary.study.site <- NA

crump$country <- rep("Tanzania", nrow(crump))

crump$setting1 <- rep("inpatient", nrow(crump))

crump$setting2 <- rep(NA, nrow(crump))


df$DateAdmission <- as.Date(as.character(df$DateAdmission),"%m/%d/%Y")
crump$year <-format(df$DateAdmission, "%Y")

df$DateOfBirth <- as.Date(as.character(df$DateOfBirth),"%m/%d/%Y")
df$DateOfBirth <- as.character(df$DateOfBirth)
df$DateOfBirth[substr(df$DateOfBirth,1,2) == "20"] <- paste0("19",substr(df$DateOfBirth[substr(df$DateOfBirth,1,2) == "20"],3,10))
df$DateOfBirth <- as.Date(df$DateOfBirth)

crump$age <- as.numeric(df$DateAdmission-df$DateOfBirth)/365.25

crump$sex <- as.character(df$SubjectSex)
crump$sex[crump$sex == "1"] <- "female"
crump$sex[crump$sex == "0"] <- "male"

# HIV pos either if prev HIV test + OR on this admission
# A couple of patients report prev pos test p
# but have neg this admssion - code them as HIV -
# in fact all patients except one had HIV test so just code HIV status by HIV test
# The one with no rapid test or elisa had two HIV RNA but is fecorded as never 
# having HIV test tests so not sure what's going onthere
# leave that one as NA


crump$HIVstatus <- df$HIV_test

crump$CD4 <- df$CD4Ct

# Assume recruitment date = admission date

crump$recruitmentDate <- df$DateAdmission 

crump$admissionDate <- df$DateAdmission 

## assume venepuncture same day as recruitment (not clear from manuscript)
crump$venepunctureDate <- df$DateAdmission

### incubation was within 1 day of venepuncture (from manuscript)
crump$incubationDate <- pmin(df$MBLoadD,df$IsolProcD,df$MBLoadD,na.rm = TRUE)
## few odd ones - neg of very large value -set any negative or > 10 to NA
crump$incubationDate[(crump$incubationDate - crump$recruitmentDate) < 0 & !is.na(crump$incubationDate - crump$recruitmentDate)] <- NA
crump$incubationDate[(crump$incubationDate - crump$recruitmentDate) > 10 & !is.na(crump$incubationDate - crump$recruitmentDate)] <- NA



crump$ttpBC <- df$fastest_ttp
crump$positive.cultureDate <- crump$incubationDate + crump$ttpBC


crump$assayBC <- rep("liquid",nrow(crump))
crump$volumeBC<- df$BC_vol
crump$numberBC <- df$n_MBC
crump$numberBC[is.na(crump$numberBC)] <- 0
crump$failedBC <-0
crump$contamBC <- df$n_contam_BC
crump$contamBC[is.na(crump$contamBC) ] <-0
crump$other.pathogen.BC <- df$other_path
crump$other.pathogen.BC[is.na(crump$other.pathogen.BC) ] <- ""

crump$availableBC <- df$n_avail_BC
crump$availableBC[is.na(crump$availableBC)] <- 0

crump$BCresult <- df$MTB_grew
crump$BCresult[crump$availableBC == 0] <- NA




crump$cxrTBprimary <- NA
crump$cxrTBmeta <- df$meta
# need meta analysis defs



crump$sputumAvailable <- 0
crump$sputumNumber <- 0
crump$sputumXpert <- NA
crump$sputumCulture <- NA
crump$sputumResult <- NA

crump$ulamAvailable <- 0

crump$ulamResult <- NA

# 

crump$cough <- df$Cough
crump$fever <- df$Fever
crump$weightloss <- df$WeightLoss
crump$nightsweats <- NA

crump$temperature <- df$Temprature
crump$RR <- df$RespRate
crump$HR <- df$HeartRate
crump$sBP <- df$BPDiastolic
crump$dBP <-  df$BPSystolic
crump$GCS <- df$GCS
crump$AVPU <- NA

crump$encephalopathy<- NA
crump$encephalopathy <- crump$GCS
crump$encephalopathy[crump$encephalopathy < 15 ] <-1
crump$encephalopathy[crump$encephalopathy == 15] <- 0

crump$ambulant <- crump$GCS
crump$ambulant[crump$ambulant <= 11 ] <-0
crump$ambulant[crump$ambulant > 11  & crump$ambulant != 0] <- 1


### Clunky code to calculate WHO screen and no. missing\
  

     
fever <-crump$fever
cough <-crump$cough
NS <- crump$nightsweats
WL <- crump$weightloss
WHOscrTEMP <- data.frame(fever,cough,NS,WL)
crump$missingWHOscreen <- apply(is.na(WHOscrTEMP),1,sum)
WHOscrTEMP[is.na(WHOscrTEMP)] <-0
crump$WHOscreen <- apply(WHOscrTEMP,1,sum)

RR<- crump$RR
temperature <- crump$temperature
HR <- crump$HR
not_ambulant <- crump$ambulant
not_ambulant[not_ambulant == 0] <- 9
not_ambulant[not_ambulant == 1] <- 0
not_ambulant[not_ambulant == 9] <- 1

WHOdangertemp <- data.frame(RR,temperature,HR,not_ambulant)
WHOdangertemp$RR[WHOdangertemp$RR <= 30] <-0
WHOdangertemp$RR[WHOdangertemp$RR > 30] <-1

WHOdangertemp$temperature[WHOdangertemp$temperature <= 39.0] <- 0
WHOdangertemp$temperature[WHOdangertemp$temperature > 39.0] <- 1

WHOdangertemp$HR[WHOdangertemp$HR <= 120] <- 0
WHOdangertemp$HR[WHOdangertemp$HR > 120] <- 1


crump$missingWHOdanger <- apply(is.na(WHOdangertemp),1,sum)
WHOdangertemp[is.na(WHOdangertemp)] <-0
crump$WHOdanger <- apply(WHOdangertemp,1,sum)
crump$WHOdanger[crump$WHOdanger > 0] <-1



crump$lactate <- NA

crump$WCC <- df$WBC


#sepsis

SIRS <- crump[,c('RR','HR','temperature','WCC')]
SIRS$RR <- as.numeric(SIRS$RR)
SIRS$HR <- as.numeric(SIRS$HR)
SIRS$temperature <- as.numeric(SIRS$temperature)
SIRS$WCC <- as.numeric(SIRS$WCC)

SIRS$RR[SIRS$RR <=20] <- 0
SIRS$RR[SIRS$RR >20] <-1

SIRS$HR[SIRS$HR <=90] <- 0
SIRS$HR[SIRS$HR >90] <-1

SIRS$temperature[SIRS$temperature <= 37.5 & SIRS$temperature >= 35.5] <- 0
SIRS$temperature[SIRS$temperature >37.5 | SIRS$temperature < 35.5] <-1

SIRS$WCC[SIRS$WCC >12 | SIRS$WCC < 4] <- 1
SIRS$WCC[SIRS$WCC <= 12 & SIRS$WCC >= 4] <- 0


crump$missingSepsis <- apply(is.na(SIRS),1,sum)
SIRS[is.na(SIRS)] <-0

crump$sepsis <-  apply(SIRS,1,sum)
crump$sepsis[crump$sepsis < 2] <- 0
crump$sepsis[crump$sepsis >= 2] <-1


crump$severe.sepsis <- NA

crump$severe.sepsis <- as.numeric(crump$sepsis == 1 & (crump$sBP <90 | crump$RR > 30 | crump$GCS < 15))


# death data

df$DischargeDate <- as.Date(as.character(df$DischargeDate),"%m/%d/%Y")

crump$dateDeath <-as.Date(NA) 
crump$dateDeath[df$Died == 1 & !is.na(df$DischargeDate) & !is.na(df$Died)] <- as.Date(df$DischargeDate[df$Died == 1 & !is.na(df$DischargeDate) & !is.na(df$Died)])

crump$censorDate <- df$DischargeDate

# remove impossible values - some of these dates are very odd ie admission > 1yr ?
 

crump$censorDate[as.numeric(crump$censorDate - crump$recruitmentDate) < 0] <-NA
#crump$censorDate[as.numeric(crump$censorDate - crump$recruitmentDate) > 100] <-NA

crump$inpatientDeath <- df$Died



crump$day30death <-as.numeric(crump$censorDate - crump$recruitmentDate)
crump$day30death[crump$day30death <= 30 & crump$inpatientDeath == 1 ] <- 1
crump$day30death[crump$day30death > 30 & crump$inpatientDeath ==1 ] <- 0
crump$day30death[crump$inpatientDeath == 0 & crump$day30death <= 30] <- NA
crump$day30death[crump$inpatientDeath == 0 & crump$day30death > 30 ] <- 0
crump$day30death[is.na(crump$inpatientDeath)] <- NA

crump$day60death <-as.numeric(crump$censorDate - crump$recruitmentDate)
crump$day60death[crump$day60death <= 60 & crump$inpatientDeath == 1 ] <- 1
crump$day60death[crump$day60death > 60 & crump$inpatientDeath ==1 ] <- 0
crump$day60death[crump$inpatientDeath == 0 & crump$day60death <= 60] <- NA
crump$day60death[crump$inpatientDeath == 0 & crump$day60death > 60 ] <- 0
crump$day60death[is.na(crump$inpatientDeath)] <- NA

crump$day90death <-as.numeric(crump$censorDate - crump$recruitmentDate)
crump$day90death[crump$day90death <= 90 & crump$inpatientDeath == 1 ] <- 1
crump$day90death[crump$day90death > 90 & crump$inpatientDeath ==1 ] <- 0
crump$day90death[crump$inpatientDeath == 0 & crump$day90death <= 90] <- NA
crump$day90death[crump$inpatientDeath == 0 & crump$day90death > 90 ] <- 0
crump$day90death[is.na(crump$inpatientDeath)] <- NA


## final diagnoses only given by blood culture - no other tB diagnostics
# so set final TB diagnosis as NA

crump$TBdiagnosis <- crump$BCresult
crump$TBdiagnosis [is.na(crump$TBdiagnosis)] <- 0

crump$priorTBRx <- df$Antituberculosis
crump$dateTBRx <- NA



######################################################################################

#### 5. export the final data set as .csv fle ####

write.csv(crump, paste0(wdpath, "/standardised/s_crump.csv"))


