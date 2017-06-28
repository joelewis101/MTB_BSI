##########################################################
#####   MTBBSI IPDMA primary data processing script   ####
#####          **** BEDELL 2012 ****                ######
##########################################################

##### NOTES
##### Remember all of these patients EITHER have 3 x negative sputa OR are unable
#### to expectorate. It's not possible from the data to distinguish which


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

df <- read.csv(paste0(wdpath , "/bedell/bedell.csv"), header=TRUE)

# One emissing enroll date - from the sequence of enrollment it must be 18, 19 or 20 or 21 of oct
# just choose one

df$EnrolDate[df$EnrolDate == ""] <- "19.10.2010" 



#### 2. Apply primary study specific inclusion criteria if needed ####

# Not required


#### 3. Set up standardise df to populate with data from primary df ####

# get standardised variable names
newvars <- read.csv(paste0(wdpath , "/vardefs.csv"), header=T, na.strings=c(".", "NA"))
newvars <- as.character(newvars[ ,1])

# make a new df for standardised data #should be nrows of df, ncols=newvars
bedell <- as.data.frame(t(matrix(data=rep(0, (nrow(df)*length(newvars))), nrow=length(newvars))))
# name the variables
names(bedell) <- newvars 





#### 4. populate the variables in standardised data frame from primary data frame ####



bedell$primary.study <- rep("Bedell2012", nrow(bedell))
bedell$primary.study.site <- rep(1,nrow(bedell))
bedell$country <- rep("Malawi", nrow(bedell))

bedell$setting1 <- rep("outpatient", nrow(bedell))
bedell$setting2 <- rep("OPD", nrow(bedell))

bedell$setting2 <- rep(NA, nrow(bedell))
bedell$year <- rep("2010", nrow(bedell))

bedell$age <- as.numeric(as.character(df$Age)) 

bedell$sex <- df$Sex

bedell$sex <- as.character(bedell$sex)
      bedell$sex[bedell$sex == "Male"] <- "male"
      bedell$sex[bedell$sex == "Female"] <- "female"

bedell$HIVstatus <- rep(1, nrow(bedell))

bedell$CD4 <- df$CD4


bedell$recruitmentDate <- as.Date(as.character(df$EnrolDate),"%d.%m.20%y") 

bedell$admissionDate <- rep(NA, nrow(bedell))

## assume venepuncture same day as recruitment (not clear from manuscript)
bedell$venepunctureDate <- bedell$recruitmentDate

### incubation was within 1 day of venepuncture (from manuscript)
bedell$incubationDate <- bedell$recruitmentDate

### ttp, date of culture flagging not available
bedell$positive.cultureDate <- rep(NA, nrow(bedell))



### all below from manuscript as per protocol
bedell$ttpBC <-  rep(NA, nrow(bedell))
bedell$assayBC <- rep("liquid",nrow(bedell))
bedell$volumeBC<- rep(5, nrow(bedell))


# Assume blank field in BC indicates sample not sent (this tallies with manuscript - 14 pts had no MB BC)

bedell$numberBC <- rep(1, nrow(bedell))
bedell$numberBC[df$TB.Blood.Culture == ""] <- 0


# No contam et to 0

bedell$contamBC <- 0

# available

bedell$availableBC <- bedell$numberBC

# only other pathogens are NTM, not speciated

bedell$other.pathogen.BC <- 0
bedell$other.pathogen.BC[df$TB.SpeciesBlood == "NTM"] <- "NTM"

# set MTB var

bedell$BCresult <- 0
bedell$BCresult[bedell$availableBC == 0 ] <-NA
bedell$BCresult[df$TB.SpeciesBlood == "MTB"] <- 1


bedell$cxrTBmeta <- rep(NA, nrow(bedell))
bedell$cxrTBprimary <- rep(NA, nrow(bedell))

### this refers to the INDUCED SPUTA taken 

bedell$sputumAvailable <- rep(1, nrow(bedell))
bedell$sputumAvailable[df$Failed.Sputum.Induction == 1] <- 0

bedell$sputumNumber <- bedell$sputumAvailable
bedell$sputumCulture <- as.character(df$TB.SpeciesCult)
bedell$sputumCulture[bedell$sputumCulture != "MTB"] <-0
bedell$sputumCulture[bedell$sputumCulture == "MTB"] <-1
bedell$sputumCulture[bedell$sputumAvailable == 0] <-NA
bedell$sputumResult <- bedell$sputumCulture

bedell$ulamAvailable <- rep(0,nrow(bedell))

bedell$ulamResult <- rep(NA, nrow(bedell))

# 

bedell$cough <- df$ChrCough
bedell$fever <- df$ChronicFever
bedell$weightloss <- df$SevWL
bedell$nightsweats <- rep(NA, nrow(bedell))

bedell$temperature <- rep(NA, nrow(bedell))
bedell$RR <- rep(NA, nrow(bedell))
bedell$HR <- rep(NA, nrow(bedell))
bedell$sBP <- rep(NA, nrow(bedell))
bedell$dBP <- rep(NA, nrow(bedell))
bedell$GCS <- rep(NA, nrow(bedell))
bedell$AVPU <- rep(NA, nrow(bedell))
bedell$encephalopathy <- rep(NA, nrow(bedell))

### assume ambulant - "ambulatory HIV patients" from manuscript

bedell$ambulant <- 1




### Clunky code to calculate WHO screen and no. missing\
  

     
fever <-bedell$fever
cough <-bedell$cough
NS <- bedell$nightsweats
WL <- bedell$weightloss
WHOscrTEMP <- data.frame(fever,cough,NS,WL)
bedell$missingWHOscreen <- apply(is.na(WHOscrTEMP),1,sum)
WHOscrTEMP[is.na(WHOscrTEMP)] <-0
bedell$WHOscreen <- apply(WHOscrTEMP,1,sum)




bedell$WHOdanger <- 0
bedell$missingWHOdanger <- 4


bedell$lactate <- NA
bedell$WCC <- as.character(df$FBC_WBC)
bedell$WCC[bedell$WCC == "#NULL!"] <- NA

#sepsis

SIRS <- bedell[,c('RR','HR','temperature','WCC')]
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


bedell$missingSepsis <- apply(is.na(SIRS),1,sum)

bedell$sepsis <- apply(SIRS,1,sum,na.rm=TRUE)
bedell$sepsis[bedell$sepsis < 2] <- 0
bedell$sepsis[bedell$sepsis >= 2] <- 1


bedell$severe.sepsis <- rep(NA,nrow(bedell))






# mortality data is a mess
# It has – 6 month outcome data for the majority of patients (but not all) – which includes transferred out (but no date)
#Date of death of patients that have died (sometimes > 1yr after enrolment)
#No censoring date for anyone except those who have died
# So some assumptions have been made:
# 1) if overall 6 month outcome is died and death date available then set date of death and censor at same date
# n = 62
df$datedeathOP<- as.Date("1900-01-01")
df$datecensorOP <- as.Date("1900-01-01")

df$datedeathOP[df$Overall_6MoOutcome == "Died" & df$DateOfDeath != ""] <- as.Date(as.character(df$DateOfDeath[df$Overall_6MoOutcome == "Died" & df$DateOfDeath != ""]),"%d.%m.20%y" )
df$datecensorOP[df$Overall_6MoOutcome == "Died" & df$DateOfDeath != ""] <- df$datedeathOP[df$Overall_6MoOutcome == "Died" & df$DateOfDeath != ""] 

df$TbTreatStart <- as.Date(as.character(df$TbTreatStart),"%d/%m/%Y")
df$ARTStart <- as.Date(as.character(df$ARTStart),"%d/%m/%Y")

# 2) if overall six month outcome is alive then set censor date as enroll + 24 weeks (168 days)
# n = 301
df$datedeathOP[df$Overall_6MoOutcome == "alive"] <- NA
df$datecensorOP[df$Overall_6MoOutcome == "alive"] <- as.Date(as.character(df$EnrolDate[df$Overall_6MoOutcome == "alive"]), "%d.%m.20%y")+168

# 3) a)  if 6 month outcome = transfer out or LTFU (n= 49) then 
# censor at TB start date or ART start date, whichever is later definately alive then
# if either of those dates are available (n=40)
#first generate last contact == either ART start or TB start date 

df$lastcontact <- df$ARTStart
df$lastcontact[is.na(df$lastcontact)] <- df$TbTreatStart[is.na(df$lastcontact)]
df$lastcontact[df$TbTreatStart > df$lastcontact & (is.na(df$TbTreatStart) == FALSE) & (is.na(df$lastcontact) == FALSE)] <- df$TbTreatStart[df$TbTreatStart > df$lastcontact & (is.na(df$TbTreatStart) == FALSE) & (is.na(df$lastcontact) == FALSE)]

#set death date to NAs
df$datedeathOP[df$Overall_6MoOutcome == "LTF" | df$Overall_6MoOutcome == "Transferred Out"] <- NA

## set censor date to lastcontact if lastcontact is not NA

df$datecensorOP[(df$Overall_6MoOutcome == "LTF" | df$Overall_6MoOutcome == "Transferred Out")& is.na(df$lastcontact) == FALSE] <- df$lastcontact[(df$Overall_6MoOutcome == "LTF" | df$Overall_6MoOutcome == "Transferred Out")& is.na(df$lastcontact) == FALSE]

# 3 b) If LTFU or transfer out but no date other than enrollment then set censor date to  NA


df$datecensorOP[(df$Overall_6MoOutcome == "LTF" | df$Overall_6MoOutcome == "Transferred Out")& is.na(df$lastcontact) == TRUE] <- NA

# 4) if 6 month outcome is died and date of death is NA (n=12) then set died date = censor date = halfway between last contact and enrolldate + 6 months
# 4a) if those dates are available is n= 8

df$datedeathOP[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == FALSE]   <- 
  as.Date(as.character(df$EnrolDate[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == FALSE]),"%d.%m.20%y") + 
  (as.Date(as.character(df$EnrolDate[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == FALSE]),"%d.%m.20%y") + 168 -
      df$lastcontact[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == FALSE])/2

df$datecensorOP[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == FALSE]   <- 
  as.Date(as.character(df$EnrolDate[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == FALSE]),"%d.%m.20%y") + 
  (as.Date(as.character(df$EnrolDate[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == FALSE]),"%d.%m.20%y") + 168 -
     df$lastcontact[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == FALSE])/2



# 4b if not available then set censor/death date to NA (n= 4)


df$datedeathOP[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == TRUE]   <- NA

df$datecensorOP[df$Overall_6MoOutcome == "Died" & df$DateOfDeath == "" & is.na(df$lastcontact) == TRUE]   <- NA

# 5 if 6 month outcome = null but art start or tb start is available censor at that date (alive) n= 33

df$datedeathOP[df$Overall_6MoOutcome =="" & is.na(df$lastcontact)== FALSE] <- NA

df$datecensorOP[df$Overall_6MoOutcome =="" & is.na(df$lastcontact)== FALSE] <- df$lastcontact[df$Overall_6MoOutcome =="" & is.na(df$lastcontact)== FALSE]

# 6 if 6 month outcome = null and no other dates, exclude (n=12)

df$datedeathOP[df$Overall_6MoOutcome =="" & is.na(df$lastcontact)== TRUE] <- NA
df$datecensorOP[df$Overall_6MoOutcome =="" & is.na(df$lastcontact)== TRUE] <- NA

bedell$dateDeath <- df$datedeathOP
bedell$censorDate <- df$datecensorOP
bedell$inpatientDeath <- rep(NA, nrow(bedell))



bedell$day30death <- as.numeric(bedell$dateDeath - bedell$recruitmentDate <= 30)
bedell$day30death[is.na(bedell$day30death)] <-0
bedell$day30death[is.na(bedell$censorDate)] <-NA

bedell$day60death <- as.numeric(bedell$dateDeath - bedell$recruitmentDate <= 60)
bedell$day60death[is.na(bedell$day60death)] <-0
bedell$day60death[is.na(bedell$censorDate)] <-NA

bedell$day90death <- as.numeric(bedell$dateDeath - bedell$recruitmentDate <= 90)
bedell$day90death[is.na(bedell$day90death)] <-0
bedell$day90death[is.na(bedell$censorDate)] <-NA

#bedell$dateDeath <- rep(NA, nrow(bedell))
#bedell$censorDate <- rep(NA, nrow(bedell))

#bedell$day30death <-rep(NA, nrow(bedell))
#bedell$day60death <-rep(NA, nrow(bedell))
#bedell$day90death <- rep(NA,nrow(bedell))

## assume final diagnosis of TB if BC or sputum culture pos
# there is no study definition

bedell$TBdiagnosis <- rep(0, nrow(bedell))
bedell$TBdiagnosis[bedell$sputumCulture == 1] <-1
bedell$TBdiagnosis[bedell$BCresult == 1] <-1

bedell$dateTBRx <- df$TbTreatStart

bedell$priorTBRx <- rep(0, nrow(bedell))
bedell$priorTBRx[bedell$dateTBRx < bedell$recruitmentDate & !is.na(bedell$dateTBRx)] <- 1 




######################################################################################

#### 5. export the final data set as .csv fle ####

write.csv(bedell, paste0(wdpath, "/standardised/s_bedell.csv"))


