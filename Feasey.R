##########################################################
#####   MTBBSI IPDMA primary data processing script   ####
#####          **** FEASEY 2013 ****                ######
##########################################################

##### NOTES
#####
##### THIS MAKES ASSUMPTIONS ON CODING OF
##### SPUTUM DATA - NEED TO CLARIFY WITH NICK
##### ASSUMES NO CONFUSION, CXR DATA - THIS
##### DATA WAS COLLECTED BUT CODING NOT CLEAR

##### IN ADDITION, DATE OF ENROLLMENT IS SET TO 01-08-2011
##### THE START DATE OF THE STUDY AS THIS DATA IS NOT COLLECTED


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

df <- read.csv(paste0(wdpath , "/feasey/feasey.csv"), header=TRUE)




#### 2. Apply primary study specific inclusion criteria if needed ####

# Not required


#### 3. Set up standardise df to populate with data from primary df ####

# get standardised variable names
newvars <- read.csv(paste0(wdpath , "/vardefs.csv"), header=T, na.strings=c(".", "NA"))
newvars <- as.character(newvars[ ,1])

# make a new df for standardised data #should be nrows of df, ncols=newvars
feasey <- as.data.frame(t(matrix(data=rep(0, (nrow(df)*length(newvars))), nrow=length(newvars))))
# name the variables
names(feasey) <- newvars 





#### 4. populate the variables in standardised data frame from primary data frame ####



feasey$primary.study <- rep("Feasey2013", nrow(feasey))

feasey$country <- rep("Malawi", nrow(feasey))

feasey$setting1 <- rep("inpatient", nrow(feasey))

feasey$setting2 <- rep(NA, nrow(feasey))

feasey$year <- rep("2011", nrow(feasey)) 

feasey$age <- df$age 

feasey$sex <- df$gender
      feasey$sex[feasey$sex == 1] <- "male"
      feasey$sex[feasey$sex == 2] <-"female"

feasey$HIVstatus <- rep(1, nrow(feasey))

feasey$CD4 <- df$CD4

# recruitment date not recorded; set to 01-08-2011 for all patients
# admission date not recorded
# venepuncture and incubation on day of recruitment

feasey$recruitmentDate <- rep(as.Date("2011-08-01"), nrow(feasey))
feasey$admissionDate <- rep(NA, nrow(feasey))
feasey$venepunctureDate <- rep(as.Date("2011-08-01"), nrow(feasey))
feasey$incubationDate <-rep(as.Date("2011-08-01"), nrow(feasey))


feasey$ttpBC <- df$timetomycobpos
feasey$positive.cultureDate <- feasey$recruitmentDate + feasey$ttpBC


feasey$assayBC <- rep("liquid",nrow(feasey))

feasey$volumeBC<- rep(5, nrow(feasey))

feasey$numberBC <- rep(1, nrow(feasey))
feasey$failedBC <- 0
feasey$contamBC <- rep(0, nrow(feasey))

feasey$availableBC <- rep(1, nrow(feasey))
feasey$other.pathogen.BC <- 0
feasey$BCresult <- rep(0, nrow(feasey))
  feasey$BCresult[df$MycobacterialBC == 1] <- 1
  
# Need to clarify with Nick the codes for CXR
  # this data exists in data set but no data dictionary
  
  
feasey$cxrTBprimary <- rep(NA, nrow(feasey))
feasey$cxrTBmeta <- rep(NA, nrow(feasey))

# culture codes  are not clear - I have interpreted as recorded on
# feasey_mycobacteraemia.csv
### FOr smear 1 neg, 2-5 pos, 9 not done
## For MGIT 1 pos, 2 neg, 9 ND
# For LJ 9 ND, 1 neg, 2-4 pos, 5 neg, 9 ND
# This is inetrnally consistent with the fact that "all microbiologically
# confirmed TB (as per the variable microbiologicallyconfirmedTB in the data set)
# had a positive sputum culture"
# But number of positive sputa do not match manuscript


N_GXP <-df$Gxpsputumresult
N_GXP[N_GXP < 9 & N_GXP != 4] <-1
N_GXP[N_GXP ==9 | N_GXP == 4] <-0

N_Sput1 <- df$LJ1 +df$MGIt1
N_Sput2 <- df$LJ2 + df$MMGIT2

N_Sput1[N_Sput1 != 18 ] <-1
N_Sput1[N_Sput1 == 18 ] <-0
N_Sput2[N_Sput2 != 18 ] <-1
N_Sput2[N_Sput2 == 18 ] <-0

feasey$sputumNumber <- N_GXP+ N_Sput1 + N_Sput2
feasey$sputumAvailable <- feasey$sputumNumber
feasey$sputumAvailable[feasey$sputumAvailable > 0] <-1

N_GXP <-df$Gxpsputumresult
N_GXP[N_GXP ==9 | N_GXP ==4] <-NA
N_GXP[N_GXP == 5] <-0
N_GXP[N_GXP > 0] <-1

feasey$sputumXpert <- N_GXP

N_LJ1 <- df$LJ1 
N_MGIT1 <- df$MGIt1

N_LJ1[N_LJ1 == 9] <-NA
N_LJ1[N_LJ1 ==5 ] <-0
N_LJ1[N_LJ1 ==1 ] <-0
N_LJ1[N_LJ1 > 1 & N_LJ1 < 5] <-1

N_MGIT1[N_MGIT1 == 9 ] <-NA
N_MGIT1[N_MGIT1 == 1 ] <-1
N_MGIT1[N_MGIT1 == 2]<-0

N<- data.frame(N_LJ1,N_MGIT1)

Sputcult1 <- apply(N,1,sum,na.rm = TRUE)
Sputcult1[is.na(N$N_LJ1) & is.na(N$N_MGIT1)] <-NA
Sputcult1[Sputcult1 > 0]<-1

N_LJ1 <- df$LJ2 
N_MGIT1 <- df$MMGIT2 

N_LJ1[N_LJ1 == 9] <-NA
N_LJ1[N_LJ1 ==5 ] <-0
N_LJ1[N_LJ1 ==1 ] <-0
N_LJ1[N_LJ1 > 1 & N_LJ1 < 5] <-1

N_MGIT1[N_MGIT1 == 9 ] <-NA
N_MGIT1[N_MGIT1 == 1 ] <-1
N_MGIT1[N_MGIT1 == 2]<-0

N<- data.frame(N_LJ1,N_MGIT1)


Sputcult2 <- apply(N,1,sum,na.rm = TRUE)
Sputcult2[is.na(N$N_LJ1) & is.na(N$N_MGIT1)] <-NA
Sputcult2[Sputcult1 > 0]<-1

N <- data.frame(Sputcult1,Sputcult2)
TotSputCult <- apply(N,1,sum,na.rm = TRUE)
TotSputCult[is.na(N$Sputcult1) & is.na(N$Sputcult2)] <-NA
TotSputCult[TotSputCult > 0]<-1

feasey$sputumCulture <-TotSputCult

N <- data.frame(feasey$sputumCulture,feasey$sputumXpert)

feasey$sputumResult<- apply(N,1,sum,na.rm = TRUE)

feasey$sputumResult[is.na(N$feasey.sputumCulture) & is.na(N$feasey.sputumXpert)] <-NA
feasey$sputumResult[feasey$sputumResult> 0] <-1


feasey$ulamAvailable <- rep(0,nrow(feasey))

feasey$ulamResult <- rep(NA, nrow(feasey))

# All patients had fever and cough - inclusion criteria

feasey$cough <- rep(1, nrow(feasey))
feasey$fever <- rep(1, nrow(feasey))

feasey$weightloss <- rep(0, nrow(feasey))
  feasey$weightloss[df$wtloss == 1] <- 1
  

feasey$nightsweats <- rep(0, nrow(feasey))
  feasey$nightsweats[df$ntsweats == 1] <-1
  
feasey$temperature <- df$temperature

feasey$RR <- df$rr

feasey$HR <- rep(NA, nrow(feasey))

feasey$sBP <- df$sbp

feasey$dBP <-rep(NA,nrow(feasey))

feasey$GCS <- rep(NA,nrow(feasey))

feasey$AVPU <- rep(NA,nrow(feasey))

# there is a "confusion" category but no data dictionary
# need to clarify with nick but i've assumed (as with the rest
# of the data that 9 = missing, 2 = no

feasey$encephalopathy <- df$confused
feasey$encephalopathy[feasey$encephalopathy == 9] <-NA
feasey$encephalopathy[feasey$encephalopathy == 2] <-0


feasey$ambulant <- rep(NA, nrow(feasey))
  feasey$ambulant[df$bedbound == 1] <- 0
  feasey$ambulant[df$bedbound == 2] <- 1


### Clunky code to calculate WHO screen and no. missing\
  

     
fever <-feasey$fever
cough <-feasey$cough
NS <- feasey$nightsweats
WL <- feasey$weightloss
WHOscrTEMP <- data.frame(fever,cough,NS,WL)
feasey$missingWHOscreen <- apply(is.na(WHOscrTEMP),1,sum)
WHOscrTEMP[is.na(WHOscrTEMP)] <-0
feasey$WHOscreen <- apply(WHOscrTEMP,1,sum)

#### More clunky code to calculate WHO danger

RRover30 <- feasey$RR
RRover30[RRover30 <= 30] <-0
RRover30[RRover30 > 30] <-1
Tover39 <-feasey$temperature
Tover39[Tover39 <=39] <-0
Tover39[Tover39> 39] <-1
HRover120<- feasey$HR
HRover120[HRover120 <=120]<-0
HRover120[HRover120 > 120]<-1
InabWalk <-feasey$ambulant
InabWalk[InabWalk == 0] <-99
InabWalk[InabWalk == 1] <-0
InabWalk[InabWalk == 99] <-1
WHOdangTEMP <- data.frame(RRover30,Tover39,HRover120,InabWalk)
feasey$missingWHOdanger <- apply(is.na(WHOdangTEMP),1,sum)
WHOdangTEMP[is.na(WHOdangTEMP)] <- 0
feasey$WHOdanger <- apply(WHOdangTEMP,1,sum)
feasey$WHOdanger[feasey$WHOdanger >= 1] <-1


feasey$lactate <- rep(NA, nrow(feasey))
feasey$WCC <- rep(NA, nrow(feasey))

#sepsis

SIRS <- feasey[,c('RR','HR','temperature','WCC')]
 
SIRS$RR[SIRS$RR <=20] <- 0
SIRS$RR[SIRS$RR >20] <-1

SIRS$HR[SIRS$HR <=90] <- 0
SIRS$HR[SIRS$HR >90] <-1

SIRS$temperature[SIRS$temperature >37.5 | SIRS$temperature < 35.5] <-1
SIRS$temperature[SIRS$temperature <= 37.5 & SIRS$temperature >= 35.5] <- 0


SIRS$WCC[SIRS$WCC <= 12 & SIRS$WCC >= 4] <- 0
SIRS$WCC[SIRS$WCC >12 | SIRS$WCC < 4] <- 1

feasey$missingSepsis <- apply(is.na(SIRS),1,sum)
SIRS[is.na(SIRS)] <- 0
feasey$sepsis <-apply(SIRS,1,sum)
feasey$sepsis[feasey$sepsis < 2] <- 0
feasey$sepsis[feasey$sepsis >= 2] <-1

### If > 1 sev sepsis variable is missing then this code sets as NA

sevsepsis <- feasey[,c('sepsis','sBP','encephalopathy','RR')]
sevsepsis$sBP[sevsepsis$sBP < 90 ] <-1
sevsepsis$sBP[sevsepsis$sBP >= 90] <-0
sevsepsis$RR[sevsepsis$RR <= 30] <-0
sevsepsis$RR[sevsepsis$RR > 30] <-1
sevsepsis$n_missing <- apply(is.na(sevsepsis),1,sum)
sevsepsis[is.na(sevsepsis)] <-0

feasey$severe.sepsis <-0
feasey$severe.sepsis[sevsepsis$sepsis == 1 & ((sevsepsis$sBP + sevsepsis$encephalopathy + sevsepsis$RR) > 0)] <-1
feasey$severe.sepsis[sevsepsis$n_missing > 1] <-NA



feasey$dateDeath <- feasey$recruitmentDate + df$timetodeath

# From manuscript - all patients were reviewed at 2 weeks
# 9 were LTFU at 2 months so censor them at 2 weeks

#feasey$censorDate[is.na(feasey$dateDeath) == FALSE] <- as.Date(feasey$dateDeath)
feasey$censorDate <- feasey$recruitmentDate
feasey$censorDate[df$vitalstatusfinal == 3] <- feasey$censorDate[df$vitalstatusfinal == 3] + 14
feasey$censorDate[df$vitalstatusfinal == 1] <- feasey$censorDate[df$vitalstatusfinal == 1] + 60
feasey$censorDate[df$vitalstatusfinal == 2] <- as.Date(feasey$dateDeath[df$vitalstatusfinal == 2])

# Not recorded whether IP/OP

feasey$inpatientDeath <- rep(NA, nrow(feasey))


deadatD30 <- feasey$censorDate - feasey$recruitmentDate
#If follow up for < 30D AND LTFU - set as NA
deadatD30[deadatD30 <= 30 & df$vitalstatusfinal == 3] <-NA
# set those that remain and < =30 to 1
deadatD30[deadatD30 <= 30]<-1
deadatD30[deadatD30 > 30 ]<-0
feasey$day30death <-deadatD30

deadatD60 <- df$vitalstatusfinal
deadatD60[deadatD60 ==3] <-NA
deadatD60[deadatD60 ==1] <- 0
deadatD60[deadatD60 ==2] <- 1

feasey$day60death <-deadatD60

feasey$day90death <- rep(NA,nrow(feasey))

feasey$TBdiagnosis <- df$microbiologicallyprovenTB
feasey$TBdiagnosis[feasey$TBdiagnosis == 2] <-0

feasey$dateTBRx <- feasey$recruitmentDate + df$timefromrecruitmenttostartoftbrx

feasey$priorTBRx <- rep(0, nrow(feasey))
feasey$priorTBRx[feasey$dateTBRx < feasey$recruitmentDate] <-1


######################################################################################

#### 5. export the final data set as .csv fle ####

write.csv(feasey, paste0(wdpath, "/standardised/s_feasey.csv"))


