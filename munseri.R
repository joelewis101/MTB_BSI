
###########################################################
#####   MTBBSI IPDMA primary data processing script   #####
#####          **** Munseri 2011 ****          ######
###########################################################

#### 0. Set up

### 0.1 set working directory
#set constant for path, in case of change (e.g. different machine)

wdpath = "/Users/joelewis/Documents/Projects/MTB_BSI/data"
spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")

setwd(wdpath)


### 0.2 required packages
library(tidyr)
library(ggplot2)
library(dplyr)

#### 1. Load raw data ####
# This is a .cvs version of the original excel file extracted from hard copy data records
# to which inclusion criteria were already applied (to avoid work of collecting data we weren't going to use)
df <- read.csv(paste0(wdpath , "/munseri/munseri.csv"), header=TRUE)
munseri_BC <- read.csv(paste0(wdpath , "/munseri/munseri_BC.csv"), header=TRUE)

for (i in (1:nrow(munseri_BC))) {if (i %% 2) {munseri_BC$PID[i+1] <- munseri_BC$PID[i]}}
munseri_BC$venepuncture_date <- as.Date(as.character(munseri_BC$venepuncture_date), "%d.%m.%Y")
munseri_BC$ISOL_inc_date <- as.Date(as.character(munseri_BC$ISOL_inc_date), "%d.%m.%Y")
munseri_BC$MGIT_inc_date <- as.Date(as.character(munseri_BC$MGIT_inc_date), "%d.%m.%Y")
munseri_BC$MGIT_r_date <- as.Date(as.character(munseri_BC$MGIT_r_date), "%d.%m.%Y")

munseri_BC$MGIT_r <- as.character(munseri_BC$MGIT_r)

# extract munseri BC data

PID <- rep(0,nrow(munseri_BC)/2)
sample_vol <- rep(0,nrow(munseri_BC)/2)
venepuncture_date <- rep(as.Date("1900-01-01"),nrow(munseri_BC)/2)
n_ISOL <- rep(0,nrow(munseri_BC)/2)
n_ISOL_failed <- rep(0,nrow(munseri_BC)/2)
n_contam_ISOL <- rep(0,nrow(munseri_BC)/2)
n_avail_ISOL <- rep(0,nrow(munseri_BC)/2)
n_MTB_ISOL <- rep(0,nrow(munseri_BC)/2)
ISOL_inc_date <- rep(as.Date("1900-01-01"),nrow(munseri_BC)/2)
n_MGIT <- rep(0,nrow(munseri_BC)/2)
n_MGIT_failed <- rep(0,nrow(munseri_BC)/2)
n_contam_MGIT <-rep(0,nrow(munseri_BC)/2)
n_avail_MGIT <-rep(0,nrow(munseri_BC)/2)
n_MTB_MGIT <-rep(0,nrow(munseri_BC)/2)
MGIT_inc_date <- rep(as.Date("1900-01-01"),nrow(munseri_BC)/2)
MGIT_ttp <- rep(NA,nrow(munseri_BC)/2)
  
MB_BC_out <- data.frame(PID,sample_vol,venepuncture_date,n_ISOL,n_ISOL_failed, n_contam_ISOL,n_avail_ISOL,n_MTB_ISOL,ISOL_inc_date, n_MGIT, n_MGIT_failed, n_contam_MGIT, n_avail_MGIT, n_MTB_MGIT, MGIT_inc_date , MGIT_ttp) 

for (i in (1:nrow(munseri_BC))) {
  if (i %% 2) { print(i)
    # PID
    
    MB_BC_out$PID[(i+1)/2] <- munseri_BC$PID[i]
    
    # sample_vol chose biggest
    
    temp1 <- munseri_BC$sample_vol[i]
    temp2 <- munseri_BC$sample_vol[i+1]
    if (is.na(temp1) & is.na(temp2)) {
      MB_BC_out$sample_vol[(i+1)/2] <- NA
    } else
      if (is.na(temp1)) {
        MB_BC_out$sample_vol[(i+1)/2] <- temp2
      } else
        if (is.na(temp2)) {
          MB_BC_out$sample_vol[(i+1)/2] <- temp1
        } else {
          if (temp1 < temp2) {
            MB_BC_out$sample_vol[(i+1)/2] <- temp2
          } else {
            MB_BC_out$sample_vol[(i+1)/2] <- temp1
          }
        }
    
    # venepuncture date - choose earliest
    temp1 <- munseri_BC$venepuncture_date[i]
    temp2 <- munseri_BC$venepuncture_date[i+1]
    
    print("minky")
    if (is.na(temp1) & is.na(temp2)) {
      MB_BC_out$venepuncture_date[(i+1)/2] <- NA
    } else
      if (is.na(temp1)) {
        MB_BC_out$venepuncture_date[(i+1)/2] <- as.Date(temp2)
      } else
        if (is.na(temp2)) {
          MB_BC_out$venepuncture_date[(i+1)/2] <- as.Date(temp1)
        } else {
          if (temp1 < temp2) {
            MB_BC_out$venepuncture_date[(i+1)/2] <- as.Date(temp1)
          } else {
            print("ballbag")
            MB_BC_out$venepuncture_date[(i+1)/2] <- as.Date(temp2)
          }
        }
    
    # n_isol
     p <- as.character(munseri_BC$ISOL_r[i])
     if (is.na(p)) {p <-"ND"}
     
     
     
     nisoltemppos <- as.numeric(grepl(pattern= "POS+",p, ignore.case = TRUE))
     nisoltempneg <- as.numeric(grepl(pattern= "neg+", p))
     nisoltempcontam <- as.numeric(grepl(pattern= "contam+",p))
     
     MB_BC_out$n_ISOL[(i+1)/2] <- 1
     MB_BC_out$n_ISOL_failed[(i+1)/2]<- 1 - (nisoltemppos + nisoltempneg + nisoltempcontam)
     MB_BC_out$n_avail_ISOL[(i+1)/2] <- nisoltemppos + nisoltempneg 
     MB_BC_out$n_MTB_ISOL[(i+1)/2] <- nisoltemppos 
     MB_BC_out$n_contam_ISOL[(i+1)/2] <- nisoltempcontam
     
     p <- munseri_BC$ISOL_r[i+1]
     if (is.na(p)) {p <-"ND"}
     nisoltemppos <- as.numeric(grepl(pattern= "POS+",p, ignore.case = TRUE))
     nisoltempneg <- as.numeric(grepl(pattern= "neg+", p))
     nisoltempcontam <- as.numeric(grepl(pattern= "contam+",p))
     
     MB_BC_out$n_ISOL[(i+1)/2] <- MB_BC_out$n_ISOL[(i+1)/2]  + 1
     MB_BC_out$n_ISOL_failed[(i+1)/2]<- MB_BC_out$n_ISOL_failed[(i+1)/2] + (1 - (nisoltemppos + nisoltempneg + nisoltempcontam))
     MB_BC_out$n_avail_ISOL[(i+1)/2] <- MB_BC_out$n_avail_ISOL[(i+1)/2] + nisoltemppos + nisoltempneg 
     MB_BC_out$n_MTB_ISOL[(i+1)/2] <- MB_BC_out$n_MTB_ISOL[(i+1)/2] + nisoltemppos 
     MB_BC_out$n_contam_ISOL[(i+1)/2] <- MB_BC_out$n_contam_ISOL[(i+1)/2] +nisoltempcontam
     
     # isol inc date
     
     
     
     temp1 <- munseri_BC$ISOL_inc_date [i]
     temp2 <- munseri_BC$ISOL_inc_date[i+1]
     
     print("binky")
     if (is.na(temp1) & is.na(temp2)) {
       MB_BC_out$ISOL_inc_date[(i+1)/2] <- NA
     } else
       if (is.na(temp1)) {
         MB_BC_out$ISOL_inc_date[(i+1)/2] <- as.Date(temp2)
       } else
         if (is.na(temp2)) {
           MB_BC_out$ISOL_inc_date[(i+1)/2] <- as.Date(temp1)
         } else {
           if (temp1 < temp2) {
             MB_BC_out$ISOL_inc_date[(i+1)/2] <- as.Date(temp1)
           } else {
             print("ballbag")
             MB_BC_out$ISOL_inc_date[(i+1)/2] <- as.Date(temp2)
           }
         }
     # MGIT
     
     
     # n_MGIT
     
     p <- as.character(munseri_BC$MGIT_r[i])
     if (is.na(p)) {p <-"ND"}
     
     
     
     nisoltemppos <- as.numeric(grepl(pattern= "POS+",p, ignore.case = TRUE))
     nisoltempneg <- as.numeric(grepl(pattern= "neg+", p))
     nisoltempcontam <- as.numeric(grepl(pattern= "contam+",p))
     
     MB_BC_out$n_MGIT[(i+1)/2] <- 1
     MB_BC_out$n_MGIT_failed[(i+1)/2]<- 1 - (nisoltemppos + nisoltempneg + nisoltempcontam)
     MB_BC_out$n_avail_MGIT[(i+1)/2] <- nisoltemppos + nisoltempneg 
     MB_BC_out$n_MTB_MGIT[(i+1)/2] <- nisoltemppos 
     MB_BC_out$n_contam_MGIT[(i+1)/2] <- nisoltempcontam
     
     p <- munseri_BC$MGIT_r [i+1]
     if (is.na(p)) {p <-"ND"}
     nisoltemppos <- as.numeric(grepl(pattern= "POS+",p, ignore.case = TRUE))
     nisoltempneg <- as.numeric(grepl(pattern= "neg+", p))
     nisoltempcontam <- as.numeric(grepl(pattern= "contam+",p))
     
     MB_BC_out$n_MGIT[(i+1)/2] <- MB_BC_out$n_MGIT[(i+1)/2] + 1
     MB_BC_out$n_MGIT_failed[(i+1)/2]<- MB_BC_out$n_MGIT_failed[(i+1)/2] + (1 - (nisoltemppos + nisoltempneg + nisoltempcontam))
     MB_BC_out$n_avail_MGIT[(i+1)/2] <- MB_BC_out$n_avail_MGIT[(i+1)/2] + nisoltemppos + nisoltempneg 
     MB_BC_out$n_MTB_MGIT[(i+1)/2] <- MB_BC_out$n_MTB_MGIT[(i+1)/2] + nisoltemppos 
     MB_BC_out$n_contam_MGIT[(i+1)/2] <- MB_BC_out$n_contam_MGIT[(i+1)/2] +nisoltempcontam
     

     
     temp1 <- munseri_BC$MGIT_inc_date [i]
     temp2 <- munseri_BC$MGIT_inc_date[i+1]
     
     print("schminky")
     if (is.na(temp1) & is.na(temp2)) {
       MB_BC_out$MGIT_inc_date[(i+1)/2] <- NA
     } else
       if (is.na(temp1)) {
         MB_BC_out$MGIT_inc_date[(i+1)/2] <- as.Date(temp2)
       } else
         if (is.na(temp2)) {
           MB_BC_out$MGIT_inc_date[(i+1)/2] <- as.Date(temp1)
         } else {
           if (temp1 < temp2) {
             MB_BC_out$MGIT_inc_date[(i+1)/2] <- as.Date(temp1)
           } else {
             print("ballbag")
             MB_BC_out$MGIT_inc_date[(i+1)/2] <- as.Date(temp2)
           }
         }
     
     ## MGIT TTP - choose lowest
     
   
     
     temp1 <- munseri_BC$MGIT_ttp[i]
     temp2 <- munseri_BC$MGIT_ttp[i+1]
     if (is.na(temp1) & is.na(temp2)) {
       MB_BC_out$MGIT_ttp[(i+1)/2] <- NA
     } else
       if (is.na(temp1)) {
         MB_BC_out$MGIT_ttp[(i+1)/2] <- temp2
       } else
         if (is.na(temp2)) {
           MB_BC_out$MGIT_ttp[(i+1)/2] <- temp1
         } else {
           if (temp1 < temp2) {
             MB_BC_out$MGIT_ttp[(i+1)/2] <- temp1
           } else {
             MB_BC_out$MGIT_ttp[(i+1)/2] <- temp2
           }
         }
     
     
     # end
  }
}
  
MB_BC_out$total_avail_culture <- MB_BC_out$n_avail_ISOL + MB_BC_out$n_avail_MGIT
MB_BC_out$any_failed <- MB_BC_out$n_ISOL_failed + MB_BC_out$n_MGIT_failed
MB_BC_out$any_contaminated <- MB_BC_out$n_contam_ISOL + MB_BC_out$n_contam_MGIT
MB_BC_out$other_pathogen <- 0
MB_BC_out$other_pathogen[MB_BC_out$total_avail_culture == 0] <- NA
MB_BC_out$any_MTB <- NA
MB_BC_out$any_MTB[MB_BC_out$total_avail_culture > 0 ] <- 0
MB_BC_out$any_MTB[MB_BC_out$n_MTB_ISOL > 0 | MB_BC_out$n_MTB_MGIT > 0] <- 1

MB_BC_out$earliest_inc <- pmin(MB_BC_out$MGIT_inc_date,MB_BC_out$ISOL_inc_date )
MB_BC_out$earliest_inc [ (is.na(MB_BC_out$earliest_inc) & !is.na(MB_BC_out$MGIT_inc_date))] <- MB_BC_out$MGIT_inc_date[(is.na(MB_BC_out$earliest_inc) & !is.na(MB_BC_out$MGIT_inc_date))]
MB_BC_out$earliest_inc[(is.na(MB_BC_out$earliest_inc) & !is.na(MB_BC_out$ISOL_inc_date))] <- MB_BC_out$ISOL_inc_date[(is.na(MB_BC_out$earliest_inc) & !is.na(MB_BC_out$ISOL_inc_date))]

### DONE WITH BCs


# DATASET- SWITCH SPSS DATES TO R

df$ddeath1 <- spss2date(df$ddeath1)
df$dfu <- spss2date(df$dfu)
df$dvisit <- spss2date(df$dvisit)
df$dcd4ob <- spss2date(df$dcd4ob)

df <- merge(df,MB_BC_out, by= PID)

## these dates are inconsistent - the "venesection date" (actually named drawn date in the original dataset) is
# in many cases long before "visit date" which seems to be th enrollment date - waiting for confirmatoin from patricia
# similarly the earliest incubation dates of the samples often seem to be before the visit date - ignore pending confirmation

# I will ignore venesection date - set to NA, safest

#### 2. Apply primary study specific inclusion criteria if needed ####

# Not required


#### 3. Set up standardise df to populate with data from primary df ####

# get standardised variable names
newvars <- read.csv(paste0(wdpath , "/vardefs.csv"), header=T, na.strings=c(".", "NA"))
newvars <- as.character(newvars[ ,1])

# make a new df for standardised data #should be nrows of df, ncols=newvars
munseri <- as.data.frame(t(matrix(data=rep(0, (nrow(df)*length(newvars))), nrow=length(newvars))))
# name the variables
names(munseri) <- newvars 





#### 4. populate the variables in standardised data frame from primary data frame ####



munseri$primary.study <- "Munseri2011"

munseri$country <- "Tanzania"

munseri$setting1 <- "inpatient"

munseri$setting2 <- NA

munseri$year <- format(df$dvisit, "%Y")

munseri$age <- df$age

munseri$sex <- as.character(df$sex)
munseri$sex[munseri$sex == "Female"] <- "female"
munseri$sex[munseri$sex == "Male"] <- "male"



munseri$HIVstatus <- 1

munseri$CD4 <- df$CD4res

munseri$admissionDate <- munseri$recruitmentDate # assumption!

munseri$recruitmentDate<- df$dvisit

munseri$venepunctureDate <- NA

munseri$incubationDate <- NA

munseri$positive.cultureDate <- NA

munseri$ttpBC <- df$MGIT_ttp

munseri$assayBC <- "liquid"

munseri$volumeBC <- df$sample_vol

munseri$numberBC <-df$n_ISOL + df$n_MGIT
munseri$failedBC <- df$any_failed
munseri$contamBC <- df$any_contaminated
munseri$availableBC <- df$total_avail_culture
munseri$other.pathogen.BC <- df$other_pathogen
munseri$BCresult <- df$any_MTB

munseri$cxrTBprimary <- NA

munseri$cxrTBmeta <- NA

munseri$sputumAvailable <- df$spf_tot
munseri$sputumAvailable[munseri$sputumAvailable == 99 ] <- 0
munseri$sputumAvailable[munseri$sputumAvailable > 0 ] <- 1

munseri$sputumNumber <- df$spf_tot
munseri$sputumNumber[munseri$sputumNumber == 99 ] <- 0

munseri$sputumXpert <- NA
munseri$sputumCulture <- df$spf_pos
munseri$sputumCulture[munseri$sputumCulture > 0] <-1
munseri$sputumCulture[munseri$sputumAvailable == 0] <- NA

munseri$sputumResult <- munseri$sputumCulture

munseri$ulamAvailable <- 0

munseri$ulamResult <- NA

munseri$cough <- df$cough
munseri$cough[munseri$cough == 99]<- NA

munseri$fever <- df$fever
munseri$fever[munseri$fever == 99] <- NA

munseri$weightloss <- df$wl
munseri$weightloss[munseri$weightloss == 99] <- NA

munseri$nightsweats <- df$ns
munseri$nightsweats[munseri$nightsweats == 99] <- NA

munseri$temperature <-df$temp

munseri$RR <- NA

munseri$HR <- NA

munseri$sBP <- NA

munseri$dBP <- NA

munseri$GCS <- NA

munseri$AVPU <- NA

munseri$encephalopathy <- NA

munseri$ambulant <- NA

fever <-munseri$fever
cough <-munseri$cough
NS <- munseri$nightsweats
WL <- munseri$weightloss
WHOscrTEMP <- data.frame(fever,cough,NS,WL)
munseri$missingWHOscreen <- apply(is.na(WHOscrTEMP),1,sum)
WHOscrTEMP[is.na(WHOscrTEMP)] <-0
munseri$WHOscreen <- apply(WHOscrTEMP,1,sum)



RR<- munseri$RR
temperature <- munseri$temperature
HR <- munseri$HR
not_ambulant <- munseri$ambulant
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


munseri$missingWHOdanger <- apply(is.na(WHOdangertemp),1,sum)
WHOdangertemp[is.na(WHOdangertemp)] <-0
munseri$WHOdanger <- apply(WHOdangertemp,1,sum)
munseri$WHOdanger[munseri$WHOdanger > 0] <-1




munseri$lactate <- NA
munseri$WCC <- NA


SIRS <- munseri[,c('RR','HR','temperature','WCC')]
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


munseri$missingSepsis <- apply(is.na(SIRS),1,sum)
SIRS[is.na(SIRS)] <-0

munseri$sepsis <-  apply(SIRS,1,sum)
munseri$sepsis[munseri$sepsis < 2] <- 0
munseri$sepsis[munseri$sepsis >= 2] <-1


munseri$severe.sepsis <- NA

munseri$dateDeath <- df$ddeath1

# it looks as though one dd/mm may have been reversed

munseri$dateDeath[74] <- as.Date("2007-06-10")
#Remove one impossible value
munseri$dateDeath[89] <- NA
df$death[89] <- NA


munseri$censorDate <- as.Date(NA)
munseri$censorDate[df$death == 1 & !is.na(df$death) ] <- as.Date(df$ddeath1[df$death == 1 & !is.na(df$death)])
munseri$censorDate[df$death == 0 & !is.na(df$death) ] <- as.Date(df$dfu[df$death == 0 & !is.na(df$death)])
munseri$censorDate[74] <- as.Date("2007-10-16")
munseri$censorDate[89] <- NA


munseri$inpatientDeath <- NA

munseri$day30death <- as.numeric(munseri$censorDate - munseri$recruitmentDate)
munseri$day60death <- as.numeric(munseri$censorDate - munseri$recruitmentDate)
munseri$day90death <- as.numeric(munseri$censorDate - munseri$recruitmentDate)

munseri$day30death[munseri$day30death <= 30 & df$death == 1 & !is.na(munseri$day30death)] <- 1
munseri$day30death[munseri$day30 < 30 & df$death == 0 & !is.na(munseri$day30death)] <- NA
munseri$day30death[munseri$day30death >= 30 & !is.na(munseri$day30death) ] <- 0

munseri$day60death[munseri$day60death <= 60 & df$death == 1 & !is.na(munseri$day60death)] <- 1
munseri$day60death[munseri$day60 < 60 & df$death == 0 & !is.na(munseri$day60death)] <- NA
munseri$day60death[munseri$day60death >= 60 & !is.na(munseri$day60death) ] <- 0

munseri$day90death[munseri$day90death <= 90 & df$death == 1 & !is.na(munseri$day90death)] <- 1
munseri$day90death[munseri$day90 < 90 & df$death == 0 & !is.na(munseri$day30death)] <- NA
munseri$day90death[munseri$day90death >= 90 & !is.na(munseri$day90death) ] <- 0


munseri$TBdiagnosis <- 0
munseri$TBdiagnosis[munseri$sputumResult == 1 & !is.na(munseri$sputumResult)] <-1
munseri$TBdiagnosis[munseri$BCresult == 1 & !is.na(munseri$BCresult)] <- 1

munseri$priorTBRx <- NA
munseri$dateTBRx <- NA



######################################################################################

#### 5. export the final data set as .csv fle ####

write.csv(munseri, paste0(wdpath, "/standardised/s_munseri.csv"))


