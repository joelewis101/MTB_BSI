##########################################################
#####   MTBBSI IPDMA primary data processing script   ####
#####          ****  Gopinath 2008 ****                ######
##########################################################

##### NOTES
##### 

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

df <- read.csv(paste0(wdpath , "/singh/singh.csv"), header=TRUE)


#### 2. Apply primary study specific inclusion criteria if needed ####

# Not required


#### 3. Set up standardise df to populate with data from primary df ####

# get standardised variable names
newvars <- read.csv(paste0(wdpath , "/vardefs.csv"), header=T, na.strings=c(".", "NA"))
newvars <- as.character(newvars[ ,1])

# make a new df for standardised data #should be nrows of df, ncols=newvars
gopinath <- as.data.frame(t(matrix(data=rep(0, (nrow(df)*length(newvars))), nrow=length(newvars))))
# name the variables
names(gopinath) <- newvars 





#### 4. populate the variables in standardised data frame from primary data frame ####



gopinath$primary.study <- rep("Gopinath2008", nrow(gopinath))
gopinath$primary.study.site <- rep(1,nrow(gopinath))
gopinath$country <- rep("India", nrow(gopinath))

gopinath$setting1 <- rep("outpatient", nrow(gopinath))
gopinath$setting2 <- rep("OPD", nrow(gopinath))

gopinath$setting2 <- rep(NA, nrow(gopinath))

df$LABID <- as.character(df$LABID)

df$year <- as.character("")
for (i in 1:nrow(df)) {
  df$year[i] <- paste0("20", strsplit(df$LABID[i], "/")[[1]][2])
}
  


gopinath$year <- df$year

df$Age_sex <- as.character(df$Age_sex)
df$Age_sex[df$Age_sex == ""] <- NA

for (i in 1:nrow(df)) {
  df$age[i] <- strsplit(df$Age_sex[i], "/")[[1]][1]
  df$sex[i] <- strsplit(df$Age_sex[i], "/")[[1]][2]
}

df$sex[df$sex == "M"] <- "male"
df$sex[df$sex == "F"] <- "female"

gopinath$age <- as.numeric(as.character(df$age)) 

gopinath$sex <- df$sex

gopinath$HIVstatus <- rep(1, nrow(gopinath))

df$CD4 <- as.character(df$CD4)
df$CD4 <- as.numeric(df$CD4)

gopinath$CD4 <- df$CD4


gopinath$recruitmentDate <- NA 

gopinath$recruitmentDate <- NA

gopinath$venepunctureDate <- NA

gopinath$incubationDate <- NA

gopinath$positive.cultureDate <- NA



gopinath$ttpBC <-  NA
gopinath$assayBC <- rep("liquid",nrow(gopinath))
gopinath$volumeBC<- rep(5, nrow(gopinath))


# All had BC sent

gopinath$numberBC <- rep(1, nrow(gopinath))

# No contam in dataset - set to 0
# Maunscript says contamination rate of ~ 5% - need to clarify

gopinath$contamBC <- 0

# available - 1 for all

gopinath$availableBC <- 1

# only other pathogens are NTM, not speciated

gopinath$other.pathogen.BC <- 0
gopinath$other.pathogen.BC[df$blood_species == "MAC"] <- "MAC"
gopinath$other.pathogen.BC[df$blood_species == "MKA"] <- "M_Kansasii"
gopinath$other.pathogen.BC[df$blood_species == "MTB+MAC"] <- "MAC"

# set MTB var

gopinath$BCresult <- 0

gopinath$BCresult[df$blood_species == "MTB" | df$blood_species == "MTB+MAC"] <- 1

## CXR

gopinath$cxrTBprimary <- rep(NA, nrow(gopinath))

gopinath$cxrTBmeta <- rep(NA, nrow(gopinath))
gopinath$cxrTBmeta[df$CXR == "?disseminated TB"] <- "possibleTB"
gopinath$cxrTBmeta[df$CXR == "?PTB"] <- "possibleTB"
gopinath$cxrTBmeta[df$CXR == "?TB"] <- "possibleTB"
gopinath$cxrTBmeta[df$CXR == "cal. Lesion"] <- "undiagnosticTB"
gopinath$cxrTBmeta[df$CXR == "cavity"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "cavity lesion"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "COAD"] <- "undiagnosticTB"
gopinath$cxrTBmeta[df$CXR == "koch"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "multiple opacity"] <- "undiagnosticTB"
gopinath$cxrTBmeta[df$CXR == "NAD"] <- "normal"
gopinath$cxrTBmeta[df$CXR == "opacity"] <- "undiagnosticTB"
gopinath$cxrTBmeta[df$CXR == "pl.effusion"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "Pl.effusion"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "PTB"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "PTB/Multiple lymph nodes in Cervical and axillary region"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "Pul Koch"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "Pulm. Disorder"] <- "undiagnosticTB"
gopinath$cxrTBmeta[df$CXR == "RMZ, RLZ effusion"] <- "suggestsTB"
gopinath$cxrTBmeta[df$CXR == "trachea central; fibrotic bands "] <- "undiagnosticTB"


### sputa 

df$sputum1 <- as.character(df$sputum1)
df$sputum1[df$sputum1 == "ND" | df$sputum1 == "NP" | df$sputum1 == "NS"] <- NA
df$sputum1[df$sputum1 == "Pos"] <- "1"
df$sputum1[df$sputum1 == "Neg"] <- "0"

df$sputum2 <- as.character(df$sputum2)
df$sputum2[df$sputum2 == "ND" | df$sputum2 == "NP" | df$sputum2 == "NS"] <- NA
df$sputum2[df$sputum2 == "Pos"] <- "1"
df$sputum2[df$sputum2 == "Neg"] <- "0"

df$sputum3 <- as.character(df$sputum3)
df$sputum3[df$sputum3 == "ND" | df$sputum3 == "NP" | df$sputum3 == "NS"] <- NA
df$sputum3[df$sputum3 == "Pos"] <- "1"
df$sputum3[df$sputum3 == "Neg"] <- "0"



gopinath$sputumNumber <- as.numeric(!is.na(df$sputum1)) + as.numeric(!is.na(df$sputum2)) + as.numeric(!is.na(df$sputum3))
gopinath$sputumAvailable <- 0
gopinath$sputumAvailable[gopinath$sputumNumber > 0] <-1

gopinath$sputumCulture <- 0

gopinath$sputumCulture[df$sputum1 == "1"] <-1
gopinath$sputumCulture[df$sputum2 == "1"] <-1
gopinath$sputumCulture[df$sputum3 == "1"] <-1

# no LAM

gopinath$ulamAvailable <- rep(0,nrow(gopinath))
gopinath$ulamResult <- rep(NA, nrow(gopinath))

# 

df$cough <- as.character(df$cough)
df$cough[df$cough == "Yes" | df$cough == "yes" | df$cough == "pos"] <- "1"
df$cough[df$cough == "No" | df$cough == "no"] <- "0"
df$cough[df$cough == ""] <- NA

gopinath$cough <- df$cough

df$fever <- as.character(df$fever)
df$fever[df$fever == "Yes" | df$fever == "yes" | df$fever == "pos"] <- "1"
df$fever[df$fever == "No" | df$fever == "no"] <- "0"
df$fever[df$fever == ""] <- NA

gopinath$fever <- df$fever

gopinath$weightloss <- NA
gopinath$nightsweats <- NA

gopinath$temperature <- rep(NA, nrow(gopinath))
gopinath$RR <- rep(NA, nrow(gopinath))
gopinath$HR <- rep(NA, nrow(gopinath))
gopinath$sBP <- rep(NA, nrow(gopinath))
gopinath$dBP <- rep(NA, nrow(gopinath))
gopinath$GCS <- rep(NA, nrow(gopinath))
gopinath$AVPU <- rep(NA, nrow(gopinath))
gopinath$encephalopathy <- rep(NA, nrow(gopinath))


gopinath$ambulant <- NA




### Clunky code to calculate WHO screen and no. missing\
  

     
fever <-as.numeric(gopinath$fever)
cough <-as.numeric(gopinath$cough)
NS <- as.numeric(gopinath$nightsweats)
WL <- as.numeric(gopinath$weightloss)
WHOscrTEMP <- data.frame(fever,cough,NS,WL)
gopinath$missingWHOscreen <- apply(is.na(WHOscrTEMP),1,sum)
WHOscrTEMP[is.na(WHOscrTEMP)] <-0
gopinath$WHOscreen <- apply(WHOscrTEMP,1,sum)




gopinath$WHOdanger <- 0
gopinath$missingWHOdanger <- 4


gopinath$lactate <- NA
gopinath$WCC <- NA

#sepsis


gopinath$missingSepsis <- 4

gopinath$sepsis <- 0


gopinath$severe.sepsis <- NA






# mortality - no data

gopinath$dateDeath <- NA
gopinath$censorDate <- NA
gopinath$inpatientDeath <- NA
gopinath$day30death <- NA
gopinath$day60death <- NA
gopinath$day90death <- NA


## assume final diagnosis of TB if BC or sputum culture pos
# there is no study definition

gopinath$TBdiagnosis <- 0
gopinath$TBdiagnosis[gopinath$sputumCulture == 1] <-1
gopinath$TBdiagnosis[gopinath$BCresult == 1] <-1

gopinath$dateTBRx <- NA

gopinath$priorTBRx <- 0





######################################################################################

#### 5. export the final data set as .csv fle ####

write.csv(gopinath, paste0(wdpath, "/standardised/s_gopinath.csv"))


