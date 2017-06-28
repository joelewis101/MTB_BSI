### logistic regression mortality model MTB BSI ###

### VERY rough code for initial mixed efefcts logistic regression model
### for 30 day mortality

library(dplyr)
library(survival)
library(coxme)
library(survminer)

wdpath = "/Users/joelewis/Documents/Projects/MTB_BSI/data/standardised"

crump <- read.csv(paste0(wdpath , "/s_crump.csv"), header=TRUE, stringsAsFactors = FALSE)
munseri <- read.csv(paste0(wdpath , "/s_munseri.csv"), header=TRUE, stringsAsFactors = FALSE)
feasey <- read.csv(paste0(wdpath , "/s_feasey.csv"), header=TRUE, stringsAsFactors = FALSE)
bedell <- read.csv(paste0(wdpath , "/s_bedell.csv"), header=TRUE, stringsAsFactors = FALSE)


lawn <- read.csv(paste0(wdpath , "/s_lawn.csv"), header=TRUE, stringsAsFactors = FALSE)
schutz <- read.csv(paste0(wdpath , "/s_schutz.csv"), header=TRUE, stringsAsFactors = FALSE)
varma <- read.csv(paste0(wdpath , "/s_varma.csv"), header=TRUE, stringsAsFactors = FALSE)
vonG <- read.csv(paste0(wdpath , "/s_vonG.csv"), header=TRUE, stringsAsFactors = FALSE)

lawn$admissionDate <- NA
schutz$admissionDate <- NA
varma$admissionDate <- NA
vonG$admissionDate <- NA
crump$admissionDate <- NA
schutz$positive.cultureDate <- NA
varma$sex <- as.character(varma$sex)
varma$sex[varma$sex == 1] <- "female"
varma$sex[varma$sex == 2] <- "male"
schutz$ulamAvailable <- 0
schutz$ulamResult <- NA

## schutz has no mortality data
### nor does varma


### exclude these 


## looking at D30 mortality only bedell feasey lawn have < 10 % missing

## so let's try them



#df <- rbind(crump,munseri)
df <- rbind(bedell,feasey)
#df <- rbind(df, bedell)
#df <- rbind(df,munseri )



df <- bind_rows(df, lawn)

#df <- bind_rows(df,schutz)
#df <- bind_rows(df, varma)
#df <- bind_rows(df, vonG)
#df <- bind_rows(df, crump)

df$admissionDate <- as.Date(df$admissionDate)
df$recruitmentDate  <- as.Date(df$recruitmentDate)
df$venepunctureDate <- as.Date(df$venepunctureDate)
df$incubationDate <- as.Date(df$incubationDate)
df$positive.cultureDate <- as.Date(df$positive.cultureDate)
df$censorDate <- as.Date(df$censorDate)
df$dateDeath <- as.Date(df$dateDeath)

df$fu_time <- as.numeric(df$censorDate - df$recruitmentDate)
df$died <- as.numeric(0)

df$died[!is.na(df$censorDate)] <- 0
df$died[!is.na(df$dateDeath)] <- 1
df$died[is.na(df$censorDate)] <- NA

df$cohort.type <- ""
df$cohort.type[df$primary.study == "Bedell2012" | df$primary.study == "Munseri2011" | df$primary.study == "Feasey2013" | df$primary.study == "vonGottberg2001"] <- "TBsuspect"
df$cohort.type[df$primary.study == "Lawn2015"] <- "HIV"
df$cohort.type[df$primary.study == "Crump2012"] <- "fever"

table(df$primary.study)

### 1000 patients

dfTB <- subset(df, TBdiagnosis == 1)
nrow(dfTB)

## 230 with TB

### logistic regression model without ixed effects to start

lrmod <- glm (dfTB$day30death ~ age + sex + CD4 + BCresult, family = binomial, data = dfTB)

#and with

lrmodme <- glmer (dfTB$day30death ~ age + sex + CD4 + BCresult + (1|primary.study), family = binomial, data = dfTB)
