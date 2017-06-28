### parameterised survival analysis MTB BSI ###

### survival analysis MTB-BSI
### experimenting with parameterised survival models (weibull)

library(dplyr)
library(survival)
library(coxme)
library(survminer)
library(rms)

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


## crump has no TB diagnoses except BC - include for now
## munseri has a lot of missing
### so does vonG

## bedell we have made assumptions


### lets do bedell feasey law munseri von g cox PH



#df <- rbind(crump,munseri)
df <- rbind(bedell,feasey)
#df <- rbind(df, bedell)
df <- rbind(df,munseri )



df <- bind_rows(df, lawn)

#df <- bind_rows(df,schutz)
#df <- bind_rows(df, varma)
df <- bind_rows(df, vonG)
df <- bind_rows(df, crump)

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


nrow(df)
### we have 1714 patients
table(df$BCresult)
#### 129 MTB BSI
table(df$died)
### 264 deaths
table(df$TBdiagnosis)
### 335 MTB diagnoses

TB <- subset(df, TBdiagnosis ==1 )
nrow(TB)

### we have 272 with outcome data
TBoc <- subset(TB, !is.na(TB$died))
nrow(TBoc)
table(TBoc$BCresult)
### in this there are 94 MTB BSI 

### make surv object

### weibull can't cope with 0 fu time
### so switch to 0.1
TBoc$fu_time[TBoc$fu_time == 0] <- 0.1


s <- Surv(TBoc$fu_time,TBoc$died)

### cheeky K-M



plot(survfit(s ~ TBoc$BCresult))

#Those dead by D5
summary(survfit(s ~ TBoc$BCresult, type = "kaplan-meier"), time = 5)

pfrail <- survreg(s ~ BCresult + CD4 + age, data = TBoc, type = "weibull")

#  + frailty(primary.study, distribution = "gaussian")

## exp(coef) for weibull model means that you have a  a times shorter survival time

exp(coef(pfrail))

### convert to cox coefficients

exp(coef(pfrail) * -1 * 1/1.68)

### perhaps unsurprisingly, neary identical to a cox frailty model

mfrail <- coxph(s ~ BCresult + CD4 + age + frailty(primary.study, distribution = "gaussian"), data = TBoc)

exp(coef(mfrail))

plot(survfit(s ~ TBoc$BCresult), col = "black")

lines(survfit(mfrail, newdata = data.frame(BCresult =1, age = mean(TBoc$age[TBoc$BCresult ==1], na.rm = TRUE), CD4= median(TBoc$CD4[TBoc$BCresult == 1], na.rm = TRUE)), col = "green"))
seq.quantiles = seq(from = 0.00, to = 1.00, by = 0.01)


lines(x = predict (pfrail, newdata = data.frame(BCresult =1, age = mean(TBoc$age[TBoc$BCresult ==1], na.rm = TRUE), CD4= median(TBoc$CD4[TBoc$BCresult == 1], na.rm = TRUE)), type = "quantile", p = seq.quantiles), y= 1 - seq.quantiles, col = "blue")

lines(x = predict (pfrail, newdata = data.frame(BCresult =0, age = mean(TBoc$age[TBoc$BCresult ==0], na.rm = TRUE), CD4= median(TBoc$CD4[TBoc$BCresult == 0], na.rm = TRUE) ), type = "quantile", p = seq.quantiles), y= 1 - seq.quantiles, col = "red")
