### survival analysis MTB-BSI

### VERY rough code for initial survival analysis

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

s <- Surv(TBoc$fu_time,TBoc$died)

### cheeky K-M

plot(survfit(s ~ TBoc$BCresult))

#Those dead by D5
summary(survfit(s ~ TBoc$BCresult, type = "kaplan-meier"), time = 5)


## simple cox PH

m <- coxph(s ~ BCresult + CD4 + age, data = TBoc)
summary(m)

### both CD4 and presence of MTB BSI are associated with increased mortality hazard
## sweeeeeeeeet


#### let's take this sheet to another mixed effects level bru

### simply - study and setting and cohort type as mixed effect
me2 <- coxme(s ~ BCresult + CD4 + age + sex + (1 | primary.study) + (1 | setting1) + (1 | cohort.type), data - TBoc)
summary(me2)
### primary study accounts for fair but of variance but setting (ip/op) and cohort type not so much
### on testing (chi sq) of nested models none of this stuff adds anything so stick with just primary study as
### random effect

### if we repeat this for inpatients only

### TBoc <- subset(TBoc, setting1 == "inpatient")

### then the findings areen't drastically different

### sensitivity: lose munseri and vong (lots of missing censor dates)
### again finds pretty much the same so let's keep

### whoops  also check whether sex adds anything to model

me3 <- coxme(s ~ BCresult + CD4 + age + (1 | primary.study) , data = TBoc)
summary(me3)
anova(me2,me3)

## nope so final model has fixed effect BCresult, age, CD4, random effect primary study

summary(me3)

### now we ned to test assumptions of cox PH model
### testing assumptions of cox ME model is hard in R for a coxme object
### you can do with survminer package having added frailty terms to coxph model
### I'll be honest, not sure if these are the same things...

mfrail <- coxph(s ~ BCresult + CD4 + age + frailty(primary.study, distribution = "gaussian"), data = TBoc)

### but look at the coefficients for mfrail and me2 - identical - and random effects across studies very similar

### so lets run diagnostics on mfrail

cox.zph(mfrail)

### no evidence of violation of PH assumption

## plot the schodenfeld residuals vs time - shoud be no change over time

scho <- residuals(mfrail, type = "schoenfeld")
#need to make a df of those who died

TBoc_no_missing <- subset(TBoc, !is.na(TBoc$age))
TBoc_no_missing <- subset(TBoc_no_missing, !is.na(TBoc_no_missing$CD4))
#TBoc <- TBoc_no_missing
d <- subset(TBoc_no_missing, died == 1)
par(mfrow = c(2,2))
plot(d$fu_time,scho[,1], xlab = "time", ylab = "scho_resid_BCres")  + abline(lm(scho[,1] ~ d$fu_time), col= "red")
plot(d$fu_time,scho[,2],xlab = "time", ylab = "scho_resid_CD4") + abline(lm(scho[,2] ~ d$fu_time), col = "red")
plot(d$fu_time,scho[,3],xlab = "time", ylab = "scho_resid_age") + abline(lm(scho[,3] ~ d$fu_time), col = "red")

### all looks ok

### can check the influencialness of individual observations

# ggcoxdiagnostics(mfrail, type = "dfbeta",
 #                linear.predictions = FALSE, ggtheme = theme_bw())

## this plots change in coefficients for each observation if that observation is removed - not much change

## martingdale residuals look at linearity assumption of continuous variables
##need to get rid of missings manually 

#TBoc_no_missing <- subset(TBoc, !is.na(TBoc$age))
#TBoc_no_missing <- subset(TBoc_no_missing, !is.na(TBoc_no_missing$CD4))
#TBoc <- TBoc_no_missing


#s <- Surv(TBoc$fu_time,TBoc$died)
#mfrail <- coxph(s ~ TBoc$BCresult + TBoc$CD4 + TBoc$age + frailty(TBoc$primary.study, distribution = "gaussian"))
#res <- residuals(mfrail, type="martingale")
#par(mfrow = c(2,2))
#plot(TBoc$CD4,res)
#lines(lowess(TBoc$CD4, resid(mfrail)),col='red')

#plot(TBoc$age,res)
#lines(lowess(TBoc$age, resid(mfrail)),col='red')

### erm ... looks ...ok

### D5 mortality analysis

#TBoc$D5death <- NA
#TBoc$D5death[TBoc$fu_time < 5 & TBoc$died == 0] <- NA
#TBoc$D5death[TBoc$fu_time <= 5 & TBoc$died == 1] <- 1
#TBoc$D5death[TBoc$fu_time > 5 & TBoc$died == 0] <- 0
#TBoc$D5death[TBoc$fu_time > 5 & TBoc$died == 1] <- 0

#TBoc$primary.study <- as.factor (TBoc$primary.study)

#studies <- levels(TBoc$primary.study)



#for (i in 1:length(studies))
