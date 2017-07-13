### survival analysis MTB-BSI

### VERY rough code for initial survival analysis

library(dplyr)
library(survival)
library(coxme)
library(survminer)

source("/Users/joelewis/Documents/Projects/MTB_BSI/R2/mortality_analyses/load_recode_for_survival_analysis.R")

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

me2 <- coxme(s ~ BCresult + CD4 + age + sex + (1 | primary.study) + (1 | setting1) + (1 | cohort.type), data = TBoc)
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
