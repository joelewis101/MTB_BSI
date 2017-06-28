#### diagnostic meta analysis for MTB BSI data ####
#### two stage bivariate model using mada ###

library(mada)

### set up extraction functions: these will output TP FP FN TN for 
#### patients who have available sputum (sputum_da), lam (lam_da) or cxr (CXR_da) and will give the number of excluded
### they expect as df with the columns BCresult and either cxrTBmeta, sputumResult or ulamResult
## the da stands for diagnostic accuracy :-)


CXR_da <- function(dfin) {
  study_name <- deparse(substitute(dfin))
  
  # rem NAs
  
  excluded <- sum(is.na(dfin$cxrTBmeta)) + sum(is.na(dfin$BCresult))
  dfin <-  subset(dfin, !is.na(cxrTBmeta) & !is.na(BCresult))
  
  # calc TP etc
  
  TP <- sum(dfin$cxrTBmeta == "suggestsTB" & dfin$BCresult == 1 )
  FP <- sum(dfin$cxrTBmeta == "suggestsTB" & dfin$BCresult == 0 )
  FN <- sum(dfin$cxrTBmeta != "suggestsTB" & dfin$BCresult == 1 )
  TN <- sum(dfin$cxrTBmeta != "suggestsTB" & dfin$BCresult == 0 )
  dfout <- data.frame(TP,FP,FN,TN, excluded)
  return(dfout)
}

sputum_da <- function(dfin) {
  study_name <- deparse(substitute(dfin))
  
  # rem NAs
  
  excluded <- sum(is.na(dfin$sputumResult)) + sum(is.na(dfin$BCresult))
  dfin <-  subset(dfin, !is.na(sputumResult))
  dfin <- subset(dfin, !is.na(BCresult))
  
  # calc TP etc
  
  TP <- sum(dfin$sputumResult == 1 & dfin$BCresult == 1 )
  FP <- sum(dfin$sputumResult == 1 & dfin$BCresult == 0 )
  FN <- sum(dfin$sputumResult == 0 & dfin$BCresult == 1  )
  TN <- sum(dfin$sputumResult == 0 & dfin$BCresult == 0 )
  dfout <- data.frame(TP,FP,FN,TN, excluded)
  return(dfout)
}

lam_da <- function(dfin) {
  study_name <- deparse(substitute(dfin))
  
  # rem NAs
  
  excluded <- sum(is.na(dfin$ulamResult)) + sum(is.na(dfin$BCresult))
  dfin <-  subset(dfin, !is.na(ulamResult))
  dfin <- subset(dfin, !is.na(BCresult))
  
  # calc TP etc
  
  TP <- sum(dfin$ulamResult == 1 & dfin$BCresult == 1 )
  FP <- sum(dfin$ulamResult == 1 & dfin$BCresult == 0 )
  FN <- sum(dfin$ulamResult == 0 & dfin$BCresult == 1  )
  TN <- sum(dfin$ulamResult == 0 & dfin$BCresult == 0 )
  dfout <- data.frame(TP,FP,FN,TN, excluded)
  return(dfout)
}




### start work ###

wdpath <- "/Users/joelewis/Documents/Projects/MTB_BSI/data/standardised"
wdpathout <- "/Users/joelewis/Documents/Projects/MTB_BSI/data"

### get all the standardised data frames and convert to the input that mada likes

study_names<-list.files(wdpath)#what's there
noitems<-length(study_names)

dftemp <- read.csv(paste0(wdpath , "/", study_names[1]))

tempoutCXR <- CXR_da(dftemp)
tempoutsputum <- sputum_da(dftemp)
tempoutlam <- lam_da(dftemp)

for (i in 2:noitems) {
  dftemp <- read.csv(paste0(wdpath , "/", study_names[i]))
  
  ## recode a couple of vars in lawn and vong
  
  if (study_names[i] == "s_lawn.csv" | study_names[i] == "s_vonG.csv") { dftemp$sputumResult[dftemp$sputumAvailable == 0] <- NA }
  tempCXR <- CXR_da(dftemp)
  tempsputum <- sputum_da(dftemp)
  templam <- lam_da(dftemp)
  
  tempoutCXR <- rbind(tempoutCXR, tempCXR)
  
  tempoutsputum <- rbind(tempoutsputum, tempsputum)
  
  tempoutlam <- rbind(tempoutlam, templam)
}


CXR <- cbind (study_names,tempoutCXR)
sputum <- cbind (study_names,tempoutsputum)
lam <- cbind (study_names,tempoutlam)

## just one study with lam data, don't take further for now

CXR$excluded_prop <- round(CXR$excluded / ( CXR$TP + CXR$FP + CXR$FN + CXR$TN + CXR$excluded), digits = 2)
sputum$excluded_prop <- round(sputum$excluded / ( sputum$TP + sputum$FP + sputum$FN + sputum$TN + sputum$excluded), digits = 2)
lam$excluded_prop <- round(lam$excluded / ( lam$TP + lam$FP + lam$FN + lam$TN + lam$excluded), digits = 2)

## print the new DFs

print(CXR)
print(sputum)
print(lam)

### get rid of crump, no sputa

sputum <- sputum[-2,]

### get rid of bedell, feasey, lawn, munseri, no cxrs

CXR <- CXR[-c(1,3,5,6),]


s <- sputum[c(2,3,4,5)]
c <- CXR[c(2,3,4,5)]

### start diagnostic accuracy meta analysis

### descriptive stats - sputum

madad(s)
par(mfrow = c(2,2))
forest(madad(s), type = "sens", xlab = "sens")
forest(madad(s), type = "spec", xlab = "spec")
ROCellipse(s, pch = "")
points(fpr(s), sens(s))

## and cxr

madad(c)


par(mfrow = c(2,2))
forest(madad(c), type = "sens", xlab = "sens")
forest(madad(c), type = "spec", xlab = "spec")
ROCellipse(c, pch = "")
points(fpr(c), sens(c))

### fit bivariate model for sputum

par(mfrow = c(1,1))
s.fit <- reitsma(s)
summary(s.fit)

plot(s.fit, sroclwd = 2, main = "SROC curve (bivariate model) for sputum data")
points(fpr(s), sens(s), pch = 2)
legend("bottomright", c("data", "summary estimate"), pch = c(2,1))
legend("bottomleft", c("SROC", "conf. region"), lwd = c(2,1))


## fit bivariate model for CXR

c.fit <- reitsma(c)
summary(c.fit)

plot(c.fit, sroclwd = 2, main = "SROC curve (bivariate model) for CXR data")
points(fpr(c), sens(c), pch = 2)
legend("bottomright", c("data", "summary estimate"), pch = c(2,1))
legend("bottomleft", c("SROC", "conf. region"), lwd = c(2,1))



