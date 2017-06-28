#### missingness assessment for MTB-BSI ###
### V1.0 15 May 2017 ####

### this code outputs a table giveing number of NAs accross different
### variables - just allows quick eyeball of missingness in different
### datasets

compl <- function(dfin) {
  study_name <- deparse(substitute(dfin))
  CXR_NAs <- sum(is.na(dfin$cxrTBmeta))/nrow(dfin)
  LAM_NAs <- (sum(is.na(dfin$ulamAvailable))+ sum(dfin$ulamAvailable == 0 & !is.na(dfin$ulamAvailable)))/nrow(dfin)
  sputum_NAs <- (sum(is.na(dfin$sputumAvailable))+sum(dfin$sputumAvailable == 0 & !is.na(dfin$sputumAvailable)))/nrow(dfin)
  BCs_NA <- (sum(is.na(dfin$BCresult)))/nrow(dfin)
  BCs_contam <- (sum(is.na(dfin$contamBC )))/nrow(dfin)
  BCs_pos <- (sum(dfin$BCresult == 1 & !is.na(dfin$BCresult)))/nrow(dfin)
  
  prop_TB <- sum(dfin$TBdiagnosis == 1)/nrow(dfin)
  missing_TB <- sum(is.na(dfin$TBdiagnosis))/nrow(dfin)
  
  IP_mort_NAs <- sum(is.na(dfin$inpatientDeath))/nrow(dfin)
  D30_mort_NAs <- sum(is.na(dfin$day30death))/nrow(dfin)
  D60_mort_NAs <- sum(is.na(dfin$day60death))/nrow(dfin)
  D90_mort_NAs <- sum(is.na(dfin$day90death))/nrow(dfin)
  censor_date_NAs <- sum(is.na(dfin$censorDate))/nrow(dfin)
  death_date_NAs <- sum(is.na(dfin$dateDeath))/nrow(dfin)
  
  
  dfout <- data.frame(BCs_NA,BCs_contam, BCs_pos, CXR_NAs,LAM_NAs,sputum_NAs, IP_mort_NAs, D30_mort_NAs, D60_mort_NAs,censor_date_NAs, death_date_NAs, prop_TB, missing_TB)
  
  
  
  return(dfout)
}


### start work ###

wdpath <- "/Users/joelewis/Documents/Projects/MTB_BSI/data/standardised"
wdpathout <- "/Users/joelewis/Documents/Projects/MTB_BSI/data"


study_names<-list.files(wdpath)#what's there
noitems<-length(study_names)

dftemp <- read.csv(paste0(wdpath , "/", study_names[1]))
tempout <- compl(dftemp)

for (i in 2:noitems) {
  dftemp <- read.csv(paste0(wdpath , "/", study_names[i]))
  temp <- compl(dftemp)
  tempout <- rbind(tempout, temp)
}

tempout <- round (tempout, digits = 2)
tempout <- cbind (study_names,tempout)
print(tempout)

#write.csv(tempout, paste0(wdpathout, "/MTB_BSI_completeness.csv"))
