#### bootstrap nonparametric estimates for 95% CI for ###
### hazard ratio of mixed effects cox model for MTB-BSI ###
### v2 - functionised ###
## still got slow cocking for loops ###
### ah well ###

library(dplyr)
library(survival)
library(coxme)
library(survminer)
library(gridExtra)

# expects to get handed a dataframe with $primary.study, $CD4, $BCresult, $age, $died, $fu_time in days
# fits model coxme(msurv ~ BCresult + CD4 + age + (1 | primary.study) , data = samplestudy)
# and returns a df with the coefficents

## replacestudies = TRUE samples studies with repacement, replacepatients = TRUE samples patients with replacement

jl_bootstrap <- function (dat, replacestudies = TRUE, replacepatients = TRUE, n = 1000) {
  
  dat$primary.study <- as.factor(dat$primary.study)
  studies <- levels(dat$primary.study)
  n_studies <- length(studies)
  
  
  
  print("Go and stick the kettle on. This involves for loops.")
  
  for (i in 1:n) {
    
    study_sample <- sample(studies, size = n_studies, replace = replacestudies)
    
    for (j in 1:length(study_sample)) {
      
      currentstudyname <- study_sample[j]
      
      currentstudy <- subset(dat,primary.study == currentstudyname)
      samplestudytemp <- currentstudy[sample(nrow(currentstudy), nrow(currentstudy), replace = replacepatients),]
      if (j == 1) {samplestudy <- samplestudytemp } else {samplestudy <- rbind(samplestudy,samplestudytemp)}
    }
    
    #run cox model and put coefficients in a matrix
    
    msurv <- Surv(samplestudy$fu_time,samplestudy$died)
    
    mod <- coxme(msurv ~ BCresult + CD4 + age + (1 | primary.study) , data = samplestudy)
    mtemp <- t(as.data.frame(mod$coefficients))
    rownames(mtemp) <- NULL
    
    if (i == 1) { 
      m <- mtemp
      } else {m<- rbind(m,mtemp )}
  }

  m <- as.data.frame(m)
  return(m)
}


jl_centile <-  function(dat, centile) {
  # expects a vector and will sort it and output the value at the specified centile
  dattemp <- sort(dat)
  return(dattemp[length(dattemp) * centile])
}

# call script to load and code data for survival analysis

source("/Users/joelewis/Documents/Projects/MTB_BSI/R2/mortality_analyses/load_recode_for_survival_analysis.R")

# fit model using coxme
# see survival_ax for model building and diagnostics

s <- Surv(TBoc$fu_time,TBoc$died)
m <- coxme(s ~ BCresult + CD4 + age + (1 | primary.study) , data = TBoc)

m

# bootstrap with 1000 iterations and replacement at study and patient levels

b <- jl_bootstrap(TBoc)

# look at the histograms of the parameters




  p1 <- ggplot(b, aes(x=(BCresult))) + geom_histogram()
   p2 <- ggplot(b, aes(x=(CD4))) + geom_histogram()
   p3 <- ggplot(b, aes(x=(age))) + geom_histogram()

   
     grid.arrange(p1,p2,p3, ncol = 2)
     

# output df 

median <- as.data.frame(apply(b,2,median))
colnames(median) <- "median"

mean <- as.data.frame(apply(b,2,mean))
colnames(mean) <- "mean"

ci_95_ul <- as.data.frame(apply(b,2,jl_centile, 0.975))
colnames(ci_95_ul) <- "ci_95_ul"

ci_95_ll <- as.data.frame(apply(b,2,jl_centile, 0.025))
colnames(ci_95_ll) <- "ci_95_ll"

out <- cbind (median, mean, ci_95_ll, ci_95_ul)

print("bootstrap results")

out

exp(out)

m
