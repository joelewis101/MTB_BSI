#### bootstrap nonparametric estimates for 95% CI for ###
### hazard ratio of mixed effects cox model for MTB-BSI ###
### bit of a mouthful ###

# Nested sampling strategy #
# That is, sample at study level then at patient level #
# The question is whether to sample with replacement or not ##
# I will try to do both

library(dplyr)
library(survival)
library(coxme)
library(survminer)
library(gridExtra)

#source("/Users/joelewis/Documents/Projects/MTB_BSI/R2/mortality_analyses/load_recode_for_survival_analysis.R")

# it uses data frame TBoc for all patients with confirmed TB who have outcome data ##
# let's take he input data frame to be dat
# we expect $fu_time numberic in days
# and $died {0,1}


dat <- TBoc
dat <- dat[c("primary.study","CD4","age","BCresult","fu_time","died")]

# flags to decide if we will sample with replacement at study and patient level

replacestudies = TRUE
replacepatients = TRUE

# this is the surv object

s <- Surv(dat$fu_time,TBoc$died)

# and this the final model

me3 <- coxme(s ~ BCresult + CD4 + age + (1 | primary.study) , data = dat)
summary(me3)

m <- t(as.data.frame(me3$coefficients))
rownames(m) <- NULL

# now then

# generate sample of studies
dat$primary.study <- as.factor(dat$primary.study)
studies <- levels(dat$primary.study)
n_studies <- length(studies)



for (i in 1:3000) {
  study_sample <- sample(studies, size = n_studies, replace = replacestudies)
 for (j in 1:length(study_sample)) {
    currentstudyname <- study_sample[j]
    currentstudy <- subset(dat,primary.study == currentstudyname)
    samplestudytemp <- currentstudy[sample(nrow(currentstudy), nrow(currentstudy), replace = replacepatients),]
    
    if (j == 1) {samplestudy <- samplestudytemp } else {samplestudy <- rbind(samplestudy,samplestudytemp)}
  }
  
#run cox model and put coefficients in a matrix
  
# the following line just samples without clustering  
# samplestudy <- dat[sample(nrow(dat), nrow(dat), replace = replacepatients),]
 
 
  msurv <- Surv(samplestudy$fu_time,samplestudy$died)
  mod <- coxme(msurv ~ BCresult + CD4 + age + (1 | primary.study) , data = samplestudy)
  mtemp <- t(as.data.frame(mod$coefficients))
  rownames(mtemp) <- NULL
  m<- rbind(m,mtemp )
  
  
  
}

m <- as.data.frame(m)

ggplot(m, aes(x=(BCresult))) + geom_histogram()

# plot histograms for 500,1000,2000,3000 iterations

p1 <- ggplot(m[1:500,], aes(x=(BCresult))) + geom_histogram()
p2 <- ggplot(m[1:1000,], aes(x=(BCresult))) + geom_histogram()
p3 <- ggplot(m[1:2000,], aes(x=(BCresult))) + geom_histogram()
p4 <- ggplot(m[1:3000,], aes(x=(BCresult))) + geom_histogram()

grid.arrange(p1,p2,p3,p4,ncol = 2)

# means for all of them 

median(m$BCresult[1:500])
median(m$BCresult[1:1000])
median(m$BCresult[1:2000])
median(m$BCresult[1:3000])

# similar to the fitted model

me3

# so lets get some 95% CIs

m500 <- m[1:500,]
m1000 <- m[1:1000,]
m2000 <- m[1:2000,]
m3000 <- m[1:3000,]

m500ord <- m500[order(m500$BCresult),]
m1000ord <- m1000[order(m1000$BCresult),]
m2000ord <- m2000[order(m2000$BCresult),]
m3000ord <- m3000[order(m3000$BCresult),]

print("95% CI for BCresult 500 iterations")
exp(m500ord$BCresult[25])
exp(m500ord$BCresult[475])

print("95% CI for BCresult 1000 iterations")
exp(m1000ord$BCresult[50])
exp(m1000ord$BCresult[950])

print("95% CI for BCresult 2000 iterations")
exp(m2000ord$BCresult[100])
exp(m2000ord$BCresult[1900])

print("95% CI for BCresult 3000 iterations")
exp(m3000ord$BCresult[150])
exp(m3000ord$BCresult[2850])

# not very different - ie pretty stable and similar to the SE from the model * 1.96
# which in this case is .52 so CI
exp(me3$coefficients[1]+0.52)
exp(me3$coefficients[1]-0.52)

## so what's the deal with NOT replacing then ...

replacestudies = TRUE
replacepatients = FALSE

# generate sample of studies
dat$primary.study <- as.factor(dat$primary.study)
studies <- levels(dat$primary.study)
n_studies <- length(studies)



# let's just do 1000 studies

for (i in 1:1000) {
  study_sample <- sample(studies, size = n_studies, replace = replacestudies)
 for (i in 1:length(study_sample)) {
  currentstudyname <- study_sample[i]
   currentstudy <- subset(dat,primary.study == currentstudyname)
    samplestudytemp <- currentstudy[sample(nrow(currentstudy), nrow(currentstudy), replace = replacepatients),]
    
    if (i == 1) {samplestudy <- samplestudytemp } else {samplestudy <- rbind(samplestudy,samplestudytemp)}
  }
  
 # run cox model and put coefficients in a matrix
  
  # the following line just samples without clustering  
  #samplestudy <- dat[sample(nrow(dat), nrow(dat), replace = replacepatients),]
  
  
  msurv <- Surv(samplestudy$fu_time,samplestudy$died)
  mod <- coxme(msurv ~ BCresult + CD4 + age + (1 | primary.study) , data = samplestudy)
  mtemp <- t(as.data.frame(mod$coefficients))
  rownames(mtemp) <- NULL
  m<- rbind(m,mtemp )
  
  
  
}

m <- as.data.frame(m)

ggplot(m, aes(x=(BCresult))) + geom_histogram()

# plot histograms for 500,1000,2000,3000 iterations

p1 <- ggplot(m[1:500,], aes(x=(BCresult))) + geom_histogram()
p2 <- ggplot(m[1:1000,], aes(x=(BCresult))) + geom_histogram()


grid.arrange(p1,p2,ncol = 2)

# means for all of them 

mean(m$BCresult[1:500])
mean(m$BCresult[1:1000])


# ? similar to the fitted model

me3

# actually seems not that different



# so lets get some 95% CIs

m500 <- m[1:500,]
m1000 <- m[1:1000,]


m500ord <- m500[order(m500$BCresult),]
m1000ord <- m1000[order(m1000$BCresult),]


print("95% CI for BCresult 500 iterations")
exp(m500ord$BCresult[25])
exp(m500ord$BCresult[475])

print("95% CI for BCresult 1000 iterations")
exp(m1000ord$BCresult[50])
exp(m1000ord$BCresult[950])

# ok makes very little difference

# conclusions: 

# 1) for loops are slow but easy to understand
# 2) sampling strategy here doesn't make much difference
# 3) 1000 iterations seems fine
# 4) next: make functions to call this more easily