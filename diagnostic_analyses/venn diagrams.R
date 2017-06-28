#### make venn diagrams from MTB-BSI data  ###

### experimenting with venn diagrams for describing overlapping diagnostic
### modalities employed in the data

#### HEALTH WARNING ####
### THE VENNEULER PACKAGE HAS SOME CLEAR ERRORS - THERE ARE OVERLAPS ####
### IN VENN DIAGRAMS WHERE THERE SHOULDN'T BE ####
### This is, I think, due to the mathematical impossiility of solving the ###
### equations for some venn diagrams ###
### need to explore stata venn stuff, may be more stable

library(venneuler)
library(VennDiagram)

overlap <- function(dfin1, dfin2, dfin3, dfin4, dfin5) {
    if (missing(dfin5)) {
      if (missing(dfin4)) {
       if (missing(dfin3)) {
         if (missing(dfin2)) { 
           stop("need at least 2 dfs to calculate overlap")} else {
             print("ballbag")
             dfout <- sum(dfin1 ==1 & dfin2 == 1, na.rm = TRUE)
           }
       } else {
         dfout <- sum(dfin1 ==1 & dfin2 == 1 & dfin3 == 1, na.rm = TRUE)
       }
      } else {
        dfout <- sum(dfin1 ==1 & dfin2 == 1 & dfin3 == 1 & dfin4 == 1, na.rm = TRUE)
      }
    } else {
      dfout <- sum(dfin1 ==1 & dfin2 == 1 & dfin3 == 1 & dfin4 == 1 & dfin5 == 1, na.rm = TRUE)
    }
  return(dfout)
}

countem<- function(dfin) {
  dfout <- sum(dfin == 1, na.rm = TRUE)
  return(dfout)
}

triplevenn <- function(a,b,c) {
  
  # this is expecting three data frames with 1s and 0s indicating membership of 
  ## group a, b, c
  # it will ignore NAs
  
  AB <- sum(a == 1 & b == 1 & c !=1, na.rm = TRUE)
  print("AB")
  print(AB)
  AC <- sum(a == 1 & b == 1 & b !=1, na.rm = TRUE)
  print("AC")
  print(AC)
  BC <- sum(b == 1 & c == 1 & a !=1, na.rm = TRUE)
  print("BC")
  print(BC)
  ABC <- sum(a == 1 & b == 1 & c == 1, na.rm = TRUE)
  print("ABC")
  print(ABC)
  
  A <- countem(a)
  
  B <- countem(b)
  C <- countem(c)
  
  A1 <- A - AB - ABC - AC
  print("A1")
  print(A1)
  B1 <- B - AB - BC - ABC
  print("B1")
  print(B1)
  C1 <- C - AC - BC - ABC
  print("C1")
  print(C1)
  
  
  myvenn <- venneuler(c(A=A1,B=B1,C=C1,"A&B" = AB, "A&C" = AC, "B&C" = BC, "A&B&C" = ABC))
  return(myvenn)
}

fivevenn <- function(a,b,c,d,e) {
  
  ABCDE <- overlap(a,b,c,d,e)
  
  ABCD <- sum(a == 1 & b== 1 & c==1 & d==1 & e != 1, na.rm = TRUE)
  ABDE <- sum(a == 1 & b== 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)
  ABCE <- sum(a == 1 & b== 1 & c==1 & d!=1 & e == 1, na.rm = TRUE)
  ACDE <- sum(a == 1 & b!= 1 & c==1 & d==1 & e == 1, na.rm = TRUE)
  
  BCDE <- sum(a!= 1 & b== 1 & c==1 & d==1 & e == 1, na.rm = TRUE)
  
  ABC <- sum(a == 1 & b== 1 & c==1 & d!=1 & e != 1, na.rm = TRUE)
  ABD <- sum(a != 1 & b== 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)
  ABE <- sum(a == 1 & b== 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)
  
  ACD <- sum(a == 1 & b!= 1 & c==1 & d==1 & e != 1, na.rm = TRUE)
  ACE <- sum(a == 1 & b!= 1 & c==1 & d!=1 & e == 1, na.rm = TRUE)
  ADE <- sum(a == 1 & b!= 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)
  
  BCD <- sum(a != 1 & b== 1 & c==1 & d==1 & e != 1, na.rm = TRUE)
  BCE <- sum(a != 1 & b== 1 & c==1 & d!=1 & e == 1, na.rm = TRUE)
  BDE <- sum(a != 1 & b== 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)
  
  CDE <- sum(a != 1 & b!= 1 & c==1 & d==1 & e == 1, na.rm = TRUE)
  
  
  AB <- sum(a == 1 & b== 1 & c!=1 & d!=1 & e != 1, na.rm = TRUE)
  AC <- sum(a == 1 & b!= 1 & c==1 & d!=1 & e != 1, na.rm = TRUE)
  AD <- sum(a == 1 & b!= 1 & c!=1 & d==1 & e != 1, na.rm = TRUE)
  AE <- sum(a == 1 & b!= 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)
  
  BC <- sum(a != 1 & b== 1 & c==1 & d!=1 & e != 1, na.rm = TRUE)
  BD <- sum(a != 1 & b == 1 & c!=1 & d==1 & e != 1, na.rm = TRUE)
  BE <- sum(a != 1 & b == 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)
  
  CD <- sum(a != 1 & b != 1 & c==1 & d==1 & e != 1, na.rm = TRUE)
  CE <- sum(a != 1 & b != 1 & c==1 & d!=1 & e == 1, na.rm = TRUE)
  
  DE <- sum(a != 1 & b != 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)
  
  A <-  sum(a == 1 & b != 1 & c!=1 & d!=1 & e != 1, na.rm = TRUE)
  print(A)
  B <-  sum(a != 1 & b == 1 & c!=1 & d!=1 & e != 1, na.rm = TRUE)
  print(B)
  C <-  sum(a != 1 & b != 1 & c==1 & d!=1 & e != 1, na.rm = TRUE)
  print(C)
  D <-  sum(a != 1 & b != 1 & c!=1 & d==1 & e != 1, na.rm = TRUE)
  print(D)
  E <-  sum(a != 1 & b != 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)
  print(E)
  
  myvenn2 <- venneuler(c(A=A,B=B,C=C,D=D,E=E,"A&B" = AB, "A&C" = AC, "A&D" = AD,"A&E" = AE,
                         "B&C" = BC, "B&D" = BD, "B&E"= BE, "C&D" = CD, "C&E" = CE,
                         "A&B&C" = ABC, "A&B&D" = ABD, "A&B&E" = ABE, "A&C&D" = ACD,
                         "A&C&E" = ACE, "A&D&E" = ADE, "B&C&D" = BCD, "B&C&E" = BCE, "B&D&E" = BDE,
                         "C&D&E" = CDE,"A&B&C&D" = ABCD, "A&B&C&E" = ABCE, "A&C&D&E" = ACDE,
                         "A&B&C&E" = ABDE, "B&C&D&E" = BCDE, "A&B&C&D&E" = ABCDE))
  
  return(myvenn2)
  
}


library(dplyr)
library(VennDiagram)

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





df <- rbind(bedell,feasey)
df <- rbind(df,munseri )
df <- bind_rows(df, lawn)
df <- bind_rows(df,schutz)
df <- bind_rows(df, varma)
df <- bind_rows(df, vonG)
df <- bind_rows(df, crump)

### recode CXR "suggests TB" as 1 and everyting else as 0
df$CXR <- df$cxrTBmeta
df$CXR[df$CXR == "normal"] <- 0
df$CXR[df$CXR == "possibleTB"] <- 0
df$CXR[df$CXR == "undiagnosticTB"] <- 0
df$CXR[df$CXR == "suggestsTB"] <- 1
df$CXR_avail <- 0
df$CXR_avail[!is.na(df$CXR)] <- 1



### 3 way venn for BC+, sputumavail, sputumpos

grid.newpage()
a <- df$sputumAvailable
#alab <- "sputum_available"
b <- df$sputumResult
#blab <- "sputum_positive"
c <- df$BCresult
#clab <- "BC_positive"
#draw.triple.venn(area1 = countem(a), area2 = countem(b), 
      
d <- df$CXR_avail
e <- df$CXR





### three catagery venn diagram

AB <- sum(a == 1 & b == 1 & c !=1, na.rm = TRUE)
AC <- sum(a == 1 & b == 1 & b !=1, na.rm = TRUE)
BC <- sum(b == 1 & c == 1 & a !=1, na.rm = TRUE)
ABC <- overlap(a,b,c)

A <- countem(a)
B <- countem(b)
C <- countem(c)

A1 <- A - AB - ABC - AC
B1 <- B - AB - BC - ABC
C1 <- C - AC - BC - ABC


myvenn <- venneuler(c(A=A1,B=B1,C=C1,"A&B" = AB, "A&C" = AC, "B&C" = BC, "A&B&C" = ABC))


### 5 category venn diagram
### egad, complex

ABCDE <- overlap(a,b,c,d,e)


ABCD <- sum(a == 1 & b== 1 & c==1 & d==1 & e != 1, na.rm = TRUE)
ABDE <- sum(a == 1 & b== 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)
ABCE <- sum(a == 1 & b== 1 & c==1 & d!=1 & e == 1, na.rm = TRUE)
ACDE <- sum(a == 1 & b!= 1 & c==1 & d==1 & e == 1, na.rm = TRUE)

BCDE <- sum(a!= 1 & b== 1 & c==1 & d==1 & e == 1, na.rm = TRUE)

ABC <- sum(a == 1 & b== 1 & c==1 & d!=1 & e != 1, na.rm = TRUE)
ABD <- sum(a == 1 & b== 1 & c!=1 & d==1 & e != 1, na.rm = TRUE)
ABE <- sum(a == 1 & b== 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)

ACD <- sum(a == 1 & b== 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)
ACE <- sum(a == 1 & b!= 1 & c==1 & d!=1 & e == 1, na.rm = TRUE)
ADE <- sum(a == 1 & b!= 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)

BCD <- sum(a != 1 & b== 1 & c==1 & d==1 & e != 1, na.rm = TRUE)
BCE <- sum(a != 1 & b== 1 & c==1 & d!=1 & e == 1, na.rm = TRUE)
BDE <- sum(a != 1 & b== 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)

CDE <- sum(a != 1 & b!= 1 & c==1 & d==1 & e == 1, na.rm = TRUE)


AB <- sum(a == 1 & b== 1 & c!=1 & d!=1 & e != 1, na.rm = TRUE)
AC <- sum(a == 1 & b!= 1 & c==1 & d!=1 & e != 1, na.rm = TRUE)
AD <- sum(a == 1 & b!= 1 & c!=1 & d==1 & e != 1, na.rm = TRUE)
AE <- sum(a == 1 & b!= 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)

BC <- sum(a != 1 & b== 1 & c==1 & d!=1 & e != 1, na.rm = TRUE)
BD <- sum(a != 1 & b == 1 & c!=1 & d==1 & e != 1, na.rm = TRUE)
BE <- sum(a != 1 & b == 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)

CD <- sum(a != 1 & b != 1 & c==1 & d==1 & e != 1, na.rm = TRUE)
CE <- sum(a != 1 & b != 1 & c==1 & d!=1 & e == 1, na.rm = TRUE)

DE <- sum(a != 1 & b != 1 & c!=1 & d==1 & e == 1, na.rm = TRUE)

A <-  sum(a == 1 & b != 1 & c!=1 & d!=1 & e != 1, na.rm = TRUE)
B <-  sum(a != 1 & b == 1 & c!=1 & d!=1 & e != 1, na.rm = TRUE)
C <-  sum(a != 1 & b != 1 & c==1 & d!=1 & e != 1, na.rm = TRUE)
D <-  sum(a != 1 & b != 1 & c!=1 & d==1 & e != 1, na.rm = TRUE)
E <-  sum(a != 1 & b != 1 & c!=1 & d!=1 & e == 1, na.rm = TRUE)

myvenn2 <- venneuler(c(A=A,B=B,C=C,D=D,E=E,"A&B" = AB, "A&C" = AC, "A&D" = AD,"A&E" = AE,
                       "B&C" = BC, "B&D" = BD, "B&E"= BE, "C&D" = CD, "C&E" = CE,
                       "A&B&C" = ABC, "A&B&D" = ABD, "A&B&E" = ABE, "A&C&D" = ACD,
                       "A&C&E" = ACE, "A&D&E" = ADE, "B&C&D" = BCD, "B&C&E" = BCE, "B&D&E" = BDE,
                       "C&D&E" = CDE,"A&B&C&D" = ABCD, "A&B&C&E" = ABCE, "A&C&D&E" = ACDE,
                       "A&B&C&E" = ABDE, "B&C&D&E" = BCDE, "A&B&C&D&E" = ABCDE))


## some kind of problem with scaling 3 set diagrams?
