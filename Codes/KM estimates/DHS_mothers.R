
# Copyright statement comment ---------------------------------------------

# @UNFPA


# Author comment ----------------------------------------------------------

# work is progress-
# Standard error calculation takes a long time (more than 1 hour).
# For countries other than Tanzania, Cote d'Ivoire and Togo, complex sample SE have been calculated based on 
# a sample of the actual data available. In addition, Se based on (weighted) random sampling have been added.
# This proivdes an upper an a lower boundary for SEs


# File description, purpose of code, inputs and output --------------------

# this code has been written in order to estimate the survival curves for girls age 0-14 in Ethiopia
# including non-sampling errors (confidence intervals)
# the calculations are based on the microdataset of the 2016 DHS in Ethiopia


# Source codes, libraries, global options and working directory -----------

library(plyr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(survey)
library(foreign)
library(reshape2)
library(survminer)
library(data.table)

options(scipen = 999)  # disable scientific notation in R

#if a primary sapling unit has only a single observation, R will crash
# option adjust calculates conservative standard errors
# reference: http://faculty.washington.edu/tlumley/survey/example-lonely.html
options(survey.lonely.psu = "adjust")

#setwd("~/rstudio")

setwd("C:/Users/weny/Google Drive/2018/FGM/01 -Survival Analysis/03 -Data/DHS_mothers")
#setwd("C:/Users/Kathrin Weny/Google Drive (weny@unfpa.org)/2018/FGM/01 -Survival Analysis/03 -Data/DHS")

listdta <- dir(pattern = "*.DTA")

# Function definitions ----------------------------------------------------

CalculateTimetoEvent <- function(x,y,z){
  # calculates the time (years) that has passed until a certain event(FGM) occurs
  # and in the case, there was no event, uses proxy (age)
  #
  #Args:
  #   x = event indicator variable
  #   y = age at wich event occured
  #   z = substitute in case no event
  #
  #Returns:
  # new dataframe with time to event column
  
  time <- ifelse(x == 0, z, ifelse(x == 1, y, NA))
  
  time <- as.numeric(time)
  
  return(time)
  
}

ReadListofDTA <- function(x){
  # reads in all DTA files in the directory and stores them in a named list
  # uses a lot of memomry, in the case of the 11 DHS files 3 661 612 264 bytes, so better do not store in RAM
  # 
  # Args:
  # x = directory
  # Returns:
  #  ldf     = list of all data frames in the directory named by x
  
  ldf <- list() # creates a list
  
  for (k in 1 : length(listdta)){
    ldf[[k]] <- read.dta(listdta[k], convert.factors = FALSE)
  }
  
  names(ldf) <- x
  
  return(ldf)
  
}

ReadSingleDTA <- function(x){
  # reads in one DTA file from a given directory
  # 
  # Args:
  #  x = list index of file to be read into R
  #
  # Returns:
  #  single dataset
  
  data <- read.dta(listdta[x], convert.factors = FALSE)
  
  return(data)
  
}


randomRows <- function(x,y){
  # randomly selects y records from original filex
  # 
  # Args:
  #  x = dataframe
  #  y = number of lines to be selected
  #
  # Returns:
  #  dataframe with y rows
  #  reports error when more cases than available rows are selected
  
  if(y > nrow(x)) stop("too many cases select")
  
  return(x[sample(nrow(x),y),])
  
}


# Executed statements -----------------------------------------------------

# Create list to store SE

ConInList          <- list()

# Gambia ------------------------------------------------------------------

i <- 1 

df<- ReadSingleDTA(i) 

df <- df%>%
  dplyr::select(c("v001","v001","v002","v005","v012","v013","v021","v022","v023","v024","v025","v130","v131","v149","v190",
                  "g100","g102","g105","g106","g107","g118","g119")) 

unique(df$g106)
time <- ifelse(df$g102 == 0, df$v012,
               ifelse(df$g102==1,df$g106,NA))

time <- as.numeric(time)
unique(time)

df <- df%>%
  mutate(time = time)

df$time <- ifelse(df$time==98,NA,df$time)

df<- df%>%
  filter(v013 == 1)

dhs_design <- svydesign(id = df$v021, #V021 Primary Sampling Unit 1-645
                        strata=df$v022, #V022 Sample strata for standard errors; same as V023 stratification used for sampling design
                        weights = df$v005/1000000, #weight expressed in 6 decimals
                        data=df)

# increase memory, is Windows specific
memory.limit(10000000) 

# garbage colleaction: clear up RAM
gc()

s1 <- svykm(Surv(time,g102>0)~1,design=dhs_design,se=T)

plot1 <-  plot(s1, main ="KM estimates for Gambia",
          xlab = "Study time", ylab = "Probability of not experiencing FGM") 
          axis(1, at=1:19, labels=1:19)
          legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
          col=c("black", "black"), lty=1:2)
          title(sub = "Data: DHS 2013", adj=1, line=4, font=3)

ses <- confint(s1, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), level = 0.95)
ses
ConInList[[i]] <- ses  


# Niger -------------------------------------------------------------------

i <- 2 

df<- ReadSingleDTA(i) 

df <- df%>%
  dplyr::select(c("v001","v001","v002","v005","v012","v013","v021","v022","v023","v024","v025","v130","v131","v149","v190",
                  "g100","g102","g105","g106","g107","g118","g119")) 
df<- df%>%
  filter(v013 == 1)
unique(df$g106)

time <- ifelse(df$g102 == 0, df$v012,
               ifelse(df$g102==1,df$g106,NA))

time <- as.numeric(time)
unique(time)

#EStimate distribution for women who said, they had been cut during "infancy" (n=10)
length(which(time==95))
total<-length(which(time<=5))

#%0
at0 <- length(which(time==0))/total
#%1
at1 <- length(which(time==1))/total
#%2
at2 <- length(which(time==2))/total
#%3
at3 <- length(which(time==3))/total
#%4
at4 <- length(which(time==4))/total
#%5
at5 <- length(which(time==5))/total
#Test
sum(at0,at1,at2,at3,at4,at5) #must be 1

x5 <- sample(0:5, size=length(which(time==95)), replace=TRUE, prob=c(at0,at1,at2,at3,at4,at5))
length(x5)

time <- ifelse(time==95,x5,time)

df <- df%>%
  mutate(time = time)

df$time <- ifelse(df$time==98,NA,df$time)

dhs_design <- svydesign(id = df$v021, #V021 Primary Sampling Unit 1-645
                        strata=df$v022, #V022 Sample strata for standard errors; same as V023 stratification used for sampling design
                        weights = df$v005/1000000, #weight expressed in 6 decimals
                        data=df)

# increase memory, is Windows specific
memory.limit(10000000) 

# garbage colleaction: clear up RAM
gc()

s2 <- svykm(Surv(time,g102>0)~1,design=dhs_design,se=T)
a <- c(15,16,17,18,19)
b <- c(0.9307951,0.9307951,0.9307951,0.9307951,0.9307951)
c <- c(0.9795378,0.9795378,0.9795378,0.9795378,0.9795378)
d <- c(0.95516645,0.95516645,0.95516645,0.95516645,0.95516645)

df <- data.frame(a,b)
df1 <- data.frame(a,c)
df2 <- data.frame(a,d)

plot2 <-  plot(s2, main ="KM estimates for Niger",
          xlab = "Study time", ylab = "Probability of not experiencing FGM",xlim = c(0,19)) 
          axis(1, at=1:19, labels=1:19)
          lines(df, lty=2)
          lines(df1, lty=2)
          lines(df2, lty=1)
          legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
          col=c("black", "black"), lty=1:2)
          title(sub = "Data: DHS 2012", adj=1, line=4, font=3)

ses <- confint(s2, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), level = 0.95)
ses
ConInList[[i]] <- ses  

# Yemen -------------------------------------------------------------------

i <- 3 

df<- ReadSingleDTA(i)  

df <- df%>%
  dplyr::select(c("v001","v001","v002","v005","v012","v013","v021","v022","v023","v024","v025","v130","v131","v149","v190",
                  "g100","g102","g106")) 

unique(df$g106)

df$g106 <- ifelse(df$g106==94,0,df$g106) #After first week, before first year = 94
df$g106 <- ifelse(df$g106==93,0,df$g106) #During first week = 93

time <- ifelse(df$g102 == 0, df$v012,
               ifelse(df$g102==1,df$g106,NA)) #g106=95 during infancy
unique(time)


##

time <- as.numeric(time)
unique(df$time)
unique(df$g102)
df <- df%>%
  mutate(time = time)

df$time <- ifelse(df$time==98,NA,df$time)

df<- df%>%
  filter(v013 == 1)

dhs_design <- svydesign(id = df$v021, #V021 Primary Sampling Unit 1-645
                        strata=df$v022, #V022 Sample strata for standard errors; same as V023 stratification used for sampling design
                        weights = df$v005/1000000, #weight expressed in 6 decimals
                        data=df)

# increase memory, is Windows specific
memory.limit(10000000) 

# garbage colleaction: clear up RAM
gc()

s3 <- svykm(Surv(time,g102>0)~1,design=dhs_design,se=T)

plot3 <-  plot(s3, main ="KM estimates for Yemen",
          xlab = "Study time", ylab = "Probability of not experiencing FGM") 
          axis(1, at=1:19, labels=1:19)
          legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
          col=c("black", "black"), lty=1:2)
          title(sub = "Data: DHS 2013", adj=1, line=4, font=3)

ses <- confint(s3, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), level = 0.95)
ses
ConInList[[i]] <- ses  

