
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

setwd("C:/Users/weny/Google Drive/2018/FGM/01 -Survival Analysis/03 -Data/MICS/fgm")

listdta_fg <- dir(pattern = "*.sav") 

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
  
  ldf_fg <- list() # creates a list
  
  for (k in 1 : length(listdta_fg)){
    ldf_fg[[k]] <- read.spss(listdta_fg[k], to.data.frame = TRUE)
  }
  
  names(ldf_fg) <- x
  
  return(ldf_fg)
  
}


ReadSingleDTA <- function(x){
  # reads in one DTA file from a given directory
  # 
  # Args:
  #  x = list index of file to be read into R
  #
  # Returns:
  #  single dataset
  
  data <- read.spss(listdta_fg[x], to.data.frame = TRUE)
  
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


# Lists -------------------------------------------------------------------

ConInList          <- list()

# Executed statements -----------------------------------------------------

# Benin -------------------------------------------------------------------

df <- ReadSingleDTA(1) %>%
  mutate(id = 1)

# Recode variables of interest 

df$FG13 <- as.numeric(as.character(df$FG13))#Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16))#Age of daughter at circumcisison

df[,'FG15'] <- as.character(df[,'FG15'])#Daughter circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15)== "Non", 0,
                             ifelse(as.character(df$FG15) =="Oui",1,NA)))


time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status==1,df$FG16,NA))
time <- as.integer(time)

unique(df$fgm_status)
unique(df$time)

df <- df%>%
  mutate(time=time)

# Bring in complex survey format

dhs_design <- svydesign(id = ~HH1 + ~HH2, #primary and secondary sampling units, reference: https://rpubs.com/trjohns/survey-cluster
                        weights = ~wmweight, #weight expressed in 6 decimals
                        data = df)

# Without standard errors (we can use full data set)
s1 <- svykm(Surv(time, fgm_status>0) ~1, design=dhs_design, se = T)

# Plot

a <- c(14,15)
b <- c(0.9854752 , 0.9854752)
c <- c(0.99248095, 0.99248095)
d <- c(0.9994867 , 0.9994867)

ab <- data.frame(a,b)
ac <- data.frame(a,c)
ad <- data.frame(a,d)

plot(s1, main = "KM estimates for Benin",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15),
     ylim = c(0.96,1)) 
axis(1, at = 1:15, labels = 1:15)
lines(ab, lty = 2)
lines(ac)
lines(ad, lty = 2)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2014", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s1, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[1]] <- ses  
ses


# Central African Republic ------------------------------------------------

df <- ReadSingleDTA(2) %>%
  mutate(id = 1)

# Recode variables of interest 

df$FG13 <- as.numeric(as.character(df$FG13))#Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16))#Age of daughter at circumcisison

df[,'FG15'] <- as.character(df[,'FG15'])#Daughter circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15)== "Non", 0,
                             ifelse(as.character(df$FG15) =="Oui",1,NA)))

# Create time to event or time to censoring variable

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status == 1, df$FG16, NA))

time <- as.integer(time)

df <- df%>%
  mutate(time=time)

# Bring in complex survey format

dhs_design <- svydesign(id = ~HH1 + ~HH2, #primary and secondary sampling units, reference: https://rpubs.com/trjohns/survey-cluster
                        weights = ~WMWEIGHT, #weight expressed in 6 decimals
                        data = df)

# KM estimates
s2 <- svykm(Surv(time, fgm_status>0) ~1, design=dhs_design, se = T)

# Plot

a <- c(0, 1, 2, 3)
b <- c(1, 1, 1, 1)

ab <- data.frame(a,b)

c <- c(14,15)
d <- c(0.9055838 , 0.9055838)
e <- c(0.92524715, 0.92524715)
f <- c(0.9449105 , 0.9449105)

cd <- data.frame(c,d)
ce <- data.frame(c,e)
cf <- data.frame(c,f)


plot(s2, main = "KM estimates for Central African Republic",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15))
axis(1, at = 1:15, labels = 1:15)
lines(ab)
lines(cd, lty=2)
lines(ce)
lines(cf, lty=2)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2010", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s2, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[2]] <- ses  
ses


# Ghana -------------------------------------------------------------------

df <- ReadSingleDTA(3) %>%
  mutate(id = 1)

# Recode variables of interest 

df$FG13 <- as.numeric(as.character(df$FG13))#Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16))#Age of daughter at circumcisison

unique(df$FG15)
df[,'FG15'] <- as.character(df[,'FG15'])#Daughter circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15)== "No", 0,
                             ifelse(as.character(df$FG15) =="Yes",1,NA)))

# Create time to event or time to censoring variable

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status==1,df$FG16,"na"))
time <- as.integer(time)

df <- df%>%
  mutate(time = time)

#Bring in complex survey format

dhs_design <- svydesign(id = ~HH1 + HH2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data = df)

# KM estimates
s3 <- svykm(Surv(time, fgm_status>0) ~1, design=dhs_design, se = T)

# plot 
a <- c(12, 13, 14, 15)
b <- c(0.9904473 , 0.9904473 , 0.9904473, 0.9904473)
c <- c(0.99309185, 0.99309185, 0.99309185, 0.99309185)
d <- c(0.9957364 , 0.9957364 , 0.9957364, 0.9957364)

ab <- data.frame(a,b)
ac <- data.frame(a,c)
ad <- data.frame(a,d)

plot(s3, main = "KM estimates for Ghana",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15))
axis(1, at = 1:15, labels = 1:15)
lines(ab, lty = 2)
lines(ac)
lines(ad, lty = 2)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2011", adj = 1, line = 4, font = 3)

# Zoom

plot(s3, main = "KM estimates for Ghana",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15),
     ylim = c(0.96,1))
axis(1, at = 1:15, labels = 1:15)
lines(ab, lty = 2)
lines(ac)
lines(ad, lty = 2)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2011", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s3, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[3]] <- ses  
ses


# Guinea-Bissau -----------------------------------------------------------

listdta_fg
df <- ReadSingleDTA(4) %>%
  mutate(id = 1)

# Recode variables of interest 

df$FG13 <- as.numeric(as.character(df$FG13)) # Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16)) # Age of daughter at circumcisison

df[,'FG15'] <- as.character(df[,'FG15']) # Daughter circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15) == "Não", 0,
                             ifelse(as.character(df$FG15) == "Sim", 1, NA)))

# Create time to event or time to censoring variable

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status == 1, df$FG16, NA))

time <- as.integer(time)

unique(df$fgm_status)
unique(df$time)

df <- df %>%
  mutate(time=time)


# Bring in complex survey format
dhs_design <- svydesign(id=~HH1+HH2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data=df)

# Without standard errors (we can use full data set)
s4 <- svykm(Surv(time, fgm_status > 0) ~1, design=dhs_design, se=T)

# plot

a <- c(14,15)
b <- c(0.5637450, 0.5637450)
c <- c(0.6126128, 0.6126128)
d <- c(0.6614806, 0.6614806)

ab <- data.frame(a, b)
ac <- data.frame(a, c)
ad <- data.frame(a, d)

plot(s4, main = "KM estimates for Guinea-Bissau",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15))
axis(1, at = 1:15, labels = 1:15)
lines(ab, lty = 2)
lines(ac)
lines(ad, lty = 2)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2014", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s4, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[4]] <- ses  
ses


# Mali --------------------------------------------------------------------

df <- ReadSingleDTA(5) %>%
  mutate(id = 1)

# Recode variables of interest 
df$FG13 <- as.numeric(as.character(df$FG13))#Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16))#Age of daughter at circumcisison

df[, 'FG15'] <- as.character(df[, 'FG15'])#Daughter circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15) == "Non", 0,
                             ifelse(as.character(df$FG15) == "Oui", 1, NA)))

# Create time to event or time to censoring variable

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status==1, df$FG16, NA))
time <- as.integer(time)

df <- df%>%
  mutate(time = time)

#Bring in complex survey format
dhs_design <- svydesign(id = ~WM1 + WM2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data = df)

#Without standard errors (we can use full data set)
s5 <- svykm(Surv(time, fgm_status > 0) ~1, design=dhs_design, se = T)

# Plot 
plot(s5, main = "KM estimates for Mali",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15))
axis(1, at = 1:15, labels = 1:15)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2015", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s5, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[5]] <- ses  
ses


# Mauritania --------------------------------------------------------------

df <- ReadSingleDTA(6) %>%
  mutate(id = 1)

# Recode variables of interest 
df$FG13 <- as.numeric(as.character(df$FG13)) # Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16)) # Age of daughter at circumcisison

df[, 'FG15'] <- as.character(df[, 'FG15']) # Daughter circumcised or not

df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15)== "Non", 0,
                             ifelse(as.character(df$FG15) =="Oui", 1, NA)))

# Create time to event or time to censoring variable

time_1 <- ifelse(df$FG16U == "MOIS", 1/12,
                 ifelse(df$FG16U == "JOUR", 1/365,
                        ifelse(df$FG16U == "ANNEE", 1,
                               NA)))
unique(time_1)
df <- df%>%
  mutate (time_2 =time_1*df$FG16)

unique(df$time_2)

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status == 1, df$time_2, NA))

time <- as.integer(time)

df <- df%>%
  mutate(time = time)


#Bring in complex survey format
dhs_design <- svydesign(id=~HH1+HH2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data=df)

#Without standard errors (we can use full data set)
s6 <- svykm(Surv(time, fgm_status > 0) ~1, design = dhs_design, se = T)

# Plot 
plot(s6, main = "KM estimates for Mauritania",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15))
axis(1, at = 1:15, labels = 1:15)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2015", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s6, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[6]] <- ses  
ses


# Nigeria -----------------------------------------------------------------

# listdta_fg
df <- ReadSingleDTA(7) %>%
  mutate(id = 1)

# weight variable was missing in fgm recode, so we add it from general dataset
# for_weight <- read.spss("wm_nigeria.sav",to.data.frame=TRUE)
for_weight <- ReadSingleDTA(12)

# create woman ID
library(stringr)

for_weight_id <- for_weight%>%
  mutate(id_woman = paste(str_pad(HH1, 4, pad = "0"), str_pad(HH2, 2, pad = "0"), str_pad(LN, 2, pad = "0"), sep = ""))%>%
  dplyr::select(id_woman, wmweight)

df <- df%>%
  mutate(id_woman = paste(str_pad(HH1, 4, pad = "0"), str_pad(HH2, 2, pad = "0"), str_pad(LN, 2, pad = "0"), sep = ""))

# add wmweight to df
df_test <- merge(df,for_weight_id, key=id_woman)
df_test_1 <- inner_join(df,for_weight_id, by="id_woman")
df_test_2 <- left_join(df,for_weight_id, by="id_woman")

####
df <- df_test
####

df$FG13 <- as.numeric(as.character(df$FG13)) # Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16)) # Age of daughter at circumcisison

df[, 'FG15'] <- as.character(df[,'FG15']) # Daughter circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15)== "No", 0,
                             ifelse(as.character(df$FG15) =="Yes",1,NA)))

# Create time to event or time to censoring variable

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status == 1, df$FG16,NA))
time <- as.integer(time)

df <- df%>%
  mutate(time=time)


#Bring in complex survey format
dhs_design <- svydesign(id = ~HH1 + HH2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data = df)

#Without standard errors (we can use full data set)
s7 <- svykm(Surv(time,fgm_status > 0) ~ 1, design = dhs_design, se = T)

# Plot 
plot(s7, main = "KM estimates for Nigeria",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0, 15))
axis(1, at = 1:15, labels = 1:15)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2017", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s7, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[7]] <- ses  
ses


# Sierra Leone ------------------------------------------------------------

df <- ReadSingleDTA(8) %>%
  mutate(id = 1)

# Recode variables of interest 

df$FG13 <- as.numeric(as.character(df$FG13))#Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16))#Age of daughter at circumcisison

df[, 'FG15'] <- as.character(df[, 'FG15'])#Daughter circumcised or not

df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15) == "No", 0,
                             ifelse(as.character(df$FG15) =="Yes", 1, NA)))

# Create time to event or time to censoring variable

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status == 1, df$FG16, NA))
time <- as.integer(time)

df <- df%>%
  mutate(time = time)


#Bring in complex survey format
dhs_design <- svydesign(id = ~HH1 + HH2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data = df)


#Without standard errors (we can use full data set)
s8 <- svykm(Surv(time, fgm_status > 0) ~1, design = dhs_design, se = T)

# Plot 

a <- c(13, 14, 15)

b <- c(0.5736008, 0.5736008, 0.5736008)
c <- c(0.6071338, 0.6071338, 0.6071338)
d <- c(0.6406668, 0.6406668, 0.6406668)

ab <- data.frame(a,b)
ac <- data.frame(a,c)
ad <- data.frame(a,d)

plot(s8, main = "KM estimates for Sierra Leone",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15))
axis(1, at = 1:15, labels = 1:15)
lines(ab, lty = 2)
lines(ac)
lines(ad, lty = 2)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2010", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s8, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[8]] <- ses  
ses

# Somalia Northeast Zone --------------------------------------------------

df <- ReadSingleDTA(9) %>%
  mutate(id = 1)

# Recode variables of interest 

df$FG13 <- as.numeric(as.character(df$FG13))#Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16))#Age of daughter at circumcisison


df[,'FG15'] <- as.character(df[,'FG15'])#Daughter circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15) == "No", 0,
                             ifelse(as.character(df$FG15) =="Yes", 1, NA)))

# Create time to event or time to censoring variable

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status == 1, df$FG16, NA))
time <- as.integer(time)

df <- df%>%
  mutate(time = time)

#Bring in complex survey format
dhs_design <- svydesign(id = ~HH1 + HH2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data = df)


#Without standard errors (we can use full data set)
s9 <- svykm(Surv(time, fgm_status>0) ~1, design = dhs_design, se = T)

# Plot

a <- c(13, 14, 15)
b <- c(0.1763162, 0.1763162, 0.1763162)
c <- c(0.21231445, 0.21231445, 0.21231445)
d <- c(0.2483127, 0.2483127, 0.2483127)

ab <- data.frame(a,b)
ac <- data.frame(a,c)
ad <- data.frame(a,d)

plot(s9, main = "KM estimates for Somalia/Northeastern Zone",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15))
axis(1, at = 1:15, labels = 1:15)
lines(ab, lty = 2)
lines(ac)
lines(ad, lty = 2)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2011", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s9, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[9]] <- ses  
ses


# Somaliland --------------------------------------------------------------

df <- ReadSingleDTA(10) %>%
  mutate(id = 1)

# Recode variables of interest

df$FG13 <- as.numeric(as.character(df$FG13))#Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16))#Age of daughter at circumcisison

unique(df$FG15)
df[,'FG15'] <- as.character(df[,'FG15'])#Daughter circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15)== "No", 0,
                             ifelse(as.character(df$FG15) =="Yes",1,NA)))
unique(df$fgm_status)
time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status==1,df$FG16,"na"))
time <- as.integer(time)

df <- df%>%
  mutate(time=time)

#Bring in complex survey format
dhs_design <- svydesign(id=~HH1+HH2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data=df)

#Without standard errors (we can use full data set)
s10 <- svykm(Surv(time, fgm_status > 0) ~1, design = dhs_design, se = T)

# Plot

a <- c(13, 14, 15)
b <- c(0.2015863, 0.2015863, 0.2015863)
c <- c(0.2352128, 0.2352128, 0.2352128)
d <- c(0.2744485, 0.2744485, 0.2744485)

ab <- data.frame(a,b)
ac <- data.frame(a,c)
ad <- data.frame(a,d)

e <- c(0, 1)
f <- c(1, 1)

ef <- data.frame(e,f)

plot(s10, main = "KM estimates for Somaliland",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0, 15))
axis(1, at = 1:15, labels = 1:15)
lines(ab, lty = 2)
lines(ac)
lines(ad, lty =2)
lines(ef)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2010", adj = 1, line = 4, font = 3)

x <- data.frame(s10$surv, s10$time)
# Standard errors

ses <- confint(s10, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[10]] <- ses  
ses


# Sudan -------------------------------------------------------------------

df <- ReadSingleDTA(11) %>%
  mutate(id = 1)


df$FG13 <- as.numeric(as.character(df$FG13)) # Age of daughter

df$FG16 <- as.numeric(as.character(df$FG16)) # Age of daughter at circumcisison

df[, 'FG15'] <- as.character(df[, 'FG15']) # Daughter circumcised or not

df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG15) == "No", 0,
                             ifelse(as.character(df$FG15) == "Yes", 1, NA)))

time <- ifelse(df$fgm_status == 0, df$FG13,
               ifelse(df$fgm_status == 1, df$FG16, NA))

time <- as.integer(time)

df <- df%>%
  mutate(time = time)


#Bring in complex survey format
dhs_design <- svydesign(id=~HH1+HH2, #primary and secondary sampling units,
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data = df)


#Without standard errors (we can use full data set)
s11 <- svykm(Surv(time, fgm_status > 0) ~1, design=dhs_design, se = T)

# Plot
plot(s11, main = "KM estimates for Sudan",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0, 15))
axis(1, at = 1:15, labels = 1:15)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2014", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s11, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[11]] <- ses  
ses
