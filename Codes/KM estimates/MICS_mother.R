
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

setwd("C:/Users/weny/Google Drive/2018/FGM/01 -Survival Analysis/03 -Data/MICS_mothers")

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

# Iraq --------------------------------------------------------------------

df <- ReadSingleDTA(1) %>%
  mutate(id = 1)

df <- df%>%
  dplyr::select(c("HH1","HH2","LN","WB2","WAGE","wmweight","stratum",
                  "FG1","FG3","FG7")) 



df[,'FG3'] <- as.character(df[,'FG3'])#circumcised or not
df <- df %>%
  mutate(fgm_status = ifelse(as.character(df$FG3)== "No", 0,
                             ifelse(as.character(df$FG3) =="Yes", 1, NA)))


df<- df%>%
  mutate(agegroup = ifelse(as.character(df$WAGE) == "15-19", 1,
                          ifelse(as.character(df$WAGE) == "20-24", 2,
                                 ifelse(as.character(df$WAGE) == "25-29", 3,
                                        ifelse(as.character(df$WAGE) == "30-34", 4,
                                               ifelse(as.character(df$WAGE) == "35-39", 5,
                                                      ifelse(as.character(df$WAGE) == "40-44", 6,
                                                             ifelse(as.character(df$WAGE)=="45-49", 7, NA))))))))

df <- df %>%
  filter(agegroup == 1)


df<- df %>%
  mutate(fgm_age =as.numeric(ifelse(FG7=="During infancy", 95, as.character(FG7))))

# Estimate distribution for women who said, they had been cut during "infancy" (n = 10)

total<-length(which(df$fgm_age<=5))
# %0
at0 <- length(which(df$fgm_age==0))/total
# %1
at1 <- length(which(df$fgm_age==1))/total
# %2
at2 <- length(which(df$fgm_age==2))/total
# %3
at3 <- length(which(df$fgm_age==3))/total
# %4
at4 <- length(which(df$fgm_age==4))/total
# %5
at5 <- length(which(df$fgm_age==5))/total

#T est
sum(at0, at1, at2, at3, at4, at5) # must be 1

x5 <- sample(0:5, size=length(which(df$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

df$fgm_age <- ifelse(df$fgm_age == 95, x5, df$fgm_age)

time <- ifelse(df$fgm_status == 0, df$WB2,
               ifelse(df$fgm_status == 1, df$fgm_age, NA))

time <- as.numeric(time)

df <- df%>%
  mutate(time = time)

df$time <- ifelse(df$time == 98, NA, df$time)

dhs_design <- svydesign(id = ~HH1 + HH2, #V021 Primary Sampling Unit 1-645
                        weights = df$wmweight, #weight expressed in 6 decimals
                        data = df)

s1 <- svykm(Surv(time,fgm_status > 0) ~ 1, design = dhs_design, se = T)

# Plot 

a <- c(0, 1)
b <- c(1, 1)

ab <- data.frame(a,b)

plot(s1, main = "KM estimates for Iraq",
     xlab = "Study time", ylab = "Probability of not experiencing FGM", xlim = c(0,15))
axis(1, at = 1:15, labels = 1:15)
lines(ab)
legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
       col=c("black", "black"), lty = 1:2)
title(sub = "Data: MICS 2011", adj = 1, line = 4, font = 3)

# Standard errors

ses <- confint(s1, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), level = 0.95)
ConInList[[1]] <- ses  
ses

