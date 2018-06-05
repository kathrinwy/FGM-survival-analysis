
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

setwd("C:/Users/weny/Google Drive/2018/FGM/01 -Survival Analysis/03 -Data/DHS")
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

# read in stata datafiles

# for pcs with a lot of memory, loop in all DHS stata files and select one later
# ldf <- ReadListofDTA() 

# create empty list to store results in loop
dfList             <- list()
SmallSurvivalList  <- list()
ConInList          <- list()
SmallSurvey_random <- list()
nRisk              <- list()
nEvent             <- list()
nSurv              <- list() 

#for (i in 1:length(listdta)){
for (i in c(4,6)){ #I am rewriting the loop in this way, as  I have been able to run all countries besides Guinea ( i <- 6) and Egypt (i <- 4)
  
# for pcs with a lot of memory select one DHS stat file from list ldf created above
# wide <- ldf[[i]]

# if limited memory only read in one DHS stat file at a time
wide <- ReadSingleDTA(i) 

# wide <- read.dta("burkina_faso.DTA", convert.factors = FALSE)

# select variables and create dataset for birth reshape and FGM reshape
wide_allchildren <- wide %>%
  dplyr::select(c("caseid","v001","v002","v005","v021","v022","v023","v024","v025",
                  "bidx_01","bidx_02","bidx_03","bidx_04","bidx_05","bidx_06","bidx_07","bidx_08","bidx_09","bidx_10","bidx_11","bidx_12","bidx_13","bidx_14","bidx_15","bidx_16","bidx_17","bidx_18","bidx_19","bidx_20",
                  "b4_01","b4_02","b4_03","b4_04","b4_05","b4_06","b4_07","b4_08","b4_09","b4_10","b4_11","b4_12","b4_13","b4_14","b4_15","b4_16","b4_17","b4_18","b4_19","b4_20",
                  "b8_01","b8_02","b8_03","b8_04","b8_05","b8_06","b8_07","b8_08","b8_09","b8_10","b8_11","b8_12","b8_13","b8_14","b8_15","b8_16","b8_17","b8_18","b8_19","b8_20"))

wide_fgm <- wide %>%
  dplyr::select(c("caseid","v001",
                  "gidx_01","gidx_02","gidx_03","gidx_04","gidx_05","gidx_06","gidx_07","gidx_08","gidx_09","gidx_10","gidx_11","gidx_12","gidx_13","gidx_14","gidx_15","gidx_16","gidx_17","gidx_18","gidx_19","gidx_20",
                  "g121_01","g121_02","g121_03","g121_04","g121_05","g121_06","g121_07","g121_08","g121_09","g121_10","g121_11","g121_12","g121_13","g121_14","g121_15","g121_16","g121_17","g121_18","g121_19","g121_20",
                  "g122_01","g122_02","g122_03","g122_04","g122_05","g122_06","g122_07","g122_08","g122_09","g122_10","g122_11","g122_12","g122_13","g122_14","g122_15","g122_16","g122_17","g122_18","g122_19","g122_20",
                  "g123_01","g123_02","g123_03","g123_04","g123_05","g123_06","g123_07","g123_08","g123_09","g123_10","g123_11","g123_12","g123_13","g123_14","g123_15","g123_16","g123_17","g123_18","g123_19","g123_20",
                  "g124_01","g124_02","g124_03","g124_04","g124_05","g124_06","g124_07","g124_08","g124_09","g124_10","g124_11","g124_12","g124_13","g124_14","g124_15","g124_16","g124_17","g124_18","g124_19","g124_20"))


# reshape FGM module
long_fgm <- reshape(wide_fgm, varying = c(3:102), direction = "long", idvar = "caseid", sep = "_", timevar = "order")

# delete order
long_fgm <- long_fgm %>%
  dplyr::select(-order)

# rename GIDX to order
colnames(long_fgm)[3] <- "order"

# create ID for merge
long_fgm <- long_fgm %>%
  mutate(id.match = paste(caseid, order, sep = ""))

# reshape ALL CHILDREN

long_allchildren <- reshape(wide_allchildren, varying = c(10:69), direction = "long", idvar = "caseid", sep = "_", timevar="order")

# create ID for merge
long_allchildren <- long_allchildren %>%
  mutate(id.match = paste(caseid, order, sep = ""))

# merge data sets

df <- merge(long_fgm, long_allchildren, by = "id.match")

colnames(df)[2] <- "caseid"

# add time to event column

time <- CalculateTimetoEvent(df$g121, df$g122, df$b8)

df <- df %>%
  mutate(time = time)

# add id column that is 1 for each recrod in order to count unweighted and weighted population sizes
df <- df %>%
  mutate(id = 1)

#NAs
df$time <- ifelse(df$time == 98 | df$time == 99, NA, df$time)

# create weight variable from v005
df$wgt <- as.numeric(df$v005 / 1000000)

# Reduce df to only absolutely necessary variables and records
df <- df %>%
  select(v021,v022,v023,g121,time,wgt)%>%
  filter(time <= 15)

# Store dataframe df in a list for later use
dfList[[i]]            <- df

# free RAM for KM estimates, remove previous dataframes and objects
rm(wide, wide_allchildren,long_allchildren, wide_fgm,long_fgm,time)

# garbage colleaction: clear up RAM
gc()


# Only enable if you have a super computer --------------------------------

# increase memory, is Windows specific, R server runs in Ubuntu
 memory.limit(10000000) 

# This part was used to test, what datasets could be run. cut off is at roughly 7000 lines

#dim(df)

#ifelse(i == 1 | i == 2 | i == 4 | # Some 3,9 and 10 are excluded, as complex survey design runs fine with Cote dIvoire, Tanzania and Togo
 #        i == 5 | i == 6 | i == 7 |
  #       i == 8, df <- randomRows(df,2500),df)

#dim(df)

# create a small sample survey to calculate standard errors with taylor-series linearization
SmallSurvey <- svydesign(id             = ~v021, 
                         strata         = df$v022, 
                         variables      = ~g121 + time,
                         weight         = df$wgt,
                         data           = df)
summary(SmallSurvey)

SmallSurvey$variables
# KM estimate
SmallSurvival <- svykm(Surv(time, g121 > 0) ~ 1, design = SmallSurvey, se = TRUE)
SmallSurvivalList[[i]] <- SmallSurvival  # store survival object in list

# visualization

plot(SmallSurvival, main ="KM estimates for Ethiopia",
  xlab = "Study time", ylab = "Probability of not experiencing FGM") 
  axis(1, at=1:15, labels=1:15)
  legend(0, 0.2, legend=c("KM estimate", "95% confidence interval"),
         col=c("black", "black"), lty=1:2)
  title(sub = "Data: DHS 2016", adj=1, line=4, font=3)
 
plot(sqrt(SmallSurvival[[3]]), main = "Standard Errors of Kaplan-Meier Estimates",
     xlab = "FGM Cases", ylab = "Standard Errors", cex = 0.5)


# Save Standard Errors
a <- as.data.table(sqrt(SmallSurvival$varlog))

write.table(a, "C:/Users/weny/Google Drive/2018/FGM/01 -Survival Analysis/05b -Uncertainty/ethiopia_se.csv")

# Store Confidence Intervals
 ses <- confint(SmallSurvival, parm = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), level = 0.95)
 ConInList[[i]] <- ses  
 ses
 
# From here, you can easly run the scrip again ----------------------------


# Standard error based on random sampling ---------------------------------

SmallSurvey_randomdesign    <- survfit(Surv(time, g121) ~ 1 , data=df,
                                    weight=wgt) # status 9 is converted to NA

ggsurvplot(SmallSurvey_randomdesign,
           conf.int=TRUE,
           ggtheme = theme_classic(),
           legend = "none",
           censor = FALSE)

tables <- surv_summary(SmallSurvey_randomdesign)

nRisk[[i]]                   <- tables[, 2]
nEvent[[i]]                  <- tables[, 3]
nSurv[[i]]                  <- tables[, 5]

SmallSurvey_random_conf      <- setNames(data.frame(matrix(ncol = 3, nrow = 16)), c("se","lower_random","upper_random"))
SmallSurvey_random_conf[, 2] <- as.data.frame(SmallSurvey_randomdesign$lower)
SmallSurvey_random_conf[, 3] <- as.data.frame(SmallSurvey_randomdesign$upper)
SmallSurvey_random_conf[, 1] <- as.data.frame(SmallSurvey_randomdesign$std.err)

SmallSurvey_random[[i]]     <- SmallSurvey_random_conf



}

# Results -----------------------------------------------------------------

names(SmallSurvey_random)<- listdta


SmallSurvey_random[1]