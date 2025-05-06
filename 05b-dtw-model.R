# mFLICA code for lead-follow relation inference
# author: Carlo Santagiustina
# data: 6 May 2025
 # Should print 20 100 10
library(mFLICA)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE)

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')
pol_issues <- c(0:91)

db <- db %>% filter(topic %in% pol_issues) %>% select(-date)

# Parameters for random time series generation
num_groups <- length(variables)      # Number of time series
num_timepoints <- 268  # Number of time steps
num_topics <- length(pol_issues)   # Number of dimensions (features)

matrix_dtw <- array(NA, c(num_groups, num_timepoints, num_topics))


for(topic_num in pol_issues){
    matrix_dtw[,,topic_num + 1] <- t(as.matrix(db %>% filter (topic == topic_num) %>% select(-topic)))
}

# Check structure

timeWindow <- 30
lagWindow <- 6
timeShift <- 1

mFLICA=mFLICA(
  matrix_dtw,
  timeWindow=  timeWindow,
  lagWindow= lagWindow,
  timeShift= timeShift,
  sigma = 0.5,
  silentFlag = FALSE
)
#Commandes intéressantes :
#plotMultipleTimeSeries(TS=model_dtw$dyNetOut$dyNetBinDensityVec) #Mesure de la coordination (de 0 à 1) 
saveRDS(mFLICA, file="data_prod/dtw/bertopic/model_dtw.RDS")

