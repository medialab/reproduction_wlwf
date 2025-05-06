# mFLICA code for lead-follow relation inference
# author: Carlo Santagiustina
# data: 6 May 2025
 # Should print 20 100 10
library(mFLICA)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
parser <- ArgumentParser()

parser$add_argument("topic_model", help="Choose a model type between lda and bertopic")
args <- parser$parse_args()

if (!(args$topic_model %in% c('bertopic', 'lda'))){
  stop("The model name is incorrect. Choose between lda and bertopic")
}

if (args$topic_model=='lda'){
    db <- read_csv("data_prod/var/lda/general_TS.csv", show_col_types = FALSE)
    pol_issues <- c(1:9, 11:70)
} else {
    db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE)
    pol_issues <- c(0:91)
}

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

db <- db %>% filter(topic %in% pol_issues) %>% select(-date)

# Parameters for random time series generation
num_groups <- length(variables)      # Number of time series
num_timepoints <- 268  # Number of time steps
num_topics <- length(pol_issues)   # Number of dimensions (features)

matrix_dtw <- array(NA, c(num_groups, num_timepoints, num_topics))

i <- 1

for(topic_num in pol_issues){
    matrix_dtw[,,i] <- t(as.matrix(db %>% filter (topic == topic_num) %>% select(-topic)))
    i <- i+1
}

if (args$estimate){
    # Check structure

    timeWindow <- 30
    lagWindow <- 6
    timeShift <- 1
    
    model_dtw=mFLICA(
    matrix_dtw,
    timeWindow=  timeWindow,
    lagWindow= lagWindow,
    timeShift= timeShift,
    sigma = 0.5,
    silentFlag = FALSE
    )
    saveRDS(model_dtw, file="data_prod/dtw/lda/model_dtw.RDS")
}


#Commandes intéressantes :
#plotMultipleTimeSeries(TS=model_dtw$dyNetOut$dyNetBinDensityVec) #Mesure de la coordination (de 0 à 1) 
#Ligne est influencé par colonne 
#model_dtw$dyNetOut$dyNetWeightedMat[9, 3,] %>% plot(type='l')
model_dtw <- readRDS("data_prod/dtw/lda/model_dtw.RDS")
mat_mean_dtw <- matrix(1, nrow=length(variables), ncol=length(variables))
for (var in 1:length(variables)){
    for (var2 in 1:length(variables)){
        if (var != var2){
            mat_mean_dtw[var,var2] <- model_dtw$dyNetOut$dyNetWeightedMat[var, var2,] %>% mean() 
        }
    }
}
mat_mean_dtw <- data.frame(mat_mean_dtw)
colnames(mat_mean_dtw) <- variables
rownames(mat_mean_dtw) <- variables
write.csv(mat_mean_dtw, file="data_prod/figures/lda/densitycorrelation.csv")

matrix_cormat_3D <- array(NA, c(length(variables), length(variables), length(pol_issues)))
j <- 1
#Réactivité le jour même 
for (topic_num in pol_issues){
    submat <- db %>% filter(topic == topic_num) %>% select(-topic)
    matrix_cormat_3D[,,j] <- cor(submat)
    j <- j+1
}

mean_cormat <- apply(matrix_cormat_3D, c(1, 2), mean, na.rm = TRUE)
mean_cormat  <- data.frame(mean_cormat)
colnames(mean_cormat) <- variables
rownames(mean_cormat) <- variables
write.csv(mean_cormat, file="data_prod/figures/lda/cormat_dtw_inst.csv")

