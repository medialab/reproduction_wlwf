library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rio)

print("Upload data")

db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE) 
pol_issues <- c(0, 38, 61)

#pol_issues <- c(0, 1, 2, 4, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 22, 27, 28, 29, 30, 31, 32, 33, 34, 36, 
#37, 38, 39, 40, 42, 43, 44, 45, 46, 47, 48, 49, 52, 53, 54, 55, 58, 59, 62, 63, 64, 66, 67, 68, 70, 71, 
#72, 73, 74, 75, 77, 80, 81, 82, 83, 84, 85, 86, 89, 90, 91) #14, 23, 33, 37, 41, 45, 51, 56, 61, 62, 63, 64, 79, 88

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

db <- db %>%
    filter(topic %in% pol_issues)

db <- db %>%
    mutate(date_index = as.integer(difftime(date, min(date), units = "days")) + 1)

print("Create outlier matrix")

outliers_mat <- data.frame(matrix(NA, nrow=4000, ncol=length(variables) +1))
colnames(outliers_mat) <- c("topic", variables)
outliers_count <- data.frame(matrix(0, nrow=length(unique(db$topic)), ncol=length(variables)))
rownames(outliers_count) <- unique(db$topic)
colnames(outliers_count) <- variables

print(length(unique(db$topic)))
print(unique(db$topic))

#Détecter Outlier
line <- 0
print("Start loop detection")
for (topic in unique(db$topic)){
    print(topic)
    outlier_counter <- 0
    db_topic <- db %>%
                filter(topic == topic)
    print(dim(db_topic))
    for (v in variables){
        line_topic <- 0
        data <- db_topic[[v]]
        QMAX <- quantile(data, 0.75)
        outlier_idx <- which(data > QMAX)
        date_indexes <- db_topic$date_index[outlier_idx]
        if(!(length(outlier_idx) ==0)){
            outliers_count[topic, v] <- length(outlier_idx)
            for (day in date_indexes){
                line_topic <- line_topic +1
                if (line_topic > outlier_counter){
                    outlier_counter <- line_topic
                }
                outliers_mat[line + line_topic, v] <- day
                outliers_mat[line + line_topic, "topic"] <- topic
            }
        }
        line <- line + outlier_counter
    }
}

outliers_mat <- outliers_mat[!rowSums(is.na(outliers_mat)) == ncol(outliers_mat), ]
outliers_mat[is.na(outliers_mat)] <- 0

print(head(outliers_mat, 10))
print(outliers_count)