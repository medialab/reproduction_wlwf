suppressPackageStartupMessages({
library(tidyverse)
library(topicmodels)
library(reshape)
library(ggplot2)
library(gtable)
library(gridExtra)
library(scales)
library(ggdendro)
library(ggthemes)
library(slam)
library(Matrix)
library(tm)
})

load("data_prod/topics/lda_results-twokenizer.Rdata")

K <- 100
num.words <- 3
normalized.topics <- exp(lda.fit@beta) / rowSums(exp(lda.fit@beta)) 

scores <- apply(normalized.topics, 2, function(x)
    x * ( log(x + 1e-05) - sum(log(x + 1e-05))/length(x)) ) 

colnames(scores) <- lda.fit@terms

words <- apply(scores, 1, function(x)
        colnames(scores)[order(x, decreasing = TRUE)[1:num.words]])

key_words <- as.vector(words)
writeLines(key_words, "data_prod/key_words.txt")

#Run filter.sh before continue

sum_change <- function(path_input, path_output){
    fichiers <- list.files(path_input)
    fichiers2 <- list.files(path_output)
    change <- rep(NA, length(fichiers))
    i <- 0
    for (file in fichiers){
        i <- i + 1
        file_path <- paste0(path_input, "/", file)
        file_path2 <- paste0(path_output, "/", file)
        input_df <- read_csv(file_path, show_col_types=FALSE)
        output_df <- read_csv(file_path2, show_col_types=FALSE)
        change[i] <- (nrow(input_df)-nrow(output_df))/nrow(input_df)
    }
    return(summary(change))
}

print("Attentive")
print(sum_change("/store/medialex/reproduction_wlwf/data_source/attentive", "/home/fynch.meynent/storage/data_source/attentive"))
print("General")
print(sum_change("/store/medialex/reproduction_wlwf/data_source/general", "/home/fynch.meynent/storage/data_source/general"))