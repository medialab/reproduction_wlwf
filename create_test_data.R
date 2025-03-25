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

path1 <- list.files("data_source/attentive")
path2 <- list.files("data_source/general")
all_files <- c(path1, path2)
i <-0

for (file in all_files){
    i <- i+1
    if (i<269){
        file_path <- paste0("data_source/attentive/", file)
    } else {
        file_path <- paste0("data_source/general/", file)
    }
    df <- read_csv(file_path, show_col_types=FALSE)
    file.remove(file_path)
    write.csv(df, file=file_path, row.names=FALSE, quote=TRUE)
}

stop()

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

#Créer matrices vides pour public random et général et remplir selon la présence de mot dans une certaine colonne
path_attentive <- "/store/medialex/reproduction_wlwf/data_source/attentive/"
path_general <-  "/store/medialex/reproduction_wlwf/data_source/general/"

filter_folder <- function(path_folder){
    fichiers <- list.files(path_folder)
    count_total_rows <- 0
    count_resting_rows <- 0
    for (file in fichiers){
        file_path <- paste0(path_folder, file)
        df_full <- read_csv(file_path, show_col_types=FALSE)
        count_total_rows <- count_total_rows + nrow(df_full)
        df_red <- df_full %>% 
                filter(str_detect(text, paste(key_words, collapse = "|")))
        count_resting_rows <- count_resting_rows + nrow(df_red)
        export_folder <- paste0("data_source/",basename(path_folder),"/", file)
        write.csv(df_red, file=export_folder, row.names=FALSE, quote=TRUE)
    }
    return(c(count_total_rows, count_resting_rows))
}

print("Attentive")
att_res <- filter_folder(path_attentive)
print(paste("Total rows before filter", att_res[1], "Total rows after filter", att_res[2]))

print("General")
att_res <- filter_folder(path_general)
print(paste("Total rows before filter", att_res[1], "Total rows after filter", att_res[2]))