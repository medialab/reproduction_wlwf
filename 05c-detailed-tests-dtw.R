 library(mFLICA)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(argparse)
library(reshape2)
library(dtw)
library(mFLICA)
parser <- ArgumentParser()

parser$add_argument("topic_model", help="Choose a model type between lda and bertopic")


parser$add_argument("--calculate", action = "store_true",
help = "Do tests calculus")

args <- parser$parse_args()
num_groups <- 11     # Number of time series
num_timepoints <- 268  # Number of time steps


if (!(args$topic_model %in% c('bertopic', 'lda'))){
  stop("The model name is incorrect. Choose between lda and bertopic")
}
cat("Choose saving paths according to topic model \n")
if (args$topic_model == 'lda'){
    init_path <- "data_prod/dtw/lda/tests/"
    path_test_density_sigma <- "data_prod/dtw/lda/test_sigma.png"
    path_test_density_TW <- "data_prod/dtw/lda/test_TW.png"
    path_test_density_LW <- "data_prod/dtw/lda/test_LW.png"
} else{
    init_path <- "data_prod/dtw/bertopic/tests/"
    path_test_density_sigma <- "data_prod/dtw/bertopic/test_sigma.png"
    path_test_density_TW <- "data_prod/dtw/bertopic/test_TW.png"
    path_test_density_LW <- "data_prod/dtw/bertopic/test_LW.png"
}
if(args$calculate){
    cat("Start calculus \n")
    if (args$topic_model=='lda'){
        db <- read_csv("data_prod/var/lda/general_TS.csv", show_col_types = FALSE)    
        pol_issues <- c(19, 2, 30, 34, 61, 16, 48, 1, 3, 5, 9, 13, 15, 17, 21, 25, 27, 29, 33, 36, 42, 44, 45, 51, 52, 53, 56, 63, 64, 66, 55, 60, 65, 40, 50, 59, 70)
        pol_issues_BQ <- c(12,8,20, 11, 4, 6, 18, 22, 31,35, 38, 39, 41, 47, 49, 54, 62, 67, 68, 14, 43, 69)
    } else {
        db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE)
        throw_topic <- c(16, 44, 54, 61, 64, 73, 76, 91, 1, 2, 5, 25, 41, 45, 3, 21, 26, 35, 50, 51, 56, 57, 58, 60, 65, 69, 78, 80)
        pol_issues <- setdiff(c(0:91), throw_topic)
        pol_issues_BQ <- c(16, 44, 54, 61, 64, 73, 76, 91, 1, 2, 25, 41, 45)
    }
    cat("Dimensions check \n")
    topic_all <- c(pol_issues, pol_issues_BQ)
    db_BQ <-  db %>% filter(topic %in% topic_all) 
    db <- db %>% filter(topic %in% pol_issues) 
    variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')
    num_topics <- length(pol_issues)   # Number of dimensions (features)

    cat("Number of lines we are waiting for \n")
    print(length(pol_issues)*268)
    cat("Current number of lines and columns \n")
    print(dim(db))
    cat("Plot attention histograms linked to filter \n")
    df_plot_loss <- data.frame(
        Variable = variables,
        Proportion_Attention_Politique = NA,
        Proportion_Attention_Politique_Valide = NA
        )

    lost_att_info_PV <- db %>% 
        select(-topic) %>%
        group_by(date) %>%
        summarise(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
    
    lost_att_infoP <- db_BQ %>% 
        select(-topic) %>%
        group_by(date) %>%
        summarise(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
    

    for (i in 1:length(variables)) {
        # Moyenne de la proportion (par date) pour les deux dataframes
        mean_prop_P <- mean(lost_att_infoP[[variables[i]]], na.rm = TRUE)
        mean_prop_PV <- mean(lost_att_info_PV[[variables[i]]], na.rm = TRUE)

        # Remplissage dans df_plot_loss
        df_plot_loss$Proportion_Attention_Politique[i] <- mean_prop_P
        df_plot_loss$Proportion_Attention_Politique_Valide[i] <- mean_prop_PV
    }

    df_long <- df_plot_loss %>%
        pivot_longer(cols = -Variable, names_to = "Type", values_to = "Proportion")

    # Création de l'histogramme
    png(filename=paste0("data_prod/figures/", args$topic_model, "/prop_att_filter.png"), width = 600, height = 600)
    p <- ggplot(df_long, aes(x = Variable, y = Proportion, fill = Type)) +
    geom_col(position = "dodge") +
    theme_minimal() +
    labs(title = paste0("Proportions moyennes d'attention par variable (",args$topic_model, ")") ,
        x = "Variable", y = "Proportion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
    dev.off()

    db <- db %>% select(-date)
    matrix_dtw <- array(NA, c(num_groups, num_timepoints, num_topics))

    i <- 1

    for(topic_num in pol_issues){
        matrix_dtw[,,i] <- t(as.matrix(db %>% filter (topic == topic_num) %>% select(-topic)))
        i <- i+1
    }
    timeWindow <- 30
    lagWindow <- 6/30
    timeShift <- 1

    cat("Start test for sigma \n")
    sigma_seuils <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    for (seuil in sigma_seuils){
        print(paste("seuil", seuil))
            model_output = paste0(init_path, "sigmatest_", sub("^[^.]*\\.", "", as.character(seuil)), ".RDS")
            model_dtw=mFLICA(
            matrix_dtw,
            timeWindow=  timeWindow,
            lagWindow= lagWindow,
            timeShift= timeShift,
            sigma = seuil,
            silentFlag = FALSE
            )
        saveRDS(model_dtw, file=model_output)
    }
    cat("Start test for time Window")
    TW_tests <- c(15,30,45,60)
    for (time_window in TW_tests){
        print(paste("TW", time_window))
        model_output = paste0(init_path, "windowtest_", time_window, ".RDS")
            model_dtw=mFLICA(
            matrix_dtw,
            timeWindow=  time_window,
            lagWindow= 6/30,
            timeShift= timeShift,
            sigma = 0.5,
            silentFlag = FALSE
            )
        saveRDS(model_dtw, file=model_output)
    }
    cat("Start test for lag Window")
    LW_tests <- c(2, 6, 10, 14, 18, 22, 26, 30)
    for (number_lag in LW_tests){
        print(paste("LW", number_lag))
        model_output = paste0(init_path, "lagtest_", number_lag, ".RDS")
            model_dtw=mFLICA(
            matrix_dtw,
            timeWindow=  timeWindow,
            lagWindow= number_lag/timeWindow,
            timeShift= timeShift,
            sigma = 0.5,
            silentFlag = FALSE
            )
        saveRDS(model_dtw, file=model_output)
    }
}

#Sigma Effects
if (args$topic_model=='lda'){
    model1 <- readRDS("data_prod/dtw/lda/tests/sigmatest_1.RDS")
    model2 <- readRDS("data_prod/dtw/lda/tests/sigmatest_3.RDS")
    model3 <- readRDS("data_prod/dtw/lda/tests/sigmatest_5.RDS")
    model4 <- readRDS("data_prod/dtw/lda/tests/sigmatest_7.RDS")
    model5 <- readRDS("data_prod/dtw/lda/tests/sigmatest_9.RDS")
    model6 <- readRDS("data_prod/dtw/lda/tests/lagtest_2.RDS")
    model7 <- readRDS("data_prod/dtw/lda/tests/lagtest_6.RDS")
    model8 <- readRDS("data_prod/dtw/lda/tests/lagtest_10.RDS")
    model9 <- readRDS("data_prod/dtw/lda/tests/lagtest_14.RDS")
    model10 <- readRDS("data_prod/dtw/lda/tests/lagtest_18.RDS")
    model15 <- readRDS("data_prod/dtw/lda/tests/lagtest_22.RDS")
    model16 <- readRDS("data_prod/dtw/lda/tests/lagtest_26.RDS")
    model17 <- readRDS("data_prod/dtw/lda/tests/lagtest_30.RDS")
    model11 <- readRDS("data_prod/dtw/lda/tests/windowtest_15.RDS")
    model12 <- readRDS("data_prod/dtw/lda/tests/windowtest_30.RDS")
    model13 <- readRDS("data_prod/dtw/lda/tests/windowtest_45.RDS")
    model14 <- readRDS("data_prod/dtw/lda/tests/windowtest_60.RDS")
} else {
    model1 <- readRDS("data_prod/dtw/bertopic/tests/sigmatest_1.RDS")
    model2 <- readRDS("data_prod/dtw/bertopic/tests/sigmatest_3.RDS")
    model3 <- readRDS("data_prod/dtw/bertopic/tests/sigmatest_5.RDS")
    model4 <- readRDS("data_prod/dtw/bertopic/tests/sigmatest_7.RDS")
    model5 <- readRDS("data_prod/dtw/bertopic/tests/sigmatest_9.RDS")
    model6 <- readRDS("data_prod/dtw/bertopic/tests/lagtest_2.RDS")
    model7 <- readRDS("data_prod/dtw/bertopic/tests/lagtest_6.RDS")
    model8 <- readRDS("data_prod/dtw/bertopic/tests/lagtest_10.RDS")
    model9 <- readRDS("data_prod/dtw/bertopic/tests/lagtest_14.RDS")
    model10 <- readRDS("data_prod/dtw/bertopic/tests/lagtest_18.RDS")
    model15 <- readRDS("data_prod/dtw/bertopic/tests/lagtest_22.RDS")
    model16 <- readRDS("data_prod/dtw/bertopic/tests/lagtest_26.RDS")
    model17 <- readRDS("data_prod/dtw/bertopic/tests/lagtest_30.RDS")
    model11 <- readRDS("data_prod/dtw/bertopic/tests/windowtest_15.RDS")
    model12 <- readRDS("data_prod/dtw/bertopic/tests/windowtest_30.RDS")
    model13 <- readRDS("data_prod/dtw/bertopic/tests/windowtest_45.RDS")
    model14 <- readRDS("data_prod/dtw/bertopic/tests/windowtest_60.RDS")
}

param1 <- model1$dyNetOut$dyNetBinDensityVec
param2 <- model2$dyNetOut$dyNetBinDensityVec
param3 <- model3$dyNetOut$dyNetBinDensityVec
param4 <- model4$dyNetOut$dyNetBinDensityVec
param5 <- model5$dyNetOut$dyNetBinDensityVec

dates <- seq.Date(from = as.Date("2022-06-20"), by = "day", length.out = num_timepoints)
df <- data.frame(
date = rep(dates, 5),
value = c(param1, param2, param3, param4, param5),
param = factor(rep(c("sigma=0.1", "sigma=0.3", "sigma=0.5", "sigma=0.7", "sigma=0.9"), each = num_timepoints))
)
png(filename = path_test_density_sigma, width = 800, height = 600)
p <- ggplot(df, aes(x = date, y = value, color = param)) +
geom_line(linewidth = 1) +
labs(title = "Coordination Measure Evolution according to Sigma",
    x = "Date", y = "Coordination Index", color = "Paramétrage") +
theme_minimal() +
theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
)
print(p)
dev.off()

param1 <- model6$dyNetOut$dyNetBinDensityVec
param2 <- model7$dyNetOut$dyNetBinDensityVec
param3 <- model8$dyNetOut$dyNetBinDensityVec
param4 <- model9$dyNetOut$dyNetBinDensityVec
param5 <- model10$dyNetOut$dyNetBinDensityVec
param6 <- model15$dyNetOut$dyNetBinDensityVec
param7 <- model16$dyNetOut$dyNetBinDensityVec
param8 <- model17$dyNetOut$dyNetBinDensityVec

df <- data.frame(
date = rep(dates, 8),
value = c(param1, param2, param3, param4, param5, param6, param7, param8),
param = factor(rep(c("lag=2/30 jours", "lag=6/30 jours", "lag=10/30 jours", "lag=14/30 jours", "lag=18/30 jours", "lag=22/30 jours", "lag=26/30 jours", "lag=30/30 jours"), each = num_timepoints))
)

png(filename = path_test_density_LW, width = 800, height = 600)
p <- ggplot(df, aes(x = date, y = value, color = param)) +
geom_line(linewidth = 1) +
labs(title = "Coordination Measure Evolution according to Lags",
    x = "Date", y = "Coordination Index", color = "Paramétrage") +
theme_minimal() +
theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
)
print(p)
dev.off()


param1 <- model11$dyNetOut$dyNetBinDensityVec
param2 <- model12$dyNetOut$dyNetBinDensityVec
param3 <- model13$dyNetOut$dyNetBinDensityVec
param4 <- model14$dyNetOut$dyNetBinDensityVec


df <- data.frame(
date = rep(dates, 4),
value = c(param1, param2, param3, param4),
param = factor(rep(c("TW=15 jours", "TW=30 jours", "TW=45 jours", "TW=60 jours"), each = num_timepoints))
)
png(filename = path_test_density_TW, width = 800, height = 600)
p <- ggplot(df, aes(x = date, y = value, color = param)) +
geom_line(linewidth = 1) +
labs(title = "Coordination Measure Evolution according to Time Window",
    x = "Date", y = "Coordination Index", color = "Paramétrage") +
theme_minimal() +
theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
)
print(p)
dev.off()