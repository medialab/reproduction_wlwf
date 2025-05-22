# mFLICA code for lead-follow relation inference
# author: Carlo Santagiustina
# data: 6 May 2025
 # Should print 20 100 10
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


parser$add_argument("--estimate_AT", action = "store_true",
help = "Estimate model for time series with all topics")

parser$add_argument("--estimate_UT", action = "store_true",
help = "Estimate model for time series with one at one time serie")

parser$add_argument("--tests", action = "store_true",
help = "Do some tests")

args <- parser$parse_args()


if (!(args$topic_model %in% c('bertopic', 'lda'))){
  stop("The model name is incorrect. Choose between lda and bertopic")
}

if (args$topic_model=='lda'){
    db <- read_csv("data_prod/var/lda/general_TS.csv", show_col_types = FALSE)    
    pol_issues <- c(19, 2, 30, 34, 61, 16, 48, 1, 3, 5, 9, 13, 15, 17, 21, 25, 27, 29, 33, 36, 42, 44, 45, 51, 52, 53, 56, 63, 64, 66, 55, 60, 65, 40, 50, 59, 70)
} else {
    db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE)
    throw_topic <- c(16, 44, 54, 61, 64, 73, 76, 91, 1, 2, 5, 25, 41, 45, 3, 21, 26, 35, 50, 51, 56, 57, 58, 60, 65, 69, 78, 80)
    pol_issues <- setdiff(c(0:91), throw_topic)
}

db <- db %>% filter(topic %in% pol_issues)
variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

cat("Choose saving paths according to topic model \n")
if (args$topic_model == 'lda'){
    path_densitycorr <- "data_prod/dtw/lda/densitycorrelation.png"
    path_inst_corr <- "data_prod/dtw/lda/cormat_dtw_inst.png"
    path_MultipleTimeSeries <- "data_prod/dtw/lda/coordination_evolution.png"
    path_starting_bivariate <- "data_prod/dtw/lda/bivariate_plot/"
    model_output <- "data_prod/dtw/lda/model_dtw.RDS"
    path_test1 <- "data_prod/dtw/lda/relations_lags_followscore.csv"
    path_mat_adj <- "data_prod/dtw/lda/adj_mat.png"
    path_MultipleTimeSeries_faction <- "data_prod/dtw/lda/factions_evol.png"
    init_unique <- "data_prod/dtw/lda/unique/"
} else{
    path_densitycorr <- "data_prod/dtw/bertopic/densitycorrelation.png"
    path_inst_corr <- "data_prod/dtw/bertopic/cormat_dtw_inst.png"
    path_MultipleTimeSeries <- "data_prod/dtw/bertopic/coordination_evolution.png"
    path_starting_bivariate <- "data_prod/dtw/bertopic/bivariate_plot/"
    model_output <- "data_prod/dtw/bertopic/model_dtw.RDS"
    path_test1 <- "data_prod/dtw/bertopic/relations_lags_followscore.csv"
    path_mat_adj <- "data_prod/dtw/bertopic/adj_mat.png"
    path_MultipleTimeSeries_faction <- "data_prod/dtw/bertopic/factions_evol.png"
    init_unique <- "data_prod/dtw/bertopic/unique/"
}

db_date <- db
db <- db %>% select(-date)
num_groups <- length(variables)      # Number of time series
num_timepoints <- 268  # Number of time steps
num_topics <- length(pol_issues)   # Number of dimensions (features)
timeWindow <- 15
lagWindow <- 4/15 
timeShift <- 1
if (args$estimate_AT){
    matrix_dtw <- array(NA, c(num_groups, num_timepoints, num_topics))

    i <- 1

    for(topic_num in pol_issues){
        matrix_dtw[,,i] <- t(as.matrix(db %>% filter (topic == topic_num) %>% select(-topic)))
        i <- i+1
    }

    if(args$tests){
        matrix_test <- array(NA, c(55,3))
        k <- 1
        for (i in 1:(length(variables)-1)){
            leader <- variables[i]
            ts_leader <- matrix_dtw[i,1:num_timepoints,]
            for (j in (i+1):length(variables)){
                follower <- variables[j]
                ts_follower <- matrix_dtw[j,1:num_timepoints,]
                obj <- dtw(x=ts_follower, y=ts_leader, K=TRUE)
                lags <- abs(mean(obj$index1 - obj$index2))
                score <- mean(sign(obj$index1 - obj$index2))
                matrix_test[k,] <- c(paste(leader, follower), lags, score)
                k <- k+1
            }     
        }
    data_test <- data.frame(matrix_test)
    colnames(data_test) <- c("Couple", "Mean Lags", "Following Score")
    write.csv(data_test, path_test1, row.names = FALSE)
    print(data_test)
    #Creation de matrices adjacentes 
    mat1 <-followingNetwork(TS=matrix_dtw[, 1:num_timepoints,], sigma =0.5)$adjWeightedMat
    rownames(mat1) <- variables
    colnames(mat1) <- variables
    mat1_long <- melt(as.matrix(mat1), varnames = c("Var1", "Var2"), value.name = "Poids")
    png(filename = path_mat_adj, width = 1000, height = 900)
    p <- ggplot(mat1_long, aes(x = Var1, y = Var2, fill = Poids)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1), name = "Poids") +
        theme_minimal() +
        labs(title = "Matrice d'adjacence du réseau de followers") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5))
    print(p)
    dev.off()
    }
    
    model_dtw=mFLICA(
    matrix_dtw,
    timeWindow=  timeWindow,
    lagWindow= lagWindow,
    timeShift= timeShift,
    sigma = 0.5,
    silentFlag = FALSE
    )
    saveRDS(model_dtw, file=model_output)

} else{
    if (!file.exists(model_output)){
        stop("Please use --estimate option, the model currently doesn't exist")
    }
    model_dtw <- readRDS(model_output)
}
if(args$estimate_UT){ #You can't do univariate like this 
    matrix_dtw_top <- array(NA, c(num_groups, num_timepoints, 1))
    for(topic_num in pol_issues){
        mat <- t(as.matrix(db %>% filter (topic == topic_num) %>% select(-topic)))
        print(dim(mat))
        print(dim(matrix_dtw_top))
        matrix_dtw_top[,,1] <- t(as.matrix(db %>% filter (topic == topic_num) %>% select(-topic)))
        print(dim(matrix_dtw_top))
        model_output_unique <- paste0(init_unique, "model_topic", topic_num, ".RDS")
        model_dtw=mFLICA(
        matrix_dtw_top,
        timeWindow=  timeWindow,
        lagWindow= lagWindow,
        timeShift= timeShift,
        sigma = 0.5,
        silentFlag = FALSE
        )
    saveRDS(model_dtw, file=model_output_unique)
    }
}

top_topic_change <- function(group1, group2, seuil_delta){
    #Idée pour faire différemment : calculer les lags, couper les données en 7, prendre le max et le min d'écart de chaque sous groupe, bind les db_topics, et choisir le max et le min de chaque période pour chaque acteur. Ensuite, virer les maxs et les mins osef selon seuil delta
    lag_events1 <- data.frame(
        date = Date(),
        topic = character(),
        lag_value = numeric(),
        stringsAsFactors = FALSE
    )
    lag_events2 <- data.frame(
        date = Date(),
        topic = character(),
        lag_value = numeric(),
        stringsAsFactors = FALSE
    )
    for (topic_num in pol_issues){
        db_topic <- db_date %>% filter (topic == topic_num)
        db_topic$week_lags1 <- db_topic[[group1]] - dplyr::lag(db_topic[[group1]], 7)
        db_topic$week_lags2 <- db_topic[[group2]] - dplyr::lag(db_topic[[group2]], 7)

        week_data <- db_topic %>%
                mutate(weekgroup = (row_number() - 1) %/% 7 + 1) %>%
                select(date, topic, weekgroup, week_lags1, week_lags2) %>%
                group_by(weekgroup) %>%
                summarise(
                    date = first(date),  
                    topic = topic_num,
                    mean_lags1 = mean(week_lags1, na.rm = TRUE),
                    mean_lags2 = mean(week_lags2, na.rm = TRUE),
                    .groups = "drop"
                )
        
        to_add1 <- week_data %>%
                select(-mean_lags2) %>%
                filter(mean_lags1 == max(mean_lags1, na.rm=TRUE) | mean_lags1 == min(mean_lags1, na.rm=TRUE)) %>%
                filter(abs(mean_lags1) > seuil_delta) %>%
                transmute(topic = topic_num, date = as.Date(date), lag_value = mean_lags1)

        to_add2 <- week_data %>%
                select(-mean_lags1) %>%
                filter(mean_lags2 == max(mean_lags2, na.rm=TRUE) | mean_lags2 == min(mean_lags2, na.rm=TRUE)) %>%
                filter(abs(mean_lags2) > seuil_delta) %>%
                transmute(topic = topic_num, date = as.Date(date), lag_value = mean_lags2)
        
        lag_events1 <- rbind(lag_events1, to_add1)
        lag_events2 <- rbind(lag_events2, to_add2)
    }
    lag_events1 <- lag_events1 %>%
        group_by(date) %>%
        filter(lag_value == max(lag_value) | lag_value == min(lag_value)) %>%
        ungroup()

    lag_events2 <- lag_events2 %>%
        group_by(date) %>%
        filter(lag_value == max(lag_value) | lag_value == min(lag_value)) %>%
        ungroup()

    return(list(group1_lags = lag_events1, group2_lags = lag_events2))
}



biv_plot_TS <- function(data1, data2, leader, follower, title, path, seuil_delta=0.1){
    res <- top_topic_change(leader, follower, seuil_delta)
    topic_info_leader <- res$group1_lags
    topic_info_follower <- res$group2_lags
    TS_plot <- c()
    dates <- seq.Date(from = as.Date("2022-06-20"), to = as.Date("2023-03-14"), length.out = num_timepoints)
    for (k in 1:num_timepoints){
        score_ij <- data1[k] - data2[k]
        TS_plot <- c(TS_plot, score_ij)
    }
    df <- data.frame(date = dates, score = TS_plot)
    annots <- bind_rows(
        topic_info_leader %>% mutate(group = leader, topic = as.character(topic)),
        topic_info_follower %>% mutate(group = follower, topic = as.character(topic))
        ) %>%
        mutate(
        couleur = ifelse(lag_value > 0, "green", "purple"),
        y_pos = case_when(
            couleur == "green" & group == follower   ~ 1.25,
            couleur == "green" & group == leader ~ -1.15,
            couleur == "purple" & group == follower   ~ 1.15,
            couleur == "purple" & group == leader ~ -1.25
        ),
        label = topic)

    png(path,  width = 1000, height = 600)
    p <- ggplot(df, aes(x = date, y = score)) +
        geom_rect(aes(xmin = min(date), xmax = max(date), ymin = 0, ymax = 1), fill = "red", alpha = 0.3) +
        geom_rect(aes(xmin = min(date), xmax = max(date), ymin = -1, ymax = 0), fill = "blue", alpha = 0.3) +
        geom_line(color = "black", linewidth = 1) +
        geom_hline(yintercept = 0, color = "white") +
         annotate("text", x = max(df$date) - 20, y = 0.9,
             label = paste(follower, "lead", leader),
             color = "red4", fontface = "bold", size = 4) +
    annotate("text", x = max(df$date) - 20, y = -0.9,
             label = paste(leader, "lead", follower),
             color = "blue4", fontface = "bold", size = 4) +
        labs(x = "Date", y = "Lead/Follow relation Index", title = title) +
        annotate("text", x = min(df$date) - 14, y = 1.2,
            label = "Leader\nTop Δ topic",
            color = "black", fontface = "bold", size = 3, angle = 90, hjust = 0.5) +

        annotate("text", x = min(df$date) - 14, y = -1.2,
                label = "Follower\nTop Δ topic",
                color = "black", fontface = "bold", size = 3, angle = 90, hjust = 0.5) +
        scale_y_continuous(limits = c(-1.4, 1.4), breaks = seq(-1, 1, by = 0.5)) + 
        scale_x_date(limits = c(as.Date("2022-06-05"), as.Date("2023-03-14")),  breaks = seq(as.Date("2022-06-20"), as.Date("2023-03-14"), by = "3 weeks"), date_labels = "%b %d") +
        theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10)),
        axis.title.x = element_text(margin = margin(t = 20)),
        panel.ontop = FALSE,
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),   
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(50, 50, 50, 5)          
        )

    if(nrow(annots) > 0){
        p <- p + geom_text(data = annots, aes(x = date, y = y_pos, label = label, color = couleur),
            size = 4, fontface = "bold", show.legend = TRUE) +
            scale_colour_manual(
                name = paste0("Sens de variation \n  de Δ (|Δ| > ", seuil_delta, ")"), 
                values = c("green" = "green4", "purple" = "purple4"),
                labels = c("Δ > 0", "Δ < 0")
            ) +
        theme(legend.position = "right", legend.title = element_text(face = "bold"))
    }
    print(p)
    dev.off()
}
cat("Start plots \n")
png(filename = path_MultipleTimeSeries, width = 800, height = 600)
plotMultipleTimeSeries(TS = model_dtw$dyNetOut$dyNetBinDensityVec, strTitle = "Coordination Mesure evolution")
dev.off()
png(filename = path_MultipleTimeSeries_faction, width = 800, height = 600)
plotMultipleTimeSeries (TS=model_dtw$factionSizeRatioTimeSeries , strTitle ="Faction Size Ratios", TSnames=variables)
dev.off()

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
mat_mean_dtw <- round(mat_mean_dtw,3) 
mat_mean_dtw_long <- melt(as.matrix(mat_mean_dtw))
mat_mean_dtw_long$Var1 <- factor(mat_mean_dtw_long$Var1, levels = rev(rownames(mat_mean_dtw)))
mat_mean_dtw_long$Var2 <- factor(mat_mean_dtw_long$Var2, levels = colnames(mat_mean_dtw))

# Créer la heatmap
png(path_densitycorr,  width = 800, height = 800)
p <- ggplot(mat_mean_dtw_long, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1,1), space = "Lab",
    name="Densité") +
    theme_minimal()+
    labs(x = "", y = "", title = "Matrice moyenne des densités en décalage") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
        size = 12, hjust = 1))+
    coord_fixed() 

    ggfinal <- p + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position.inside = c(0.6, 0.7),
    legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                    title.position = "top", title.hjust = 0.5))
    print(ggfinal)
    dev.off()


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
mean_cormat <- round(mean_cormat,3) 
mat_mean_dtw_long <- melt(as.matrix(mean_cormat))
mat_mean_dtw_long$Var1 <- factor(mat_mean_dtw_long$Var1, levels = rev(rownames(mean_cormat)))
mat_mean_dtw_long$Var2 <- factor(mat_mean_dtw_long$Var2, levels = colnames(mean_cormat))

# Créer la heatmap
png(path_inst_corr,  width = 800, height = 800)
p <- ggplot(mat_mean_dtw_long, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1,1), space = "Lab",
    name="Corrélation") +
    theme_minimal()+
    labs(x = "", y = "", title = "Matrice de corrélation instatanée des séries temporelles") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
        size = 12, hjust = 1))+
    coord_fixed() 

    ggfinal <- p + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position.inside = c(0.6, 0.7),
    legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                    title.position = "top", title.hjust = 0.5))
    print(ggfinal)
    dev.off()

for (i in 1:(length(variables)-1)){
    leader <- variables[i]
    for (j in (i+1):length(variables)){
        follower <- variables[j]
        path_ij <- paste0(path_starting_bivariate, follower, "_", leader, ".png")
        biv_plot_TS(model_dtw$dyNetOut$dyNetWeightedMat[i, j,], model_dtw$dyNetOut$dyNetWeightedMat[j, i,], leader, follower, title = paste("Dynamiques d'influence entre", leader, "et", follower), path=path_ij) #Plot ligne i influencé par ligne j
        stop()
    }
}