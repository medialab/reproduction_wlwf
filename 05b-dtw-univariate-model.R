library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(argparse)
library(reshape2)
library(dtw)
library(mFLICA) #detach to remove monkey
parser <- ArgumentParser()

parser$add_argument("topic_model", help="Choose a model type between lda and bertopic")

parser$add_argument("--estimate", action = "store_true",
help = "Estimate model")

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
    throw_topic <- c(16, 44, 54, 61, 64, 73, 76, 91, 1, 2, 5, 25, 41, 45, 3, 21, 26, 35, 50, 51, 56, 57, 58, 60, 65, 69, 78, 80, 87)
    pol_issues <- setdiff(c(0:91), throw_topic)
}

db <- db %>% filter(topic %in% pol_issues)
variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

cat("Choose saving paths according to topic model \n")
if (args$topic_model == 'lda'){
    init_unique <- "data_prod/dtw/lda/unique/"
} else{
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

topic_follow = data.frame(
    topic = numeric(),
    leader = character(),
    follower=character(),
    value=numeric()
)

matrix_dtw <- array(NA, c(num_groups, num_timepoints, 1))
for(topic_num in pol_issues){
    matrix_dtw[,,1] <- t(as.matrix(db %>% filter (topic == topic_num) %>% select(-topic)))
    for(i in 1:(length(variables)-1)){
        for (j in (i+1):length(variables)){
            leader <- matrix_dtw[i,1:num_timepoints,]
            follower <- matrix_dtw[j,1:num_timepoints,]
            score <- mFLICA::followingRelation(Y=follower ,X=leader, lagWindow = lagWindow)$follVal
            
            if (score < 0) {
                new_row <- data.frame(
                topic = topic_num,
                leader = variables[j],
                follower = variables[i],
                value = abs(score),
                stringsAsFactors = FALSE
                )
            } else {
                new_row <- data.frame(
                topic = topic_num,
                leader = variables[i],
                follower = variables[j],
                value = score,
                stringsAsFactors = FALSE
                )
            }

            topic_follow <- rbind(topic_follow, new_row)
        }
    }
}

print(head(topic_follow))

stop()
if (args$estimate){
    getDynamicFollNet <- function(TS, timeWindow, timeShift, lagWindow, sigma = 0.5, silentFlag = FALSE){
        if (length(dim(TS)) == 2) {
            B <- array(0, c(dim(TS), 2))
            B[, , 1] <- TS
            TS <- B
        }
        invN <- dim(TS)[1]
        Tlength <- dim(TS)[2]
        dimensionsN <- dim(TS)[3]
        if(dimensionsN==1){
            B <- array(0, c(invN, Tlength, 2))
            B[, , 1] <- TS[,,1]
            TS <- B
            dimensionsN <- dim(TS)[3]
        }
        dyNetBinMat <- array(0, dim = c(invN, invN, Tlength))
        dyNetWeightedMat <- array(0, dim = c(invN, invN, Tlength))
        dyNetBinDensityVec <- array(0, dim = c(1, Tlength))
        dyNetWeightedDensityVec <- array(0, dim = c(1, Tlength))
        if (missing(timeWindow)) {
            timeWindow <- ceiling(0.1 * Tlength)
        }
        if (missing(timeShift)) {
            timeShift <- max(1, ceiling(0.1 * timeWindow))
        }
        for (t in seq(1, Tlength, by = timeShift)) {
            if (t + timeWindow >= Tlength) {
                dyNetBinMat[, , t:Tlength] <- follOut$adjBinMat
                dyNetWeightedMat[, , t:Tlength] <- follOut$adjWeightedMat
                dyNetBinDensityVec[t:Tlength] <- dval1
                dyNetWeightedDensityVec[t:Tlength] <- dval2
                break
            }
            else {
                currTWInterval <- t:(t + timeWindow - 1)
                currTS <- TS[, currTWInterval, ]
                follOut <- followingNetwork(TS = currTS, sigma = sigma, 
                    lagWindow = lagWindow)
                dyNetBinMat[, , currTWInterval] <- follOut$adjBinMat
                dyNetWeightedMat[, , currTWInterval] <- follOut$adjWeightedMat
                dval1 <- getADJNetDen(follOut$adjBinMat)
                dval2 <- getADJNetDen(follOut$adjWeightedMat)
                dyNetBinDensityVec[currTWInterval] <- dval1
                dyNetWeightedDensityVec[currTWInterval] <- dval2
                if (silentFlag == FALSE) 
                    print(sprintf("TW%d shift-%d - t%d", timeWindow, 
                    timeShift, t))
            }
        }
        return(list(dyNetBinMat = dyNetBinMat, dyNetWeightedMat = dyNetWeightedMat, 
            dyNetBinDensityVec = dyNetBinDensityVec, dyNetWeightedDensityVec = dyNetWeightedDensityVec))
    }

    assignInNamespace("getDynamicFollNet", getDynamicFollNet, ns = "mFLICA")

#You can't do univariate like this 
    matrix_dtw <- array(NA, c(num_groups, num_timepoints, 1))
    for(topic_num in pol_issues){
        print(paste("Model for topic", topic_num))
        matrix_dtw[,,1] <- t(as.matrix(db %>% filter (topic == topic_num) %>% select(-topic)))
        model_output_unique <- paste0(init_unique, "Model_topic", topic_num, ".RDS")
        model_dtw=mFLICA::mFLICA(
        matrix_dtw,
        timeWindow=  timeWindow,
        lagWindow= lagWindow,
        timeShift= timeShift,
        sigma = 0.5,
        silentFlag = FALSE
        )
    saveRDS(model_dtw, file=model_output_unique)
    }
}
stop()
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
            label = "Follower\nTop Δ topic",
            color = "black", fontface = "bold", size = 3, angle = 90, hjust = 0.5) +

        annotate("text", x = min(df$date) - 14, y = -1.2,
                label = "Leader\nTop Δ topic",
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

dates <- seq.Date(from = as.Date("2022-06-20"), to = as.Date("2023-03-14"), length.out = num_timepoints)
df_evol_leader <- data.frame(
    date = as.Date(character()),
    is_leader = numeric(),
    number_group = numeric()
)

all_nums <- 1:11

for (i in 1:num_timepoints){
    nums <- model_dtw$leadersTimeSeries[[i]]
    presence <- as.integer(all_nums %in% nums)
    add_df <- data.frame(
    date = rep(dates[i], length(all_nums)),
    is_leader = presence,
    number_group = all_nums
    )
    df_evol_leader <- rbind(df_evol_leader, add_df)
}

df_evol_leader <- df_evol_leader %>%
    mutate(group = variables[number_group]) %>%
    mutate(y_pos = is_leader + number_group*1/12)

colors_dict <- c(
  "lr" = "blue4",
  "lr_supp" = "cyan3",
  "majority" = "orange",
  "majority_supp" = "darkorange1",
  "nupes" = "chartreuse4",
  "nupes_supp" = "seagreen1",
  "rn" = "lightsalmon3",
  "rn_supp" = "brown4",
  "attentive" = "red",
  "general" = "darkgrey",
  "media" = "darkorchid3"
)


#Création d'un graphique changement leader/follower
png(path_evolv_leader, width = 800, height = 800)
p <- ggplot(df_evol_leader, aes(x = date, y = y_pos, group = group, color=group)) +
    geom_point() +        
    geom_hline(yintercept = 1, color = "black") +
    labs(x = "Date", y = "", title = "Évolution des positions de leader dans le temps") +
    scale_x_date(breaks = seq(as.Date("2022-06-20"), as.Date("2023-03-14"), by = "3 weeks"), date_labels = "%b %d") +
    scale_colour_manual(values = colors_dict) +
    scale_y_continuous(
        limits = c(0, 2),
        breaks = c(0.5, 1.5),
        labels = c("Follower", "Leader")
        ) + 
    theme_minimal()

    print(p)
    dev.off()

#Afficher les appartenance de factions de manière plus précise
#Créer une matrice d'appartenance
factions_ts <- model_dtw$factionMembersTimeSeries

print("Create evolution faction representation")
#Exclure les cas où un groupe appartient à plusieurs factions en le forçant à appartenir à celle où le follow est le plus fort
for (i in seq_along(factions_ts)){
    factions_i <-  factions_ts[[i]]
    list_index_leaders <- c()
    list_index_followers <- c()
    leader_indices <- c()
    for (j in seq_along(factions_i)){
        elem <- factions_i[[j]]
        if(length(elem)>1){ #Si le leader est seul dans le groupe, on a pas besoin de vérifier qu'un groupe de follower apparait dedans
            for (k in 2:length(elem)){
                list_index_leaders <- c(list_index_leaders, elem[1])
                list_index_followers <- c(list_index_followers, elem[k])
                leader_indices <- c(leader_indices, j)
            }
        }
    }
    if (any(duplicated(list_index_followers))){
        duplicated_elements <- as.integer(names(which(table(unlist(list_index_followers)) > 1)))
        for (dup in duplicated_elements){
            indexs_dup <- which(list_index_followers == dup)
            values_follow <- numeric(length(indexs_dup))
            for (l in seq_along(indexs_dup)) {
                val_follow <- model_dtw$dyNetOut$dyNetWeightedMat[dup, list_index_leaders[indexs_dup[l]], i]
                values_follow[l] <- val_follow
            }

            max_idx <- which.max(values_follow)
            indexs_dup <- indexs_dup[-max_idx]
            
            for (rmv_idx in indexs_dup){
                j_faction <- leader_indices[rmv_idx] 
                factions_i[[j_faction]] <- factions_i[[j_faction]][factions_i[[j_faction]] != dup]
            }
        }
    }
}

#Heatmap des factions
#Forcer chaque élément à être membre d'une seule faction
heatmap_matrix <- matrix(0, nrow = num_groups, ncol = num_timepoints)
leader_data <- matrix(0, nrow=num_groups, ncol=num_timepoints)

for (t in seq_len(num_timepoints)) {
    factions_t <- factions_ts[[t]]
    for (i in seq_along(factions_t)){
        faction <-factions_t[[i]]
        leader <- faction[1]
        leader_data[leader, t] <- 1
        for (member in faction){
            heatmap_matrix[member, t] <- leader
        }
    }
}

df_heatmap <- as.data.frame(heatmap_matrix)
df_heatmap$group <- 1:11
df_leader <- as.data.frame(leader_data)
df_leader$group <- 1:11

df_long <- df_heatmap %>%
  pivot_longer(
    cols = -group,
    names_to = "time",
    values_to = "leader_id"
  ) %>%
  mutate(
    time = as.integer(gsub("[^0-9]", "", time)),  # Nettoie noms de colonnes (ex: V1 → 1)
    group = as.factor(group),
    leader_id = as.factor(leader_id)
  )

df_leader_long <- df_leader %>%
  pivot_longer(
    cols = -group,
    names_to = "time",
    values_to = "is_leader"
  ) %>%
  mutate(
    time = as.integer(gsub("[^0-9]", "", time)),
    group = as.factor(group),
    is_leader = as.integer(is_leader)
  )



df_final <- left_join(df_long, df_leader_long, by = c("group", "time"))
variablesC <- c("Hors Faction", variables)
leader_dict <- setNames(variablesC, as.character(0:11))
print(leader_dict)
group_names <- variablesC
df_final$leader_name <- leader_dict[as.character(df_final$leader_id)]
df_final$group_name <- factor(df_final$group, levels = 0:11, labels = group_names)
df_final$date <- dates[df_final$time]
colors_dict["Hors Faction"] <- "white" 

# Plot
png(path_factions_heatmap, width=2800, height=800)
p <- ggplot(df_final, aes(x = date, y = group_name, fill = leader_name)) +
  geom_tile(color = "white") +  
  coord_fixed(ratio=6) +
  geom_tile(data = df_final %>% filter(is_leader == 1),
            aes(x = date, y = group_name),
            fill = NA, color = "black", linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = colors_dict, na.value = "white") +
  scale_x_date(breaks = seq(as.Date("2022-06-20"), as.Date("2023-03-14"), by = "3 weeks"), 
               date_labels = "%b %d") +
  theme_minimal() +
  labs(x = "Date", y = "Groupe", fill = "Factions Leaders") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
dev.off()

print(df_final[!complete.cases(df_final), ])

for (i in 1:(length(variables)-1)){
    leader <- variables[i]
    for (j in (i+1):length(variables)){
        follower <- variables[j]
        path_ij <- paste0(path_starting_bivariate, follower, "_", leader, ".png")
        biv_plot_TS(model_dtw$dyNetOut$dyNetWeightedMat[i, j,], model_dtw$dyNetOut$dyNetWeightedMat[j, i,], leader, follower, title = paste("Dynamiques d'influence entre", leader, "et", follower), path=path_ij) #Plot ligne i influencé par ligne j
    }
}