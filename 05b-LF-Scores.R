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
if (args$estimate){
    topic_follow = data.frame(
    topic = numeric(),
    leader = character(),
    follower=character(),
    value=numeric()
)  
    matrix_dtw <- array(NA, c(num_groups, num_timepoints, 1))
    for(topic_num in pol_issues){
        print(topic_num)
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
                    topic_follow <- rbind(topic_follow, new_row)
                } else if (score > 0) {
                    new_row <- data.frame(
                    topic = topic_num,
                    leader = variables[i],
                    follower = variables[j],
                    value = score,
                    stringsAsFactors = FALSE
                    )
                    topic_follow <- rbind(topic_follow, new_row)
                }
            }
        }
    }
    write.csv(topic_follow, paste0(init_unique, "liste_fig4.csv"), row.names=FALSE)
} else{
    if (!file.exists(paste0(init_unique, "liste_fig4.csv"))){
        stop("Please use --estimate option, the scores weren't calculated")
    }
}

sigma <- 0.5
scores <- read_csv(paste0(init_unique, "liste_fig4.csv"), show_col_types=FALSE)

if(args$topic_model=='lda'){
    pa2our <- read_csv("data_prod/figures/translate_number_name/LDA_unmerged.csv",col_names=FALSE, show_col_types=FALSE)
} else {
    pa2our <- read_csv("data_prod/figures/translate_number_name/BERTopic_unmerged.csv",col_names=FALSE, show_col_types=FALSE)
}
colnames(pa2our) <- c("topic", "label")
agenda_type <- data.frame(
  var = variables,
  type = c("pol", "pol", "pol", "pol", "pub", "pub", "pub", "pub", "pub", "pub", "media")
)
leader_agenda_type <- agenda_type %>%
  rename(leader = var, leader_agenda_type = type)
follower_agenda_type <- agenda_type %>%
  rename(follower = var, follower_agenda_type = type)

leader_agenda_type$leader <- as.character(leader_agenda_type$leader)
follower_agenda_type$follower <- as.character(follower_agenda_type$follower)
scores$leader <- as.character(scores$leader)
scores$follower <- as.character(scores$follower)

scores <- left_join(scores, leader_agenda_type)
scores <- left_join(scores, follower_agenda_type)

scores <- scores %>%
  filter(leader_agenda_type != follower_agenda_type | leader_agenda_type == "pol") %>%
  filter(value>sigma)

# - merging to the dataset a human readable name for the topics

scores <- left_join(scores, pa2our)

scores$leader <- recode(scores$leader,
                    `lr` = "LR in\nCongress",
                    `majority` = "Majority in\nCongress",
                    `nupes` = "NUPES in\nCongress",
                    `rn` = "RN in\nCongress",
                    `lr_supp` = "LR\nSupporters",
                    `majority_supp` = "Majority\nSupporters",
                    `nupes_supp` = "NUPES\nSupporters",
                    `rn_supp` = "RN\nSupporters",
                    `attentive` = "Attentive\nPublic",
                    `general` = "General\nPublic",
                    `media` = "Media")

scores$follower <- recode(scores$follower,
                    `lr` = "LR in\nCongress",
                    `majority` = "Majority in\nCongress",
                    `nupes` = "NUPES in\nCongress",
                    `rn` = "RN in\nCongress",
                    `lr_supp` = "LR\nSupporters",
                    `majority_supp` = "Majority\nSupporters",
                    `nupes_supp` = "NUPES\nSupporters",
                    `rn_supp` = "RN\nSupporters",
                    `attentive` = "Attentive\nPublic",
                    `general` = "General\nPublic",
                    `media` = "Media")

# - reordering the covariate and outcome categories

scores$leader <- factor(scores$leader,
                      levels = rev(c("LR in\nCongress",
                                      "Majority in\nCongress",
                                      "NUPES in\nCongress",
                                      "RN in\nCongress",
                                      "LR\nSupporters",
                                      "Majority\nSupporters",
                                      "NUPES\nSupporters",
                                      "RN\nSupporters",
                                      "Attentive\nPublic",
                                      "General\nPublic",
                                      "Media")))

scores$follower <- factor(scores$follower,
                    levels = c("LR in\nCongress",
                                "Majority in\nCongress",
                                "NUPES in\nCongress",
                                "RN in\nCongress",
                                "LR\nSupporters",
                                "Majority\nSupporters",
                                "NUPES\nSupporters",
                                "RN\nSupporters",
                                "Attentive\nPublic",
                                "General\nPublic", 
                                "Media"))


plot_db <- scores %>%
  mutate(label = factor(label, levels = unique(label)))

path_img <- paste0(init_unique, "FLScore_bytopic.png")

colors_dict <- c(
"LR in\nCongress" = "blue4",
  "Majority in\nCongress" = "orange",
  "NUPES in\nCongress" = "chartreuse4",
  "RN in\nCongress" = "lightsalmon3",
  "LR\nSupporters" = "cyan3",
  "Majority\nSupporters" = "darkorange1",
  "NUPES\nSupporters"= "seagreen1",
  "RN\nSupporters" = "brown4",
  "Attentive\nPublic" = "red", 
  "General\nPublic"= "darkgrey",
  "Media" = "darkorchid3"
)

# PLOT -- FIGURE 4
#===============================================================================
png(path_img, width = 1600, height = 1400)
p <- ggplot(plot_db,
       aes(x = label, y = value, ymin = 0, ymax = 1)) +
  geom_point(aes(col = leader), alpha = 0.4, size = 3) +
  facet_wrap(~follower, nrow = 1) +
  coord_flip() +
  xlab("") +
  ylab("\nLead/Follow Score") +
  scale_color_manual("", values = colors_dict) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text.x = element_text(size = 10, angle=45),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14, margin = margin(t = 20), vjust = 5)
  )

print(p)
dev.off()
