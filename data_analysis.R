library(dplyr)
library(tidyverse)
library(tidyr)
library(reshape2)
library(purrr)
library(remotes)
install_version("ggplot2", version = "3.5.2")
library(ggplot2)


db <- read_csv("data_prod/var/general_TS.csv", show_col_types = FALSE)
db_prop <- read_csv("data_prod/var/general_TS_prop.csv", show_col_types = FALSE)
variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'media')
titles <- read_csv("data_prod/figures/translate_number_name/BERTOPIC_LABEL.csv", col_names = TRUE, show_col_types=FALSE)
colnames(titles) = c("Topic", "label")
pol_issues <- unique(titles$Topic)
pol_issues <- setdiff(pol_issues, c(82,60,50,68))
db <- db %>% mutate(topic = ifelse(topic == 82, 0, topic)) %>%
  mutate(topic = ifelse(topic == 50, 5, topic)) %>%
  mutate(topic = ifelse(topic == 60, 40, topic)) %>%
  group_by(date, topic) %>%                                  
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") 

db_prop <- db_prop %>% mutate(topic = ifelse(topic == 82, 0, topic)) %>%
  mutate(topic = ifelse(topic == 50, 5, topic)) %>%
  mutate(topic = ifelse(topic == 60, 40, topic)) %>%
  group_by(date, topic) %>%                                  
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") 
#Set a pol_issue temp if needed

readable_variables <- c("Députés LR", 
                        "Députés Majorité",
                        "Députés NUPES",
                        "Députés RN",
                        "Supporters LR",
                        "Supporters Majorité",
                        "Supporters NUPES",
                        "Supporters RN",
                        "Public Attentif",
                        "Média")

label_var <- setNames(readable_variables, variables)

#Outliers plot 
db_outliers <- db_prop %>% 
          filter(topic < 0) %>% 
          dplyr::select(-c(topic,date)) 

plot_outliers <- cbind(readable_variables, colMeans(db_outliers)) 

colnames(plot_outliers) <- c("group", "mean_outliers")
plot_outliers <- as.data.frame(plot_outliers)
plot_outliers$mean_outliers <- as.numeric(plot_outliers$mean_outliers)

plot_outliers <- plot_outliers %>% 
                mutate(pct_outliers = mean_outliers * 100) %>% 
                select(-mean_outliers)

png("data_prod/figures/outliers_plot.png", width=1000, height=800)
p <- ggplot(data = plot_outliers, aes(x = group, y=pct_outliers)) +
  geom_col(fill = "darkblue", width = .7) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
  xlab("Groupe") + 
  ylab("Pourcentage moyen de tweets classés comme outlier")
print(p)
dev.off()

#Nombre de tweets moyen par groupe 

db_nbr <- db %>% 
        filter (topic %in% pol_issues) %>%
        dplyr::select(-topic) %>%
        group_by(date) %>% 
        summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
        summarise(across(everything(), mean, na.rm = TRUE)) %>%  
        select(-date) %>%
        pivot_longer(everything(), names_to = "group", values_to = "nombre") %>% 
        mutate (
          group = recode(group, !!!label_var)
        ) %>%
        mutate(
        group = factor(group, levels = readable_variables))

write.csv(db_nbr, "nombre_tweets_moyen.csv")

png("data_prod/figures/nb_tweets.png", width = 1200, height = 900)
p <- ggplot(db_nbr, aes(x = group, y = nombre)) +
    geom_col(fill = "darkgreen", width = 0.8) + 
    scale_y_continuous(breaks = seq(0, 3000, 250), limits = c(0,3000)) +
    xlab("Groupe") + 
    ylab("Nombre moyen de tweets par jours inclus dans les VAR")
print(p)
dev.off()

if (FALSE) {
  #Loss by semantic validity plot 
  pol_issues_bert <- c(pol_issues, c(26, 49, 80, 82))
  db_prop_bert <- db_prop %>%
                filter(topic %in% pol_issues_bert) %>%
                dplyr::select(-topic) %>%
                group_by(date) %>% 
                summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%  # somme par date pour chaque colonne numérique
                summarise(across(everything(), mean, na.rm = TRUE)) %>%  
                select(-date) %>%
                pivot_longer(everything(), names_to = "group", values_to = "prop_pol_issues_bert")


  db_prop <- db_prop %>%
    filter(topic %in% pol_issues)   


  db_valid_bert <- db_prop %>%
                  dplyr::select(-topic) %>%
                  group_by(date) %>% 
                  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%  # somme par date pour chaque colonne numérique
                  summarise(across(everything(), mean, na.rm = TRUE)) %>%  
                  select(-date) %>%
                  pivot_longer(everything(), names_to = "group", values_to = "prop_val_pol_issues_bert")


  db_prop_lda <- read_csv("data_prod/var/general_TS_LDA.csv", show_col_types = FALSE)
  pol_valid_lda <- c(19, 2, 30, 34, 61, 16, 48, 1, 3, 5, 9, 13, 15, 17, 21, 25, 27, 29, 33, 36, 40, 42,
  44, 45, 50, 51, 52, 53, 55, 56, 59, 60, 63, 64, 65, 66, 70)
  pol_issues_lda <- c(pol_valid_lda, c(20, 11, 4, 6, 18, 22, 31, 35, 38, 39, 41,47, 69, 14, 43, 49, 54, 62, 67, 68))

  db_prop_lda_full <- db_prop_lda %>%
            filter(topic %in% pol_issues_lda)  %>%
            dplyr::select(-topic) %>%
            group_by(date) %>% 
            summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%  # somme par date pour chaque colonne numérique
            summarise(across(everything(), mean, na.rm = TRUE)) %>%  
            select(-date) %>%
            pivot_longer(everything(), names_to = "group", values_to = "prop_pol_issues_lda")

  db_prop_lda_valid <- db_prop_lda %>%
            filter(topic %in% pol_valid_lda)  %>%
            dplyr::select(-topic) %>%
            group_by(date) %>% 
            summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%  # somme par date pour chaque colonne numérique
            summarise(across(everything(), mean, na.rm = TRUE)) %>% 
            select(-date) %>%
            pivot_longer(everything(), names_to = "group", values_to = "prop_val_pol_issues_lda")

  var_order <- c("prop_pol_issues_bert", "prop_val_pol_issues_bert", "prop_pol_issues_lda", "prop_val_pol_issues_lda")

  label_map <- c(
    "prop_pol_issues_bert" = "Sujets politiques BERTopic",
    "prop_val_pol_issues_bert" = "Sujets politiques valides sémantiquement BERTopic",
    "prop_pol_issues_lda" = "Sujets politiques LDA",
    "prop_val_pol_issues_lda" = "Sujets politiques valides sémantiquement LDA"
  )

  custom_colors <-c(
    "Sujets politiques BERTopic" = 'red',
    "Sujets politiques valides sémantiquement BERTopic" = 'orchid',
    "Sujets politiques LDA" = 'blue',
    "Sujets politiques valides sémantiquement LDA" = 'cyan'
  )


  plot_db <- reduce(list(db_prop_bert, db_valid_bert, db_prop_lda_full, db_prop_lda_valid), left_join, by = "group") %>%
      pivot_longer(
        cols = -group,
        names_to = "variable",
        values_to = "value"
      ) %>% 
      mutate (
        group = recode(group, !!!label_var)
      ) %>%
      mutate(
      group = factor(group, levels = readable_variables),                         
      variable = factor(label_map[variable], levels = label_map[var_order])  
    )

  png("data_prod/figures/loss_plot.png", width=1500, height=1500)
  p <- ggplot(plot_db, aes(x = group, y = value, fill = variable)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = custom_colors, name = "Catégorie") +
    labs(x = "Groupe", y = "Proportion restante", fill = "Variable") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )

  print(p)
  dev.off()
}
#Figure 1
pa2our <- titles
colnames(pa2our) <- c("Topic", "Name")
db_prop <- left_join(db_prop, pa2our, by = c("topic" = "Topic"))

db_long <- db_prop %>%
  filter(topic %in% pol_issues) %>%
  dplyr::select(Name, lr, majority, nupes, rn, lr_supp, majority_supp, nupes_supp, rn_supp, attentive, media) %>%
  gather(group, att, -Name)

out_db <- db_long %>%
  group_by(group, Name) %>%
  summarize(av = mean(att, na.rm = TRUE)) %>%
  as.data.frame()  

# - provide readble labels to the groups
out_db$group <- recode(factor(out_db$group),
                       `lr` = "Députés LR",
                       `majority` = "Députés Majorité",
                       `nupes` = "Députés NUPES",
                       `rn` ="Députés RN",
                       `lr_supp` = "Supporters LR",
                       `majority_supp` = "Supporters Majorité",
                       `nupes_supp` =   "Supporters NUPES",
                       `rn_supp` =  "Supporters RN",
                       `attentive` = "Public Attentif",
                       `media` = "Média")

# - re-level the group variable: first the political agendas and then the public
#   ones
out_db$group <- factor(out_db$group,
                       levels = readable_variables)

# - sort the labels in descending order by Democrats in Congress attention
out_db <- out_db %>%
  arrange(group, av)

out_db$Name <- factor(out_db$Name, levels = unique(out_db$Name))

png("data_prod/figures/figure1.png", width=1800, height=1400)
p <- ggplot(out_db, 
      aes(x = Name, y = av)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~ group, nrow = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.03),
                     labels = scales::percent_format(accuracy = 1)) +
  ylab("\nAttention moyenne quotidienne accordée à chaque sujet par groupe") +
  xlab("") +
  coord_flip() +
  theme(panel.background = element_blank(),
        strip.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 9, angle=45),
        axis.title.x = element_text(size = 18),
        panel.grid.major.x = element_line(color = "gray80"),
        panel.grid.major.y = element_line(color = "gray70", linetype = "dotted"))
print(p)
dev.off()

#Correlation matrix
db <- db %>%
  filter(topic %in% pol_issues)

list_mat <- list()
for (top in pol_issues){
  db_topic <- db %>% filter (topic == top)
  new_mat <- cor(db_topic[,variables])
  list_mat <- c(list_mat, list(new_mat))
}


mat <- Reduce("+", list_mat) / length(list_mat)
#Correlation matrix 


cormat <- round(mat,3)  
rownames(cormat) <- variables

# Melt pour ggplot
melted_cormat <- melt(cormat)
melted_cormat$Var1 <- factor(melted_cormat$Var1, levels = rev(rownames(cormat)))
melted_cormat$Var2 <- factor(melted_cormat$Var2, levels = colnames(cormat))

png(paste0("data_prod/figures/topic_corrmat.png"),  width = 800, height = 800)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
geom_tile(color = "white")+
scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
midpoint = 0, limit = c(-1,1), space = "Lab",
name="Pearson\nCorrelation") +
theme_minimal()+ # minimal theme
theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
coord_fixed() 

ggfinal <- ggheatmap + 
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