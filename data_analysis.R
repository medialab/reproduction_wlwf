library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(reshape2)

db <- read_csv("data_prod/var/general_TS.csv", show_col_types = FALSE)
db_prop <- read_csv("data_prod/var/general_TS_prop.csv", show_col_types = FALSE)
variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'media')
throw_topic <- c(16, 44, 54, 61, 64, 73, 76, 91, 1, 2, 5, 25, 41, 45, 3, 21, 26, 35, 50, 51, 56, 57, 58, 60, 65, 69, 78, 80, 87)
pol_issues_temp <- setdiff(c(0:91), throw_topic)
db <- db %>% mutate(topic = ifelse(topic == 89, 74, topic)) %>%
  group_by(date, topic) %>%                                  
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") 

db_prop <- db_prop %>% mutate(topic = ifelse(topic == 89, 74, topic)) %>%
  group_by(date, topic) %>%                                  
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") 
pol_issues <- setdiff(pol_issues_temp, 89)  

db_prop <- db_prop %>%
  filter(topic %in% pol_issues)

#Figure 1
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

pa2our <- read_csv("data_prod/figures/translate_number_name/BERTOPIC_merged.csv", show_col_types=FALSE)
db_prop <- left_join(db_prop, pa2our, by = c("topic" = "Topic"))

db_long <- db_prop %>%
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
exclude_issues <- c(52, 71, 79, 85, 86)
pol_issues <- setdiff(pol_issues, exclude_issues)
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