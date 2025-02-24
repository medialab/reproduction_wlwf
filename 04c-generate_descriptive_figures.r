library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)

#===Figure1===

db <- read.csv("data_prod/var/lda/general_TS.csv")

pol_issues <- c(1:99)
stationary_issues <- c(3,7, 8, 10, 15, 21, 23, 26, 30, 33, 34, 37, 39, 40, 42, 43, 46, 47, 48, 53, 54, 61, 62, 64, 65, 66, 67, 73, 77, 79, 82, 83, 84, 85, 87, 88, 89, 90, 92, 95, 98) #Obtained in 05b script
stationary_issues_no_media <- c(2, 3, 7, 8, 10, 15, 18, 21, 23, 25, 26, 29, 30, 33, 34, 37, 39, 40, 42, 43, 46, 47, 48, 53, 54, 57, 61, 62, 64, 65, 66, 67, 71, 73, 74, 77, 79, 82, 83, 84, 85, 87, 88, 89, 90, 92, 94, 95, 98)
db <- db %>%
  filter(topic %in% pol_issues)

db_long <- db %>%
  dplyr::select(topic, lr, majority, nupes, rn, lr_supp, majority_supp, nupes_supp, rn_supp, general, attentive, media) %>%
  gather(group, att, -topic)

out_db <- db_long %>%
  group_by(group, topic) %>%
  summarize(av = mean(att, na.rm = TRUE)) %>%
  as.data.frame()

out_db$group <- recode(factor(out_db$group),
                       `lr` = "LR in\nAssemblée",
                       `majority` = "Majority in\nAssemblée",
                       `nupes` = "NUPES in\nAssemblée",
                       `rn` = "RN in\nAssemblée",
                       `lr_supp` = "LR\nSupporters",
                       `majority_supp` = "Majority\nSupporters",
                       `nupes_supp` = "NUPES\nSupporters",
                       `rn_supp` = "RN\nSupporters",
                       `general` = "General\nPublic",
                       `attentive` = "Attentive\nPublic",
                        `media` = "Media"
                       )

out_db$group <- factor(out_db$group,
                       levels = c(
                        "LR in\nAssemblée",
                        "Majority in\nAssemblée",
                        "NUPES in\nAssemblée",
                        "RN in\nAssemblée",
                        "LR\nSupporters",
                        "Majority\nSupporters",
                        "NUPES\nSupporters",
                        "RN\nSupporters",
                        "General\nPublic",
                        "Attentive\nPublic",
                        "Media"
                       ))

out_db <- out_db %>%
  arrange(group, av)

out_db$topic <- as.character(out_db$topic)
out_db$topic <- factor(out_db$topic, levels = unique(out_db$topic))
stationary_issues <- as.character(stationary_issues)
stationary_issues_no_media <- as.character(stationary_issues_no_media)
label_colors <- ifelse(unique(out_db$topic) %in% stationary_issues, "green", "red")

png("data_prod/figures/figure1.png", width = 13000, height = 2400)
ggplot(out_db, 
       aes(x = topic, y = av)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~ group, nrow = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.01),
                     labels = paste0(seq(0, 100, 1), "%")) +
  ylab("\nAverage daily attention (LDA topic posterior probability) to each political issue (113th Congress)") +
  xlab("") +
  coord_flip() +
  theme(panel.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.text.y = element_text(size = 18, color = label_colors),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        panel.grid.major.x = element_line(color = "gray80"),
        panel.grid.major.y = element_line(color = "gray70", linetype = "dotted"))
dev.off()

#====Table3====
# - calculate correlation between agenda of Democrats and Republicans in 
#   Congress, and the different groups of the Public as well as the Media
outgroups <- c("lr", "majority", "nupes", "rn")
covgroups <- c("lr_supp", "majority_supp", "nupes_supp", "rn_supp", "general", "attentive", "media")

results <- NULL
for (polgroup in outgroups) {
  new_col <- NULL
  for (compgroup in covgroups) {
    # - create a dataframe only with the two groups to compare
    comp_df <- data.frame(
      y = db[,polgroup],
      x = db[,compgroup]
    )
    
    # - calculate correlation
    cor_out <- round(cor(comp_df)[2,1], 2)
    new_cell <- data.frame(cor_out)
    colnames(new_cell) <- polgroup
    rownames(new_cell) <- compgroup
    new_col <- rbind(new_col, new_cell)
  }
  if (is.null(results)) {
    results <- new_col
  } else {
    results <- cbind(results, new_col)
  }
}

# OUTPUT
# - adding human readable labels to the column and row names
rownames(results) <- c("LR Supporters","Majority Supporters","NUPES Supporters","RN Supporters","General Public","Attentive Public","Media")
colnames(results) <- c("LR in Assemblée", "Majority in Assemblée", "NUPES in Assemblée", "RN in Assemblée")

print(results)