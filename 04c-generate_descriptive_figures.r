library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)

library(argparse)

parser <- ArgumentParser()

parser$add_argument("topic_model", help="Choose a model type between lda and bertopic")

args <- parser$parse_args()

if (!(args$topic_model %in% c('bertopic', 'lda'))){
  stop("The model name is incorrect. Choose between lda and bertopic")
}

#===Figure1===

if (args$topic_model == 'lda') {

    db <- read.csv("data_prod/var/lda/general_TS.csv") #Data from 05a script 

    pol_issues <- c(1:99)
    stationary_issues <- c(3, 7, 8, 10, 15, 21, 23, 26, 30, 33, 34, 37, 39, 40, 42, 43, 46, 47, 48, 53, 54, 61, 64, 65, 66, 67, 73, 77, 79, 82, 83, 84, 87, 88, 89, 92, 93, 95, 98 ) #Obtained in 05b script
} else {
    db <- read.csv("data_prod/var/bertopic/general_TS.csv") #Data from 05a script 

    pol_issues <- c(0:211)
    stationary_issues <- c(12, 21, 25, 29, 35, 41, 47, 52, 58, 59, 62, 64, 65, 79, 81, 85, 86, 88, 89, 90, 91, 93, 95, 98, 99, 100, 101, 102, 105, 106, 110, 111, 114, 115, 116, 117, 118, 120, 124, 126, 127, 129, 130, 132, 134, 136, 137, 139, 141, 142, 143, 145, 152, 153, 158, 162, 163, 166, 174, 176, 178, 180, 183, 187, 188, 190, 193, 195, 196, 197, 198, 201, 203, 205, 206, 211) #Obtained in 05b script
}

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
label_colors <- ifelse(unique(out_db$topic) %in% stationary_issues, "green", "red")

if (args$topic_model == 'lda') {
    png("data_prod/figures/lda/figure1.png", width = 13000, height = 2400)
} else {
    png("data_prod/figures/bertopic/figure1.png", width = 13000, height = 2400) 
}
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
    cor_out <- round(cor(comp_df)[2,1], 2) # Calcul du test t pour tester si la corrélation est significativement différente de 0
    n <- nrow(comp_df)  # Nombre d'observations sans NA
    t_stat <- cor_out * sqrt((n - 2) / (1 - cor_out^2))  # Calcul de la statistique t
    p_value <- 2 * (1 - pt(abs(t_stat), df = n - 2))  # Calcul de la p-value (bilatéral)

    # Test 
    is_significant <- p_value < 0.05

    # Add tests infos in DF
    new_cell <- data.frame(correlation = round(cor_out, 2), p_value = round(p_value, 4), significant = is_significant, outgroup = polgroup, covgroup = compgroup)
    
    colnames(new_cell) <- c("correlation", "p_value", "significant", "outgroup", "covgroup")
    new_col <- rbind(new_col, new_cell)
}

# Add new column
if (is.null(results)) {
    results <- new_col
} else {
    results <- rbind(results, new_col)
}
}

print(results)