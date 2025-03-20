library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(reshape2)

db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE)

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

db <- db %>% filter(topic >= 0)
# - logit transform all series
for (v in variables) {
# - pulling the series-agenda for that group
    db[[v]] <- as.numeric(db[[v]])
    x <- db[,v]

    if (any(x > 0.998)) { #Treat the case of a proportion value >= 1 (x can be > 1 because of the previous line). We chose 0.999 because rounding of 0.9999 is equal to 1 in db 
        for (i in 1:nrow(x)) {
        if (db[i, v] > 0.998) {
            print(paste("WARNING: due to proportion value equal to 1 or almost equal to 1,  small transformation has been applied for a value in the following group :", v))
            db[i, v] <- 0.998
            }
        }
        }

    x <- db[,v]
    # - adding 1 percentage point to avoid 0s before the logit transformation
    x <- x + 0.001

    # - applying the non-linear transformation
    logit_x <- log(x / (1 - x))
    db[,v] <- logit_x
}

db_corr <- db %>% select(variables) 
cormat <- round(cor(db_corr),2)
melted_cormat <- melt(cormat)
melted_cormat$Var1 <- factor(melted_cormat$Var1, levels = rev(colnames(db_corr)))
png("data_prod/figures/bertopic/corrmat_day.png",  width = 800, height = 800)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab",
   name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed() 

ggheatmap + 
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
dev.off()

  
db$topic <- as.character(db$topic)
db_bis <- db %>%
    filter(date > as.Date("2022-06-21")) %>% #Remove the two first days to have a number of days divisible by 7. 
    group_by(topic) %>%
    mutate(group = ceiling(row_number() /7)) %>% 
    group_by(group, topic) %>%
    summarise(date = first(group), across(where(is.numeric), mean), .groups='drop') %>%
    arrange(as.numeric(topic)) 

db_corr <- db_bis %>% select(variables) 
cormat <- round(cor(db_corr),2)
melted_cormat <- melt(cormat)
melted_cormat$Var1 <- factor(melted_cormat$Var1, levels = rev(colnames(db_corr)))
png("data_prod/figures/bertopic/corrmat_week.png",  width = 800, height = 800)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab",
   name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed() 

ggheatmap + 
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
dev.off()

print("start boucle")

for (i in 2:6){
    if(i==3){
        db_bis <- db %>%
        filter(date > as.Date("2022-06-20")) 
    } else if (i==5){
        db_bis<- db %>%
        filter(date > as.Date("2022-06-22")) 
    } else if (i==6){
        db_bis <- db %>%
        filter(date > as.Date("2022-06-23")) 
    } else {
        db_bis <- db 
    }

    db_bis <- db_bis %>%  #Remove the two first days to have a number of days divisible by 7. 
    group_by(topic) %>%
    mutate(group = ceiling(row_number() /i)) %>% 
    group_by(group, topic) %>%
    summarise(date = first(group), across(where(is.numeric), mean), .groups='drop') %>%
    arrange(as.numeric(topic)) 

    db_corr <- db_bis %>% select(variables)

    cormat <- round(cor(db_corr),2)
    melted_cormat <- melt(cormat)
    melted_cormat$Var1 <- factor(melted_cormat$Var1, levels = rev(colnames(db_corr)))

    png(paste0("data_prod/figures/bertopic/corrmat_", i, "day.png"),  width = 800, height = 800)

    ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
        midpoint = 0, limit = c(-1,1), space = "Lab",
        name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
        size = 12, hjust = 1))+
    coord_fixed() 

    final_plot <- ggheatmap + 
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
    
    print(final_plot)
    dev.off()
    }

