library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rio)
library(vars)
library(pscl)
library(performance)
library(MASS)
library(DHARMa)
library(ggstats)
library(broom.helpers)
library(parameters)

#https://larmarange.github.io/guide-R/analyses_avancees/modeles-zero-inflated.html

if(!file.exists("data_prod/var/bertopic/outliers_mat.Rdata")){
    print("Upload data")

    db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE) 
    pol_issues <- c(0, 1, 2, 4, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 22, 27, 28, 29, 30, 31, 32, 33, 34, 36, 
    37, 38, 39, 40, 42, 43, 44, 45, 46, 47, 48, 49, 52, 53, 54, 55, 58, 59, 62, 63, 64, 66, 67, 68, 70, 71, 
    72, 73, 74, 75, 77, 80, 81, 82, 83, 84, 85, 86, 89, 90, 91) #14, 23, 33, 37, 41, 45, 51, 56, 61, 62, 63, 64, 79, 88
    statio_issues <- c(2, 6, 9, 11, 12, 13, 16, 17, 20, 22, 27, 30, 31, 32, 33, 34, 36, 37, 39, 40, 41, 43, 44, 45, 46, 47, 48, 50, 52, 53, 55, 57, 58, 59, 60, 64, 66, 67, 68, 72, 73, 74, 76, 77, 78, 79, 81, 82, 83, 84, 86, 87, 88, 90)
    exclude_issues <- c(0,1,3,4,5)
    event_issues <- setdiff(pol_issues, statio_issues)
    event_issues <- setdiff(event_issues, exclude_issues)
    variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

    db <- db %>%
        filter(topic %in% event_issues)

    db <- db %>%
        mutate(date_index = as.integer(difftime(date, min(date), units = "days")) + 1)

    print("Create outlier matrix")

    outliers_mat <- data.frame(matrix(NA, nrow=4000, ncol=length(variables) +1))
    colnames(outliers_mat) <- c("topic", variables)
    outliers_count <- data.frame(matrix(0, nrow=length(unique(db$topic)), ncol=length(variables)))
    rownames(outliers_count) <- unique(db$topic)
    colnames(outliers_count) <- variables

    print(length(unique(db$topic)))
    print(unique(db$topic))

    #Détect Outlier
    line <- 0
    print("Start loop detection")
    for (topic_num in unique(db$topic)){
        print(topic_num)
        outlier_counter <- 0
        db_topic <- db %>%
                    filter(topic == topic_num)
        count_var_init <- 0
        for (v in variables){
            print(v)
            line_topic <- 0
            count_day <- 1
            data <- db_topic[[v]]
            QMAX <- quantile(data, 0.95)
            outlier_idx <- which(data > QMAX)
            date_indexes <- db_topic$date_index[outlier_idx]
            if(!(length(outlier_idx) ==0)){
                topic_str <- as.character(topic_num)
                outliers_count[topic_str, v] <- length(outlier_idx)
                for (day in date_indexes){
                    if (count_day > 1){
                        ecart_last_value <- day - date_indexes[count_day-1]
                    
                        if (ecart_last_value <10){
                            remove_idx <- which(date_indexes == day)
                            date_indexes <- date_indexes[-remove_idx]
                            next
                        }
                    }
                    line_topic <- line_topic +1
                    outliers_mat[line + line_topic, v] <- day
                    outliers_mat[line + line_topic, "topic"] <- topic_num
                    count_day <- count_day + 1
                }        
            outlier_counter <- max(outlier_counter, length(date_indexes))
            }
        }
        line <- line + outlier_counter
    }

        
    row_indices <- apply(outliers_mat[, colnames(outliers_mat)], 1, function(x) !all(is.na(x)))
    outliers_mat <- outliers_mat[row_indices, ]
    outliers_mat[is.na(outliers_mat)] <- 0
    print(summary(outliers_mat))


    #Improve detection of pic
    add_row_ind <- 0
    for (i in 1:nrow(outliers_mat)){
        row_vals <- outliers_mat[i,][2:ncol(outliers_mat)]
        pos_vals <- row_vals[row_vals>0]
        min_pic <- min(pos_vals)
        max_pic <- max(pos_vals)
        topic_num <- outliers_mat[i, "topic"]
        diff_min_max <- max_pic - min_pic
        if(diff_min_max >14){
            index_treat <- 1:(ncol(outliers_mat)-1)
            iter <- 1
            new_min <- min_pic
            while(diff_min_max >14){
                new_line <- rep(0, ncol(outliers_mat) - 1)
                for (j in index_treat){
                    if(row_vals[j] ==0){
                        index_treat <- index_treat[index_treat != j]
                        next
                    }
                    diff_min <- row_vals[j] - new_min
                    if(diff_min <15){
                        new_line[j] <- row_vals[j]
                        index_treat <- index_treat[index_treat != j]
                    }
                }
                if (iter==1){
                    outliers_mat[i,][2:ncol(outliers_mat)] <- new_line
                    iter <- 2
                } else {
                    add_row_ind <- add_row_ind + 1
                    outliers_mat[nrow(outliers_mat) + add_row_ind,] <- c(topic_num, new_line)
                }
                new_min <- min(row_vals[row_vals>new_min+14])
                if (is.infinite(new_min)){
                    diff_min_max <- 0
                } else {
                    diff_min_max <- max_pic - new_min
                }
            }
        }
    }
    row_indices <- apply(outliers_mat[, colnames(outliers_mat)], 1, function(x) !all(is.na(x)))
    outliers_mat <- outliers_mat[row_indices, ]
    print(summary(outliers_mat))
    #Detect pics and create order 
    for (i in 1:nrow(outliers_mat)){
        #Generate orders
        row_val <- outliers_mat[i,][2:ncol(outliers_mat)]
        pos_vals <- row_val[row_val>0]
        min_pos <- min(pos_vals)
        offset <- min_pos-1
        outliers_mat[i,][2:ncol(outliers_mat)] <- ifelse(row_val != 0, row_val - offset, 0)
    }

    print(head(outliers_mat,10))
    print(summary(outliers_mat))

    save(outliers_mat, file="data_prod/var/bertopic/outliers_mat.Rdata")
}
load("data_prod/var/bertopic/outliers_mat.Rdata")
variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')
outliers_mat <- as.data.frame(lapply(outliers_mat, function(col) as.numeric(as.character(col))))
print(paste("Number of event happening", nrow(outliers_mat)))

keep_rows <- apply(outliers_mat[, -1], 1, function(row) sum(row) > 1) #We drop the line were only one group made an event bc no following logic
filtered_mat <- outliers_mat[keep_rows, ]

print(paste("Number of events where more than one actor appears", nrow(filtered_mat)))


#Plot histograms
for(v in variables){
    png(paste0("data_prod/figures/bertopic/hist_",v,".png"))
    hist <- hist(outliers_mat[[v]],
                main = paste("Order of participation for", v),
                breaks = 15,
                xlab = v,
                ylab = "count",
                col = "purple",
        )
    dev.off()
}

#Convert topic as dummy
maindb <- outliers_mat
maindb$topic <- as.character(outliers_mat$topic)
maindb <- maindb[, !colnames(maindb) %in% "topic"]
#dummies <- model.matrix(~ topic - 1, data = maindb)
#colnames(dummies) <- gsub("^topic", "topic_", colnames(dummies))
#maindb <- cbind(maindb, dummies)

results_list <- list() 
for (v in variables){
    reg_formula <- as.formula(paste(v,"~ ."))
    mod_poiss <- glm(reg_formula, family=poisson, data = maindb)
    print(v)
    print("Poisson")
    print(performance::check_overdispersion(mod_poiss))
    print("Negative Binomial")
    mod_nb <- MASS::glm.nb(reg_formula, data = maindb)
    print(performance::check_overdispersion(mod_nb))
    print(performance::check_zeroinflation(mod_nb,tolerance = 0.05))
    model_zip <- pscl::zeroinfl(reg_formula, data = maindb) 
    model_hurdle <- pscl::hurdle(reg_formula, data = maindb)
    model_zinb <- pscl::zeroinfl(reg_formula, dist="negbin", data=maindb)
    results_list[[v]] <- summary(model_hurdle)

    print(performance::compare_performance(mod_poiss, mod_nb, model_zip, model_zinb, model_hurdle))
    
    plot_hurdle <- ggstats::ggcoef_table(
    model = model_hurdle,
    tidy_fun = broom.helpers::tidy_zeroinfl,
    exponentiate = TRUE,
    component_label = c(
        conditional = "Facteurs associés à l'ordre de passage",
        zero_inflated = "Facteurs associés au fait d'avoir vécu l'évènement"
        )
    ) 

    # Sauvegarder la figure (ex. en PNG)
    ggplot2::ggsave(
    filename = paste0("data_prod/var/bertopic/comptage/hurdle_model_plot_",v,".png"),
    plot = plot_hurdle,
    width = 20,
    height = 12,
    dpi = 300
    )

    print(plot_hurdle)
    }
#print("ZIP")
#print(results_list)

save(results_list, file="data_prod/var/bertopic/resultREGOUTLIERS.RData")