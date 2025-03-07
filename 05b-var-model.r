#install.packages(c("rio", "vars", "tseries"))

library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(vars)
library(boot)
library(rio)
library(tseries)
library(argparse)
library(stats)
library(urca)

parser <- ArgumentParser()

parser$add_argument("topic_model", help="Choose a model type between lda and bertopic")

parser$add_argument("--tests", action = "store_true",
                    help = "Activate to do the part where we test stationnarity, stationnarity after differentiation, PACF and ACF informations")


args <- parser$parse_args()

if (!(args$topic_model %in% c('bertopic', 'lda'))){
  stop("The model name is incorrect. Choose between lda and bertopic")
}

#Put our main databse generated thanks to script 05a 
if (args$topic_model == 'lda') {
  db <- read_csv("data_prod/var/lda/general_TS.csv", show_col_types = FALSE)
  pol_issues <- c(1:100)
} else {
  db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE) 
  pol_issues <- c(0:211)
}

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

results_list <- list()
results_list2 <- list()

# - logit transform all series
for (v in variables) {
  # - pulling the series-agenda for that group
  db[[v]] <- as.numeric(db[[v]])


  x <- db[,v]
  #     making these a 0 
  x[which(is.na(x))] <- 0.01
  # - adding 1 percentage point to avoid 0s before the logit transformation
  x <- x + 0.01
  # - applying the non-linear transformation
  logit_x <- log(x / (1 - x))
  db[,v] <- logit_x

  if (args$tests) {
    no_stationarity_TS_data <- data.frame()
    for (topic_n in pol_issues) { 
      # Augmented Dickey-Fuller (ADF) test for stationarity
      db_topic <- db[db$topic == topic_n, ]
      data <- db_topic[[v]]
      data <- as.ts(data)
      if (sd(data) > 0) {
        adf <- adf.test(data)
        p_value <- adf$p.value
        if (p_value > 0.05) {  # Vérifier si la série est non stationnaire
          results_list <- append(results_list, list(list(topic_n, v, p_value)))
          }
      }
    }
  }  
}

if (args$tests) {
  results_df <- do.call(rbind, lapply(results_list, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
  colnames(results_df) <- c("topic", "variable", "p_value")
  non_full_stationarity_topics <- unique(results_df$topic)
  stationary_topics <- setdiff(pol_issues, non_full_stationarity_topics)
  print(paste("Number of topics where time series for each group are stationary: ", length(stationary_topics)))
  cat("The topic numbers that satisfy this property:", stationary_topics, "\n")

  for (topic in non_full_stationarity_topics) {
    for (v in variables) {
      db_topic <- db[db$topic == topic, ]
      data <- db_topic[[v]]
      data <- as.ts(data)
      if (sd(data) > 0) {
        adf <- adf.test(data)
        p_value <- adf$p.value
        if (p_value > 0.05) {  # Vérifier si la série est non stationnaire
          data_diff <- diff(data)
          if (sd(data_diff) > 0) {
            adf2 <- adf.test(data_diff)
            p_value2 <- adf2$p.value
            if (p_value2 > 0.05) {
              results_list2 <- append(results_list2, list(list(topic, v, p_value2)))
            }
          }
        }
      }
    }
  }



  if (length(results_list2) == 0) {
    print("results_list2 est vide, aucun résultat à transformer en dataframe.")
    stationary_topics2 <- non_full_stationarity_topics
  } else {
    print("results_list2 contient des données.")
    results_df2 <- do.call(rbind, lapply(results_list2, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
    colnames(results_df2) <- c("topic", "variable", "p_value")
    non_full_stationarity_topics2 <- unique(results_df2$topic)
    stationary_topics2 <- setdiff(non_full_stationarity_topics, non_full_stationarity_topics2)
  }
  merged_statio <- c(stationary_topics, stationary_topics2)

  print(paste("Number of topics where time series for each group are stationary or I(1): ", length(merged_statio)))
  cat("The topic numbers that satisfy this property:", merged_statio, "\n")

  ACF_data <- data.frame(matrix(NA, nrow = length(variables), ncol = length(pol_issues)))
  PACF_data <- data.frame(matrix(NA, nrow = length(variables), ncol = length(pol_issues)))
  colnames(ACF_data) <- as.character(pol_issues) 
  rownames(ACF_data) <- variables
  colnames(PACF_data) <- as.character(pol_issues)
  rownames(PACF_data) <- variables

  counter_const <- 0
  for (topic in pol_issues) {
    topic_str <- as.character(topic)
    db_topic <- db[db$topic == topic, ]
    for (v in variables) {
      data <- db_topic[[v]]
      if (sd(data) == 0) {
        counter_const <- counter_const + 1
      } else{
        data_diff <- diff(data)
        acf_values <- acf(data_diff, lag.max = 30, plot= FALSE)$acf
        pacf_values <- pacf(data_diff, lag.max = 30, plot= FALSE)$acf

        # Trouver les indices où acf_values >= 0.1 et pacf_values >= 0.1
        above_threshold_acf <- which(abs(acf_values) >= 0.1)
        above_threshold_pacf <- which(abs(pacf_values) >= 0.1)

        # Trouver le dernier indice où acf >= 0.1
        if (length(above_threshold_acf) == 0) {
          number_lag_acf <- 0  # Si aucun ACF >= 0.1, on prend lag 0
        } else {
          number_lag_acf <- max(above_threshold_acf) - 1
        }

        #De même pour PACF
        if (length(above_threshold_pacf) == 0) {
          number_lag_pacf <- 0  # Si aucun ACF >= 0.1, on prend lag 1
        } else {
          number_lag_pacf <- max(above_threshold_pacf) 
        }

        ACF_data[v, topic_str] <- number_lag_acf
        PACF_data[v, topic_str] <- number_lag_pacf
      }
    }
  }

  # Résumé du nombre de séries constantes
  print(paste("Nombre de séries constantes :", counter_const))

  # Afficher les résultats
  print(summary(t(ACF_data)))
  print(summary(t(PACF_data)))
}

maindb <- db %>%
  filter(topic %in% pol_issues)

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media', 'topic')

variables2 <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')
maindb$topic <- as.character(maindb$topic)
#Create the differenciated data
diff_data_list <- list()

for (t in unique(maindb$topic)) {
  sub_db <- maindb %>% filter(topic == t)
  diff_matrix <- diff(as.matrix(sub_db[, variables2]))
  diff_df <- as.data.frame(diff_matrix)
  diff_df$topic <- sub_db$topic[-1]
  diff_data_list[[t]] <- diff_df
}

final_db <- bind_rows(diff_data_list)

mformula <- formula(paste0("~", 
                           paste0(variables, collapse = " + ")))
model_data <- model.matrix(mformula, final_db[, variables])
model_data <- model_data[, 2:ncol(model_data)] # removing intercept

# - splitting the covariates of interest from the issue dummy variables
X_endogenous <- model_data[, which(!grepl("topic", colnames(model_data)))]
X_exogenous <- model_data[, which(grepl("topic", colnames(model_data)))]

# - estimating the model: 7 lags
var_model_merged <- VAR(y = X_endogenous, p = 7, exogen = X_exogenous)

#Inverser le log avant impélmentation 

var_irfs_cum_merged <- irf(var_model_merged, n.ahead = 60, cumulative = TRUE)

if (args$topic_model == 'lda') {
  save(var_model_merged, file = "data_prod/var/lda/var_model-MAIN.Rdata")
  save(var_irfs_cum_merged, file = "data_prod/var/lda/var_irfs-MAIN.Rdata")
} else {
  save(var_model_merged, file = "data_prod/var/bertopic/var_model-MAIN.Rdata")
  save(var_irfs_cum_merged, file = "data_prod/var/bertopic/var_irfs-MAIN.Rdata")
}

var_irfs <- var_irfs_cum_merged
variables <- names(var_irfs$irf)
elements_to_pull <- c("irf", "Upper", "Lower")


irf_data <- NULL
for (el in elements_to_pull) {
  new_irf_info <- var_irfs[el][[1]]
  for (out in variables) {
    new_irf_var_data <- as.data.frame(new_irf_info[out][[1]])
    # - take inverse logit to transform the effects to percentage point changes
    new_irf_var_data_transf <- as.data.frame(
      sapply(1:ncol(new_irf_var_data), function(j)
        inv.logit(new_irf_var_data[,j]) - 0.5))
    colnames(new_irf_var_data_transf) <- colnames(new_irf_var_data)
    new_irf_var_data_long <- new_irf_var_data_transf %>%
      gather(cov, value)
    new_irf_var_data_long$out <- out
    new_irf_var_data_long$day <- rep(1:nrow(new_irf_var_data), 
                                     length(unique(new_irf_var_data_long$cov)))
    new_irf_var_data_long$e_type <- el
    irf_data <- rbind(irf_data, new_irf_var_data_long)
  }
}

# - give easier labels to the estimate types (e.g. Lower --> lwr)
irf_data$e_type <- recode(irf_data$e_type,
                          `irf` = "pe",
                          `Lower` = "lwr", 
                          `Upper` = "upr")


new_irf_data <- NULL

# - a vector with the name of the variables
variables <- unique(irf_data$cov)

# - deciding the number of days to simulate
DAYS <- 60

irf_data <- irf_data %>%
  filter(day <= (DAYS + 1))

# - iterating through covariates
for (covariate in variables) {
  # -iterating through outcomes
  for (outcome in variables) {
    # - skipping when covariate and response are the same
    if (covariate != outcome) {
      # - initializing a cummulative-shocks matrix for this scenario: two 3-dim 
      #     matrix, one matrix for the covariate and one matrix for the response,
      #     and one dimension for the point estimate and the two other dimensions
      #     for the lower and upper bounds of the estimate
      cov_mat <- array(0, dim = c(DAYS, DAYS, 3))
      out_mat <- array(0, dim = c(DAYS, DAYS, 3))
      
      # - pull the full 15-day IRFs for the endogenous covariate
      cov_resp <- irf_data %>%
        filter(cov == covariate, out == covariate) %>%
        # - remove 1st row: it's part of the set-up (repsonse at day 0)
        filter(day != 1) %>%
        mutate(day = day -1)
      
      # - pull the full 15-day IRFs for the particular outcome variable
      out_resp <- irf_data %>%
        filter(cov == covariate, out == outcome) %>%
        # - remove 1st row: it's part of the set-up (repsonse at day 0)
        filter(day != 1) %>%
        mutate(day = day -1)
      
      # - transforming the 15-day IRFs for the covariate and outcome to a wide
      #   3-column format (one column per estimate type: pe, lwr, upr)
      or_cov_resp <- cov_resp %>%
        dplyr::select(day, value, e_type) %>%
        spread(e_type, value) %>%
        dplyr::select(-day)
      
      or_out_resp <- out_resp %>%
        dplyr::select(day, value, e_type) %>%
        spread(e_type, value) %>%
        dplyr::select(-day)
      
      # - fill up the first rows of the scenario matrices with the original 
      #   1-day shock responses
      cov_mat[1,,1:3] <- or_cov_resp %>%
        as.matrix()
      out_mat[1,,1:3] <- or_out_resp %>%
        as.matrix()
      
      for (i in 2:DAYS) {
        # - iterating through the rest of the 15 days, beyond the 1st one
        # - chekcing first how much attention the covariate group is predicted 
        #   to pay to the issue in day i-1
        cov_att_pe <- sum(cov_mat[,(i-1),2])
        
        # - calculating how big a new shock needs to be in order for the 
        #   covariate group to keep its attention to 100%
        cov_new_shock <- 1 - cov_att_pe
        
        # - re-scaling the original 100 percentage point shock to the new shock
        cov_new_resp <- or_cov_resp[1:(DAYS-(i-1)),] * cov_new_shock
        out_new_resp <- or_out_resp[1:(DAYS-(i-1)),] * cov_new_shock
        
        # - adding the response to this new shock to the scenario matrices
        cov_mat[i,i:DAYS,1:3] <- cov_new_resp %>%
          as.matrix()
        out_mat[i,i:DAYS,1:3] <- out_new_resp %>%
          as.matrix()
      }
      # - saving the output for this cov --> out 
      new_rows <- rbind(
        data.frame(
          cov = covariate,
          value = colSums(out_mat[,,1]),
          out = outcome,
          day = 1:DAYS,
          e_type = "lwr",
          data_type = "structural"),
        data.frame(
          cov = covariate,
          value = colSums(out_mat[,,2]),
          out = outcome,
          day = 1:DAYS,
          e_type = "pe",
          data_type = "structural"),
        data.frame(
          cov = covariate,
          value = colSums(out_mat[,,3]),
          out = outcome,
          day = 1:DAYS,
          e_type = "upr",
          data_type = "structural")
      )
      new_irf_data <- rbind(new_irf_data, new_rows)
    }
  }

  irf_data$data_type <- "one_time_shock"
irf_data <- irf_data %>%
  # - correct the original data for the fact that day 1 is just pre-setting
  filter(day != 1) %>% 
  mutate(day = day -1)

all_irf_data <- rbind(irf_data, new_irf_data)

# - removing from the dataset cases in which covariate and outcome are the same
all_irf_data <- all_irf_data %>%
  filter(cov != out)

# - a wide version of the dataset, with a separate column for each estimate type
all_irf_data_wide <- all_irf_data %>%
  spread(e_type, value)

# - save a copy of this dataset (uncomment to overwrite the file)
#write.csv(all_irf_data_wide, paste0(data_path, "all_irf_data_wide.csv"),
#          row.names = FALSE)

# - simulate one-time and structural shocks of 10 instead of 100 percentage pts.
#   Present the results in 0-100 scale instead of 0-1
all_irf_data_wide <- all_irf_data %>%
  mutate(value = (value / 10) * 100) %>%
  spread(e_type, value)

all_irf_data_wide$cov <- recode(all_irf_data_wide$cov,
                                `lr` = "LR in Congress",
                                `majority` = "Majority in Congress",
                                `nupes` = "NUPES in Congress",
                                `rn` = "RN in Congress",
                                `lr_supp` = "LR Supporters",
                                `majority_supp` = "Majority Supporters",
                                `nupes_supp` = "NUPES Supporters",
                                `rn_supp` = "RN Supporters",
                                `attentive` = "Attentive Public",
                                `general` = "General Public",
                                `media` = "Media")

all_irf_data_wide$out <- recode(all_irf_data_wide$cov,
                                `lr` = "LR in Congress",
                                `majority` = "Majority in Congress",
                                `nupes` = "NUPES in Congress",
                                `rn` = "RN in Congress",
                                `lr_supp` = "LR Supporters",
                                `majority_supp` = "Majority Supporters",
                                `nupes_supp` = "NUPES Supporters",
                                `rn_supp` = "RN Supporters",
                                `attentive` = "Attentive Public",
                                `general` = "General Public",
                                `media` = "Media")

# - reorder the levels of the outcome and covariate factor variables
all_irf_data_wide$out <- factor(all_irf_data_wide$out,
                                levels = c("LR in Congress",
                                          "Majority in Congress",
                                           "NUPES in Congress",
                                           "RN in Congress",
                                           "LR Supporters",
                                           "Majority Supporters",
                                           "NUPES Supporters",
                                           "RN Supporters",
                                           "Attentive Public",
                                           "General Public",
                                           "Media"))

all_irf_data_wide$cov <- factor(all_irf_data_wide$cov,
                                levels = c("LR in Congress",
                                          "Majority in Congress",
                                           "NUPES in Congress",
                                           "RN in Congress",
                                           "LR Supporters",
                                           "Majority Supporters",
                                           "NUPES Supporters",
                                           "RN Supporters",
                                           "Attentive Public",
                                           "General Public",
                                           "Media"))

# - better labels for the data type
all_irf_data_wide$data_type <- recode(
  all_irf_data_wide$data_type,
  `one_time_shock` =  "Effect of a one time 10 percentage point attention increase at day 0",
  `structural` = "Effect of a structural 10 percentage point attention increase at day 0")


if (args$topic_model == 'lda') {
  write.csv(all_irf_data_wide,
          "data_prod/var/lda/onetime-structural-shock-irfs-results.csv",
          row.names = FALSE)
  final_input <- import("data_prod/var/lda/onetime-structural-shock-irfs-results.csv")
} else {
    write.csv(all_irf_data_wide,
          "data_prod/var/lda/onetime-structural-shock-irfs-results.csv",
          row.names = FALSE)
    final_input <- import("data_prod/var/bertopic/onetime-structural-shock-irfs-results.csv")
}

# DATA WRANLGING
#===============================================================================
# - exploring only 15-day effects, and removing the Media from the analysis.
plot_db <- final_input %>%
  filter(day == 15)

# - relabel the covariates so they fit/look better in the plot
plot_db$cov <- recode(plot_db$cov,
                      `LR in Congress` = "LR in\nCongress",
                      `Majority in Congress` = "Majority in\nCongress",
                      `NUPES in Congress` = "NUPES in\nCongress",
                      `RN in Congress` = "RN in\nCongress",
                      `LR Supporters` = "LR\nSupporters",
                      `Majority Supporters` = "Majority\nSupporters",
                      `NUPES Supporters` = "NUPES\nSupporters",
                      `RN Supporters` = "RN\nSupporters",
                      `Attentive Public` = "Attentive\nPublic",
                      `General Public` = "General\nPublic")

plot_db$out <- recode(plot_db$out,
                      `LR in Congress` = "LR in\nCongress",
                      `Majority in Congress` = "Majority in\nCongress",
                      `NUPES in Congress` = "NUPES in\nCongress",
                      `RN in Congress` = "RN in\nCongress",
                      `LR Supporters` = "LR\nSupporters",
                      `Majority Supporters` = "Majority\nSupporters",
                      `NUPES Supporters` = "NUPES\nSupporters",
                      `RN Supporters` = "RN\nSupporters",
                      `Attentive Public` = "Attentive\nPublic",
                      `General Public` = "General\nPublic")

# - reordering the covariate and outcome categories
plot_db$cov <- 
  plot_db$cov <- factor(plot_db$cov,
                        levels = rev(c("LR in\nCongress",
                                        "Majority in\nCongress",
                                        "NUPES in\nCongress",
                                        "RN in\nCongress",
                                        "LR\nSupporters",
                                        "Majority\nSupporters",
                                        "NUPES\nSupporters",
                                        "RN\nSupporters",
                                        "Attentive\nPublic",
                                        "General\nPublic")))

plot_db$out <- factor(plot_db$out,
                      levels = c("LR in\nCongress",
                                  "Majority in\nCongress",
                                  "NUPES in\nCongress",
                                  "RN in\nCongress",
                                  "LR\nSupporters",
                                  "Majority\nSupporters",
                                  "NUPES\nSupporters",
                                  "RN\nSupporters",
                                  "Attentive\nPublic",
                                  "General\nPublic"))

# - re-phrase the shock labels
plot_db$data_type <- ifelse(
  grepl("one time", plot_db$data_type),
  "The effect of a one time 10 percentage point increase in day 0             ",
  "The effect of a permanent 10 percentage point increase in day 0"
)

# OUTPUT -- FIGURE 2
#===============================================================================
if (args$topic_model == 'lda') {
  png("data_prod/figures/lda/figure2.png", width = 1600, height = 700)
} else {
  png("data_prod/figures/bertopic/figure2.png", width = 1600, height = 700)
}


ggplot(plot_db,
       aes(x = cov, y = pe, ymin = lwr, ymax = upr, col = data_type)) +
  geom_segment(aes(x = cov, xend = cov, y = lwr, yend = upr), 
               size = 2.5) +
  facet_wrap(~ out, nrow = 1) +
  coord_flip() +
  xlab("") +
  scale_y_continuous("\n15-day Responses (in percentage points)",
                     limits = c(0, 8), expand = c(0,0)) +
  scale_color_manual("",values = c("gray60", "gray10")) +
  theme(
    panel.spacing = unit(1.05, "lines"),
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text = element_text(size = 18),
    axis.text.y = element_text(hjust=0),
    strip.text = element_text(size = 20),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )
dev.off()
