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

parser <- ArgumentParser()

parser$add_argument("topic_model", help="Choose a model type between lda and bertopic")

args <- parser$parse_args()

if (!(args$topic_model %in% c('bertopic', 'lda'))){
  stop("The model name is incorrect. Choose between lda and bertopic")
}

#Put our main databse generated thanks to script 05a 
if (args$topic_model == 'lda') {
  db <- read_csv("data_prod/var/lda/general_TS.csv", show_col_types = FALSE)
  pol_issues <- c(1:99)
} else {
  db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE) 
  pol_issues <- c(0:211)
}

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

results_list <- list()

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

  no_stationarity_TS_data <- data.frame()
  for (topic_n in pol_issues) { 
    # Augmented Dickey-Fuller (ADF) test for stationarity
    db_topic <- db[db$topic == topic_n, ]
    data <- db_topic[[v]]
    if (sd(data) > 0) {
      adf <- adf.test(data)
      p_value <- adf$p.value
      if (p_value > 0.05) {  # Vérifier si la série est non stationnaire
        results_list <- append(results_list, list(list(topic_n, v, p_value)))
        }
    }
  }
}

results_df <- do.call(rbind, lapply(results_list, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
colnames(results_df) <- c("topic", "variable", "p_value")
non_full_stationarity_topics <- unique(results_df$topic)
stationary_topics <- setdiff(pol_issues, non_full_stationarity_topics)
print(paste("Number of topics where time series for each group are stationary: ", length(stationary_topics)))
cat("The topic numbers that satisfy this property:", stationary_topics, "\n")

'''
maindb <- db %>%
  filter(topic %in% pol_issues)

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'topic')

maindb$topic <- as.character(maindb$topic)
mformula <- formula(paste0("~", 
                           paste0(variables, collapse = " + ")))
model_data <- model.matrix(mformula, maindb[, variables])
model_data <- model_data[, 2:ncol(model_data)] # removing intercept


# - splitting the covariates of interest from the issue dummy variables
X_endogenous <- model_data[, which(!grepl("topic", colnames(model_data)))]
print(head(X_endogenous, 10))
X_exogenous <- model_data[, which(grepl("topic", colnames(model_data)))]
print(head(X_exogenous, 10))
# - estimating the model: 7 lags
var_model_merged <- VAR(y = X_endogenous, p = 7, exogen = X_exogenous)

#Inverser le log avant impélmentation 

var_irfs_cum_merged <- irf(var_model_merged, n.ahead = 60, cumulative = TRUE)

#Save
if (args$topic_model == 'bertopic') {
  save(var_model_merged, file = "data_prod/var/bertopic/var_model-MAIN.Rdata")
  save(var_irfs_cum_merged, file = "data_prod/var/bertopic/var_irfs-MAIN.Rdata")
} else {
  save(var_model_merged, file = "data_prod/var/lda/var_model-MAIN.Rdata")
  save(var_irfs_cum_merged, file = "data_prod/var/lda/var_irfs-MAIN.Rdata")
}
'''