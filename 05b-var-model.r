#install.packages(c("rio", "vars", "tseries"))

library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(vars)
library(boot)
library(rio)
library(tseries)

#Put our main databse generated thanks to script 05a 
db <- read_csv("/store/medialex/reproduction_wlwf/data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE)

#Keep only political topics
pol_issues <- c(0,1,2,3,4,5,6) #Insert numbers

db <- db %>%
  filter(topic %in% pol_issues)

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')
topic_values <- unique(db$topic)
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

  na_positions <- which(is.na(logit_x))  # Récupérer les positions des NA
  print(c("step 1", v, na_positions))
  db[,v] <- logit_x

  na_positions <- which(is.na(db[[v]]))  # Récupérer les positions des NA
  print(c("step 2", v, na_positions))

'
  for (topic_n in topic_values) { 
    # Augmented Dickey-Fuller (ADF) test for stationarity
    db_topic <- db[db$topic == topic_n, ]
    data <- db_topic[[v]]

    cat("ADF Test for variable:", v, "topic", topic_n, "\n")
    print(adf.test(data))
    } 
    '
}

maindb$topic <- as.character(maindb$topic)
mformula <- formula(paste0("~", 
                           paste0(variables, collapse = " + ")))
model_data <- model.matrix(mformula, maindb[, variables])
model_data <- model_data[, 2:ncol(model_data)] # removing intercept

# - splitting the covariates of interest from the issue dummy variables
X_endogenous <- model_data[, which(!grepl("topic", colnames(model_data)))]
X_exogenous <- model_data[, which(grepl("topic", colnames(model_data)))]

# - estimating the model: 7 lags
var_model_merged <- VAR(y = X_endogenous, p = 7, exogen = X_exogenous)

var_irfs_cum_merged <- irf(var_model_merged, n.ahead = 60, cumulative = TRUE)

#Save
save(var_model_merged, file = "data_prod/var/bertopic/var_model-MAIN.Rdata")
save(var_irfs_cum_merged, file = "data_prod/var/bertopic/var_irfs-MAIN.Rdata")
