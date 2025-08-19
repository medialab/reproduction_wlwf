library(tidyverse)
library(tidyr)
library(stats)
library(ggplot2)
library(vars)
library(boot)
library(rio)
library(tseries)
library(argparse)
library(stringr)
library(data.table)
library(lubridate)
library(urca)
source("utils_R.r")
library(dplyr)

parser <- ArgumentParser()

parser$add_argument("--estimate", action = "store_true",
help = "Run the script who estimate VAR and IRF")

parser$add_argument("--tests", action = "store_true",
                    help = "Activate to do the part where we test stationnarity, stationnarity after differentiation, PACF and ACF informations")

parser$add_argument("--tests_post", action = "store_true",
                    help = "Compute check tests after estimation with optimal parameters")

parser$add_argument("--number_irf", help="Choose a int who will represent the number of days in IRF calculation", type="integer", default=40)


args <- parser$parse_args()

variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'media')

if (args$estimate || args$tests || args$tests_post){
  #Put our main databse generated thanks to script 05a 
  db <- read_csv("data_prod/var/general_TS.csv", show_col_types = FALSE)
  throw_topic <- c(4, 17, 21, 25, 43, 50, 51, 54, 66, 70, 73, 75, 8, 16, 18, 19, 26, 38, 42, 49, 71, 80, 82, 84)
  pol_issues_temp <- setdiff(c(0:85), throw_topic)
  #db <- db %>% mutate(topic = ifelse(topic == 89, 74, topic)) %>%
      #group_by(date, topic) %>%                                  
      #summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") 
  pol_issues <- pol_issues_temp #setdiff(pol_issues_temp, 89)

  db <- db %>%
    filter(topic %in% pol_issues)

  write.csv(db, file="data_prod/var/general_TS_clean.csv")

  for (v in variables){
    db[[v]] <- log(db[[v]] + 1)
    db[[v]] <- scale(db[[v]], center=TRUE, scale=TRUE)[,1] 
  }
}

if (args$tests){
  print("Start testing process")
  print(paste("Nombre de topics", length(unique(db$topic))))
  results_list <- list()
  results_list2 <- list()
  infos_topic <- data.frame(matrix(NA, nrow=length(pol_issues), ncol=7)) #Topic, Stationnarité, VAR select (4), rank AC
  colnames(infos_topic) <- c("Topic", "Statio_type", "AIC", "HQ", "SC", "FPE", "AC_OK")
  statio_by_group <- data.frame(matrix(NA, nrow=length(pol_issues), ncol=length(variables)+1))
  colnames(statio_by_group) <- c('Topic', variables)
  statio_by_group$Topic <- pol_issues 
  for (v in variables){
    for (topic_n in pol_issues) {
      status <- NA
      # Augmented Dickey-Fuller (ADF) test for stationarity
      db_topic <- db[db$topic == topic_n, ]
      data <- db_topic[[v]]
      data <- as.ts(data)
      if (sd(data) > 0) {
        adf <- adf.test(data)
        p_value <- adf$p.value
        if (is.nan(p_value)) { #ADF ne gère pas certains cas. On va donc vérifier manuellement par ACF et PACF si les données sont stationnaires
          cat("P-value not calculated for topic:", topic_n, "and variable:", v, "because of data structure and weak variance of the time series. The variance is:", var(data), "A special treatment was done to determine stationnarity using ACF and PACF functions")
          acf_values <- acf(data, plot = FALSE, lag.max = 6)
          pacf_values <- pacf(data, plot = FALSE, lag.max = 6)

          if ((acf_values$acf[3] > 0.1 && acf_values$acf[4] > 0.1 && acf_values$acf[5] > 0.1) || (pacf_values$acf[3] > 0.1 && pacf_values$acf[4] > 0.1 && pacf_values$acf[5] > 0.1)) { #Si l'ACF et la PACF décroient pas assez rapidement, on suppose que la série est pas stationnaire
            results_list <- append(results_list, list(list(topic_n, v, p_value)))
          } else {
            status <- "OK"
          }
        } else if (p_value > 0.05) {  # Vérifier si la série est non stationnaire
          results_list <- append(results_list, list(list(topic_n, v, p_value)))
          status <- "PB"
        } else {
          status <- "OK"
        }
      } else {
        status <- "CST"
      }
    statio_by_group[statio_by_group$Topic == topic_n, v] <- status 
    } 
  }
  titles <- read_csv("data_prod/figures/translate_number_name/BERTOPIC_85.csv", col_names = FALSE, show_col_types=FALSE)
  colnames(titles) = c("Topic", "label")
  statio_by_group <- statio_by_group %>%
    mutate(n_OK = rowSums(across(everything(), ~ . == "OK")))

  statio_by_group <- merge(statio_by_group, titles, by="Topic", all.x = TRUE)
  write.csv(statio_by_group, file="data_prod/var/issue-level/statio_details.csv",  row.names = FALSE)
  cat("Résultats de stationnarité sur les séries concaténées \n")
  results_df <- do.call(rbind, lapply(results_list, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
  colnames(results_df) <- c("topic", "variable", "p_value")
  non_full_stationarity_topics <- unique(results_df$topic)

  list_full_top <- list()
  count_full_top <- 0
  for (topic_n in non_full_stationarity_topics){
      db_top <- results_df %>% filter(topic == topic_n)
      if (nrow(db_top) == length(variables)){
          count_full_top <- count_full_top + 1
          list_full_top <- append(list_full_top, topic_n) 
      }
  count_partial_topic <- length(non_full_stationarity_topics) - count_full_top
  }
  stationary_topics <- setdiff(pol_issues, non_full_stationarity_topics)
  cat("Number of topics where time series for each group are stationary: ", length(stationary_topics), "\n")
  cat("The topic numbers that satisfy this property:", stationary_topics, "\n")
  cat("Number of topics where time series for each group are not stationary: ", count_full_top, "\n")
  cat("These topics are : ", paste(list_full_top, collapse= " "), "\n")
  cat("Number of topics where time series are stationary and other are not according to group: ", count_partial_topic, "\n")

  ind_top <- 0
  for (topic in non_full_stationarity_topics){
    ind_top <- ind_top + 1
    db_topic <- db[db$topic == topic, ]
    for (v in variables) {
      data <- db_topic[[v]]
      data <- as.ts(data)
      if (sd(data) > 0) {
        adf <- adf.test(data)
        p_value <- adf$p.value
        if (!is.nan(p_value) && p_value > 0.05) {  # Vérifier si la série est non stationnaire
          data_diff <- diff(data)
          if (sd(data_diff) > 0) {
            adf2 <- adf.test(data_diff)
            p_value2 <- adf2$p.value
            if (is.nan(p_value2)) { #ADF ne gère pas certains cas. On va donc vérifier manuellement par ACF et PACF si les données sont stationnaires
              cat("P-value not calculated for topic:", topic_n, "and variable:", v, "because of data structure and weak variance of the differentiated time series. The variance is:", var(data_diff), "A special treatment was done to determine stationnarity using ACF and PACF functions")
            acf_values <- acf(data, plot = FALSE, lag.max = 6)
            pacf_values <- pacf(data, plot = FALSE, lag.max = 6)

              if ((acf_values$acf[3] > 0.1 && acf_values$acf[4] > 0.1 && acf_values$acf[5] > 0.1) || (pacf_values$acf[3] > 0.1 && pacf_values$acf[4] > 0.1 && pacf_values$acf[5] > 0.1)) { #Si l'ACF et la PACF décroient pas assez rapidement, on suppose que la série est pas stationnaire
                results_list2 <- append(results_list, list(list(topic_n, v, p_value)))
              }
            }
            else if (p_value2 > 0.05) {
              results_list2 <- append(results_list2, list(list(topic, v, p_value2)))
            }
          }
        }
      }
    }
  }



  if (length(results_list2) == 0) {
    cat("results_list2 est vide, aucun résultat à transformer en dataframe. \n")
    stationary_topics2 <- non_full_stationarity_topics
  } else {
    cat("results_list2 contient des données. \n")
    results_df2 <- do.call(rbind, lapply(results_list2, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
    colnames(results_df2) <- c("topic", "variable", "p_value")
    non_full_stationarity_topics2 <- unique(results_df2$topic)
    stationary_topics2 <- setdiff(non_full_stationarity_topics, non_full_stationarity_topics2)
    print(results_df2)
  }
  merged_statio <- c(stationary_topics, stationary_topics2)

  cat("Number of topics where time series for each group are stationary or I(1): ", length(merged_statio), "\n")
  cat("The topic numbers that satisfy this property:", merged_statio, "\n")

  ACF_data <- data.frame(matrix(NA, nrow = length(variables)*2, ncol = length(pol_issues)))
  PACF_data <- data.frame(matrix(NA, nrow = length(variables)*2, ncol = length(pol_issues)))
  colnames(ACF_data) <- as.character(pol_issues) 
  colnames(PACF_data) <- as.character(pol_issues)
  rownames(ACF_data) <- c(variables, paste0(variables, "_diff"))
  rownames(PACF_data) <- c(variables, paste0(variables, "_diff"))
  iter_info <-0
  list_const <- c()
  for (topic in pol_issues) {
    iter_info <- iter_info + 1
    infos_topic[iter_info, "Topic"] <- as.character(topic)
    if (topic %in% non_full_stationarity_topics){
      if(topic %in% unlist(list_full_top)){
        infos_topic[iter_info, "Statio_type"] <- "I(1)"
      } else if (topic %in% merged_statio){
        infos_topic[iter_info, "Statio_type"] <- "Mixed"
      } else {
        infos_topic[iter_info, "Statio_type"] <- "Other"
      }
    } else {
      infos_topic[iter_info, "Statio_type"] <- "I(0)"
    }
    topic_str <- as.character(topic)
    db_topic <- db[db$topic == topic, ]
    for (v in variables) {
      data <- db_topic[[v]]
      data <- as.ts(data)
      if (sd(data) == 0) {
        list_const <- c(list_const, topic)
      } else{
        data_differ <- diff(data)

        acf_values <- acf(data, lag.max = 30, plot = FALSE)$acf
        pacf_values <- pacf(data, lag.max = 30, plot = FALSE)$acf
        acf_values_diff <- acf(data_differ, lag.max = 30, plot = FALSE)$acf
        pacf_values_diff <- pacf(data_differ, lag.max = 30, plot = FALSE)$acf
      
      # Trouve les premiers lags où ACF et PACF < 0.1
      below_threshold_acf <- which(abs(acf_values) < 0.1)
      below_threshold_pacf <- which(abs(pacf_values) < 0.1)
      below_threshold_acf_diff <- which(abs(acf_values_diff) < 0.1)
      below_threshold_pacf_diff <- which(abs(pacf_values_diff) < 0.1)
      
      # ACF
      if (length(below_threshold_acf) > 0) {
        number_lag_acf <- below_threshold_acf[1]
      } else {
        number_lag_acf <- 31
      }

      if (length(below_threshold_acf_diff) > 0) {
        number_lag_acf_diff <- below_threshold_acf_diff[1]
      } else {
        number_lag_acf_diff <- 31
      }
      
      # PACF
      if (length(below_threshold_pacf) > 0) {
        number_lag_pacf <- below_threshold_pacf[1]
      } else {
        number_lag_pacf <- 31
      }    
      
      if (length(below_threshold_pacf_diff) > 0) {
        number_lag_pacf_diff <- below_threshold_pacf_diff[1]
      } else {
        number_lag_pacf_diff <- 31
      }
      ACF_data[v, topic_str] <- number_lag_acf
      PACF_data[v, topic_str] <- number_lag_pacf
      ACF_data[paste0(v, "_diff"), topic_str] <- number_lag_acf_diff
      PACF_data[paste0(v, "_diff"), topic_str] <- number_lag_pacf_diff
      }
    }
  }

  # Résumé du nombre de séries constantes
  list_const <- unique(list_const)
  cat("Nombre de séries constantes :", length(list_const), "\n")
  cat("Then, we will remove the following topics from analysis : \n")
  print(list_const)

  acf_sum <- summary(t(ACF_data))
  pacf_sum <- summary(t(PACF_data))

  acf_exp <- transfo_acf(acf_sum)
  pacf_exp <- transfo_acf(pacf_sum)

  #Afficher et enregistrer les résultats
  write.csv(ACF_data, file=paste0("data_prod/var/issue-level/ACF_full.csv"))
  write.csv(PACF_data, file=paste0("data_prod/var/issue-level/PACF_full.csv"))
  write.csv(acf_exp, file=paste0("data_prod/var/issue-level/ACF_results.csv"))
  write.csv(pacf_exp, file=paste0("data_prod/var/issue-level/PACF_results.csv"))
  
  plot_PACFS(ACF_data, "ACF")
  plot_PACFS(PACF_data, "PACF")
  
   #Présence de constantes
  for (topic_num in pol_issues){
    if (topic_num %in% list_const){
      next
    } 
    topic_num <- as.character(topic_num)
    db_topic <- db %>% filter(as.character(topic) == topic_num)
    db_topic <- db_topic[, variables, drop = FALSE]
    db_topic <- diff(as.matrix(db_topic), differences = 1)
    #db_topic <- scale(as.matrix(db_topic))
    db_topic <- as.data.frame(db_topic)
    print(paste("Currently testing topic", topic_num))
    #Selection criteria
    AIC_and_co <- VARselect(db_topic, lag.max = 20, season = NULL)
    row_index <- which(as.character(infos_topic$Topic) == topic_num)
    infos_topic[row_index, "AIC"] <- AIC_and_co$selection[[1]]
    infos_topic[row_index, "HQ"] <- AIC_and_co$selection[[2]]
    infos_topic[row_index, "SC"] <- AIC_and_co$selection[[3]]
    infos_topic[row_index, "FPE"] <- AIC_and_co$selection[[4]]

    for (numlag in 1:20){
      var_model <- VAR(db_topic, p=numlag, type="const")
      APT <- auto.portmanteau.test(var_model, correction="holm")
      nb_accepted <- APT %>% 
        summarise(n = sum(H0 == "accepted")) %>%
        pull(n)
      if (nb_accepted == length(variables)){
        infos_topic[row_index, "AC_OK"] <- numlag
        break
      } else {
        if (numlag==20){
          infos_topic[row_index, "AC_OK"] <- "Inf"
        }
      }
    }
  }

  path_info <- "data_prod/var/issue-level/infos_topics.csv"
  write.csv(infos_topic, file=path_info, row.names = FALSE)
}

if (args$estimate){
  path_infos_topic <- "data_prod/var/issue-level/infos_topics.csv"
  if(!(file.exists(path_infos_topic))){
    stop("Please, run --tests before to estimate the model, the file to determine lags number in VAR process doesn't exist.")
  }
  print("Estimation step")
  infos_topic <- read_csv(path_infos_topic, show_col_types=FALSE)
  infos_topic <- as.data.frame(infos_topic)
  exclude_issues <- c(39, 69, 79) #Présence de constantes
  list_topic_iter = as.character(setdiff(pol_issues, exclude_issues))
  db$topic <- as.character(db$topic)
  for (topic_num in list_topic_iter){
    print(paste("Model calculated", topic_num))
    #Choose lag number according to Schwarz criterion and avoid serial autocorrelation problem
    topic_num <- as.character(topic_num)
    row_index <- which(as.character(infos_topic$Topic) == topic_num)
    values_lag <- c(infos_topic[row_index, "SC"], infos_topic[row_index, "AC_OK"])
    lag_number <- as.integer(max(values_lag, na.rm = TRUE))
    db_topic <- db %>% filter(as.character(topic) == topic_num)
    db_topic <- db_topic[, variables, drop = FALSE]
    db_topic <- diff(as.matrix(db_topic), differences = 1)
    #db_topic <- scale(as.matrix(db_topic))
    db_topic <- as.data.frame(db_topic)
    var_model <- VAR(db_topic, p=lag_number, type="const")
    save(var_model, file = paste0("data_prod/var/issue-level/var_model_", topic_num, ".Rdata"))
    var_irfs_cum <- irf.varest.edit(var_model,n.ahead = 60, irf_type = "generalized", cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 500) #Calculate cumulative GIRF 
    save(var_irfs_cum, file = paste0("data_prod/var/issue-level/var_girf_topic_", topic_num, ".Rdata"))
  }
}

if(args$tests_post){
  exclude_issues <- c(39, 69, 79) #Présence de constantes
  list_topic_iter = as.character(setdiff(pol_issues, exclude_issues))
  infos_topic_post <- data.frame(matrix(NA, nrow=0, ncol=4))
  colnames(infos_topic_post) <- c("Topic", "Max_Modul", "Serial_AC", "Norm.")
  for (topic_num in list_topic_iter){
    print(paste("Posterior tests for topic", topic_num))
    var_path <- paste0("data_prod/var/issue-level/var_model_", topic_num, ".Rdata")
    if (!(file.exists(var_path))){
      stop("run estimate option because the model weren't estimated")
    }
    load(var_path)
    var_roots <- tryCatch({
      roots(var_model)
      }, error = function(e) {
      warning(paste("Problème de calcul dans roots() pour le topic", topic_num, ":", e$message))
      return(Inf)  
      })


    if (all(is.finite(var_roots))) {
      max_mod <- max(var_roots)
    } else {
      max_mod <- Inf
    }

    #Serial autocorrelation test robust to heteroskedasticity 
    APT <- auto.portmanteau.test(var_model, correction="holm")
    nb_accepted <- APT %>% 
      summarise(n = sum(H0 == "accepted")) %>%
      pull(n)
    if (nb_accepted==length(variables)){
      AC_Info <- "OK"
    } else {
      AC_Info <- "PB"
    }

    #Normality Test
    p_val_norm <- tryCatch({
      res <- normality.test(var_model)$jb.mul$JB$p.value
      res[1]
      }, error = function(e) {
      warning(paste("It wasn't possible to run normality test for topic", topic_num, ":", e$message))
      return(NA)  
      })
    
    if(!(is.na(p_val_norm))){
      if (p_val_norm < 0.05) {
        N_Info <- 0
      } else {
        N_Info <- 1
      }
    } else {
      N_Info <- NA
    }
    new_row <- c(topic_num, max_mod, AC_Info, N_Info)
    infos_topic_post <- rbind(infos_topic_post, new_row)
  }
    
  path_post <-  "data_prod/var/issue-level/post_checks.csv"
  colnames(infos_topic_post) <- c("Topic", "Max_Modul", "Serial_AC", "Norm.")
  write.csv(infos_topic_post, file=path_post, row.names = FALSE)
} 
throw_topic <- c(4, 17, 21, 25, 43, 50, 51, 54, 66, 70, 73, 75, 8, 16, 18, 19, 26, 38, 42, 49, 71, 80, 82, 84, 39, 69, 79)
pol_issues <- setdiff(c(0:85), throw_topic)
last_topic <- tail(pol_issues, 1)
last_top_path <- paste0("data_prod/var/issue-level/var_girf_topic_", last_topic, ".Rdata")
if (!file.exists(last_top_path)){
  stop(paste("Tous les GIRFS n'ont pas été estimés, veuillez recommencer avec l'option --estimate"))
}

print("Format IRF data in a human-friendly way")
pa2our <- read_csv("data_prod/figures/translate_number_name/BERTOPIC_85.csv", col_names=FALSE, show_col_types=FALSE)
colnames(pa2our) <- c("issue_num", "label")
pa2our$issue_num = as.character(pa2our$issue_num)

#L'objet var_irf_cums contient dans $irf$lr les réponses générées par un impulse de lr 
# - initializing an empty dataset where to put all IRF info by topic
irf_data <- as.data.frame(matrix(NA, nrow=0, ncol=6))
colnames(irf_data) <- c("topic", "cov", "out", "pe", "lwr", "upr")
total <- length(pol_issues)
counter <- 0
number_irf <- args$number_irf
row_number <- number_irf + 1
for (top in pol_issues) {
  counter <- counter + 1
  print(paste0("[", counter, "/", total, "]"))
  file_name <- paste0("data_prod/var/issue-level/var_girf_topic_", top, ".Rdata")
  load(file_name) # object name: 'var_irfs_cum'
  girf <- var_irfs_cum 
  # - iterating through endogenous covariates and endogenous responses
  covs <- names(girf$irf)
  for (covariate in covs) {
      new_rows <- data.frame(
        topic= rep(as.character(top), length(covs)), 
        cov = rep(covariate, length(covs)),
        out = covs,
        pe = girf$irf[[covariate]][row_number,],
        lwr = girf$Lower[[covariate]][row_number,],
        upr = girf$Upper[[covariate]][row_number,]
      )
    irf_data <- rbind(irf_data, new_rows)
  }
}

irf_plot <- irf_data
agenda_type <- data.frame(
  var = variables,
  type = c("pol", "pol", "pol", "pol", "pub", "pub", "pub", "pub", "pub", "media")
)
cov_agenda_type <- agenda_type %>%
  rename(cov = var, cov_agenda_type = type)
out_agenda_type <- agenda_type %>%
  rename(out = var, out_agenda_type = type)

cov_agenda_type$cov <- as.character(cov_agenda_type$cov)
out_agenda_type$out <- as.character(out_agenda_type$out)
irf_plot$cov <- as.character(irf_plot$cov)
irf_plot$out <- as.character(irf_plot$out)

irf_plot <- left_join(irf_plot, cov_agenda_type)
irf_plot <- left_join(irf_plot, out_agenda_type)

irf_plot <- irf_plot %>%
  arrange(out, cov, topic) %>%
  filter(cov != out)

irf_plot <- left_join(irf_plot, pa2our, by = c("topic" = "issue_num"))

write.csv(irf_plot, file="data_prod/var/irf_data.csv", row.names = FALSE)

irf_data <- irf_plot #Cov (origine impulse) : ligne , Out (reçoit impulse) : colonne 
n_topic <- length(unique(irf_data$topic))
variables <- c('lr', 'majority', 'nupes', 'rn',
               'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp',
               'attentive', 'media')
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
filt_irf <- irf_data %>% 
  filter(sign(lwr) == sign(upr)) %>%
  mutate(
    cov = recode(cov,
                 `lr` = "Députés LR",
                 `majority` = "Députés Majorité",
                 `nupes` = "Députés NUPES",
                 `rn` ="Députés RN",
                 `lr_supp` = "Supporters LR",
                 `majority_supp` = "Supporters Majorité",
                 `nupes_supp` =   "Supporters NUPES",
                 `rn_supp` =  "Supporters RN",
                 `attentive` = "Public Attentif",
                 `media` = "Média"
    ),
    out = recode(out,
                 `lr` = "Députés LR",
                 `majority` = "Députés Majorité",
                 `nupes` = "Députés NUPES",
                 `rn` ="Députés RN",
                 `lr_supp` = "Supporters LR",
                 `majority_supp` = "Supporters Majorité",
                 `nupes_supp` =   "Supporters NUPES",
                 `rn_supp` =  "Supporters RN",
                 `attentive` = "Public Attentif",
                 `media` = "Média"
    )
  )

#Tables 
#Top themes by LF pairs 
all_top3 <- data.frame(matrix(NA, nrow=0, ncol=5))
for(covar in readable_variables){
  for (outvar in readable_variables){
    if (covar == outvar){
      next
    }
    top3 <- filt_irf %>% 
      filter(cov == covar) %>%
      filter (out == outvar) %>%
      arrange(desc(abs(pe))) %>%              # Trier par pe décroissant
      dplyr::select(cov, out, pe, label) %>%    # Garder uniquement les colonnes voulues
      slice_head(n = 3) %>%
      mutate(rank=row_number())
    all_top3 <- rbind(all_top3, top3)
  }
}

colnames(all_top3) <- c("cov", "out", "pe", "label", "rank")

write.csv(all_top3, file="data_prod/var/irf-analysis/full_top3.csv", row.names=FALSE)

#Top 3 influences by topic
top3_topic <- filt_irf %>%
        group_by(topic, cov, label) %>%
        summarise(sum_pe = sum(pe, na.rm = TRUE),
      .groups='drop') %>%
    group_by(topic) %>%
    slice_max(order_by = sum_pe, n = 3, with_ties = TRUE)  %>% 
    ungroup()

top3_topic$rank <- rep(1:3, n_topic)

topics_leaders <- top3_topic %>%
      count(cov, rank) %>%
    tidyr::pivot_wider(
      names_from = cov,
      values_from = n,
      values_fill = 0  # remplit les NA par 0
    )

write.csv(topics_leaders, file="data_prod/var/irf-analysis/leader_bytopic_top3.csv", row.names=FALSE)

#Top 3 leading topics by group
top3_topics_group <- filt_irf %>%
          group_by(cov, topic, label) %>%
          summarise(sum_pe = sum(pe, na.rm= TRUE), .groups='drop') %>%
          group_by(cov) %>%
          slice_max(order_by = sum_pe, n = 3, with_ties = TRUE) %>%
          ungroup()

top3_topics_group$rank <- rep(1:3, length(variables))

groups_leaders <- top3_topics_group %>%
             dplyr::select(cov, rank, label) %>%
                tidyr::pivot_wider(
                  names_from = cov,
                  values_from = label
                )

write.csv(groups_leaders, file="data_prod/var/irf-analysis/topiclead_bygroup_top3.csv", row.names=FALSE)
#Plots : number leader, follower

#Matrix lead follow
matrix_LF <- filt_irf %>%
    count(cov, out) %>%
    complete(cov = readable_variables, out = readable_variables, fill = list(n = 0))  %>%
    mutate(
      cov = factor(cov, levels = readable_variables),
      out = factor(out, levels = readable_variables)
    )

png("data_prod/var/irf-analysis/number_leading_relations_pairs.png",width = 800, height = 600)
p <- ggplot(matrix_LF, aes(x = out, y = cov, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = n), color = "black", size = 3) + 
  scale_fill_gradient(low = "#efffff", high = "#FF0000") +
  theme_minimal(base_size = 14) +
  labs(title = "Number of topic with significative influence",
       x = "outcome", y = "origin", fill = "Occurrences") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))
  print(p)
  dev.off()


matrix_LF <- filt_irf %>%
  group_by(cov, out) %>%
  summarise(pe_sum = round(sum(pe, na.rm = TRUE), 3), .groups = "drop") %>%
  complete(cov = readable_variables, out = readable_variables, fill = list(pe_sum = 0)) %>%
  mutate(
    cov = factor(cov, levels = readable_variables),
    out = factor(out, levels = readable_variables)
  )

png("data_prod/var/irf-analysis/total_GIRF_relations_pairs.png",width = 800, height = 600)
p <- ggplot(matrix_LF, aes(x = out, y = cov, fill = pe_sum)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = pe_sum), color = "black", size = 3) + 
  scale_fill_gradient(low = "#efffff", high = "#FF0000") +
  theme_minimal(base_size = 14) +
  labs(title = "Cumulative impulse by pair",
       x = "outcome", y = "origin", fill = "Occurrences") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))
print(p)
dev.off()

#Figure 4 Barbera
plot_db <- irf_data  %>%
          mutate(
            cov = recode(cov,
                         `lr` = "Députés LR",
                         `majority` = "Députés Majorité",
                         `nupes` = "Députés NUPES",
                         `rn` ="Députés RN",
                         `lr_supp` = "Supporters LR",
                         `majority_supp` = "Supporters Majorité",
                         `nupes_supp` =   "Supporters NUPES",
                         `rn_supp` =  "Supporters RN",
                         `attentive` = "Public Attentif",
                         `media` = "Média"
            ),
            out = recode(out,
                         `lr` = "Députés LR",
                         `majority` = "Députés Majorité",
                         `nupes` = "Députés NUPES",
                         `rn` ="Députés RN",
                         `lr_supp` = "Supporters LR",
                         `majority_supp` = "Supporters Majorité",
                         `nupes_supp` =   "Supporters NUPES",
                         `rn_supp` =  "Supporters RN",
                         `attentive` = "Public Attentif",
                         `media` = "Média"
            )
          ) %>%
          mutate (
            cov = factor(cov, levels=readable_variables),
            out = factor(out, levels=readable_variables)
          ) %>%
          filter(cov_agenda_type != out_agenda_type | cov_agenda_type == "pol") %>%
          filter(sign(lwr) == sign(upr)) %>%
          group_by(topic, out) %>%
          slice_max(order_by = abs(pe), n = 2, with_ties = TRUE) %>%
          ungroup() %>%
          mutate(label = factor(label, levels = unique(label)))

colors_dict <- c(
  "Députés LR" = "blue",
  "Députés Majorité" = "darkorange1",
  "Députés NUPES" = "red",
  "Députés RN" = "gray19",
  "Supporters LR" = "cyan3",
  "Supporters Majorité" = "gold",
  "Supporters NUPES"= "orchid1",
  "Supporters RN" = "gray68",
  "Public Attentif"= "darkorchid3", 
  "Média" = "green4"
)
png("data_prod/var/irf-analysis/figure4.png", width = 1600, height = 1400)       
p <- ggplot(plot_db,
       aes(x = label, y = pe, ymin = lwr, ymax = upr)) +
  geom_pointrange(aes(col = cov), alpha = 0.4, size = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~out, nrow = 1) +
  coord_flip() +
  xlab("") +
  ylab(paste("\nThe effect of a standard error impulse", args$number_irf, "days ago by the covariate group, measured in std(Δresponse)")) +
  scale_color_manual("", values = colors_dict) +
  theme(
    panel.spacing = unit(1.25, "lines"),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text.x = element_text(size = 10, angle=45),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14, margin = margin(t = 20), vjust = 5)
  )
print(p)
dev.off()

#Start prepare aggregated figures
plot_db2 <- filt_irf %>%
          group_by(cov, out, cov_agenda_type, out_agenda_type) %>%
          summarise(
            lwr_sum = sum(lwr, na.rm = TRUE),
            pe_sum = sum(pe, na.rm = TRUE),
            upr_sum = sum(upr, na.rm = TRUE),
            .groups = "drop"
          )  %>%
          mutate(
            lwr_mean = lwr_sum / n_topic,
            pe_mean = pe_sum / n_topic, 
            upr_mean = upr_sum / n_topic
          ) %>%
          dplyr::select(-all_of(c("lwr_sum", "pe_sum", "upr_sum")))  %>%   
          mutate(
            cov = factor(cov, levels = readable_variables),
            out = factor(out, levels = readable_variables)
          )  %>%
          rename(
            lwr = lwr_mean,
            pe = pe_mean,
            upr = upr_mean
          ) 

#Figure : Influence entre députés 
plot_db <- plot_db2 %>%
            filter(cov_agenda_type == 'pol' & out_agenda_type=='pol')

plot_db$cov <- factor(plot_db$cov,
                      levels = rev(readable_variables[1:4]))
png("data_prod/var/irf-analysis/girf_between_deputes.png",width = 1000, height = 800)
p <- ggplot(plot_db,
       aes(x = cov, y = pe, ymin = lwr, ymax = upr)) +
  geom_segment(aes(x = cov, xend = cov, y = lwr, yend = upr), 
               size = 2.5) +
  facet_wrap(~ out, nrow = 1) +
  coord_flip() +
  xlab("") +
  scale_y_continuous(paste0("\n", args$number_irf,"-day Responses (in std(Δresponse)) of a one standard error shock of std(Δimpulse)"),
                     limits = c(0, 0.3), expand = c(0,0)) +
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text = element_text(size = 15),
    axis.text.y = element_text(hjust=0),
    strip.text = element_text(size = 15),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )
print(p)
dev.off()    

#Figure 3 Barbera
plot_db <- plot_db2 %>%
            filter(cov_agenda_type != "media", out_agenda_type != "media") %>%
            filter(cov_agenda_type != out_agenda_type) %>%
            mutate(
              polgroup = ifelse(cov_agenda_type == "pol", as.character(cov), as.character(out)),
              pubgroup = ifelse(cov_agenda_type == "pub", as.character(cov), as.character(out)),
              var1 = cov,
              var2 = out,
              direction = ifelse(cov_agenda_type == "pol" & out_agenda_type == "pub", "députés→public", 
                                 ifelse(cov_agenda_type == "pub" & out_agenda_type == "pol", "public→députés", NA))
            ) %>%
            dplyr::select(polgroup, pubgroup, direction, pe, lwr, upr)%>%
              mutate(
                polgroup_f = factor(polgroup),
                polgroup_num = as.numeric(polgroup_f) + ifelse(direction == "députés→public", -0.15, 0.15)
              )


png("data_prod/var/irf-analysis/figure3.png",width = 1000, height = 1000)
p <- ggplot(plot_db,
       aes(x = polgroup_num, y = pe, ymin = lwr, ymax = upr, col = direction)) +
  geom_segment(aes(xend = polgroup_num, y = lwr, yend = upr),
               size = 4, alpha = 1) +  # retirer position_dodge ici
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~ pubgroup, nrow = 1) +
  scale_x_continuous(
    name = "",
    breaks = unique(as.numeric(plot_db$polgroup_f)),
    labels = levels(plot_db$polgroup_f)
  ) +
  coord_flip() +
  ylab(paste0("\n", args$number_irf,"-day cumulative effect of one standard error shock in day 0")) +
  scale_color_manual("", values = c("gray70", "gray30")) +
  theme(
    panel.spacing = unit(1.1, "lines"),
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 8, angle=45),
    strip.text = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(fill = "gray80", color = "black"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text.y = element_text(hjust=0),
    plot.margin = margin(t = 10, r = 40, b = 10, l = 10)  
  )

print(p)
dev.off()
#Figure 6 Barbera
plot_db <- plot_db2 %>%
          filter(cov_agenda_type == 'media' | out_agenda_type == 'media') %>%
          mutate(data_type = ifelse(cov_agenda_type == "media", "média→groupe", 
                                    ifelse(out_agenda_type == "media", "groupe→média", NA))) %>%
          mutate(y = ifelse(cov_agenda_type == 'media', as.character(out), as.character(cov)))


png("data_prod/var/irf-analysis/figure6.png",width = 800, height = 800)
p <- ggplot(plot_db,
       aes(x = y, y = pe, ymin = lwr, ymax = upr)) +
  geom_segment(aes(x = y, xend = y, y = lwr, yend = upr), 
               size = 4, alpha = 0.6) +
  #geom_segment(alpha = 0.8, size = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  facet_grid(~data_type) +
  coord_flip() +
  geom_vline(xintercept = 15) +
  xlab("") +
  scale_y_continuous(paste0("\nThe ", args$number_irf, "-day cumulative effect of a one standard error shock of std(Δimpulse) in day 0"),
                     expand = c(0,0.001)) +
  theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(hjust=0),
    strip.text = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 16)
  )
print(p)
dev.off()

#Checking figures 
annot <- read_csv("annot.csv", show_col_types = FALSE)
annot <- annot %>% 
        filter(topic %in% pol_issues) %>%
        dplyr::select(topic,semantic_validity_random)

annot$topic <- as.character(annot$topic)

irf_attentive <- irf_plot %>% 
                filter(cov == "attentive") %>% 
                mutate(val = case_when(
                  sign(lwr) == sign(upr) ~ pe, 
                  !(sign(lwr) == sign(upr)) ~ 0
                )) %>%
              dplyr::select(topic,val) %>%
              group_by(topic) %>%
              summarise(across(where(is.numeric), sum, na.rm = TRUE))

plot_irf <- left_join(annot, irf_attentive, by="topic") %>%
            arrange(semantic_validity_random)

png("data_prod/var/check/semantic_girfval.png", width = 800, height = 800)
p <- ggplot(plot_irf, aes(x=semantic_validity_random, y = val)) +
    geom_point(size = 3, fill='darkblue') + 
    geom_smooth(method = "lm")
print(p)
dev.off()

db <- read_csv("data_prod/var/general_TS.csv", show_col_types = FALSE)
throw_topic <- c(4, 17, 21, 25, 43, 50, 51, 54, 66, 70, 73, 75, 8, 16, 18, 19, 26, 38, 42, 49, 71, 80, 82, 84)
pol_issues <- setdiff(c(0:85), throw_topic)

db <- db %>%
  filter(topic %in% pol_issues)

db_size_top <- db %>% 
              dplyr::select(-date) %>%
              group_by(topic) %>%
              summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
              ungroup() %>% 
              mutate(total = rowSums(across(-topic)))

db_size_top$topic = as.character(db_size_top$topic)
plot_irf <- left_join(db_size_top, irf_attentive, by="topic") %>%
            arrange(total)

png("data_prod/var/check/sizetop_girfval.png", width = 800, height = 800)
p <- ggplot(plot_irf, aes(x=total, y = val)) +
    geom_point(size = 3, fill='darkblue') + 
    geom_smooth(method = "lm")
print(p)
dev.off()

for (v in variables){
  db[[v]] <- log(db[[v]] + 1)
  db[[v]] <- scale(db[[v]], center=TRUE, scale=TRUE)[,1] 
}
db_long <- db %>%
          dplyr::select(all_of(variables)) %>%
          pivot_longer(cols = everything(), names_to = "variable", values_to = "valeur") %>%
          mutate(
            variable = recode(variable,
                         `lr` = "Députés LR",
                         `majority` = "Députés Majorité",
                         `nupes` = "Députés NUPES",
                         `rn` ="Députés RN",
                         `lr_supp` = "Supporters LR",
                         `majority_supp` = "Supporters Majorité",
                         `nupes_supp` =  "Supporters NUPES",
                         `rn_supp` =  "Supporters RN",
                         `attentive` = "Public Attentif",
                         `media` = "Média"
            ))

png("data_prod/var/check/groups_FDR.png", width = 1200, height = 1200)
p <- ggplot(db_long, aes(x = valeur, color=variable)) +
  stat_ecdf(size = .5) +
  scale_color_manual("", values = colors_dict) +
  labs(x = "Valeur", y = "Fonction de répartition F(x)", color = "Variable") +
  theme_minimal()
print(p)
dev.off()

