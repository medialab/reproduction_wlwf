#install.packages(c("rio", "vars", "tseries"))
#remotes::install_github(repo = "cran/pco")
install.packages("resample")
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
library(plm)
library(panelvar)
library(resample)

parser <- ArgumentParser()

parser$add_argument("topic_model", help="Choose a model type between lda and bertopic")

parser$add_argument("--estimate", action = "store_true",
help = "Run the script who estimate PVAR and IRF")

parser$add_argument("--tests", action = "store_true",
                    help = "Activate to test the absence of unit roots and to print the selection criteria")

parser$add_argument("--number_irf", help="Choose a int who will represent the number of days in IRF calculation", type="integer", default=21)


args <- parser$parse_args()

if (!(args$topic_model %in% c('bertopic', 'lda'))){
  stop("The model name is incorrect. Choose between lda and bertopic")
}

if (!(args$estimate)){
  if (args$topic_model == 'lda') {
    if (!file.exists("data_prod/var/lda/Pvar_model-MAIN.Rdata") ||
    !file.exists("data_prod/var/lda/Pvar_irfs-MAIN.Rdata")) {
      stop("No var and irf files were found. Please run the --estimate option")
      }
  } else {
    if (!file.exists("data_prod/var/bertopic/Pvar_model-MAIN.Rdata") ||
    !file.exists("data_prod/var/bertopic/Pvar_irfs-MAIN.Rdata")) {
      stop("No var and irf files were found. Please run the --estimate option")
    }
  }
}

if (args$estimate){
  count_msg <- 0
  variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')
  #Put our main databse generated thanks to script 05a 
  print("Files recuperation and preprocessing")
  if (args$topic_model=='lda'){
  db <- read_csv("data_prod/var/lda/general_TS.csv", show_col_types = FALSE)
  db <-  db %>% mutate(topic = ifelse(topic == 55, 16, topic)) %>% 
              mutate(topic = ifelse(topic == 60, 51, topic)) %>%
              mutate(topic = ifelse(topic %in% c(65, 40, 50, 59, 70), 27, topic)) %>%
              group_by(date, topic) %>%                                  
              summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")       
  pol_issues <- c(19, 2, 30, 34, 61, 16, 48, 1, 3, 5, 9, 13, 15, 17, 21, 25, 27, 29, 33, 36, 42, 44, 45, 51, 52, 53, 56, 63, 64, 66)
  } else {
    db <- read_csv("data_prod/var/bertopic/general_TS.csv", show_col_types = FALSE)
    throw_topic <- c(16, 44, 54, 61, 64, 73, 76, 91, 1, 2, 5, 25, 41, 45, 3, 21, 26, 35, 50, 51, 56, 57, 58, 60, 65, 69, 78, 80)
    pol_issues_temp <- setdiff(c(0:91), throw_topic)
    db <- db %>% mutate(topic = ifelse(topic == 29, 20, topic)) %>%
          mutate(topic = ifelse(topic %in% c(75,89), 74, topic)) %>%
          group_by(date, topic) %>%                                  
          summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") 
    pol_issues <- setdiff(pol_issues_temp, c(29, 75, 89))  
  }
  
  db <- db %>%
    filter(topic %in% pol_issues)

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

  print("VAR Estimation")
  # - estimating the model for p lags

  #db <- db %>%  #Remove the two first days to have a number of days divisible by 7. 
  #group_by(topic) %>%
  #mutate(group = ceiling(row_number() /2)) %>% 
  #group_by(group, topic) %>%
  #summarise(date = first(group), across(where(is.numeric), mean), .groups='drop') %>%
  #arrange(as.numeric(topic)) 

  db <- as.data.frame(db)

  db$topic <- as.character(db$topic)
  db$date <- as.factor(db$date)
  db$topic <- as.factor(db$topic)


  if (args$tests) {
    count_stat <- 0
    pdb <- pdata.frame(db, index=c("topic", "date"))
    cat("Hadri Test (H0 : All panels don't contain unit root) \n")
    for (v in variables){
      data_p <- pdb[[v]]
      hadri <- purtest(data_p, test="hadri", exo="intercept")
      p_value <- hadri$statistic$p.value
      if (p_value <0.05){
        print(paste(v, "H0 rejected", p_value, "At least one panel is not stationary"))
      } else{
        print(paste(v, "H0 not rejected", p_value, "All panels are stationary"))
        count_stat <- count_stat + 1
      }
    }
    

    if(count_stat != length(variables)){
      print("At least one group is represented by a non-stationary time series in one panel. Check the stationnarity after a differentiation")
      cat("Hadri Test (H0 : All panels don't contain unit root) \n")
      pdb_diff <- pdb
      for (v in variables){
        pdb_diff[[v]] <- diff(pdb[[v]], 1L)
      }

      pdb_diff <- na.omit(pdb_diff)
      pdb_diffc <- pdb_diff

      list_const_top <- list()

      for (v in variables){
        count_stat_topic <- 0
        for (i in unique(pdb_diff$topic)){
          pdb_topic <- pdb_diff %>% filter(topic == i)
          if (sd(pdb_topic[[v]]) == 0) {
            list_const_top <- append(list_const_top, i)
            pdb_diffc <- pdb_diffc %>% filter(topic !=i)
            print(paste("For variable", v, "topic", i, "removed for Hadri Test in diff data because constant"))
            count_stat_topic <- 1
          }
        }

        if(count_stat_topic==1){
          rownames(pdb_diffc) <- NULL
          pdb_diffc <- pdata.frame(pdb_diffc, index=c("topic", "date"))
        }

        data_p <- pdb_diffc[[v]]
        hadri2 <- purtest(data_p, test="hadri", exo="intercept")
        p_value2 <- hadri2$statistic$p.value
        if (p_value2 <0.05){
          print(paste("Diff", v, "H0 rejected", p_value2, "At least one panel is not stationary"))
        } else{
          print(paste("Diff", v, "H0 not rejected", p_value2, "All panels are stationary"))
        }
      }

      if(args$topic_model == 'lda'){
        topic_ips <- setdiff(pol_issues, unlist(unique(list_const_top)))

        cat("Topics", unlist(unique(list_const_top)), "will be remove for following tests to avoid constants or quasi-constants \n")
        cat("Test IPS test (H0 : All panels contain unit root) \n")
        pdb_ips <- pdb %>% filter(topic %in% topic_ips)
        cat("Dimensions de pdb_ips :", dim(pdb_ips), "\n")
        if (!is.pbalanced(pdb_ips)){
          stop("Unbalanced panel data in ips test")
        }
        for (v in variables){
          ips <- purtest(data_p, test="ips", exo="intercept", lags="AIC", pmax=10)
          p_val <- ips$statistic$p.value
          if(p_val < 0.05){
            print(paste(v, "H0 rejected", p_val, "At least one panel is stationary"))
          } else {
            print(paste(v, "H0 not rejected", p_val, "All panels are not stationary"))
            count_stat_top_ips <- count_stat_top_ips + 1
          }
        } 

        Groen_Kleibergen_Test <- function(db, n_boot=500){
          print("Start panel cointegration test")
          jo_mat <- matrix(NA, nrow=length(unique(db$topic)), ncol=length(variables))
          iter_jo <- 0
          list_subdb <- list()
          for(i in unique(db$topic)){
            iter_jo <- iter_jo+1
            data_jo <- db %>%
                      filter(topic==i)
            jo <- summary(ca.jo(data_jo[, variables], type="trace", ecdet = "const", spec="longrun"))@teststat
            jo_mat[iter_jo,] <- jo 
            list_subdb[[iter_jo]] <- data_jo
          }
      
          mean_jo <- colMeans(jo_mat, na.rm = TRUE)

          bootstrap_stats <- matrix(NA, nrow=n_boot, ncol=length(variables))
          N <- length(unique(db$topic))
          seq <- seq(1:length(unique(db$topic)))
          for (b in 1:n_boot){
            boot_sample <- sample(1:N, N, replace = TRUE)
            boot_trace_stats <- matrix(NA, nrow=length(unique(db$topic)), ncol=length(variables))
            iter_jo <- 0
            for (i in boot_sample){
              iter_jo <- iter_jo+1
              jo_sim <- ca.jo(list_subdb[[boot_sample[i]]][, variables], type="trace", ecdet = "const", spec="longrun")@teststat
              boot_trace_stats[iter_jo, ] <- jo_sim 
            }
            bootstrap_stats[b,] <- colMeans(boot_trace_stats)
          }

          Esp_jo <- colMeans(bootstrap_stats)
          V_jo <- colVars(bootstrap_stats)

          stats_GK <- (mean_jo - Esp_jo) / V_jo
          p_value <- 2 * (1 - pnorm(abs(stats_GK)))
          num_ranks <- rev(seq_along(stats_GK)) - 1
          line_rank <- paste0("r<=", num_ranks)

          result_table <- data.frame(
            Rank = line_rank,
            GK_Stat = stats_GK,
            P_value = p_value
          )
          return(result_table)
        } 
        db_ips <- db %>% filter(topic %in% topic_ips)
        GK_res <- Groen_Kleibergen_Test(db_ips)
        print(GK_res)
      } else {
        cat("No further stationarity or cointegration tests can be performed for BERTopic because of data structure \n")
        #Refer to a do file with these tests 
      }
    }


    PVAR_post <- function(model){
      residuals <- model$residuals
      lags <- model$lags
      n_obs <- nrow(residuals)
      k <- ncol(residuals)
      Sigma <- cov(residuals)
      n_star <- k*lags
      AIC <- log(det(Sigma)) + 2*k^2*lags/n_obs
      SC <- log(det(Sigma)) + log(n_obs)*k^2*lags/n_obs
      HQ <- log(det(Sigma)) + log(log(n_obs))*lags*k^2/n_obs
      FPE <- det(Sigma)*((n_obs + n_star)/(n_obs - n_star))^k
      return(c(AIC = AIC, SC = SC, HQ = HQ, FPE=FPE))
    }
    
    data_test = matrix(NA, nrow=30, ncol = 6)
    for (p in 1:nrow(data_test)){
      print(p)
      model <- pvarfeols(variables, lags = p, data = db, panel_identifier=c("topic", "date"))
      modulus <- panelvar::stability(model)$Modulus
      max_modul <- max(modulus)
      list_crit <- PVAR_post(model)
      criteria <- c(list_crit[[1]], list_crit[[2]], list_crit[[3]], list_crit[[4]])
      data_test[p,] = c(p, max_modul, criteria)
    }

    df_test <- as.data.frame(data_test)
    colnames(df_test) <- c("Lags", "Max eigenvalue process", "AIC", "SC", "HQ", "FPE")

    max_AIC <- df_test$Lags[which.min(df_test$AIC)]
    max_SC <- df_test$Lags[which.min(df_test$SC)]
    max_HQ <- df_test$Lags[which.min(df_test$HQ)]
    max_FPE <- df_test$Lags[which.min(df_test$FPE)]

    cat("Selection criteria suggests to choose the following number(s) of lags: \n")
    cat("AIC", max_AIC, "\n", "SC", max_SC, "\n", "HQ", max_HQ, "\n", "FPE", max_FPE, "\n")
    
    if(args$topic_model == 'lda') {
      write.csv(pdb, "data_prod/var/lda/general_filt_nodiff.csv", row.names = FALSE)
      write.csv(pdb_diff, "data_prod/var/lda/general__filt_diff.csv", row.names = FALSE)
      write.csv(df_test,
      "data_prod/var/lda/tests_results.csv",
      row.names = FALSE)
    } else {
      write.csv(pdb, "data_prod/var/bertopic/general_filt_nodiff.csv", row.names = FALSE)
      write.csv(pdb_diff, "data_prod/var/bertopic/general_filt_diff.csv", row.names = FALSE)
      write.csv(df_test,
      "data_prod/var/bertopic/tests_results.csv",
      row.names = FALSE)
    }
  }
  if (args$topic_model == "lda"){
    lags <- 15
    lags_filter <- NA
  } else {
    lags <- 8
    lags_filter <- 8  
  }

  PVAR_model<- pvarfeols(variables, lags = lags, data = db, panel_identifier=c("topic", "date"))
  print("Non-Cumulative IRF preparation")
  irf_NC <- panelvar::oirf(PVAR_model, n.ahead = 60)
  irf_NC_BS <- panelvar::bootstrap_irf(PVAR_model, typeof_irf="OIRF", n.ahead = 60, nof_Nstar_draws=500, mc.cores = 50)

  print("Create data according to vars package (including cumulation)")
  for (v in variables){
    irf_NC[[v]] <- apply(irf_NC[[v]], 2, cumsum)
    irf_NC_BS$Lower[[v]] <- apply(irf_NC_BS$Lower[[v]], 2, cumsum)
    irf_NC_BS$Upper[[v]] <- apply(irf_NC_BS$Upper[[v]], 2, cumsum)
  }

  var_irfs <- list(
    irf        = irf_NC,
    Lower      = irf_NC_BS$Lower,
    Upper      = irf_NC_BS$Upper,
    response   = variables,
    impulse    = variables,
    ortho      = TRUE,         
    cumulative = TRUE,           
    runs       = 500,
    ci         = 0.95,
    boot       = TRUE,
    model      = "pvarfeols"           
  )

  class(var_irfs) <- "varirf"

  if (args$topic_model == 'lda') {
    save(PVAR_model, file = "data_prod/var/lda/Pvar_model-MAIN.Rdata")
    save(var_irfs, file = "data_prod/var/lda/Pvar_irfs-MAIN.Rdata")
  } else {
    save(PVAR_model, file = "data_prod/var/bertopic/Pvar_model-MAIN.Rdata")
    save(var_irfs, file = "data_prod/var/bertopic/Pvar_irfs-MAIN.Rdata")
  }
} else {
  variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')
  if (args$topic_model == 'lda') {
    load("data_prod/var/lda/Pvar_irfs-MAIN.Rdata")
  } else {
    load("data_prod/var/bertopic/Pvar_irfs-MAIN.Rdata")
  }
}

var_irfs <- var_irfs
variables <- names(var_irfs$irf)
elements_to_pull <- c("irf", "Upper", "Lower")

print("Creation one-time shock IRF data")
irf_data <- NULL
DAYS <- 59
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

DAYS <- 59

# - deciding the number of days to simulate
print("Creation structural shock IRF data")
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
      
      # - pull the full args$number_irf-day IRFs for the endogenous covariate
      cov_resp <- irf_data %>%
        filter(cov == covariate, out == covariate) %>%
        # - remove 1st row: it's part of the set-up (repsonse at day 0)
        filter(day != 1) %>%
        mutate(day = day -1)
      
      # - pull the full args$number_irf-day IRFs for the particular outcome variable
      out_resp <- irf_data %>%
        filter(cov == covariate, out == outcome) %>%
        # - remove 1st row: it's part of the set-up (repsonse at day 0)
        filter(day != 1) %>%
        mutate(day = day -1)
      
      # - transforming the args$number_irf-day IRFs for the covariate and outcome to a wide
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
        #   covariate group to keep its attention change to 0% with we sum all changes 
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

print("Format IRF data in a human-friendly way")

# - reorder the levels of the outcome and covariate factor variables
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

all_irf_data_wide$out <- recode(all_irf_data_wide$out,
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
        "data_prod/var/bertopic/onetime-structural-shock-irfs-results.csv",
        row.names = FALSE)
  final_input <- import("data_prod/var/bertopic/onetime-structural-shock-irfs-results.csv")
   
}
print("Create plot data")


# DATA WRANLGING
#===============================================================================
# - exploring only args$number_irf-day effects, and removing the Media from the analysis.
plot_db <- final_input %>%
  filter(day == args$number_irf)

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
                                      "General\nPublic",
                                      "Media")))

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
                                "General\nPublic", 
                                "Media"))

# - re-phrase the shock labels
plot_db$data_type <- ifelse(
  grepl("one time", plot_db$data_type),
  "The effect of a one time 10 percentage point increase in day 0             ",
  "The effect of a permanent 10 percentage point increase in day 0"
)

print("Generate Plot")
# OUTPUT -- FIGURE 2
#===============================================================================
if (args$topic_model == 'lda') {
  png("data_prod/figures/lda/figure2.png", width = 1600, height = 700)
} else {
  png("data_prod/figures/bertopic/figure2.png", width = 1600, height = 700)
}

y_text <- paste("\n", args$number_irf, "-day Responses (in percentage points)")

min_value <- min(plot_db$lwr)
max_value <- max(plot_db$upr)

ggplot(plot_db,
       aes(x = cov, y = pe, ymin = lwr, ymax = upr, col = data_type)) +
  geom_segment(aes(x = cov, xend = cov, y = lwr, yend = upr), 
               linewidth = 2.5) +
  facet_wrap(~ out, nrow = 1) +
  coord_flip() +
  xlab("") +
  scale_y_continuous(y_text,
                     limits = c(min_value - 0.01, max_value + 0.01), expand = c(0,0)) +
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
