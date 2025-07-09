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
source("utils_R.r")
library(resample)
library(tidyr)
library(tidyverse)
library(dplyr)

parser <- ArgumentParser()

parser$add_argument("--retweets", action = "store_true",
help = "Run the script counting the retweets")

parser$add_argument("--estimate", action = "store_true",
help = "Run the script who estimate PVAR and IRF")

parser$add_argument("--tests", action = "store_true",
                    help = "Activate to test the absence of unit roots and to print the selection criteria")

parser$add_argument("--tests_post", action = "store_true",
                    help = "Activate to test the absence of unit roots and to print the selection criteria")

parser$add_argument("--number_irf", help="Choose a int who will represent the number of days in IRF calculation", type="integer", default=15)


args <- parser$parse_args()

if (args$estimate || args$tests){
  variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'media')
  print("Files recuperation and preprocessing")
  if (args$retweets){
    db <- read_csv("data_prod/var/general_TS_RT.csv", show_col_types = FALSE) #Change Name when retweets will be available
  }
  else(
    db <- read_csv("data_prod/var/general_TS.csv", show_col_types = FALSE)
  )
  throw_topic <- c(16, 44, 54, 61, 64, 73, 76, 91, 1, 2, 5, 25, 41, 45, 3, 21, 26, 35, 50, 51, 56, 57, 58, 60, 65, 69, 78, 80, 87)
  pol_issues_temp <- setdiff(c(0:91), throw_topic)
  #db <- db %>% 
    #select(-general)
  GTS <- db %>% mutate(topic = ifelse(topic == 89, 74, topic)) %>%
        group_by(date, topic) %>%                                  
        summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  exclude_issues <- c(52, 71, 79, 85, 86, 89) #Exclusion du topic merged + des topics avec au moins une série constante + ceux posant des soucis d'autocorrélation
  pol_issues <- setdiff(pol_issues_temp, exclude_issues) 
  #pol_issues <- c(11, 12, 18, 19, 27, 29, 30, 31, 32, 33, 34, 37, 46, 49, 53, 55, 62, 63, 66, 7, 72, 74, 75, 77, 8, 82, 83, 84, 88, 90)
  db <- GTS %>%
    filter(topic %in% pol_issues) 
}

if(args$tests){
  pol_issues_temp <- setdiff(pol_issues_temp, 89)
  db_bf <- GTS %>% filter(topic %in% pol_issues_temp)
  cat("Proportion of remaining data linked to modeling filter \n")
  tot <- db_bf %>% select(-topic) %>% 
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
  remain <- db %>% select(-topic) %>% 
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
  print(tot)
  print(remain)
  prop <- remain / tot
  print(prop) 
}

if (args$estimate || args$tests){
  for (v in variables){
    db[[v]] <- log(1 + db[[v]])
  }
}

if (args$tests){
  cat("Selection criteria optimized for the following lags number : \n")
  print(PVARselect(db, variables, 20, panel_identifier=c("topic", "date"))$selection)
}

if(args$estimate){
  print("PVAR Estimation")
  db <- as.data.frame(db)
  db$topic <- as.character(db$topic)
  db$date <- as.factor(db$date)
  db$topic <- as.factor(db$topic)
  lags <- 16
  PVAR_model<- pvarfeols(variables, lags = lags, data = db, panel_identifier=c("topic", "date"))
  save(PVAR_model, file = "data_prod/var/Pvar_model-MAIN.Rdata")
  #print("Calculate boostrapped GIRF")
  #irf_NC_BS <- panelvar::bootstrap_irf(PVAR_model, typeof_irf="GIRF", n.ahead = 30, nof_Nstar_draws=500, mc.cores = 50)
  #print("Create data according to vars package (including cumulation)")
  #for (v in variables){
    #irf_NC_BS$Lower[[v]] <- apply(irf_NC_BS$Lower[[v]], 2, cumsum)
    #irf_NC_BS$Upper[[v]] <- apply(irf_NC_BS$Upper[[v]], 2, cumsum)
  #}

  #var_irfs <- list(
   # Lower      = irf_NC_BS$Lower,
    #Upper      = irf_NC_BS$Upper,
    #response   = variables,
    #impulse    = variables,         
    #cumulative = TRUE,           
    #runs       = 500,
    #ci         = 0.95,
    #boot       = TRUE,
    #model      = "pvarfeols"           
  #)
  #save(var_irfs, file = "data_prod/var/Pvar_irfs-MAIN.Rdata")
}

if (args$tests_post){
  if (!file.exists("data_prod/var/Pvar_model-MAIN.Rdata")){
    stop("Please run --estimate option before posterior checks.")
  }
  load("data_prod/var/Pvar_model-MAIN.Rdata")
  lags <- PVAR_model$lags
  stab <- max(panelvar::stability(PVAR_model)$Modulus)
  if(stab>=1){
    cat(paste("Stability problem, max Modul = ", stab, "\n"))
  } else {
    cat("PVAR Process is stable \n")
  }
  cat("Autocorrelation tests \n")
  PTTest <- panel_portmanteau_test(PVAR_model)
  print(PTTest$summary)
  write.csv(PTTest$summary, paste0("data_prod/var/summary_AC_", lags, ".csv"), row.names=FALSE)
  write.csv(PTTest$details, paste0("data_prod/var/details_AC_", lags, ".csv"), row.names=FALSE)
  cat("Normality test \n")
  print(panel_normality_test(PVAR_model)$jb.mul$JB)
}

stop()

if(!file.exists("data_prod/var/Pvar_irfs-MAIN.Rdata")){
  stop("Please estimate the model before creating figures")
}

load("data_prod/var/Pvar_irfs-MAIN.Rdata")
var_irfs <- var_irfs
variables <- names(var_irfs$Lower)
elements_to_pull <- c("Upper", "Lower")

print("Creation one-time shock IRF data")
irf_data <- NULL
DAYS <- 29
for (el in elements_to_pull) {
  new_irf_info <- var_irfs[el][[1]]
  for (out in variables) {
    new_irf_var_data <- as.data.frame(new_irf_info[out][[1]])
    # - take inverse logit to transform the effects to percentage point changes
    new_irf_var_data_transf <- as.data.frame(
      sapply(1:ncol(new_irf_var_data), function(j)
        exp(new_irf_var_data[,j]) - 1))
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
  geom_hline(yintercept = 0, color = "red", linewidth = 0.4) +
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
