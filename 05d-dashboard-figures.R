library(shiny)
library(ggplot2)
library(ggthemes)
library(readr)
library(tidyverse)
library(dplyr)


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

throw_topic <- c(16, 44, 54, 61, 64, 73, 76, 91, 1, 2, 5, 25, 41, 45, 3, 21, 26, 35, 50, 51, 56, 57, 58, 60, 65, 69, 78, 80, 87)
pol_issues_temp <- setdiff(c(0:91), throw_topic)
exclude_issues <- c(52, 71, 79, 85, 86, 88, 89)
pol_issues <- setdiff(pol_issues_temp, exclude_issues)
pa2our <- read_csv("data_prod/figures/translate_number_name/BERTOPIC_merged.csv")
pa2our <- pa2our %>% filter (Topic %in% pol_issues)

plot_fig4  <- function(df, checked_actors, top_rank, variables, readable_variables, colors_dict){
    plot_db <- df |> 
        filter(cov %in% checked_actors) |>
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
          ) |>
          mutate (
            cov = factor(cov, levels=readable_variables),
            out = factor(out, levels=readable_variables)
          ) |>
          filter(cov_agenda_type != out_agenda_type | cov_agenda_type == "pol") |>
          filter(sign(lwr) == sign(upr)) |>
          group_by(topic, out) |>
          slice_max(order_by = abs(pe), n = top_rank, with_ties = TRUE) |>
          ungroup() |>
          mutate(label = factor(label, levels = unique(label)))
    missing_colors <- setdiff(levels(plot_db$cov), names(colors_dict))
    if(length(missing_colors) > 0) {
        warning("Valeurs dans 'cov' sans couleur définie : ", paste(missing_colors, collapse = ", "))
    }

    ggplot(plot_db,
       aes(x = label, y = pe, ymin = lwr, ymax = upr,
           col = cov, text = paste0("Groupe: ", cov, "\nEffet: ", round(pe, 2), "\nIntervalle: [", round(lwr, 2), ", ", round(upr, 2), "]"))) +
    geom_pointrange(aes(col = cov), alpha = 0.4, size = 0.5) +
    geom_hline(yintercept = 0, color = "red") +
    facet_wrap(~out, nrow = 1) +
    coord_flip() +
    xlab("") +
    ylab(paste("\nThe effect of a standard error impulse 40 days ago by the covariate group, measured in std(Δimpulse)")) +
    scale_color_manual("Origine de l'impulsion", values = colors_dict) +
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
}

plot_figdep <- function(df, pa2our, pol_theme, variables, readable_variables){
  pa_temp <- pa2our %>% filter(Name == pol_theme)
  topic_num <- min(pa_temp$Topic)
  plot_db <- df %>% 
      filter(topic == topic_num) %>%
      filter(cov_agenda_type == 'pol' & out_agenda_type=='pol') %>%
             mutate(
            cov = recode(cov,
                         `lr` = "Députés LR",
                         `majority` = "Députés Majorité",
                         `nupes` = "Députés NUPES",
                         `rn` ="Députés RN"
            ),
            out = recode(out,
                         `lr` = "Députés LR",
                         `majority` = "Députés Majorité",
                         `nupes` = "Députés NUPES",
                         `rn` ="Députés RN"
            )
          )
  
  plot_db$cov <- factor(plot_db$cov,
                      levels = rev(readable_variables[1:4]))


  ggplot(plot_db,
       aes(x = cov, y = pe, ymin = lwr, ymax = upr)) +
  geom_segment(aes(x = cov, xend = cov, y = lwr, yend = upr), 
               size = 2.5) +
  geom_hline(yintercept = 0, color='red') + 
  facet_wrap(~ out, nrow = 1) +
  coord_flip() +
  xlab("") +
  scale_y_continuous(paste0("\n 40-day Responses (in std(Δresponse)) of a one standard error shock of std(Δimpulse)"),
                     limits = c(-0.25, 0.9), expand = c(0,0)) +
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
}

plot_figdeppub <- function(df, pa2our, pol_theme, variables, readable_variables){
  pa_temp <- pa2our %>% filter(Name == pol_theme)
  topic_num <- min(pa_temp$Topic)
  plot_db <- df %>% 
      filter(topic == topic_num) %>%
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
                         `attentive` = "Public Attentif"
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
                         `attentive` = "Public Attentif"
            )
          ) %>%
      mutate(
            cov = factor(cov, levels=readable_variables),
            out = factor(out, levels=readable_variables)
          ) %>%
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

  ggplot(plot_db,
       aes(x = polgroup_num, y = pe, ymin = lwr, ymax = upr, col = direction)) +
  geom_segment(aes(xend = polgroup_num, y = lwr, yend = upr),
               size = 4, alpha = 1) +  
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~ pubgroup, nrow = 1) +
  scale_x_continuous(
    name = "",
    breaks = unique(as.numeric(plot_db$polgroup_f)),
    labels = levels(plot_db$polgroup_f)
  ) +
  coord_flip() +
  ylab(paste0("\n 40-day cumulative effect of one standard error shock in day 0")) +
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
}

plot_figmedia<- function(df, pa2our, pol_theme, variables, readable_variables){
  pa_temp <- pa2our %>% filter(Name == pol_theme)
  topic_num <- min(pa_temp$Topic)
  plot_db <- df %>% 
    filter(topic == topic_num) %>%
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
  filter(cov_agenda_type == 'media' | out_agenda_type == 'media') %>%
  mutate(data_type = ifelse(cov_agenda_type == "media", "média→groupe", 
                            ifelse(out_agenda_type == "media", "groupe→média", NA))) %>%
  mutate(y = ifelse(cov_agenda_type == 'media', as.character(out), as.character(cov)))

  ggplot(plot_db,
       aes(x = y, y = pe, ymin = lwr, ymax = upr)) +
  geom_segment(aes(x = y, xend = y, y = lwr, yend = upr), 
               size = 4, alpha = 0.6) +
  #geom_segment(alpha = 0.8, size = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  facet_grid(~data_type) +
  coord_flip() +
  geom_vline(xintercept = 15) +
  xlab("") +
  scale_y_continuous(paste0("\nThe 40-day cumulative effect of a one standard error shock of std(Δimpulse) in day 0"),
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
}
ui <- fluidPage(
  fluidRow(plotOutput("fig4", width = "1600px", height = "1400px")),
  fluidRow(checkboxGroupInput("acteurs", "Afficher :",
                       choices = c(
                                   "majority", "lr", "nupes", "rn", 
                                   "media", 
                                   "majority_supp", "lr_supp", "nupes_supp", "rn_supp", 
                                   "attentive"), 
                       selected = c("majority", "lr", "nupes", "rn"),
                       inline = TRUE
    )),
      fluidRow(numericInput("rank_max", "Top :", value = 2, min = 1, max = 10)
    ),
      fluidRow(selectInput("pol_theme", "Choisir un numéro de topic : ", choices = sort(pa2our$Name))),
      fluidRow(h3("Relations entre les députés")),
      fluidRow(plotOutput("girf_dep", width = 1000, height = 800)),
      fluidRow(h3("Relations entre les députés et les publics")),
      fluidRow(plotOutput("girf_dep_pub", width = 1000, height = 1000)),
      fluidRow(h3("Relations avec les médias")),
      fluidRow(plotOutput("girf_med", width = 800, height = 800)),
      fluidRow(h3("Top 3 des sujets d'influence par paire de variables")),
      selectInput("group_1", "Group d'origine :", choices = readable_variables),
      selectInput("group_2", "Groupe réponse :", choices = readable_variables),
      fluidRow(tableOutput("pairedtop3"))
    )


server <- function(input, output, session) {
   df <- reactive({
    file_name <- paste0("data_prod/var/irf_data.csv")
    read_csv(file_name)
  })
  rank <- reactive(input$rank_max)
  checked_partys <- reactive(input$acteurs)

 output$fig4 <- renderPlot({
  plot_fig4(df(), checked_partys(), rank(), variables, readable_variables, colors_dict)
    })
  
  top_theme <- reactive(input$pol_theme)
  output$girf_dep <- renderPlot({
    plot_figdep(df(), pa2our, top_theme(), variables, readable_variables)
  })

  output$girf_dep_pub <- renderPlot({
    plot_figdeppub(df(), pa2our, top_theme(), variables, readable_variables)
  })

  output$girf_med <- renderPlot({
    plot_figmedia(df(), pa2our, top_theme(), variables, readable_variables)
  })

  observeEvent(input$group_1, {
    remaining_choices <- setdiff(readable_variables, input$group_1)
    updateSelectInput(session, "group_2", choices = remaining_choices)
  })

  top3_df <- reactive({
    file_name <- "data_prod/var/irf-analysis/full_top3.csv"
    read_csv(file_name)
  })

  cov_val <- reactive(input$group_1)
  out_val <- reactive(input$group_2)

  output$pairedtop3 <- renderTable({
    top3 <- top3_df()
    df_filt <- top3 %>% 
              filter(cov == cov_val(), out == out_val())
    df_filt
  })
}

shinyApp(ui = ui, server = server)