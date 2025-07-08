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
    ylab(paste("\nThe effect of a standard error impulse 40 days ago by the covariate group, measured in standard error change")) +
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
    )
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
}

shinyApp(ui = ui, server = server)