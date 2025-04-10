### first try on dashboarding Topic Models from a LDA on French MP's tweets
if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny", "topicmodels", "ggplot2", "ggthemes", "readr"
               )

library(shiny)
library(topicmodels)
library(ggplot2)
library(ggthemes)
library(readr)
library(tidyverse)
library(argparse)

parser <- ArgumentParser()

parser$add_argument("topic_model", help="Choose a model type between lda and bertopic")

args <- parser$parse_args()

if (!(args$topic_model %in% c('bertopic', 'lda'))){
  stop("The model name is incorrect. Choose between lda and bertopic")
}

# Charge data

# Take data and values according to topic model type
if (args$topic_model == 'lda') {
  choices_top = 1:100
  titleUI <- "Annotation de Topics LDA"
  source_ts <- paste0("data_prod/dashboard/lda/data/ts-")
  name_img <- "words-plot-"
  source_img <- "data_prod/dashboard/lda/img"
  load("data_prod/dashboard/lda/congress-rs-tweets.rdata")
  load("data_prod/dashboard/lda/media-rs-tweets.rdata")

} else {
  choices_top = 0:92
  titleUI <- "Annotation de Topics BERTopic" 
  source_ts <- paste0("data_prod/dashboard/bertopic/data/bertopic_ts_")
  name_img <- "bertopic_" 
  source_img <- "data_prod/dashboard/bertopic/img"
  load("data_prod/dashboard/bertopic/representative_docs_congress.rdata")
  load("data_prod/dashboard/bertopic/representative_docs_media.rdata")
  congress_rs <- get(ls()[5]) 
  media_rs <- get(ls()[6]) 
}

# qois_long <- qois |>
#   select(topic, starts_with("prop_")) |>
#   pivot_longer(cols = starts_with("prop"),
#                names_to = "party",
#                names_prefix = "prop_",
#                values_to = "prop")

# UI



ui <- fluidPage(
  titlePanel(titleUI),
 fluidRow(
   column(2,
      selectInput("topic", "Choisissez un topic :", choices = choices_top)
      )
    ),
 fluidRow(
        column(8, plotOutput("topic_ts", height = "500px"#,
                             #brush = brushOpts("plot_brush"),
                             )),
        column(4, imageOutput("topwords_image"))
    ),
 fluidRow(
    checkboxGroupInput("acteurs", "Afficher :",
                       choices = c(
                                   "dep. majo.", "dep. lr", "dep. nupes", "dep. rn", 
                                   "medias", 
                                   "sup. majo.", "sup. lr", "sup. nupes", "sup. rn", 
                                   "pub. attentif", "pub. general"), #qois_long |> distinct(parti) |> pull(),
                       selected = c("dep. majo.", "dep. lr", "dep. nupes", "dep. rn"), #qois_long |> distinct(parti) |> pull()
                       inline = TRUE
    )
   ),
 # fluidRow(
 #   tableOutput("brushed_data")
 # )
 fluidRow(
   #tags$head(
  #   tags$script(src = "https://platform.twitter.com/widgets.js")
   #),
   titlePanel("Tweets représentatifs de député⋅es (à gauche) et de médias (à droite)"),
   #uiOutput("congress_tweets")
   column(6, htmlOutput("congress_tweets")),
   column(6, htmlOutput("media_tweets")
          #tags$head(
          #  tags$script(src = "https://platform.twitter.com/widgets.js")
          #),
          #titlePanel("Test d'intégration de tweet"),
          #uiOutput("tweets_test")
          )
   )#,
)

# time serie des topics

plot_ts  <- function(df, checked_actors, selected_topic){

  if(args$topic_model == 'bertopic') { #Adapt bertopic to script structure
    df <- df %>% mutate(party = recode(party, "lr" = "dep. lr", "majority" = "dep. majo.", "nupes" = "dep. nupes", "rn" = "dep. rn", "lr_supp" = "sup. lr", "majority_supp" = "sup. majo.", "nupes_supp" = "sup. nupes", "rn_supp" = "sup. rn", "attentive" = "pub. attentif", "general" = "pub. general", "media" = "medias"))
    colnames(df) <- c("date", "actor", "topic", "prop")
  }
  df |>
    filter(actor %in% {{checked_actors}}) |>
    ggplot() +
    aes(x = date, y = prop, color = actor, group = actor) +
    geom_line() +
    scale_x_date(#date_breaks = "month",
                 breaks = c(seq(ymd("2022-06-20"), ymd("2023-03-14"), by = "1 month"), ymd("2022-06-20"), ymd("2023-03-14")),
                 date_minor_breaks = "2 weeks",
                 date_labels = "%d-%b-%y",
                 limits = c(ymd("2022-06-20"), ymd("2023-03-14"))
                # expand = expansion(c(0,0))
                 ) +
    scale_color_manual(values = c(
                                  "dep. majo." = "orange",
                                  "dep. lr" = "blue",
                                  "dep. nupes" = "red",
                                  "dep. rn" = "purple",
                                  "medias" = "black", 
                                  "sup. majo." = "darkorange", 
                                  "sup. lr" = "darkblue", 
                                  "sup. nupes" = "darkred", 
                                  "sup. rn" = "purple4", 
                                  "pub. attentif" = "forestgreen", 
                                  "pub. general" = "lightgrey"
                                  )) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(color = "",
         x = "",
         y = "% d'attention accordée au topic") +
  #  ylim(0,1) +
    theme_clean() +
    theme(axis.text.x = element_text(size = rel(0.8)))+
    ggtitle(paste0("Évolution de l'attention accordée au topic ", selected_topic))
}

# Server

server <- function(input, output){
  df <- reactive({
    file_name <- paste0(source_ts, input$topic, ".csv")
    read_csv(file_name, show_col_types = FALSE)
  })
  
  selected_topic <- reactive(input$topic)
  checked_partys <- reactive(input$acteurs)

 output$topic_ts <- renderPlot({
    plot_ts(df(), checked_partys(), selected_topic())
    }, res = 96
              )
#df_brushed <- reactive({
#  df() |> select(-topic)
#})
# output$brushed_data <- renderTable(
#   {
#     brushedPoints(df(), input$plot_brush)
#   }
# )

  # Image des mots spécifiques du topic

  output$topwords_image <- renderImage({
    source_topwords <- file.path(source_img, paste0(name_img, input$topic, ".png"))
    list(src = source_topwords,
         contentType = 'image/png',
         alt = "Mots spécifiques",
         width = "100%",
         height = "130%"
         )
  }, deleteFile = FALSE)

# Affichage des tweets les plus représentatifs
# congress_tweets_data <- reactive({
#   load("data_prod/dashboard/congress-rs-tweets.rdata") # Charge les données, ex. `tweets_topic_data`
#   congress_rs # Remplacez par le nom de l'objet chargé
# })
  # Mettre à jour les topics dans le selectInput
#  observe({
#    updateSelectInput(session, "topic",
#                      choices = unique(congress_tweets_data()$topic))
#  })
  
  # Create tweets HTML for selected topic
  output$congress_tweets <- renderUI({
    req(input$topic) # Attendre que l'utilisateur sélectionne un topic
    selected_congress_tweets <- reactive({
      congress_rs |> filter(topic == input$topic)
                                })
    
    # Organize in two columns
    HTML(#paste(
      selected_congress_tweets()$embed#, collapse = "<br><br>")
      )
  })
  
  output$media_tweets <- renderUI({
    req(input$topic) # Wait topic selection by the user
    selected_media_tweets <- reactive({
      media_rs |> filter(topic == input$topic)
    })
    
    HTML(#paste(
      selected_media_tweets()$embed#, collapse = "<br><br>")
    )
  })
  #output$tweets_test <- renderUI({
  #  HTML('<blockquote class="twitter-tweet" data-theme="light">
  #              <a href="https://twitter.com/franceinfo/status/1539578460305330176"></a>
  #            </blockquote>')
  #})
}

# Run the application
shinyApp(ui = ui, server = server)


