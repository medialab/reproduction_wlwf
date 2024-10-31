### first try on dashboarding Topic Models from a LDA on French MP's tweets
if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny", "topicmodels", "ggplot2", "readr")

library(shiny)
library(topicmodels)
library(ggplot2)
library(readr)

# Chargement des données

# load("data_prod/topics/lda_results-twokenizer.Rdata")  # results lda
# load("dashboard/qois.rdata")  # topic_scores

# qois_long <- qois |> 
#   select(topic, starts_with("prop_")) |>  
#   pivot_longer(cols = starts_with("prop"), 
#                names_to = "party", 
#                names_prefix = "prop_", 
#                values_to = "prop")
  
# UI
ui <- fluidPage(
  titlePanel("Annotation de Topics LDA"),
 fluidRow(
   column(2,
      selectInput("topic", "Choisissez un topic :", choices = 1:100)
      )
    ),
 fluidRow(
        column(8, plotOutput("topic_ts", height = "500px",
                             brush = brushOpts("plot_brush"),
                             )),
        column(4, imageOutput("topwords_image"))
      #,
      # fluidRow(
      #   h3("Tweets Représentatifs"),
      #   uiOutput("tweets_mostrep")
      # )
    ),
 fluidRow(
    checkboxGroupInput("partys", "Afficher :", 
                       choices = c("majority", "lr", "rn", "nupes"), #qois_long |> distinct(parti) |> pull(), 
                       selected = c("majority", "lr", "rn", "nupes"), #qois_long |> distinct(parti) |> pull()
                       inline = TRUE
    )
  ),
 fluidRow(
   tableOutput("brushed_data")
 )
)

# time serie des topics

plot_ts  <- function(df, checked_partys, selected_topic){

  df |>
    filter(party %in% {{checked_partys}}) |>
    ggplot() +
    aes(x = date, y = prop, color = party, group = party) +
    geom_line() +
    scale_x_date(#date_breaks = "month", 
                 breaks = c(seq(ymd("2022-06-20"), ymd("2023-03-14"), by = "1 month"), ymd("2022-06-20"), ymd("2023-03-14")),
                 date_minor_breaks = "2 weeks",
                 date_labels = "%y-%b-%d",
                 limits = c(ymd("2022-06-20"), ymd("2023-03-14")),
                # expand = expansion(c(0,0))
                 ) +
    scale_color_manual(values = c("lr" = "darkblue",
                                  "majority" = "orange",
                                  "nupes" = "red",
                                  "rn" = "purple")) +
    labs(color = "",
         x = "",
         y = "Score moyen") +
  #  ylim(0,1) +
    theme_clean() +
    theme(axis.text.x = element_text(size = rel(0.8)))+
    ggtitle(paste0("Évolution de l'attention accordée au topic ", selected_topic))
}

# Server
server <- function(input, output){
  df <- reactive({
    file_name <- paste0("dashboard/files/data/ts-", input$topic,".csv")
    read_csv(file_name) 
  })
  selected_topic <- reactive(input$topic)
  checked_partys <- reactive(input$partys)

 output$topic_ts <- renderPlot({
    plot_ts(df(), checked_partys(), selected_topic())
    }, res = 96
              )
 
 output$brushed_data <- renderTable(
   {
     brushedPoints(df(), input$plot_brush)
   }
 )

    # avec dygraph
  # output$topic_timeseries <- renderPlot({
  #   selected_topic_scores <- topic_scores[[input$selected_topic]]
  #   filtered_scores <- selected_topic_scores[, input$actors, drop = FALSE]
  #
  #   dygraph(filtered_scores, main = paste("Évolution des scores pour le topic", input$selected_topic)) %>%
  #     dyOptions(stackedGraph = TRUE)
  #})

  # Image des mots spécifiques du topic
  output$topwords_image <- renderImage({
    list(src = file.path("dashboard/files/img", paste0("words-plot-", input$topic, ".png")),
         contentType = 'image/png',
         alt = "Mots spécifiques",
         width = "100%",
         height = "130%"
         )
  }, deleteFile = FALSE)

  # Affichage des tweets les plus représentatifs
  # output$tweets_mostrep <- renderUI({
  #   tweets_for_topic <- tweets[[input$selected_topic]]
  #
  #   tweet_html <- lapply(tweets_for_topic, function(tweet) {
  #     tags$div(
  #       class = "tweet",
  #       tags$img(src = tweet$profile_image_url, class = "tweet-profile"),
  #       tags$div(class = "tweet-text", tweet$text),
  #       tags$div(class = "tweet-timestamp", tweet$created_at)
  #     )
  #   })
  #
  #   do.call(tagList, tweet_html)
  # })
}

# Run the application
shinyApp(ui = ui, server = server)


