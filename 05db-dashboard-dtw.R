library(shiny)
library(stringr)
library(ggplot2)

# Conversion des couples en chaînes lisibles
# Conversion des couples en chaînes lisibles
variables <- c('lr', 'majority', 'nupes', 'rn', 'lr_supp', 'majority_supp', 'nupes_supp', 'rn_supp', 'attentive', 'general', 'media')

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
        #input-panel {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            z-index: 1000;
            background-color: white;
            padding: 10px;
            border-bottom: 1px solid #ccc;
        }
        #main-content {
            margin-top: 200px;
            padding: 20px;
        }
        "))
    ),

  div(id = "input-panel",
    titlePanel("Représentation Résultats DTW"),
    fluidRow(
    column(3, selectInput("topic_model", "Topic model", choices = c("LDA", "BERTopic"))),
    column(2, numericInput("sigma", "Sigma", min = 0.1, max = 0.9, value = 0.5, step = 0.2)),
    column(2, numericInput("TW", "Time Window", min = 7, max = 56, value = 28, step = 7)),
    column(2, selectInput("actor1", "Acteur 1", choices = variables)),
    column(2, selectInput("actor2", "Acteur 2", choices = variables))
    )), 
  div(id = "main-content",
    textOutput("textTWSV"),
    imageOutput("plotTWSV", height=600, width=800),
    textOutput("texttopic"),
    imageOutput("plottopic", height=1600, width=1400),
    textOutput("textTWVS"),
    imageOutput("plotTWVS", height=600, width=800))
)

server <- function(input, output, session) {
    topic_model <- reactive({
        input$topic_model
    })

    topic_model_lwr <- reactive({
        str_to_lower(topic_model())
    })

    sigma <- reactive({
        input$sigma
    })

    sigma_name <- reactive({
        sub("^[^.]*\\.", "", as.character(sigma()))
    })

    TW <- reactive({
        input$TW
    })

    actor1 <- reactive({
        input$actor1
    })

    observeEvent(input$actor1, {
    updateSelectInput(session, "actor2",
                      choices = variables[variables != input$actor1],
                      selected = variables[variables != input$actor1][1])
  })

    actor2 <- reactive({
        input$actor2
    })

    output$textTWSV <- renderText({
    "Evolution de la mesure de coordination selon sigma (Time Window fixé)"
    })

    output$plotTWSV <- renderImage({
    list(
        src = paste0("data_prod/dtw/", topic_model_lwr(), "/tests/Evol_coord_Sigma_", TW(), ".png"),
        contentType = 'image/png'
    )
    }, deleteFile = FALSE)

    output$textTWVS <- renderText({
        "Evolution de la mesure de coordination selon Time Window (sigma fixé)"
    })

    output$plotTWVS <- renderImage({
    list(
        src = paste0("data_prod/dtw/", topic_model_lwr(), "/tests/Evol_coord_TW_", sigma_name(), ".png"),
        contentType = 'image/png'
    )
    }, deleteFile = FALSE)

    output$texttopic <- renderText({
    "Coordination par topic selon sigma"
    })

    output$plottopic <- renderImage({
    list(
        src = paste0("data_prod/dtw/", topic_model_lwr(), "/tests/FLScore_bytopic_sigma", sigma_name(), ".png"),
        contentType = 'image/png'
    )
    }, deleteFile = FALSE)
}

shinyApp(ui, server)