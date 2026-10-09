library(shiny)
library(DT)
library(plotly)

source("global.r")

ui <- fluidPage(
  titlePanel("Detroit Tigers Analytics Project"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("game", "Game", choices = c("Game 1" = "1", "Game 2" = "2")),
      selectInput("pitcher", "Pitcher", choices = NULL)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Pitcher Summary", br(), DTOutput("summary_table")),
        tabPanel("Arsenal", br(), DTOutput("arsenal_table")),
        tabPanel("Movement", br(), plotlyOutput("movement_plot", height = "550px")),
        tabPanel("Location",  br(), plotlyOutput("location_plot", height = "550px"))
      )
    )
  )
)

# ---------- LOGIC ----------
server <- function(input, output, session) {
  
  game_data <- reactive({
   filter(pitches, GamePk == as.numeric(input$game))
  })
  
  observe({
    ids <- game_data() %>% count(PitcherId, sort = TRUE) %>% pull(PitcherId)
    updateSelectInput(session, "pitcher",
                      choices = setNames(ids, paste("Pitcher", ids)),
                      selected = ids[1])
  })
  
  output$summary_table <- renderDT({
    datatable(summarize_pitchers(game_data()),
               options = list(pageLength = 20))
  })
  
  output$arsenal_table <- renderDT({
    req(input$pitcher)
    datatable(filter(summarize_arsenal(game_data()), PitcherId == input$pitcher),
             options = list(dom = "t"))
  })
  
  output$movement_plot <- renderPlotly({
    req(input$pitcher)
    ggplotly(plot_movement(game_data(), input$pitcher), tooltip = "text")
  })
  
  output$location_plot <- renderPlotly({
    req(input$pitcher)
    ggplotly(plot_location(game_data(), input$pitcher), tooltip = "text")
  })
}

shinyApp(ui, server)