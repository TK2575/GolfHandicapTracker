library(shiny)
source("../R/calculations.R")

score_data <- readr::read_csv("../data/example.csv")
calculated_data <- transform_inputs(score_data)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Golf Performance Tracker"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "recent_slider",
                    label = "Recent Rounds:",
                    min = 3, max = 30,
                    value = 10)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "tabspanel", type = "tabs",
                    tabPanel(title = "Summary",
                             plotOutput(outputId = "summary")
                             ),
                    tabPanel(title = "Trend",
                            plotOutput(outputId = "trend")
                             ),
                    tabPanel(title = "Data",
                             dataTableOutput(outputId = "calc_data")
                             )
                    )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  filtered_data <- reactive({
    tail(calculated_data, input$recent_slider)
  })

   output$calc_data <- renderDataTable ({
     filtered_data()
   })

   output$trend <- renderPlot({
     ggplot(filtered_data(), aes(Date, `Handicap Index`)) +
       geom_point()
   })

   output$summary <- renderPlot({
     filtered_data() %>%
     ggplot(aes(GIR, FIR, col = Putts)) +
       geom_jitter(aes(size = `Net Over/Under`)) +
       xlab("Greens in Regulation") +
       ylab("Fairways in Regulation") +
       scale_color_gradientn(colors = terrain.colors(9))
   })
}

# Run the application
shinyApp(ui = ui, server = server)

