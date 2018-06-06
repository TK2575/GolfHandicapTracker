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
                    tabPanel(title = "Input Data",
                             dataTableOutput(outputId = "score_data")
                             ),
                    tabPanel(title = "Calculated Data",
                             dataTableOutput(outputId = "calc_data")
                             )
                    )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$score_data <- renderDataTable ({
      score_data
   })

   output$calc_data <- renderDataTable ({
     calculated_data
   })

   output$trend <- renderPlot({
     ggplot(calculated_data, aes(Date, `Handicap Index`)) +
       geom_point()
   })

   output$summary <- renderPlot({
     ggplot(calculated_data, aes(GIR, FIR, col = Putts)) +
       geom_jitter(aes(size = Score, alpha = Date)) +
       xlab("Greens in Regulation") +
       ylab("Fairways in Regulation")
   })
}

# Run the application
shinyApp(ui = ui, server = server)

