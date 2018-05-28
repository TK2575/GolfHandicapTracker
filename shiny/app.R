library(shiny)

score_data <- readr::read_csv("../data/example.csv")

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
                    tabPanel(title = "Plot",
                            "Plot here at some point"
                             ),
                    tabPanel(title = "Input Data",
                             dataTableOutput(outputId = "score_data")
                             ),
                    tabPanel(title = "Calculated Data",
                             dataTableOutput(outputId = "calc_data"))
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
     score_data
   })
}

# Run the application
shinyApp(ui = ui, server = server)

