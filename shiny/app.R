library(shiny)
source("../R/calculations.R")

score_data <- suppressMessages(readr::read_csv("../data/example.csv"))
calculated_data <- transform_inputs(score_data)

ui <- fluidPage(

   titlePanel("Golf Performance Tracker"),

   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "y",
                    label = "Y-axis:",
                    choices = c("Handicap Index" = "hndcp_index",
                                "Fairways in Regulation" = "fir",
                                "Greens in Regulation" = "gir",
                                "Putts Per Hole" = "pph"),
                    # TODO Round/Row count, Median/Sum Over/Under, Distinct Courses
                    selected = "hndcp_index"),

        sliderInput(inputId = "recent_slider",
                    label = "Recent Rounds:",
                    min = 1, max = nrow(calculated_data),
                    value = floor(nrow(calculated_data)/2),
                    step = 1)
      ),

      mainPanel(
        tabsetPanel(id = "tabspanel", type = "tabs",
                    tabPanel(title = "Over Time",
                             plotOutput(outputId = "summary")
                             ),
                    # tabPanel(title = "By Slope",
                    #         plotOutput(outputId = "slope")
                    #          ),
                    tabPanel(title = "By Transport",
                             plotOutput(outputId = "transport")
                             )
                    )
      )
   )
)

server <- function(input, output) {

  filtered_data <- reactive({
    calculated_data %>%
      tail(input$recent_slider)
  })

  output$slope <- renderPlot({
    filtered_data() %>%
      ggplot(aes(factor()))
  })

  output$transport <- renderPlot({
    filtered_data() %>%
      ggplot(aes(factor(Transport), input$y, col = desc(quarter))) +
        geom_jitter(width = .2) +
        scale_color_gradientn(colors = terrain.colors(9))
    # FIXME display of quarter in legend
    # FIXME display scale in Y axis
    # FIXME clean up x/y labels
  })

  # Example working plot
  output$summary <- renderPlot({
     calculated_data %>%
       tail(input$recent_slider) %>%
       ggplot(aes(gir, fir, col = pph)) +
         geom_jitter(aes(size = net_over_under)) +
         xlab("Greens in Regulation") +
         ylab("Fairways in Regulation") +
         scale_color_gradientn(colors = terrain.colors(9))
   })
}

# Run the application
shinyApp(ui = ui, server = server)

