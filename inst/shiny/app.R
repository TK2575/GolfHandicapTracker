score_data <- suppressMessages(readr::read_csv("../../data/example.csv"))
calculated_data <- transform_inputs(score_data)
# TODO rebuild other package imports (ggplot, shiny, plotly, etc)

ui <- fluidPage(

   titlePanel("Golf Performance Tracker"),

   sidebarLayout(
      sidebarPanel(
        # TODO build data input section, single rounds, full files, and helper for finding courses via ncrdb.usga.org
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
                             plotOutput(outputId = "over_time")
                             ),
                    tabPanel(title = "GIR vs FIR",
                             plotly::plotlyOutput(outputId = "gir_fir")
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

  output$gir_fir <- plotly::renderPlotly({
     p <- filtered_data() %>%
       ggplot(aes(x=fir, y=gir, size = pph, color=net_over_under)) +
         geom_jitter(width = .2, alpha = .5) +
         ylab("Greens in Regulation") +
         xlab("Fairways in Regulation") +
         xlim(c(0,100)) +
         ylim(c(0,100)) +
         scale_size_continuous(trans="exp") +
         scale_color_gradientn(colors = terrain.colors(9))
     ggplotly(p)
   })

  output$over_time <- renderPlot({
    filtered_data() %>%
      ggplot(aes(date, over_under)) +
        geom_jitter() +
        xlab("Date") +
        ylab("Over/Under") +
        geom_line(aes(date, hndcp_index))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

