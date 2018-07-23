score_data <- suppressMessages(readr::read_csv("../../data/example.csv"))
calculated_data <- transform_inputs(score_data)

#TODO preload UI elements

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
                    selected = "fir"),
        # TODO add ability to adjust rolling average size
        sliderInput(inputId = "recent_slider",
                    label = "Recent Rounds:",
                    min = 1, max = nrow(calculated_data),
                    value = min(40,floor(nrow(calculated_data)/2)),
                    step = 1)
      ),

      mainPanel(
        tabsetPanel(id = "tabspanel", type = "tabs",
                    tabPanel(title = "Handicap Index",
                             plotly::plotlyOutput(outputId = "hi")
                             ),
                    tabPanel(title = "Trends",
                             plotly::plotlyOutput(outputId = "trends")
                             ),
                    tabPanel(title = "GIR vs FIR",
                             plotly::plotlyOutput(outputId = "gir_fir")
                             ),
                    tabPanel(title = "Data",
                             DT::dataTableOutput(outputId = "table")
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
  
  # TODO fix display of date and percent columns
  # TODO limit the number of columns in output, reorder
  output$table <- DT::renderDataTable({
    filtered_data()
  })

  output$slope <- renderPlot({
    filtered_data() %>%
      ggplot(aes(factor()))
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
     plotly::ggplotly(p)
   })

  output$hi <- plotly::renderPlotly({
    q <- filtered_data() %>%
      ggplot(aes(date, over_under)) +
        geom_jitter() +
        xlab("Date") +
        ylab("Over/Under") +
        geom_line(aes(date, hndcp_index))
    plotly::ggplotly(q)
  })
  
  output$trends <- plotly::renderPlotly({
    r <- filtered_data() %>%
      ggplot(aes_string(x="date",
                        y=ifelse(input$y=="hndcp_index",
                                 "over_under",
                                 input$y))) +
      geom_jitter() +
      geom_line(aes_string(x="date",y=switch(input$y,
                                             "hndcp_index" = "hndcp_index",
                                             "fir" = "fir_avg",
                                             "gir" = "gir_avg",
                                             "pph" = "pph_avg")))
    plotly::ggplotly(r)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

