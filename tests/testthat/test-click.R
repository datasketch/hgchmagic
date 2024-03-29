library(shiny)
library(dplyr)
library(highcharter)
library(hgchmagic)

ui <- fluidPage(
  highchartOutput("hgch_viz"),
  verbatimTextOutput("click")
)

# Server logic
server <- function(input, output) {

  output$hgch_viz <- renderHighchart({
    #data <- ggplot2::diamonds |> select(cut, color, everything())
    # hgch_line_CatDat(data,
    #                  palette_colors = "#ffa92a",
    #                  shiny_cursor = "pointer",
    #                  shiny_clickable = TRUE
    # )
    data <- ggplot2::diamonds |> select(cut, color)
    hgch_sankey(data, var_cat = c("cut", "color"),
                     shiny_cursor = "pointer",
                     shiny_clickable = TRUE
    )
  })

  output$click <- renderPrint({
    input$hcClicked
  })

}

# Complete app with UI and server components
shinyApp(ui, server)


