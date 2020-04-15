library(highcharter)

logo <- 'http://quantlabs.net/blog/wp-content/uploads/2015/12/highcharts-logo.png'
logo <- knitr::image_uri(f = "R/ds_logo.png")



h <- highchart() %>%
  hc_chart(type = "line",
           events = list(
             load = JS(paste0(" function() {this.renderer.image('",logo,"', this.chartWidth - 105, this.chartHeight - 25, 100, 20).addClass('logo').add();}")
           ))) %>%
  hc_title(text = "esto es un titulo") %>%
  hc_credits(enabled = TRUE, text = "esto son los creditos",
             position = list(
               align =  'left',
               x = 15),
             style = list(
               fontSize = "19px"
             )) %>%
  hc_xAxis(
    categories = list('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) %>%
  hc_add_series(
    data = list(-29.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4)
  )
htmltools::save_html(h, "aver.html")
library(webshot)
htmlwidgets::saveWidget(h, "grafico.html")
webshot("grafico.html", file = "aver.png", delay = 1) %>%
  resize("100%")
