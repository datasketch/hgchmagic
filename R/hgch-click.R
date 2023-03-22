
click_functions <- function(viz, frtype = NULL, id_click = NULL) {
  if (is.null(id_click)) return()
  click_func <- paste0("function(event) {Shiny.onInputChange('", id_click, "', {id:event.point.name, timestamp: new Date().getTime()});}")

  if (frtype %in% c("CatCatNum", "CatCat", "CatDat", "CatDatNum")) {
    click_func <- paste0("function(event) {Shiny.onInputChange('", id_click, "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")
  }

  if (viz %in% "treemap") {
    if (frtype %in% c("CatCatNum", "CatCat")) {
    click_func <- paste0("function(event) {Shiny.onInputChange('", id_click, "', {cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}")
    }
  }

  click_func
}