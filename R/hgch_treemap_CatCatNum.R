#' Treemap chart Cat Cat Num
#'
#' @description
#' `hgch_treemap_CatCatNum()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts and second columns are
#' **categoricals columns** and the third must be  a **numeric class column**, or be sure that
#' three firts columns they meet this condition
#' @export
#' @inheritParams hgch_treemap_CatNum
#' @family Cat-Cat-Num plots
#' @section Ftype:
#' Cat-Cat-Num
#' @examples
#' data <- sample_data("Cat-Cat-Num", n = 30)
#' hgch_treemap_CatCatNum(data)
#'
#' # Activate data labels
#' hgch_treemap_CatCatNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_treemap_CatCatNum(data,
#'                        agg = "mean",
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Dat-Yea-Cat", n = 30)
#' hgch_treemap_CatCatNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_treemap_CatCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_treemap_CatCatNum(tooltip = info_tool)
#'
hgch_treemap_CatCatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  data[[1]] <- homodatum::as_Cat(data[[1]])
  data[[2]] <- homodatum::as_Cat(data[[2]])
  #data[[3]] <- homodatum::as_Num(data[[3]])

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- hgchmagic_prep(data, opts = opts, plot = "treemap", ftype = "Cat-Cat-Num")

  d <- l$d

  color_by <- "a"#l$color_by
  paleta <- d[,c(color_by, "..colors")]
  paleta <- paleta %>% dplyr::distinct(..colors, .keep_all = TRUE)

  listaId <- purrr::map(1:length(paleta[[color_by]]), function(i) {
    list(
      id = as.character(paleta[[color_by]][i]),
      name = as.character(paleta[[color_by]][i]),
      color = as.character(paleta$..colors[i])
    )
  })

  listaMg <- purrr::map(1:nrow(d), function(z) {
    nm <- ifelse(is.na(d$b[z]), "NA", d$b[z])
    list(
      name = nm,
      parent = d$a[z],
      value = d[[3]][z],
      label = d$labels[z],
      colorValue = d[[3]][z]
    )

  })

  data <- c(listaId, listaMg)



   fml1 <- JS('function () {return  this.point.name +  "<br/>"  }')#
   dl_list1 <-list()
   dl_list2 <-list()
   dl_list3 <-list()
   dl_list4 <-list()
   # format_treemap_catcatnum  = FALSE

    if (opts$style$datalabel_formmater_js  == TRUE) {
     dl_list1 <- list(
       enabled = l$extra$treemap_dataLabels_levelOne,
       align = 'left',
       verticalAlign = 'top',
       formatter =fml1
     )

     dl_list2  <- list(
       enabled = l$extra$treemap_dataLabels_levelTwo,
       align = 'left',
       verticalAlign = 'bottom',
       # formatter = JS('function () { return this.point.name }')
       formatter = JS('function () {
                                              if (this.point.value > 1000000) {
                                                return   "<br/>" + this.point.name +   "<br/>" +  "Average: " + Math.floor(this.point.value / 1000000,2) + "M" + " " +  "usd";
                                              } else if (this.point.value > 1000 &&  this.point.value < 1000000) {
                                                return    "<br/>" + this.point.name +  "<br/>" + "Average: " + Math.floor(this.point.value/ 1000,2) + "K" + " " + "usd";
                                              } else {
                                                return  "<br/>" + this.point.name + "<br/>" + "Average: " +  this.point.value + " " +  "usd";
                                              }
                                       }'
       )
     )

     dl_list3 <-  list(enabled = TRUE,
          align = 'left',
          verticalAlign = 'bottom',
          # format='{point.name} - {point.value}',
          formatter = JS('function () {
                                              if (this.point.value > 1000000) {
                                                return    "<br/>"  + "Average: " + Math.floor(this.point.value / 1000000,2) + "M" + " " +  "usd";
                                              } else if (this.point.value > 1000 &&  this.point.value < 1000000) {
                                                return   "<br/>"  + this.point.name + "Average: " + Math.floor(this.point.value/ 1000,2) + "K" + " " +  "usd";
                                              } else {
                                                return   "<br/>"  + this.point.name + "Average: " +  this.point.value + " " +  "usd";
                                              }
                                       }'
          )
     )

     dl_list4  <- list(
       allowPointSelect= l$allow_point,
       cursor =  l$cursor,
       dataLabels = dl_list3,
       events = list(
         click = l$clickFunction
       )
     )
   }
   else {
     dl_list1 <- list(
                       enabled = l$extra$treemap_dataLabels_levelOne,
                       align = 'left',
                       verticalAlign = 'top',
                       format = "{point.name}"
                  )

     dl_list2  <- list(
                       enabled = l$extra$treemap_dataLabels_levelTwo,
                       verticalAlign = 'top'
                   )

     dl_list3 <- list(allowPointSelect= l$allow_point,
                      cursor =  l$cursor,
                      events = list(
                        click = l$clickFunction
                      )
                    )

     dl_list4  <- list(
       allowPointSelect= l$allow_point,
       cursor =  l$cursor,
       events = list(
         click = l$clickFunction
       )
     )


   }



 #  print(class(data))
 #  print(colnames(data))
 # print(l$extra$treemap_borderWidth_levelOne)
  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(events = list(
               load = add_branding(l$theme)
             )) %>%
   #hc_add_event_series(JS("H.addEvent(H.Legend, 'afterGetAllItems', function(e) {e.allItems.splice(1, 1) });")) %>%
    hc_series(

      list(
        type = "treemap",
        # point = purrr::map(as.character(unique(d$a)), function(z) z),
        # showInLegend =TRUE,
        # legendType = 'point',
        # layoutAlgorithm = 'stripes',
        # alternateStartingDirection = TRUE,
        layoutAlgorithm = l$extra$treemap_layout,
        levels = list(list(
          level = 1,
          dataLabels =  dl_list1,
          borderWidth = l$extra$treemap_borderWidth_levelOne,
          borderColor = l$extra$treemap_borderColor_levelOne
        ),
        list(
        level = 2,
        dataLabels = dl_list2)),
        data = data
      )) %>%
    hc_plotOptions(
      series =dl_list4) %>%
     hc_tooltip(useHTML = TRUE,
                formatter = JS(paste0("function () {return this.point.label;}")),
                style = list(width = "300px", whiteSpace = "normal")) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme, datalabel_formmater_js =  opts$style$datalabel_formmater_js,
                               cats = "{point.name} <br/>")))

  hc

}


#' Treemap chart Cat Yea Num
#'
#' @description
#' `hgch_treemap_CatYeaNum()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column is a
#' **categorical column**, second is a **year column** and the third must be  a **numeric class column**,
#'  or be sure that three firts columns they meet this condition
#' @export
#' @inheritParams hgch_treemap_CatCatNum
#' @section Ftype:
#' Cat-Yea-Num
#' @examples
#' data <- sample_data("Cat-Yea-Num", n = 30)
#' hgch_treemap_CatYeaNum(data)
#'
#' # Activate data labels
#' hgch_treemap_CatYeaNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Yea-Num-Dat-Yea-Cat", n = 30)
#' hgch_treemap_CatYeaNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_treemap_CatYeaNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_treemap_CatYeaNum(tooltip = info_tool)
#'
hgch_treemap_CatYeaNum <- hgch_treemap_CatCatNum


#' Treemap chart Yea Cat Num
#'
#' @description
#' `hgch_treemap_YeaCatNum()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column is a
#' **year column**, second is a **categorical column** and the third must be  a **numeric class column**,
#'  or be sure that three firts columns they meet this condition
#' @export
#' @inheritParams hgch_treemap_CatNum
#' @family Yea-Cat-Num plots
#' @section Ftype:
#' Yea-Cat-Num
#' @examples
#' data <- sample_data("Yea-Cat-Num", n = 30)
#' hgch_treemap_YeaCatNum(data)
#'
#' # Activate data labels
#' hgch_treemap_YeaCatNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Cat-Num-Dat-Yea-Cat", n = 30)
#' hgch_treemap_YeaCatNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_treemap_YeaCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_treemap_YeaCatNum(tooltip = info_tool)
#'
hgch_treemap_YeaCatNum <- hgch_treemap_CatCatNum

