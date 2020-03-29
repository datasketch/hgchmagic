# aggregation
#'@export
agg <- function(aggregation,...){
  f <- NULL
  if(aggregation == "sum")
    f <- sum(..., na.rm = TRUE)
  if(aggregation == "mean")
    f <- mean(..., na.rm = TRUE)
  if(aggregation == "median")
    f <- median(..., na.rm = TRUE)
  if(aggregation == "max")
    f <- max(..., na.rm = TRUE)
  if(aggregation == "min")
    f <- min(..., na.rm = TRUE)
  f
}

# defines horLabel and verLabel depending orientation
#'@export
orientationXY <- function(orientation, x, y, hor, ver, line = FALSE) {
  if (orientation == "hor") {
    x0 <- ver %||% x
    y0 <- hor %||% y
  } else {
    x0 <- hor %||% x
    y0 <- ver %||% y
  }
  if (line) {
    x0 <- hor %||% x
    y0 <- ver %||% y
    if (orientation != "hor") {
      x1 <- x0
      x0 <- y0
      y0 <- x1
    }
  }
  v <- c(x0, y0)

  v
}

# defines horLine and verLine depending orientation
#'@export
linesOrientation <- function(orientation, horLine = NULL, verLine = NULL) {


  if (orientation  == 'hor') {
    if (!is.null(horLine)) {
      lineY <- horLine - 1
    } else {
      lineY <- 'NULL' }
    if (!is.null(verLine)) {
      lineX <- verLine
    } else {
      lineX <- 'NULL'
    }
  } else {
    if (!is.null(horLine)) {
      lineX <- horLine
    } else {
      lineX <- 'NULL' }
    if (!is.null(verLine)) {
      lineY <- verLine - 1
    } else {
      lineY <- 'NULL'
    }
  }

  v <- c(lineX, lineY)
  v
}

# defines horLine and verLine label depending orientation
#'@export
linesOrLabel <- function(orientation, horLineLabel = NULL, verLineLabel = NULL) {

  if (orientation  == 'ver') {
    if (!is.null(horLineLabel)) {
      labelY <- horLineLabel
    } else {
      labelY <- 'NULL' }
    if (!is.null(verLineLabel)) {
      labelX <- verLineLabel
    } else {
      labelX <- 'NULL'
    }
  } else {
    if (!is.null(horLineLabel)) {
      labelX <- horLineLabel
    } else {
      labelX <- 'NULL' }
    if (!is.null(verLineLabel)) {
      labelY <- verLineLabel
    } else {
      labelY <- 'NULL'
    }
  }

  v <- c(labelX, labelY)
  v
}

# ds palette
#' @export
dsColorsHex <- function(hex = FALSE) {
  if (hex) {
    c <- c(0:9, "A", "B", "C", "D", "E")

  } else {
    c <- c("#FECA84", "#3DB26F", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  }
}

# colores
#' @export
fillColors <- function(data, col, colors, colorScale) {
  #set.seed(321)
  cat <- unique(data[[col]])
  ds <- dsColorsHex(TRUE)
  dc <- dsColorsHex()
  if (!is.null(colors)) {
    cl <- col2rgb(colors)
    colors <- purrr::map_chr(1:ncol(cl), function(s) {
      rgb(cl[1, s],
          cl[2, s],
          cl[3, s],
          maxColorValue = 255)
    })
  }
  if (length(colors) == 1 & colorScale != 'no') {
    colors <- c(colors, sample(dc, 1))
  }
  #print(colors)
  if (colorScale == "no") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[2]
    }
    fillCol <- rep(colors, length(cat))[1:length(cat)]
  }

  if (colorScale == "discrete") {
    dom <- factor(sample.int(length(cat), length(cat), FALSE))
    p <- colorFactor(colors, domain = NULL, dom)
    fillCol <- p(1:length(cat))
  }

  if (colorScale == "continuous") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[c(1, 7, 3, 4)]
    }
    fillCol <- leaflet::colorNumeric(colors, 1:length(cat))(1:length(cat))
  }

  fillCol
}




# order category column
#'@export
orderCategory <- function(data, col, order, label_wrap) {
  if (!is.null(order)) {
    order <- union(order, unique(data[[col]])[!is.na(unique(data[[col]]))])
    if (all(!is.na(order)) & any(is.na(data[[col]]))) order <- c(union(order, unique(data[[col]][!is.na(data[[col]])])), NA)
    order[is.na(order)] <- "NA"
    data <- data[order(match(data[[col]], order)), ]
  }
  if (is.null(label_wrap)) {
    data <- data
  } else {
    data[[col]] <- gsub("\\\n", "<br/>", str_wrap(data[[col]], label_wrap))
  }
  data
}

# sort and slice
#'@export
sortSlice <- function(data, col, sort, sliceN) {
  if (sort == "asc") {
    data <- data %>%
      dplyr::arrange_(col)
  }
  if (sort == "desc") {
    col <- paste0('desc(', col, ')')
    data <- data %>%
      dplyr::arrange_(.dots = col)
  }
  if (!is.null(sliceN)) {
    data <- data %>%
      dplyr::slice(1:sliceN)
  }
  data
}
# default tooltip for highcharts
#'@export
tooltipHc <- function(data, names, tooltip, agg, colAgg, percentage,  nDt, stacked100 = FALSE) {
  if (is.null(unlist(tooltip))) {
    n0 <- length(names)
    if (sum(names(data) %in% letters) == length(names)) {
      n0 <- length(names) - 1
    }
    pf <- purrr::map_chr(1:n0, ~paste0(names[.x], ": <b>{point.", letters[.x], "}</b>"))
    pf <- paste(pf, collapse = "<br>")
    if (!is.null(colAgg) & !is.null(agg)) {
      pf <- paste(pf,
                  "<br>",
                  paste0(agg,
                         ": <b>{point.",
                         ifelse(stacked100, paste0("percentage:.", nDt,"f}%"), paste0(colAgg, "} ")),
                         ifelse(percentage, paste0("({point.percent:.",nDt,"f}%)</b>"), "</b>")))
    }
    tooltip <- list("headerFormat" = "",
                    "pointFormat" = pf,
                    "shared" = FALSE)
  }
  tooltip
}


# default thounsands separate and decimap Points highcharts
#'@export
sepThous <- function(marksT) {
  hcopts <- getOption("highcharter.options")
  hcopts$lang$thousandsSep <-marksT[1]
  hcopts$lang$decimalPoint <-marksT[2]
  hcopts$lang$numericSymbols <- highcharter::JS("null")
  options(highcharter.options = hcopts)
}

#' @name count_pl
#' @export
count_pl <- function(x) {
  if (is.na(x)) {return(0)}
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}




#' translate

#' @export
exportLang <- function(language = 'es') {
  if (language == 'en') {
    langOpt <- getOption("highcharter.lang")
    langOpt$contextButtonTitle <- "Download options"
    langOpt$printChart <- "Print chart"
    langOpt$downloadJPEG <- 'Download JPEG image'
    langOpt$downloadPNG <- 'Download PNG image'
    langOpt$downloadSVG <- 'Download SVG image'
    langOpt$downloadPDF <- 'Download PDF image'
  }
  if (language == 'es') {
    langOpt <- getOption("highcharter.lang")
    langOpt$contextButtonTitle <- 'Opciones de descarga'
    langOpt$printChart <- 'Imprimir Grafico'
    langOpt$downloadJPEG <- 'Descarga en JPEG'
    langOpt$downloadPNG <- 'Descarga en PNG'
    langOpt$downloadSVG <- 'Descarga en SVG'
    langOpt$downloadPDF <- 'Descarga en PDF' }
  if (language == 'pt') {
    langOpt <- getOption("highcharter.lang")
    langOpt$contextButtonTitle <- 'Opcoes de baixar'
    langOpt$printChart <- 'Impressao visualizacao'
    langOpt$downloadJPEG <- 'Baixar em JPEG'
    langOpt$downloadPNG <- 'Baixar em PNG'
    langOpt$downloadSVG <- 'Baixar em SVG'
    langOpt$downloadPDF <- 'Baixar em PDF'
  }
  options(highcharter.lang = langOpt)
}

#' select discrete default color
#' @export

discreteColorSelect <- function (colorDefault, d) {
  lengData <- length(unique(d$a))
  lengColor <- length(colorDefault)
  if (lengData == lengColor) {
    colorDefault <- colorDefault
  } else if (lengData > lengColor) {
    colorDefault <- c(colorDefault, sample(colorDefault, lengData-lengColor,replace = TRUE))
  } else {
    colorDefault <- colorDefault[1:lengData]
  }
  colorDefault
}

