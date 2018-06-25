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
  c(x0, y0)
}

# order category column
#'@export
orderCategory <- function(data, col, order) {
  if (!is.null(order)) {
    order <- union(order, unique(data[[col]])[!is.na(unique(data[[col]]))])
    if (all(!is.na(order)) & any(is.na(data[[col]]))) order <- c(union(order, unique(data[[col]][!is.na(data[[col]])])), NA)
    order[is.na(order)] <- "NA"
    data <- data[order(match(data[[col]], order)), ]
  }
  data
}


# converts a numeric column into the equivalent percentage column
#'@export
percentColumn <- function(data, col, percentage = TRUE, nDt) {
  if (percentage)
    data$percent <- round((data[[col]] * 100) / sum(data[[col]], na.rm = TRUE),
                          #esto puede ser variable dep el format...
                          digits = nDt)
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

# sorting <- paste0('desc(', sortby, ')') #nse
# ord <- dplyr::arrange_(filt, .dots = sorting) #use arrange_
#return(ord)

# highlight value
#'@export
highlightValueData <- function(data, col, highlightValue, color, highlightColor) {
  data$color <- color
  if (!is.null(highlightValue)) {
    w <- which(data[[col]] %in% highlightValue)
    data$color[w] <- highlightColor
  }
  data
}

# confirms ctypes
#'@export
confirmCtypes <- function(data, ctypes) {
  d <- data
  map(1:length(ctypes), function(e) {
    if (ctypes[e] == "Num")
      d[[e]] <<- as.numeric(d[[e]])
    if (ctypes[e] == "Cat")
      d[[e]] <<- as.character(d[[e]])
    # if (ctypes[e] == "Yea")
    #   d[[e]] <<- as.Date(as.character(d[[e]]), "%Y")
    if (ctypes[e] == "Dat")
      d[[e]] <<- as.Date(d[[e]], "%Y-%m-%d")
  })
  d
}

# default tooltip for highcharts
#'@export
tooltipHc <- function(data, names, tooltip, agg, colAgg, percentage,  nDt, stacked100 = FALSE) {
  if (is.null(unlist(tooltip))) {
    n0 <- length(names)
    if (sum(names(data) %in% letters) == length(names)) {
      n0 <- length(names) - 1
    }
    pf <- map_chr(1:n0, ~paste0(names[.x], ": <b>{point.", letters[.x], "}</b>"))
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
    lang <- getOption("highcharter.lang")
    lang$thousandsSep <- marksT[1]
    lang$decimalPoint <- marksT[2]
    lang$numericSymbols <- highcharter::JS("null")
  lang
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

