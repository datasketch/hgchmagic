
#
#
# if (is.null(data)) {
#   stop("Load an available dataset")
# }
#
# defaultOptions <- list(
#   agg = agg,
#   agg_text = agg_text,
#   allow_point = allow_point,
#   background = background,
#   border_color = border_color,
#   border_width = border_width,
#   caption = caption,
#   clickFunction = clickFunction,
#   colors = colors,
#   color_click = color_click,
#   color_hover = color_hover,
#   color_opacity = color_opacity,
#   color_scale = color_scale,
#   cursor =  cursor,
#   drop_na = drop_na,
#   export = export,
#   horLabel = horLabel,
#   horLine = horLine,
#   horLine_label = horLine_label,
#   labelWrap = labelWrap,
#   lang = lang,
#   legend_position  = legend_position,
#   legend_show = legend_show,
#   marks = marks,
#   nDigits = nDigits,
#   null_color = null_color,
#   order = order,
#   orientation = orientation,
#   percentage = percentage,
#   prefix = prefix,
#   text_show = text_show,
#   sort = sort,
#   subtitle = subtitle,
#   suffix = suffix,
#   title = title,
#   theme = theme,
#   tooltip = tooltip,
#   verLabel = verLabel,
#   verLine = verLine,
#   verLine_label = verLine_label
# )
#
# opts <- modifyList(defaultOptions, opts %||% list())
#
# f <- fringe(data)
# nms <- getClabels(f)
# d <- f$d
#
# title <-  opts$title %||% ""
# subtitle <- opts$subtitle %||% ""
# caption <- opts$caption %||% ""
#
#
#
