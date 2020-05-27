
tooltip_codes <- list(
  bar = list(
  `Cat-Num` = list(
    a = "{point.name}",
    b = "{point.y}"
  ),
  `Cat-Cat-Num` = list(
    a = "{series.name}",
    b = "{point.category}",
    c = "{point.y}"
  )
  ),
  pie = list(
    `Cat-Num` = list(
      a = "{point.name}",
      b = "{point.value}"
    )
  ),
  donut = list(
    `Cat-Num` = list(
      a = "{point.name}",
      b = "{point.value}"
    )
  ),
  bubbles = list(
    `Cat-Num` = list(
      a = "{point.name}",
      b = "{point.value}"
    ),
    `Cat-Cat-Num` = list(
      a = "{series.name}",
      b = "{point.name}",
      c = "{point.y}"
    )
  ),
  treemap = list(
    `Cat-Num` = list(
      a = "{point.name}",
      b = "{point.value}"
    ),
    `Cat-Cat-Num` = list(
      a = "{point.node.name}",
      b = "{point.name}",
      c = "{point.y}"
    )
  ),
  scatter = list(
    `Num-Num` = list(
      a = "{point.x}",
      b = "{point.y}"
    )
  )
)
usethis::use_data(tooltip_codes,
                  internal = TRUE, overwrite = TRUE)


