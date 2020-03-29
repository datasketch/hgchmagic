#' save
#' @export
save_hgchmagic <- function (viz, filename, format = NULL,
                            delay = 0.5,
                            width = 660, height = 500, ...) {
  if (is.null(format)) {
    format <- file_ext(filename) %||% "png"
  }
  filename <- file_path_sans_ext(filename)
  tmp <- paste(tempdir(), "html", sep = ".")
  htmltools::save_html(viz, tmp)
  tmpSave <- filename
  if (format == "html") {
    htmlwidgets::saveWidget(viz, paste0(filename, ".", format))
  }
  else {
    webshot::webshot(tmp, paste0(filename, ".", format),
                     delay = delay,
                     vwidth = width, vheight = height)
  }
  file.copy(filename, filename)
}

