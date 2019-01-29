#' save
#' @export
save_hgchmagic <- function(viz,
                           filename,
                           format = NULL,
                           width = 660,
                           height = 500, ...) {
  format <- file_ext(filename) %||% "png"
  tmp <- paste(tempdir(), 'html', sep ='.')
  htmltools::save_html(viz, tmp)
  tmpSave <- filename
  if (format == '.html') {
    htmlwidgets::saveWidget(viz, filename)
  } else {
    webshot(tmp, filename, vwidth = width, vheight = height)
  }
  file.copy(filename, filename)

}

