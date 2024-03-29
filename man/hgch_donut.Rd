% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_donut.R
\name{hgch_donut}
\alias{hgch_donut}
\title{Generate a Highcharts donut chart.}
\usage{
hgch_donut(data, dic = NULL, var_cat = NULL, var_num = NULL, ...)
}
\arguments{
\item{data}{A data frame containing the data to be plotted.}

\item{dic}{A named list of character vectors that replaces column names in data. The names of the list should match column names in data, and each vector should contain the replacement names.}

\item{var_cat}{A character vector of categorical variable(s) to use in the chart.}

\item{var_num}{A character vector of numeric variable(s) to use in the chart.}

\item{...}{Additional arguments to be passed to \code{\link{plot_opts}}.}
}
\value{
A Highcharts donut chart object.
}
\description{
This function generates a Highcharts donut chart based on the given data.
}
\examples{
data1 <- data.frame(name = c("A", "B", "C"),
                    y = c(10, 20, 30))
hgch_donut(data1,
           var_cat = "name",
           var_num = "y")

# Custom theme
 test_theme <- list(
   theme = list(
     palette_colors = c("#ffa92a", "lightgreen", "salmon"),
     subtitle_align = "center",
     subtitle_family = "Roboto",
     subtitle_size = 15,
     subtitle_color = "#3b83b8",
     subtitle_weight = 700
   )
 )

 hgch_donut(data1,
              var_num = "y",
              var_cat = "name",
              opts = test_theme,
              title = "test title",
              subtitle = "test subtitle")
}
\seealso{
\code{\link{data_draw}}
}
