library(tidyverse)
library(homodatum)
# Create meta data for funs
db <- tools::Rd_db("hgchmagic")
if(length(db)==0) stop("Restart session")
f <- function(rd){
  #rd <- db[[1]]
  rd <- capture.output(rd)
  con <- textConnection(rd)
  l <- Rd2roxygen::parse_file(con)
  l <- map_if(l,~length(.)==0,function(x)'')
  map(l,paste,collapse = "_")
}
funs <- map(db,f)
funsMeta <- funs %>% bind_rows()
funsMeta <- funsMeta %>% filter(grepl("^hgch_",name))
meta <- funsMeta[c("name","title","desc","section")]
meta$ctypes <- stringr::str_extract(meta$section,"(?<=\n).*?(?=\n)$")
meta$section <- NULL
meta$ftype <- ctypesToFtype(meta$ctypes, as_string = TRUE)
meta$group <- stringr::str_extract(meta$name,"(?<=_).*?(?=_)")
meta <- left_join(meta, select(read_csv("inst/meta-status.csv"), name, status), by = "name")
write_csv(meta,file.path("inst","meta.csv"))


## meta-status
metaStatus <- data.frame("name" = c("hgch_area_DatNum", "hgch_area_CatNum", "hgch_area_YeaNum", "hgch_area_CatCatNum", "hgch_area_CatYeaNum",
                                    "hgch_area_stacked_CatCatNum", "hgch_area_stacked_CatYeaNum", "hgch_area_stacked_100_CatCatNum",
                                    "hgch_area_stacked_100_CatYeaNum", "hgch_area_CatDatNum", "hgch_area_stacked_CatDatNum",
                                    "hgch_area_stacked_100_CatDatNum", "hgch_2yline_YeaNumNum", "hgch_multilines_YeaNumP",

                                    "hgch_bar_ver_Cat", "hgch_bar_ver_top_Cat", "hgch_bar_hor_Cat", "hgch_bar_hor_top_Cat", "hgch_bar_ver_CatNum",
                                    "hgch_bar_ver_top_CatNum", "hgch_bar_ver_YeaNum", "hgch_bar_ver_DatNum", "hgch_bar_hor_CatNum",
                                    "hgch_bar_hor_top_CatNum", "hgch_bar_hor_YeaNum", "hgch_bar_hor_DatNum", "hgch_bar_grouped_ver_CatCatNum", "hgch_bar_grouped_ver_CatYeaNum",
                                    "hgch_bar_grouped_ver_CatDatNum", "hgch_bar_grouped_hor_CatCatNum", "hgch_bar_grouped_hor_CatYeaNum",
                                    "hgch_bar_grouped_hor_CatDatNum", "hgch_bar_stacked_ver_CatCatNum", "hgch_bar_stacked_100_ver_CatCatNum",
                                    "hgch_bar_stacked_hor_CatCatNum", "hgch_bar_stacked_100_hor_CatCatNum", "hgch_bar_stacked_ver_CatDatNum",
                                    "hgch_bar_stacked_ver_CatYeaNum", "hgch_bar_stacked_hor_CatDatNum", "hgch_bar_stacked_hor_CatYeaNum",
                                    "hgch_bar_stacked_100_ver_CatDatNum", "hgch_bar_stacked_100_ver_CatYeaNum", "hgch_bar_stacked_100_hor_CatDatNum",
                                    "hgch_bar_stacked_100_hor_CatYeaNum", "hgch_bar_grouped_ver_CatNumNumNum", "hgch_bar_grouped_ver_CatNumNumNumNum","hgch_bar_grouped_ver_CatNumNum",
                                    "hgch_bar_grouped_hor_CatNumNum", "hgch_bar_grouped_hor_CatNumNumNum", "hgch_bar_grouped_hor_CatNumNumNumNum",
                                    "hgch_bar_ver_Num", "hgch_bar_ver_NumNum", "hgch_bar_ver_NumNumNum", "hgch_bar_ver_NumNumNumNum",
                                    "hgch_bar_hor_Num", "hgch_bar_hor_NumNum", "hgch_bar_hor_NumNumNum", "hgch_bar_hor_NumNumNumNum",
                                    "hgch_waterfall_CatNum", "hgch_bar_grouped_ver_CatCat", "hgch_bar_grouped_hor_CatCat",
                                    "hgch_bar_stacked_ver_CatCat", "hgch_bar_stacked_hor_CatCat", "hgch_bar_stacked_100_ver_CatCat",
                                    "hgch_bar_stacked_100_hor_CatCat", "hgch_circular_bar_CatNum", "hgch_bar_grouped_ver_CatNum", "hgch_bar_grouped_hor_CatNum",

                                    "hgch_line_DatNum", "hgch_line_CatNum", "hgch_line_CatYeaNum", "hgch_line_YeaNum", "hgch_line_CatCatNum",
                                    "hgch_line_CatDatNum", "hgch_2yline_YeaNumNum", "hgch_multilines_YeaNum", "hgch_multilines_YeaNumNum",
                                    "hgch_multilines_YeaNumNumNum", "hgch_multilines_YeaNumNumNumNum", "hgch_slope_CatYeaNum ",

                                    "hgch_pie_Cat", "hgch_pie_CatNum", "hgch_donut_Cat", "hgch_donut_CatNum", "hgch_radar_Cat",
                                    "hgch_radar_CatNum", "hgch_donut_CatNumNum", "hgch_radar_CatNumNumNum", "hgch_radar_CatNumNumNumNum",

                                    "hgch_treemap_Cat", "hgch_treemap_discrete_color_Cat", "hgch_treemap_CatNum",
                                    "hgch_treemap_discrete_color_CatNum", "hgch_treemap_nested_CatCatNum ",
                                    "hgch_treemap_nested_grouped_CatCatNum", "hgch_treemap_nested_grouped_CatNumNum",

                                    "hgch_scatter_CatNumNum", "hgch_scatter_grouped_CatNumNum", "hgch_scatter_CatNumNumNum", "hgch_scatter_CatCatNumNum", "hgch_scatter_CatCatNumNumNum"),



                         "status" = c("OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", NA, NA,

                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",
                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",
                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",
                                      "OK", NA, NA,

                                      "OK", "OK", "OK", "OK", "OK", "OK", NA, "OK", "OK", "OK", "OK", NA,

                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",

                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK",

                                      "OK", "OK", "OK", "OK", "OK"),

                         "coments" = c("Esta gráfica no sirve cuando hay un sólo dato. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       NA,
                                       "Esta gráfica no sirve cuando hay un sólo dato. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Falta revisar y habilitar función.", "Falta revisar y habilitar función.",

                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Sólo sirve para variables con pocos niveles categóricos",
                                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,

                                       "Ordenar los años", "Ordenar los años", "Ordenar los años", "Ordenar los años", "Ordenar los años", NA, NA, NA, NA, NA, NA, NA,

                                       NA, NA, NA, NA, NA, NA, NA, NA, NA,

                                       NA, NA, NA, NA, NA, NA, NA,

                                       NA, NA, NA, NA, NA)
)
#metaStatus <- left_join(select(hgchmagic::hgchMeta(), name), metaStatus, by = name)
write_csv(metaStatus, "inst/meta-status.csv")
