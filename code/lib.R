theme_report <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
    ret <- ggplot2::theme_minimal(base_family = "Roboto-Regular",
                                  base_size = base_size, ...)
    ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                            margin=margin(b=strip_text_margin),
                                            family="Roboto-Bold")
    ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                               margin=margin(b=subtitle_margin),
                                               family="PT Sans")
    ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                            family="Oswald")
    ret
}

import_data <- function(){
    library(tidyverse)
    library(RSQLite)
    db <- dbConnect(dbDriver("SQLite"), 
                    dbname = here::here("data/atlas_v2.db"))
    
    municipios = tbl(db, 'atlas1e2') %>% 
        left_join(tbl(db, 'ibge_pop'), by = c("cidade", "uf", "regiao")) %>% 
        count(cidade, uf, regiao, codmun, segmento_principal, pop_dou_2017) %>% 
        collect() %>% 
        spread(key = segmento_principal, value = n, fill = 0)
    
}

read_projectdata <- function(){

}
