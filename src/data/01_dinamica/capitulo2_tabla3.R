# Librerias
library(xlsx)
library(dplyr)
library(data.table)
library(tidyverse)
library(tidyr)
library(janitor)
library(lubridate)
library(janitor)
library(sjmisc)
library(gt)
library(magrittr)
library(gt)
library(scales)
library(readr)
library(paletteer)
library(sbpiper)
library(wesanderson)
options(ztable.type="html")
# Carpeta de referencia
setwd('D:/CONSULTORIAS/UGPP/2022/Proyectos/Boletin_UGPP/report/')
source(file = file.path('../src/utils/utils_dinamica.R'))

# Datos de entrada
# # Lectura del master de salidas
dat_master_meses <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Meses')
dat_master_rangoIBC <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Rango IBC')
dat_master_periodos <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'periodos_datos')
dat_master_anomes <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Ano_Mes')

## Definición de variables generales 
mes_interes <- 12
mes_referencia <- mes_interes
ano_interes <- 2021
ano_referencia <- 2020
meses_variacion <- 5
tabla3 <- fun_tabla3(mes_interes, mes_referencia, 
           ano_interes, ano_referencia,
           dat_master_anomes, dat_master_rangoIBC, meses_variacion)

salida_table3_total<- tabla3$total %>% 
  gt(groupname_col = 'Tipologia', rowname_col = 'RangoIBC') %>% 
  data_color(
    columns = 6, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material", direction = -1) %>% as.character(),
      domain = NULL)) %>% 
  fmt_percent(columns = 6, scale_values = TRUE, decimals = 1)  %>% 
  fmt_number(columns = 3:5, sep_mark =  '.', dec_mark = ',', decimals = 0)  %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )) %>%
  cols_label(
    Tipologia = 'Tipología',
    RangoIBC = 'Rango IBC',
    dif = 'Diferencia',
    porc = '%'
  )

salida_table3_total %>%
  gtsave("salida_total_cotizantes.png", path = '../results/01_dinamica/')

tabla3_variaciones <- merge(tabla3$variaciones_referencia, tabla3$variaciones_interes)
tabla3_variaciones <- tabla3_variaciones %>% arrange(Tipologia, RangoIBC)

salida_table3_variaciones <- tabla3_variaciones %>% 
  gt(groupname_col = 'Tipologia', rowname_col = 'RangoIBC') %>% 
  data_color(
    columns = 3:12, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material", direction = -1) %>% as.character(),
      domain = NULL)) %>%
  fmt_percent(columns = 3:12, scale_values = TRUE, decimals = 1)  %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )) %>%
  cols_label(
    Tipologia = 'Tipología',
    RangoIBC = 'Rango IBC'
  )

salida_table3_variaciones %>%
  gtsave("salida_total_cotizantes_variaciones.png", path = '../results/01_dinamica/')
             