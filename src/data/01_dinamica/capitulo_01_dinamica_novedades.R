# Librerias
library(xlsx)
library(dplyr)
library(data.table)
library(tidyverse)
library(tidyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
# Carpeta de referencia
setwd('D:/CONSULTORIAS/UGPP/2022/Proyectos/Boletin_UGPP/report/')
source(file = file.path('../src/utils/utils_01_dinamica.R'))

# Datos de entrada
# # Lectura del master de salidas
dat_master_meses <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Meses')
dat_master_rangoIBC <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Rango IBC')
dat_master_periodos <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'periodos_datos')
dat_master_anomes <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Ano_Mes')

## Definición de variables generales 
mes_interes <- 'Dic.21'
mes_referencia <- 'Jul.20'
mes_referencia2 <- 'Ene.21'

mes_interes_comparativo <- 'Dic.20'
mes_referencia2_comparativo <- 'Ene.20'


string_all <- 'Salidas_generales_rangoIBC'
list_csv_files <- dir(path = '../data/raw/')
list_datos <- list_csv_files[grep(string_all, list_csv_files)]
dat_consolidado_all <- NULL
for(ii in 1:length(list_datos)){
  datos_lectura <- fread(file = file.path('../data/raw/', list_datos[ii]))
  dat_consolidado_all <- rbind(dat_consolidado_all, datos_lectura)
} 

dat_consolidado_all$ano_mes <- with(dat_consolidado_all, paste(YEAR,MONTH,'1', sep = '/'))
dat_consolidado_all$ano_mes <- as.Date(dat_consolidado_all$ano_mes)

dat_consolidado_dependientes <- dat_consolidado_all %>% filter(Tipologia %in% c('Dep_sec_priv', 'Dep_No_priv'))
tabla4_dependientes <- fun_tabla4(dat_consolidado_dependientes)

tabla_plot4_Dep_1 <- tabla4_dependientes$totales %>% filter(Novedad %in% c('Ingreso', 'Retiro') & Nombre >= mes_referencia)

gg_figure_dependientes_1 <- ggplot(tabla_plot4_Dep_1, aes(x = Nombre, y = Total, group = Novedad, fill = Novedad, col = Novedad)) +
  geom_point(alpha = 0.4) +
  geom_smooth(aes(fill = Novedad), se = FALSE, span = 0.6) +
  theme_calc()+ scale_colour_calc() +
  scale_y_continuous(name = 'Total cotizantes', breaks = breaks_pretty(n = 20),
                     labels = scales:::number_format(big.mark = '.', decimal.mark = ',')) +
  scale_x_discrete(name = '') +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

ggsave(filename = '../figures/01_dinamica/total_novedades_dependientes_ingret.png',
              width = 10, height = 6,
              plot = gg_figure_dependientes_1)


# tabla_plot4_Dep_2 <- tabla4$totales %>% filter(Novedad %in% c('Suspension', 'Incapacidades', 'Vacaciones') & 
#                                                  Nombre >= mes_referencia)
# 
# gg_figure_dependientes_2 <- ggplot(tabla_plot4_Dep_2, aes(x = Nombre, y = Total, group = Novedad, fill = Novedad, col = Novedad)) +
#   geom_point(alpha = 0.4) +
#   geom_smooth(aes(fill = Novedad), se = FALSE, span = 0.6) + 
#   theme_calc()+ scale_colour_calc() + 
#   scale_y_continuous(name = 'Total cotizantes', breaks = breaks_pretty(n = 20), 
#                      labels = scales:::number_format(big.mark = '.', decimal.mark = ',')) + 
#   scale_x_discrete(name = '')


tabla_4_2_referencia <- tabla4_dependientes$totales %>% filter(Nombre %in% c(mes_referencia2, mes_interes) & 
                                                               Novedad %in% c('Suspension', 'Incapacidades', 'Vacaciones')) %>% 
  select(Nombre, Novedad, Total)
tabla_4_2_referencia <- tabla_4_2_referencia %>% spread(Nombre, Total)

tabla_4_2_meses <- tabla4_dependientes$totales %>% filter(Nombre >= mes_referencia2 & Nombre <= mes_interes  & 
                                                            Novedad %in% c('Suspension', 'Incapacidades', 'Vacaciones'))
tabla_4_2_meses_split <- split(tabla_4_2_meses$Total, tabla_4_2_meses$Novedad)
tabla_4_2_meses_split[1:2] <- NULL

salida_dependientes_novedades_resto <- fun_genera_tabla_4_2(tabla_4_2_referencia, tabla_4_2_meses_split, mes_interes, mes_referencia2)
salida_dependientes_novedades_resto %>%
  gtsave("salida_dependientes_novedades_resto.png", path = '../results/01_dinamica/')

##### Comparativo con ano pasado 
  
tabla_4_2_comparativo <- tabla4_dependientes$totales %>% filter(Nombre %in% c(mes_referencia2_comparativo, mes_interes_comparativo) & 
                                                                 Novedad %in% c('Suspension', 'Incapacidades', 'Vacaciones')) %>% 
  select(Nombre, Novedad, Total)
tabla_4_2_comparativo <- tabla_4_2_comparativo %>% spread(Nombre, Total)

tabla_4_2_meses_comparativo <- tabla4_dependientes$totales %>% filter(Nombre >= mes_referencia2_comparativo & Nombre <= mes_interes_comparativo  & 
                                                            Novedad %in% c('Suspension', 'Incapacidades', 'Vacaciones'))
tabla_4_2_meses_split_comparativo <- split(tabla_4_2_meses_comparativo$Total, tabla_4_2_meses_comparativo$Novedad)
tabla_4_2_meses_split_comparativo[1:2] <- NULL

salida_dependientes_novedades_resto_comparativo <- fun_genera_tabla_4_2(tabla_4_2_comparativo, tabla_4_2_meses_split_comparativo, mes_interes_comparativo, mes_referencia2_comparativo)
salida_dependientes_novedades_resto_comparativo %>%
  gtsave("salida_dependientes_novedades_resto_comparativo.png", path = '../results/01_dinamica/')



###  por dependientes e independientes
dat_consolidado_independientes <- dat_consolidado_all %>% filter(Tipologia %in% c('Independiente'))
tabla4_independientes <- fun_tabla4(dat_consolidado_independientes)

tabla_plot4_Indep_1 <- tabla4_independientes$totales %>% filter(Novedad %in% c('Ingreso', 'Retiro') & Nombre >= mes_referencia)

gg_figure_independientes_1 <- ggplot(tabla_plot4_Indep_1, aes(x = Nombre, y = Total, group = Novedad, fill = Novedad, col = Novedad)) +
  geom_point(alpha = 0.4) +
  geom_smooth(aes(fill = Novedad), se = FALSE, span = 0.6) + 
  theme_calc()+ scale_colour_calc() + 
  scale_y_continuous(name = 'Total cotizantes', breaks = breaks_pretty(n = 10), 
                     labels = scales:::number_format(big.mark = '.', decimal.mark = ',')) + 
  scale_x_discrete(name = '') +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))
gg_figure_independientes_1
ggsave(filename = '../figures/01_dinamica/total_novedades_independientes_ingret.png', 
       width = 10, height = 6, plot = gg_figure_independientes_1)

