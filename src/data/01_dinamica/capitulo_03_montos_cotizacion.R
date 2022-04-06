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
library(gt)
library(scales)
library(readr)
library(paletteer)
library(sbpiper)
library(wesanderson)
options(ztable.type="html")
# Carpeta de referencia
setwd('D:/CONSULTORIAS/UGPP/2022/Proyectos/Boletin_UGPP/report/')
source(file = file.path('../src/utils/utils_01_dinamica.R'))

# Datos de entrada
# # Lectura del master de salidas
dat_master_meses <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Meses')
dat_master_rangoIBC <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Rango IBC')
dat_master_periodos <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'periodos_datos')
dat_master_anomes <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Ano_Mes')
dat_deflactar <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'deflactar')

## Definición de variables generales 
mes_acumulado <- 12 
mes_interes <- as.Date('2021-12-01')
mes_referencia <- as.Date('2021-01-01')

mes_interes_comparativo <- as.Date('2020-12-01')
mes_referencia_comparativo <- as.Date('2020-01-01')
mes_interes_comparativo_2 <- as.Date('2019-01-01')
cat('Se va a calcular acumulado entre:', as.character(mes_interes), ' y ', as.character(mes_referencia))
cat('Se va a calcular acumulado comparativo entre:', as.character(mes_interes_comparativo), ' y ', as.character(mes_referencia_comparativo))

nombres_rangoIBC <- dat_master_rangoIBC$Nombre
limites_rangoIBC <- c(dat_master_rangoIBC$Limite_inferior, dat_master_rangoIBC$limite_superior, 100)
limites_rangoIBC <- unique(sort(limites_rangoIBC))

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
dat_consolidado_all <- dat_consolidado_all %>% 
  mutate(total_monto = 
           cot_obligatoria_salud_Max_Sum + 
           aporte_cot_obligatoria_pension_Max_Sum + 
           cot_obligatoria_arl_Max_Sum + 
           valor_aportes_ccf_Max_Sum + 
           valor_aportes_parafiscales_sena_Max_Sum + 
           valor_aportes_parafiscales_icbf_Max_Sum )
dat_consolidado_all$rangoIBC_salida <- with(dat_consolidado_all, 
                                        cut(Rango_IBC, breaks = limites_rangoIBC, labels = nombres_rangoIBC))

dat_consolidado_all$tipologia_salida <- with(dat_consolidado_all, 
                                         ifelse(Tipologia == 'Independiente', 'Independiente', 'Dependiente'))

dat_consolidado_all <- merge(dat_consolidado_all, dat_deflactar[, c('ano_mes', 'Coeficiente')], all.x = T)
dat_consolidado_all$total_monto_actualizado <- with(dat_consolidado_all, total_monto * Coeficiente)
  
salida_tabla17 <- dat_consolidado_all %>% 
  group_by(MONTH, YEAR, ano_mes, rangoIBC_salida, tipologia_salida) %>%
  summarise(Acumulado = sum(total_monto_actualizado), 
            Acumulado2 = sum(total_monto))

lll <-salida_tabla17 %>% filter(ano_mes == '2021-12-01')


salida_tabla17 <- merge(salida_tabla17, 
                        dat_master_anomes,
                        by.x = c('MONTH', 'YEAR'),
                        by.y = c('Codigo', 'Ano'))
salida_tabla17$Nombre <- factor(salida_tabla17$Nombre, levels = dat_master_anomes$Nombre)


salida_tabla17_interes <- salida_tabla17 %>%
  filter(ano_mes >= mes_referencia & ano_mes <= mes_interes) %>%
  group_by(tipologia_salida, rangoIBC_salida) %>%
  summarise(Acumulado_interes = sum(Acumulado)/1e6)

salida_tabla17_comparativo <- salida_tabla17 %>%
  filter(ano_mes >= mes_referencia_comparativo & ano_mes <= mes_interes_comparativo) %>%
  group_by(tipologia_salida  , rangoIBC_salida) %>%
  summarise(Acumulado_comparativo = sum(Acumulado)/1e6)

nombre_int <- dat_master_anomes %>% filter(Ano %in% year(mes_interes) & Codigo %in% month(mes_interes))
nombre_ref <- dat_master_anomes %>% filter(Ano %in% year(mes_referencia) & Codigo %in% month(mes_referencia))
nombre_interes <- paste(nombre_ref$Nombre, nombre_int$Nombre, sep = '-')

nombre_intComp <- dat_master_anomes %>% filter(Ano %in% year(mes_interes_comparativo) & Codigo %in% month(mes_interes_comparativo))
nombre_refComp <- dat_master_anomes %>% filter(Ano %in% year(mes_referencia_comparativo) & Codigo %in% month(mes_referencia_comparativo))
nombre_comparativo <- paste(nombre_refComp$Nombre, nombre_intComp$Nombre, sep = '-')

salida_tabla17_completa <- merge(salida_tabla17_interes, salida_tabla17_comparativo, by = c('tipologia_salida', 'rangoIBC_salida'))

salida_tabla17_completa <- salida_tabla17_completa%>%
  adorn_totals("row")
salida_tabla17_completa$variacion <- salida_tabla17_completa$Acumulado_interes/salida_tabla17_completa$Acumulado_comparativo - 1

nombre_interes <- paste(nombre_interes, '($)', sep = ' ')
nombre_comparativo <- paste(nombre_comparativo, '($)', sep = ' ')
salida_tabla17_completa <- salida_tabla17_completa %>% rename(!!nombre_interes := 'Acumulado_interes')
salida_tabla17_completa <- salida_tabla17_completa %>% rename(!!nombre_comparativo := 'Acumulado_comparativo')


salida_tabla17_formato <- salida_tabla17_completa %>% 
  gt(groupname_col = 'tipologia_salida', rowname_col = 'rangoIBC_salida') %>% 
  data_color(
    columns = 5, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material") %>% as.character(),
      domain = NULL)) %>% 
  fmt_percent(columns = 5, scale_values = TRUE, decimals = 1)  %>% 
  fmt_number(columns = 3:4, sep_mark =  '.', dec_mark = ',', decimals = 0, accounting = TRUE)  %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )) %>%
  cols_label(
    tipologia_salida = 'Tipología',
    rangoIBC_salida = 'Rango IBC',
    variacion = '%'
  )

salida_tabla17_formato %>%
  gtsave("salida_montos_cotizacion_interes_coomparativo.png", path = '../results/03_financiero/')



dat_consolidado_all <- dat_consolidado_all %>% 
  mutate(actualizado_salud = cot_obligatoria_salud_Max_Sum  * Coeficiente,
           actualizado_pension =   aporte_cot_obligatoria_pension_Max_Sum * Coeficiente,
           actualizado_arl =   cot_obligatoria_arl_Max_Sum * Coeficiente,
           actualizado_ccf =   valor_aportes_ccf_Max_Sum * Coeficiente,
           actualizado_sena =  valor_aportes_parafiscales_sena_Max_Sum * Coeficiente,
           actualizado_icbf =  valor_aportes_parafiscales_icbf_Max_Sum * Coeficiente) 

dat_montos_subsistema <- dat_consolidado_all %>%   
  filter(ano_mes %in% c(mes_interes, mes_interes_comparativo, mes_interes_comparativo_2)) %>%
  group_by(tipologia_salida, ano_mes) %>%
  summarise(total_salud = sum(actualizado_salud)/1e6, 
            total_pension = sum(actualizado_pension)/1e6,
            total_arl = sum(actualizado_arl)/1e6,
            total_ccf = sum(actualizado_ccf)/1e6,
            total_sena = sum(actualizado_sena)/1e6,
            total_icbf = sum(actualizado_icbf)/1e6)
dat_montos_subsistema$total_sena <- ifelse(dat_montos_subsistema$tipologia_salida == 'Independiente', NA, dat_montos_subsistema$total_sena)
dat_montos_subsistema$total_icbf <- ifelse(dat_montos_subsistema$tipologia_salida == 'Independiente', NA, dat_montos_subsistema$total_icbf)
dat_montos_subsistema <- dat_montos_subsistema%>%
  adorn_totals("col")

dat_montos_subsistema$year <- year(dat_montos_subsistema$ano_mes)
dat_montos_subsistema$ano_mes <- NULL

df_t = as_tibble(t(dat_montos_subsistema[, -1]))
colnames(df_t) = dat_montos_subsistema[, 1]

salida_subsistema_formato <- dat_montos_subsistema %>% 
  gt(groupname_col = 'tipologia_salida', rowname_col = 'year') %>% cols_width(
    c(tipologia_salida) ~ px(125),
  ) %>%
  tab_options(
    table.width = px(800)
  ) %>%
  fmt_missing(
    columns = 6:7,
    missing_text = "-"
  ) %>%
  fmt_number(columns = 2:8,  sep_mark =  '.', dec_mark = ',', decimals = 0, accounting = FALSE)  %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )) %>%
  cols_label(
    total_salud = 'Salud',
    total_pension = 'Pensión',
    total_arl = 'ARL',
    total_ccf = 'CCF', 
    total_sena = 'SENA', 
    total_icbf = 'ICBF',
    year = 'Año'
  )

salida_subsistema_formato %>%
  gtsave("salida_montos_cotizacion_subsistema.png", path = '../results/03_financiero/')



tabla_plot4_Dep_1 <- dat_consolidado_all %>% filter(tipologia_salida %in% c('Dependiente') & Nombre >= mes_referencia)

gg_figure_dependientes_1 <- ggplot(tabla_plot4_Dep_1, aes(x = Nombre, y = Total, group = Novedad, fill = Novedad, col = Novedad)) +
  geom_point(alpha = 0.4) +
  geom_smooth(aes(fill = Novedad), se = FALSE, span = 0.6) +
  theme_calc()+ scale_colour_calc() +
  scale_y_continuous(name = 'Total cotizantes', breaks = breaks_pretty(n = 20),
                     labels = scales:::number_format(big.mark = '.', decimal.mark = ',')) +
  scale_x_discrete(name = '') +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))




dat_consolidado <- dat_consolidado_all
dat_consolidado$tipologia_salida <- with(dat_consolidado, 
                                         ifelse(Tipologia == 'Independiente', 'Independiente', 'Dependiente'))

dat_consolidado$rangoIBC_salida <- with(dat_consolidado, 
                                        cut(Rango_IBC, breaks = limites_rangoIBC, labels = nombres_rangoIBC))
dat_consolidado$ano_mes <- with(dat_consolidado, paste(YEAR,MONTH,'1', sep = '/'))
dat_consolidado$ano_mes <- as.Date(dat_consolidado$ano_mes)

lll <- dat_consolidado %>%
  group_by(ano_mes, rangoIBC_salida) %>%
  summarise(total_monto = sum(total_monto))


dat_consolidado_all <- dat_consolidado %>%
  filter(MONTH %in% mes_interes & YEAR %in% c(ano_referencia, ano_interes)) %>%
  mutate_at(vars(c(rangoIBC_salida)), funs(as.character(.))) %>%
  bind_rows(mutate(., rangoIBC_salida = "Total")) %>%
  group_by(tipologia_salida, YEAR, rangoIBC_salida) %>%
  summarise(total_cotizantes = sum(Total_relaciones_laborales))



fun_tabla3 <- function(mes_interes, mes_referencia, 
                       ano_interes, ano_referencia,
                       dat_master_anomes, dat_master_rangoIBC, meses_variacion = 5){
  tipo <- 'rangoIBC'
  mes_variaciones <- mes_interes - meses_variacion
  mes_interes_mes5 <- ifelse(mes_variaciones < 0, mes_variaciones%%12, mes_variaciones)
  mes_referencia_mes5 <- mes_interes_mes5
  ano_interes_mes5 <- ifelse(mes_variaciones < 0, ano_interes - 1, ano_interes)
  ano_referencia_mes5 <- ifelse(mes_variaciones < 0, ano_referencia - 1, ano_referencia)
  
  limites_rangoIBC <- c(dat_master_rangoIBC$Limite_inferior, 100)
  
  
  nombre_datos_interes <- fun_crea_nombre_datos(dat_master_periodos, ano_interes, mes_interes, tipo)
  nombre_datos_referencia <- fun_crea_nombre_datos(dat_master_periodos, ano_referencia, mes_interes, tipo)
  nombre_datos_interes_mes5 <- fun_crea_nombre_datos(dat_master_periodos, ano_interes_mes5, mes_interes_mes5, tipo)
  nombre_datos_referencia_mes5 <- fun_crea_nombre_datos(dat_master_periodos, ano_referencia_mes5, mes_referencia_mes5, tipo)
  
  dat_interes <- fread(file = file.path('../data/raw/', nombre_datos_interes))
  dat_referencia <- fread(file = file.path('../data/raw/', nombre_datos_referencia))
  
  dat_interes_mes5 <- fread(file = file.path('../data/raw/', nombre_datos_interes_mes5))
  dat_referencia_mes5 <- fread(file = file.path('../data/raw/', nombre_datos_referencia_mes5))
  
  if(nombre_datos_interes != nombre_datos_interes_mes5){
    dat_consolidado <- rbind(dat_referencia_mes5, 
                             dat_referencia, 
                             dat_interes, 
                             dat_interes_mes5)
  }else{
    stop('Error: Revisar datos coomparativos por mes')
  }
  dat_consolidado$tipologia_salida <- with(dat_consolidado, 
                                           ifelse(Tipologia == 'Independiente', 'Independiente', 'Dependiente'))
  
  dat_consolidado$rangoIBC_salida <- with(dat_consolidado, 
                                          cut(Rango_IBC, breaks = limites_rangoIBC, labels = nombres_rangoIBC))
  dat_consolidado$ano_mes <- with(dat_consolidado, paste(YEAR,MONTH,'1', sep = '/'))
  dat_consolidado$ano_mes <- as.Date(dat_consolidado$ano_mes)
  
  dat_consolidado <- dat_consolidado %>% arrange(ano_mes)
  salida_tabla3_total <- dat_consolidado %>%
    filter(MONTH %in% mes_interes & YEAR %in% c(ano_referencia, ano_interes)) %>%
    mutate_at(vars(c(rangoIBC_salida)), funs(as.character(.))) %>%
    bind_rows(mutate(., rangoIBC_salida = "Total")) %>%
    group_by(tipologia_salida, YEAR, rangoIBC_salida) %>%
    summarise(total_cotizantes = sum(Total_relaciones_laborales))
  salida_tabla3_total <- salida_tabla3_total %>% spread(YEAR, total_cotizantes)
  salida_tabla3_total$rangoIBC_salida <- factor(salida_tabla3_total$rangoIBC_salida, 
                                                levels = c(nombres_rangoIBC, 'Total'))
  salida_tabla3_total <- salida_tabla3_total %>% 
    arrange(tipologia_salida, rangoIBC_salida)
  colnames(salida_tabla3_total)[c(3,4)] <- c('referencia','interes')
  
  salida_tabla3_total <- salida_tabla3_total %>%
    mutate(diferencia = interes - referencia, 
           variacion = interes/referencia - 1)
  
  nombres_salida_total <- dat_master_anomes %>% filter(Codigo == mes_interes & Ano %in% c(ano_interes, ano_referencia))
  setnames(salida_tabla3_total, old = c('referencia','interes', 'diferencia', 'variacion'), 
           new = c(nombres_salida_total$Nombre, 'dif', 'porc'))
  # Variaciones en meses
  
  ano_mes_interes <- paste(ano_interes, mes_interes, '1', sep ='/')
  ano_mes_interes <- as.Date(ano_mes_interes)
  ano_mes_interes_mes5 <- paste(ano_interes_mes5, mes_interes_mes5, '1', sep ='/')
  ano_mes_interes_mes5 <- as.Date(ano_mes_interes_mes5)
  sequencia_interes <- seq(ano_mes_interes_mes5, ano_mes_interes, by = 'month')
  
  ano_mes_referencia <- paste(ano_referencia, mes_referencia, '1', sep ='/')
  ano_mes_referencia <- as.Date(ano_mes_referencia)
  ano_mes_referencia_mes5 <- paste(ano_referencia_mes5, mes_referencia_mes5, '1', sep ='/')
  ano_mes_referencia_mes5 <- as.Date(ano_mes_referencia_mes5)
  sequencia_referencia <- seq(ano_mes_referencia_mes5, ano_mes_referencia, by = 'month')
  
  salida_tabla3_interes_var <- dat_consolidado %>%
    filter(ano_mes %in% sequencia_interes) %>%
    mutate_at(vars(c(rangoIBC_salida)), funs(as.character(.))) %>%
    bind_rows(mutate(., rangoIBC_salida = "Total")) %>%
    group_by(tipologia_salida, rangoIBC_salida, ano_mes) %>%
    summarise(total_cotizantes = sum(Total_relaciones_laborales))
  
  salida_tabla3_interes_var <- salida_tabla3_interes_var %>%
    mutate(lag_total = lag(total_cotizantes)) %>%
    mutate(variacion = (total_cotizantes - lag_total) / lag_total) 
  
  salida_tabla3_interes_var$mes <- month(salida_tabla3_interes_var$ano_mes)
  salida_tabla3_interes_var$ano <- year(salida_tabla3_interes_var$ano_mes)
  
  salida_tabla3_interes_var <- merge(salida_tabla3_interes_var, 
                                     dat_master_anomes[, c('Codigo', 'Ano','Nombre')],
                                     by.x = c('mes', 'ano'),
                                     by.y = c('Codigo', 'Ano'))
  salida_tabla3_interes_var$Nombre <- factor(salida_tabla3_interes_var$Nombre, dat_master_anomes$Nombre)
  salida_tabla3_interes_var <- salida_tabla3_interes_var %>%
    filter(ano_mes != ano_mes_interes_mes5) %>%
    select(-one_of(c('mes', 'ano', 'ano_mes', 'total_cotizantes', 'lag_total'))) %>%
    arrange(tipologia_salida, Nombre, rangoIBC_salida) %>%
    spread(Nombre, variacion)
  
  
  salida_tabla3_referencia_var <- dat_consolidado %>%
    filter(ano_mes %in% sequencia_referencia) %>%
    mutate_at(vars(c(rangoIBC_salida)), funs(as.character(.))) %>%
    bind_rows(mutate(., rangoIBC_salida = "Total")) %>%
    group_by(tipologia_salida, rangoIBC_salida, ano_mes) %>%
    summarise(total_cotizantes = sum(Total_relaciones_laborales))
  salida_tabla3_referencia_var <- salida_tabla3_referencia_var %>%
    mutate(lag_total = lag(total_cotizantes)) %>%
    mutate(variacion = (total_cotizantes - lag_total) / lag_total) 
  salida_tabla3_referencia_var$mes <- month(salida_tabla3_referencia_var$ano_mes)
  salida_tabla3_referencia_var$ano <- year(salida_tabla3_referencia_var$ano_mes)
  
  salida_tabla3_referencia_var <- merge(salida_tabla3_referencia_var, 
                                        dat_master_anomes[, c('Codigo', 'Ano','Nombre')],
                                        by.x = c('mes', 'ano'),
                                        by.y = c('Codigo', 'Ano'))
  salida_tabla3_referencia_var$Nombre <- factor(salida_tabla3_referencia_var$Nombre, dat_master_anomes$Nombre)
  salida_tabla3_referencia_var <- salida_tabla3_referencia_var %>%
    filter(ano_mes != ano_mes_referencia_mes5) %>%
    select(-one_of(c('mes', 'ano', 'ano_mes', 'total_cotizantes', 'lag_total'))) %>%
    arrange(tipologia_salida, Nombre, rangoIBC_salida) %>%
    spread(Nombre, variacion)
  
  colnames(salida_tabla3_total)[c(1:2)] <- c('Tipologia', 'RangoIBC')
  colnames(salida_tabla3_interes_var)[c(1:2)] <- c('Tipologia', 'RangoIBC')
  colnames(salida_tabla3_referencia_var)[c(1:2)] <- c('Tipologia', 'RangoIBC')
  
  return(list(total = salida_tabla3_total, 
              variaciones_interes = salida_tabla3_interes_var,
              variaciones_referencia = salida_tabla3_referencia_var))
}

