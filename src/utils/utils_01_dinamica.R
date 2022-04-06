fun_crea_nombre_datos <- function(dat_master_periodos, ano_interes, mes_interes, tipo){
  dat_periodo_interes <- dat_master_periodos %>% filter(Ano == ano_interes)
  for(ii in 1:nrow(dat_periodo_interes)){
    mes_inicial <- dat_periodo_interes[ii, 'Mes_inicial']
    mes_final <- dat_periodo_interes[ii, 'Mes_final']
    if(mes_inicial <= mes_interes & mes_interes <= mes_final){
      referencia_raw_dat <- paste(mes_inicial, '_', mes_final, sep = '')
    }
  }
  nombre_datos_referencia <- paste('Salidas_generales_', tipo, referencia_raw_dat, '_', ano_interes, '.csv', sep = '')
  return(nombre_datos_referencia)
}


fun_tabla3 <- function(mes_interes, mes_referencia, 
                       ano_interes, ano_referencia,
                       dat_master_anomes, dat_master_rangoIBC, meses_variacion = 5){
  tipo <- 'rangoIBC'
  mes_variaciones <- mes_interes - meses_variacion
  mes_interes_mes5 <- ifelse(mes_variaciones < 0, mes_variaciones%%12, mes_variaciones)
  mes_referencia_mes5 <- mes_interes_mes5
  ano_interes_mes5 <- ifelse(mes_variaciones < 0, ano_interes - 1, ano_interes)
  ano_referencia_mes5 <- ifelse(mes_variaciones < 0, ano_referencia - 1, ano_referencia)
  
  limites_rangoIBC <- c(dat_master_rangoIBC$Limite_inferior, dat_master_rangoIBC$limite_superior, 100)
  limites_rangoIBC <- unique(sort(limites_rangoIBC))
  nombres_rangoIBC <- dat_master_rangoIBC$Nombre
  
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



fun_tabla4 <- function(dat_consolidado){
  salida_tabla4A_total <- dat_consolidado %>%
    # filter(MONTH %in% c(mes_interes, mes_referencia) & 
    #          YEAR %in% c(ano_interes, ano_referencia, ano_referencia_2)) %>%
    group_by(MONTH, YEAR) %>%
    summarise(Ingreso = sum(ingreso_Max_Sum),
              Retiro = sum(retiro_Max_Sum),
              Suspension = sum(suspension_temporal_Max_Sum),
              Incapacidades = sum(incapacidas_por_trabajo_Max_Sum),
              Vacaciones = sum(vacaciones_Max_Sum))
  
  salida_tabla4A_total <- melt(setDT(salida_tabla4A_total), id.vars = c('MONTH', 'YEAR'), 
                               variable.name = 'Novedad', value.name = 'Total')
  salida_tabla4A_total <- merge(salida_tabla4A_total, 
                                dat_master_anomes,
                                by.x = c('MONTH', 'YEAR'),
                                by.y = c('Codigo', 'Ano'))
  salida_tabla4A_total <- salida_tabla4A_total %>% arrange(Novedad, YEAR, MONTH)
  salida_tabla4A_total <- salida_tabla4A_total %>% 
    mutate(lag_total_mes = lag(Total)) %>%
    mutate(variacion_mes = (Total - lag_total_mes) / lag_total_mes) ## PAra obtener datos de variación mensual, eliminar todos los primeros de cada ano 
  
  salida_tabla4A_total <- salida_tabla4A_total %>% arrange(MONTH, Novedad, YEAR) %>% 
    mutate(lag_total_year = lag(Total)) %>%
    mutate(variacion_ano = (Total - lag_total_year) / lag_total_year)  ### PAra obtener variaciones inter anuales eliminar datos de 2018 

  salida_tabla4A_total$Nombre <- factor(salida_tabla4A_total$Nombre, dat_master_anomes$Nombre)
  salida_tabla4A_total$Nombre <- ordered(salida_tabla4A_total$Nombre, levels = dat_master_anomes$Nombre)
  
  salida_tabla4A_total$Mes_corto <- factor(salida_tabla4A_total$Mes_corto, dat_master_anomes$Mes_corto[1:12])
  salida_tabla4A_total$Mes_corto <- ordered(salida_tabla4A_total$Mes_corto, levels = dat_master_anomes$Mes_corto[1:12])
  
  salida_tabla4A_total$Ano_corto <- factor(salida_tabla4A_total$Ano_corto)
  
  salidas_inter_mensuales <- salida_tabla4A_total %>% 
    filter(MONTH != 1) %>% select(MONTH, YEAR, Ano_corto, Mes_corto, Novedad, Nombre, variacion_mes) %>% 
    arrange(Novedad, YEAR, MONTH)
  
  salidas_inter_anuales <- salida_tabla4A_total %>% 
    filter(YEAR != 2018) %>% 
    select(MONTH, YEAR, Ano_corto, Novedad, Nombre, variacion_ano) %>% 
    arrange(MONTH, Novedad, YEAR)
  return(list(mensual = salidas_inter_mensuales, anual = salidas_inter_anuales, totales = salida_tabla4A_total))
  
}


fun_genera_tabla_4_2 <- function(tabla_4_2_referencia, tabla_4_2_meses_split, mes_interes, mes_referencia2){
  salida_dependientes_novedades_resto <- tabla_4_2_referencia %>% 
    mutate(spark = map(tabla_4_2_meses_split, kableExtra::spec_plot),
           spark = map(spark, "svg_text"),
           spark = map(spark, ~html(as.character(.x)))) %>% 
    gt() %>% 
    cols_label(
      spark = paste(mes_referencia2, mes_interes, sep = '-')
    ) %>% 
    fmt_number(columns = 2:3, sep_mark =  '.', dec_mark = ',', decimals = 0)  %>% 
    tab_spanner(
      label = md("Dependientes"),
      columns = c(2,3)
    ) %>% 
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_column_spanners(everything()),
        cells_column_labels(everything())
      )
    ) %>%  
    tab_options(
      row_group.border.top.width = px(3),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      table.border.top.color = "white",
      table.border.top.width = px(3),
      table.border.bottom.color = "white",
      table.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(2),
    )
  return(salida_dependientes_novedades_resto)
}
