library(data.table)
library(xlsx)
library(dplyr)
library(tidyverse)
setwd('D:/CONSULTORIAS/UGPP/2022/Enero/Informacion_general')


already_read <- FALSE
if(!already_read){
  list_csv_files <- dir()
  string_completo <- 'Salidas_generales_Completo'
  list_datos_completo <- list_csv_files[grep(string_completo, list_csv_files)]
  list_datos_completo <- list_datos_completo[grep('2019|2020|2021', list_datos_completo)]
  list_assign_sub <- gsub('.csv', '', list_datos_completo)
  
  dat_consolidate_completo <- NULL
  for(ii in 2:length(list_assign_sub)){
    datos_lectura <- fread(file = list_datos_completo[ii])
    /datos_lectura <- datos_lectura %>% filter(departamento_COT_Max == 11)
    #assign(list_assign_sub[ii], datos_lectura)
    #save(list = list_assign_sub[ii], file = paste(list_assign_sub[ii], ".Rdata", sep = ""))
    #
    cat('finish\n')
    dat_consolidate_completo <- rbind(dat_consolidate_completo, datos_lectura)
    #rm(list = datos_lectura):gc()
  }  
  save(dat_consolidate_completo, file = 'dat_consolidate_completo_11001.Rdata')
}else{
  load('dat_consolidate_completo_11001.Rdata')
}

month <- data.frame(mes = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'), MONTH = 1:12)
ano <- data.frame(ano = 19:21)
ano_month <- expand.grid(month$mes, ano$ano)
ano_month$ano_mes <- with(ano_month, paste(Var1, Var2))

dat_consolidate_completo <- merge(dat_consolidate_completo, month, all.x = T)
dat_consolidate_completo$ano <- substr(dat_consolidate_completo$YEAR, 3, 4)
dat_consolidate_completo$ano_mes <- with(dat_consolidate_completo, paste(mes, ano))

dat_consolidate_completo$ano_mes <- factor(dat_consolidate_completo$ano_mes, levels = ano_month$ano_mes)
dat_consolidate_completo <- dat_consolidate_completo %>% arrange(ano_mes)

datt = fread('Salidas_generales_DPTO_MPIO1_4_2018.csv')

########################################################################################################
#### PRocesamiento por sexo y edad
########################################################################################################

group_edad_sexo_tipologia <- dat_consolidate_completo %>% group_by(Tipologia, ano_mes, Edad_fin, Sexo_fin) %>% summarise(Total = sum(Total_relaciones_laborales, na.rm = T))
group_edad_sexo_tipologia_long <-  group_edad_sexo_tipologia %>% spread(ano_mes , Total)

grouped_edad_sexo <- dat_consolidate_completo %>% group_by(ano_mes, Edad_fin, Sexo_fin) %>% summarise(Total = sum(Total_relaciones_laborales, na.rm = T))
grouped_edad_sexo_long <-  grouped_edad_sexo %>% spread(ano_mes, Total)
grouped_edad_sexo_long$Tipologia  <- 'Total'

output_edad_sexo_long <- rbind(group_edad_sexo_tipologia_long, grouped_edad_sexo_long)
output_edad_sexo_long <- output_edad_sexo_long %>% arrange(Tipologia,Edad_fin,Sexo_fin)
output_edad_sexo_long <- data.frame(output_edad_sexo_long)
output_edad_sexo_long <- output_edad_sexo_long %>% filter(!Edad_fin %in%c(NA, NULL, '') & Sexo_fin %in% c('F', 'M') & 
                                                            Tipologia %in% c('Dep_sec_priv', 'Independiente', 'Total'))
output_edad_sexo_long$Sexo_fin <- factor(output_edad_sexo_long$Sexo_fin)
levels(output_edad_sexo_long$Sexo_fin) <- c('Mujer', 'Hombre')
output_edad_sexo_long$Tipologia  <- factor(output_edad_sexo_long$Tipologia )
levels(output_edad_sexo_long$Tipologia ) <- c('Dependientes Sector Privado', 'Independientes', 'Total')

output_edad_sexo_long <-  rename(output_edad_sexo_long, Edad = Edad_fin, Sexo = Sexo_fin)


output_edad_sexo_long_1 <- output_edad_sexo_long %>% filter(Tipologia == 'Total')
output_edad_sexo_long_2 <- output_edad_sexo_long %>% filter(Tipologia == 'Independientes')
output_edad_sexo_long_3 <- output_edad_sexo_long %>% filter(Tipologia == 'Dependientes Sector Privado')

write.xlsx(output_edad_sexo_long_1, file='../resultados_total_rel_laborales_edad_sexo_11001.xlsx', sheetName="Total", row.names=FALSE, showNA = FALSE)
write.xlsx(output_edad_sexo_long_2, file='../resultados_total_rel_laborales_edad_sexo_11001.xlsx', sheetName="Independientes", row.names=FALSE, append =  T, showNA = FALSE)
write.xlsx(output_edad_sexo_long_3, file='../resultados_total_rel_laborales_edad_sexo_11001.xlsx', sheetName="Dependiente_Privado", row.names=FALSE, append =  T, showNA = FALSE)



########################################################################################################
#### PRocesamiento por actividad económica 
########################################################################################################

group_activ_econ_tipologia <- dat_consolidate_completo %>% group_by(Tipologia, ano_mes, codigo_actividad_economica_Max ) %>% summarise(Total = sum(Total_relaciones_laborales, na.rm = T))
group_activ_econ_tipologia_long <-  group_activ_econ_tipologia %>% spread(ano_mes , Total)

group_activ_econ <- dat_consolidate_completo %>% group_by(ano_mes, codigo_actividad_economica_Max) %>% summarise(Total = sum(Total_relaciones_laborales, na.rm = T))
group_activ_econ_long <-  group_activ_econ %>% spread(ano_mes, Total)
group_activ_econ_long$Tipologia  <- 'Total'

output_activ_econ_long <- rbind(group_activ_econ_tipologia_long, group_activ_econ_long)
output_activ_econ_long <- output_activ_econ_long %>% arrange(Tipologia,codigo_actividad_economica_Max)
output_activ_econ_long <- data.frame(output_activ_econ_long)
dim(output_activ_econ_long)
output_activ_econ_long <- merge(output_activ_econ_long, dat_ciiu, by.x = 'codigo_actividad_economica_Max', by.y = 'Clase_Rev._4_(2020)')
dim(output_activ_econ_long)

output_activ_econ_long <- output_activ_econ_long %>% filter(!is.na(codigo_actividad_economica_Max ) & 
                                                              !codigo_actividad_economica_Max == 'NULL' &
                                                              Tipologia %in% c('Dep_sec_priv', 'Independiente', 'Total'))

output_activ_econ_long$Tipologia  <- factor(output_activ_econ_long$Tipologia )
levels(output_activ_econ_long$Tipologia ) <- c('Dependientes Sector Privado', 'Independientes', 'Total')

output_activ_econ_long <-  rename(output_activ_econ_long, Codigo_Actividad_Economica = codigo_actividad_economica_Max)

output_activ_econ_long_1 <- output_activ_econ_long %>% filter(Tipologia == 'Total')
output_activ_econ_long_2 <- output_activ_econ_long %>% filter(Tipologia == 'Independientes')
output_activ_econ_long_3 <- output_activ_econ_long %>% filter(Tipologia == 'Dependientes Sector Privado')

write.xlsx(output_activ_econ_long_1, file='../resultados_total_rel_laborales_activ_econ.xlsx', sheetName="Total", row.names=FALSE, showNA = FALSE)
write.xlsx(output_activ_econ_long_2, file='../resultados_total_rel_laborales_activ_econ.xlsx', sheetName="Independientes", row.names=FALSE, append =  T, showNA = FALSE)
write.xlsx(output_activ_econ_long_3, file='../resultados_total_rel_laborales_activ_econ.xlsx', sheetName="Dependiente_Privado", row.names=FALSE, append =  T, showNA = FALSE)
