library(data.table)
library(xlsx)
library(dplyr)
library(tidyverse)
library(readxl)
rm(list = ls())
setwd('D:/CONSULTORIAS/UGPP/2022/Enero/Informacion_general')

####################################################################################################################################
# # Salidas por actividad económica 
####################################################################################################################################

already_read <- FALSE
if(!already_read){
  list_csv_files <- dir()
  string_activ_econ <- 'Salidas_generales_Aeconomica'
  list_datos_activ_econ <- list_csv_files[grep(string_activ_econ, list_csv_files)]
  list_assign_sub <- gsub('.csv', '', list_datos_activ_econ)
  
  dat_consolidate_activ_econ <- NULL
  for(ii in 1:length(list_assign_sub)){
    datos_lectura <- fread(file = list_datos_activ_econ[ii])
    assign(list_assign_sub[ii], datos_lectura)
    save(list = list_assign_sub[ii], file = paste(list_assign_sub[ii], ".Rdata", sep = ""))
    rm(list = list_assign_sub[ii])
    dat_consolidate_activ_econ <- rbind(dat_consolidate_activ_econ, datos_lectura)
  }  
  save(dat_consolidate_activ_econ, file = 'dat_consolidate_activ_econ.Rdata')
}else{
  load('dat_consolidate_activ_econ.Rdata')
}

dat_ciiu <- read_excel('../CRUCE_DIAN_CIIU_2020.xlsx', 1)
dat_ciiu <- dat_ciiu[, c("Clase_Rev._4_(2020)",  "Descripción_Clase_Rev._4_(2020)")]
dat_ciiu <- unique(dat_ciiu)
dat_ciiu <- dat_ciiu %>% filter(!duplicated(`Clase_Rev._4_(2020)`))
##########################################################################################################################################


month <- data.frame(mes = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'), MONTH = 1:12)
ano <- data.frame(ano = 18:21)
ano_month <- expand.grid(month$mes, ano$ano)
ano_month$ano_mes <- with(ano_month, paste(Var1, Var2))

dat_consolidate_activ_econ <- merge(dat_consolidate_activ_econ, month, all.x = T)
dat_consolidate_activ_econ$ano <- substr(dat_consolidate_activ_econ$YEAR, 3, 4)
dat_consolidate_activ_econ$ano_mes <- with(dat_consolidate_activ_econ, paste(mes, ano))

dat_consolidate_activ_econ$ano_mes <- factor(dat_consolidate_activ_econ$ano_mes, levels = ano_month$ano_mes)
dat_consolidate_activ_econ <- dat_consolidate_activ_econ %>% arrange(ano_mes)

group_activ_econ_tipologia <- dat_consolidate_activ_econ %>% group_by(Tipologia, ano_mes, codigo_actividad_economica_Max ) %>% summarise(Total = sum(Total_relaciones_laborales, na.rm = T))
group_activ_econ_tipologia_long <-  group_activ_econ_tipologia %>% spread(ano_mes , Total)

group_activ_econ <- dat_consolidate_activ_econ %>% group_by(ano_mes, codigo_actividad_economica_Max) %>% summarise(Total = sum(Total_relaciones_laborales, na.rm = T))
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

#######################################################################################################################################
# # Resultados solo para Bogotá 
#######################################################################################################################################
