library(data.table)
library(xlsx)
library(dplyr)
library(tidyverse)
setwd('D:/CONSULTORIAS/UGPP/2022/Enero/Informacion_general')


####################################################################################################################################
# # Salidas por edad y sexo 
####################################################################################################################################

already_read <- TRUE

if(already_read){
  list_csv_files <- dir()
  string_edad_sexo <- 'Salidas_generales_edad_sexo'
  list_datos_edad_sexo <- list_csv_files[grep(string_edad_sexo, list_csv_files)]
  list_assign_sub <- gsub('.csv', '', list_datos_edad_sexo)
  
  dat_consolidate_edad_sexo <- NULL
  for(ii in 1:length(list_assign_sub)){
    datos_lectura <- fread(file = list_datos_edad_sexo[ii])
    assign(list_assign_sub[ii], datos_lectura)
    save(list = list_assign_sub[ii], file = paste(list_assign_sub[ii], ".Rdata", sep = ""))
    rm(list = list_assign_sub[ii])
    dat_consolidate_edad_sexo <- rbind(dat_consolidate_edad_sexo, datos_lectura)
  }  
  save(dat_consolidate_edad_sexo, file = 'dat_consolidate_edad_sexo.Rdata')
}else{
  load('dat_consolidate_edad_sexo.Rdata')
}

month <- data.frame(mes = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'), MONTH = 1:12)
ano <- data.frame(ano = 18:21)
ano_month <- expand.grid(month$mes, ano$ano)
ano_month$ano_mes <- with(ano_month, paste(Var1, Var2))

dat_consolidate_edad_sexo <- merge(dat_consolidate_edad_sexo, month, all.x = T)
dat_consolidate_edad_sexo$ano <- substr(dat_consolidate_edad_sexo$YEAR, 3, 4)
dat_consolidate_edad_sexo$ano_mes <- with(dat_consolidate_edad_sexo, paste(mes, ano))

dat_consolidate_edad_sexo$ano_mes <- factor(dat_consolidate_edad_sexo$ano_mes, levels = ano_month$ano_mes)
dat_consolidate_edad_sexo <- dat_consolidate_edad_sexo %>% arrange(ano_mes)

group_edad_sexo_tipologia <- dat_consolidate_edad_sexo %>% group_by(Tipologia, ano_mes, Edad_fin, Sexo_fin) %>% summarise(Total = sum(Total_relaciones_laborales, na.rm = T))
group_edad_sexo_tipologia_long <-  group_edad_sexo_tipologia %>% spread(ano_mes , Total)

grouped_edad_sexo <- dat_consolidate_edad_sexo %>% group_by(ano_mes, Edad_fin, Sexo_fin) %>% summarise(Total = sum(Total_relaciones_laborales, na.rm = T))
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

write.xlsx(output_edad_sexo_long_1, file='../resultados_total_rel_laborales_edad_sexo.xlsx', sheetName="Total", row.names=FALSE, showNA = FALSE)
write.xlsx(output_edad_sexo_long_2, file='../resultados_total_rel_laborales_edad_sexo.xlsx', sheetName="Independientes", row.names=FALSE, append =  T, showNA = FALSE)
write.xlsx(output_edad_sexo_long_3, file='../resultados_total_rel_laborales_edad_sexo.xlsx', sheetName="Dependiente_Privado", row.names=FALSE, append =  T, showNA = FALSE)







 