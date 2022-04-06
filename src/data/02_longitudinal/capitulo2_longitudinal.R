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
library(kableExtra)
library(wesanderson)
options(ztable.type="html")
# Carpeta de referencia
setwd('D:/CONSULTORIAS/UGPP/2022/Proyectos/Boletin_UGPP/report/')
source(file = file.path('../src/utils/utils_02_longitudinal.R'))

###### Lectura de datos 

### Matrices
matriz_12_21<-fun_panel("Matriz_12_21.csv")
matriz_11_21<-fun_panel("Matriz_11_21.csv")
matriz_10_21<-fun_panel("Matriz_10_21.csv")
matriz_9_21<-fun_panel("Matriz_9_21.csv")
matriz_8_21<-fun_panel("Matriz_8_21.csv")
matriz_7_21<-fun_panel("Matriz_7_21.csv")

matriz_12_20<-fun_panel("Matriz_12_20.csv")
matriz_11_20<-fun_panel("Matriz_11_20.csv")
matriz_10_20<-fun_panel("Matriz_10_20.csv")
matriz_9_20<-fun_panel("Matriz_9_20.csv")
matriz_8_20<-fun_panel("Matriz_8_20.csv")
matriz_7_20<-fun_panel("Matriz_7_20.csv")

matriz_12_19<-fun_panel("Matriz_12_19.csv")
matriz_11_19<-fun_panel("Matriz_11_19.csv")
matriz_10_19<-fun_panel("Matriz_10_19.csv")
matriz_9_19<-fun_panel("Matriz_9_19.csv")
matriz_8_19<-fun_panel("Matriz_8_19.csv")
matriz_7_19<-fun_panel("Matriz_7_19.csv")

matriz_12_18<-fun_panel("Matriz_12_18.csv")

matriz_names_21 <- c('matriz_12_21', 'matriz_11_21', 
                     'matriz_10_21', 'matriz_9_21', 
                     'matriz_8_21', 'matriz_7_21')

vect_meses_21 <- seq.Date(as.Date('2021-12-01'), as.Date('2021-07-01'), by = "-1 month")
vect_meses_20 <- seq.Date(as.Date('2020-12-01'), as.Date('2020-07-01'), by = "-1 month")
vect_meses_19 <- seq.Date(as.Date('2019-12-01'), as.Date('2019-07-01'), by = "-1 month")

matriz_names_20 <- c('matriz_12_20', 'matriz_11_20', 
                     'matriz_10_20', 'matriz_9_20', 
                     'matriz_8_20', 'matriz_7_20')

matriz_names_19 <- c('matriz_12_19', 'matriz_11_19', 
                     'matriz_10_19', 'matriz_9_19', 
                     'matriz_8_19', 'matriz_7_19')
meses_names <- c('Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
matriz_dependientes_19_21 <- NULL
matriz_independientes_19_21 <- NULL
for(ii in 1:6){
  sub_21 <- get(matriz_names_21[ii])
  sub_20 <- get(matriz_names_20[ii])
  sub_19 <- get(matriz_names_19[ii])
  
  matriz_dependientes_21 <- sub_21$matriz_dep
  matriz_dependientes_21$mes <- vect_meses_21[ii]
  
  matriz_dependientes_20 <- sub_20$matriz_dep
  matriz_dependientes_20$mes <- vect_meses_20[ii]
  
  matriz_dependientes_19 <- sub_19$matriz_dep
  matriz_dependientes_19$mes <- vect_meses_19[ii]
  
  matriz_dependientes_19_21 <- rbind(matriz_dependientes_19_21, matriz_dependientes_21, matriz_dependientes_20, matriz_dependientes_19)
  
  
  matriz_independientes_21 <- sub_21$matriz_ind
  matriz_independientes_21$mes <- vect_meses_21[ii]
  
  matriz_independientes_20 <- sub_20$matriz_ind
  matriz_independientes_20$mes <- vect_meses_20[ii]
  
  matriz_independientes_19 <- sub_19$matriz_ind
  matriz_independientes_19$mes <- vect_meses_19[ii]
  
  matriz_independientes_19_21 <- rbind(matriz_independientes_19_21, matriz_independientes_21, matriz_independientes_20, matriz_independientes_19)

}

## SECCION - Matrices resumen

fun_output_matriz_transicion(matriz_12_21$matriz_dep, title = "salida_resumen_dependientes_interes.png")
fun_output_matriz_transicion(matriz_11_21$matriz_dep, title = "salida_resumen_dependientes_referencia.png")

dinamica_serie_1 <- matriz_dependientes_19_21 %>% filter(Rango_agrupa_2 == 'Total') %>% select(Por_entran, Por_salen, mes) 
dinamica_serie_1$Tipo <- 'Dependientes'
dinamica_serie_2 <- matriz_independientes_19_21 %>% filter(Rango_agrupa_2 == 'Total') %>% select(Por_entran, Por_salen, mes) 
dinamica_serie_2$Tipo <- 'Independientes'

dinamica_serie <- rbind(dinamica_serie_1, dinamica_serie_2)

dinamica_serie <- rbind(dinamica_serie)
dinamica_serie$mes_format <- factor(month(dinamica_serie$mes), labels = meses_names)
dinamica_serie$Año <- factor(year(dinamica_serie$mes))

fun_salidas_entradas_anual(dinamica_serie)

## SECCION - Relaciones laborales que permanecieron

column_names_IBC <- c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV")
column_names_complemento <- c("Incrementan","Disminuyen")

tabla_dependientes_21 <- data.frame(matriz_12_21$transision_dep)
colnames(tabla_dependientes_21)<-c(column_names_IBC, column_names_complemento)
tabla_dependientes_21$rownames_1 <- column_names_IBC

tabla_dependientes_20 <- data.frame(matriz_12_20$transision_dep)
colnames(tabla_dependientes_20)<-c(column_names_IBC, column_names_complemento)
tabla_dependientes_20$rownames_1 <- column_names_IBC

tabla_dependientes_19 <- data.frame(matriz_12_19$transision_dep)
colnames(tabla_dependientes_19)<-c(column_names_IBC, column_names_complemento)
tabla_dependientes_19$rownames_1 <- column_names_IBC

fun_output_matriz_transicion(tabla_dependientes_21, "salida_matriz_transicion_dependientes_21.png")
fun_output_matriz_transicion(tabla_dependientes_20, "salida_matriz_transicion_dependientes_20.png")
fun_output_matriz_transicion(tabla_dependientes_19, "salida_matriz_transicion_dependientes_19.png")

