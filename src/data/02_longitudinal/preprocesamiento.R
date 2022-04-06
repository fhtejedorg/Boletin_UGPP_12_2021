rm(list=ls())
library(dplyr)
#Yes
library(haven)
library(xlsx)
library(readxl)
library(dummies)

####################################################################
##########salidas tablero###########################################
#######Primera parte informacion actualización tablero##############
##Primera parte

mes1<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
mes2<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mes_rep<-data.frame(c(1:12),mes1,mes2);colnames(mes_rep)<-c("month","mes1","mes2")

#setwd("C:/Users/aemendoza/Documents/2021/tablero/Información/")
setwd("D:/Nuevos_Insumos_IMC/")
#tabla<-"Informacion_general/Salidas_generales_rangoIBC9_11_2021.csv"

tabla_incial<-function(tabla){
  informacion<-read.csv2(tabla) #CAMBIAR POR EXPORTACION MANUAL DE MODELER, PARA LOS CASOS DE 4 A 7 EN GENERAL
  #informacion<-read.csv2(tabla,sep = ',', dec = '.')#version 2
  #informacion<-read.csv2(tabla)#version enero21
  resultados<-data.frame(informacion%>%group_by(Tipologia,YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
  resultados_dependientes<-data.frame(informacion%>%filter(Tipologia %in% c("Dep_No_priv","Dep_sec_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
  resultados_dependientes$Tipologia<-rep("Dependientes",nrow(resultados_dependientes))
  resultados_total<-data.frame(informacion%>%filter(Tipologia %in% c("Dep_No_priv","Dep_sec_priv","Independiente"))%>%group_by(YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
  resultados_total$Tipologia<-rep("Total",nrow(resultados_dependientes))
  resultados<-rbind(resultados,resultados_dependientes,resultados_total)
  resultados
}

informacion_1_4_2018<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC1_4_2018.csv")
informacion_5_8_2018<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC5_8_2018.csv")
informacion_9_12_2018<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC9_12_2018.csv")
informacion_1_4_2019<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC1_4_2019.csv")
informacion_5_8_2019<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC5_8_2019.csv")
informacion_9_12_2019<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC9_12_2019.csv")
informacion_1_4_2020<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC1_4_2020.csv")
informacion_5_8_2020<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC5_8_2020.csv")
informacion_9_12_2020<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC9_12_2020.csv")
informacion_1_4_2021<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC1_4_2021.csv")
informacion_5_8_2021<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC5_8_2021.csv")
informacion_9_12_2021<-tabla_incial(tabla = "Informacion_general/Salidas_generales_rangoIBC9_12_2021_.csv")

informacion_completa<-rbind(informacion_1_4_2018,informacion_5_8_2018,informacion_9_12_2018,
                            informacion_1_4_2019,informacion_5_8_2019,informacion_9_12_2019,
                            informacion_1_4_2020,informacion_5_8_2020,informacion_9_12_2020,
                            informacion_1_4_2021,informacion_5_8_2021,informacion_9_12_2021)
tipologias<-data.frame(informacion_completa%>%group_by(Tipologia)%>%summarise(total = n()))
tipologias<-tipologias[,1];total_tipologias = length(tipologias)
time_date<-data.frame(informacion_completa%>%group_by(YEAR,MONTH)%>%summarise(total = n()))
time_date<-time_date[,1:2];total_time = nrow(time_date)
tot_tipologias<-rep(tipologias,rep(total_time,total_tipologias))
tot_YEAR<-rep(time_date[,1],total_tipologias)
tot_month<-rep(time_date[,2],total_tipologias)
data_estructura = data.frame(tot_tipologias,tot_YEAR,tot_month);colnames(data_estructura)<-c('Tipologia','YEAR','MONTH')

informacion_completa<-merge(data_estructura,informacion_completa,by = c( "Tipologia","YEAR","MONTH"), all.x = TRUE)

informacion_completa<-merge(informacion_completa,mes_rep, by.x  = "MONTH", by.y = "month", all.x = TRUE)
informacion_completa<-informacion_completa[order(informacion_completa$Tipologia,informacion_completa$YEAR,informacion_completa$MONTH),]
informacion_completa$var_mensual<-rep(0,nrow(informacion_completa))
informacion_completa$var_anual<-rep(0,nrow(informacion_completa))
informacion_completa$secuencia<-seq(1:nrow(informacion_completa))

for(tip in tipologias){
  for(j in (min(informacion_completa[informacion_completa$Tipologia == tip, 'secuencia'])+1):
            max(informacion_completa[informacion_completa$Tipologia == tip, 'secuencia'])){
    informacion_completa[informacion_completa$secuencia == j,'var_mensual']<-
      (informacion_completa[informacion_completa$secuencia == j,'Total_cotizantes']-informacion_completa[informacion_completa$secuencia == (j-1),'Total_cotizantes'])/
       informacion_completa[informacion_completa$secuencia == (j-1),'Total_cotizantes']
  }
  for(y in 2019:2021){
    subtabla<-informacion_completa[informacion_completa$Tipologia == tip & informacion_completa$YEAR == y,]
    for(peri in min(subtabla[,'secuencia']):max(subtabla[,'secuencia'])){
      informacion_completa[informacion_completa$secuencia == peri,'var_anual']<-
        (informacion_completa[informacion_completa$secuencia == peri,'Total_cotizantes']-informacion_completa[informacion_completa$secuencia == (peri-12),'Total_cotizantes'])/
          informacion_completa[informacion_completa$secuencia == (peri-12),'Total_cotizantes']
    }
  }
}

#salida 2
#tabla<-"Resumen_general/Informacion_general_IBC1_4_19.csv"
tabla_inicial2<-function(tabla){
  informacion<-read.csv2(tabla) #CAMBIAR POR EXPORTACION MANUAL DE MODELER, PARA LOS CASOS DE 4 A 7 EN GENERAL
  #informacion<-read.csv2(tabla,sep = ',', dec = '.')#version 2
  #informacion<-read.csv2(tabla)#version enero21
  informacion$monto_total<-informacion$cot_obligatoria_salud_Max_Sum+informacion$aporte_cot_obligatoria_pension_Max_Sum+informacion$cot_obligatoria_arl_Max_Sum+informacion$valor_aportes_ccf_Max_Sum+informacion$valor_aportes_parafiscales_sena_Max_Sum+informacion$valor_aportes_parafiscales_icbf_Max_Sum
  resumen_t<-data.frame(informacion%>%group_by(YEAR,MONTH,Tipologia)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE ), Monto_total = sum(monto_total, na.rm = TRUE)));resumen_t$subsistema <- rep("Total", nrow(resumen_t))
  resumen_s<-data.frame(informacion%>%group_by(YEAR,MONTH,Tipologia)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE ), Monto_total = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE)));resumen_s$subsistema <- rep("Salud", nrow(resumen_s))
  resumen_p<-data.frame(informacion%>%group_by(YEAR,MONTH,Tipologia)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE ), Monto_total = sum(aporte_cot_obligatoria_pension_Max_Sum, na.rm = TRUE)));resumen_p$subsistema <- rep("Pension", nrow(resumen_p))
  resumen_arl<-data.frame(informacion%>%group_by(YEAR,MONTH,Tipologia)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE ), Monto_total = sum(cot_obligatoria_arl_Max_Sum, na.rm = TRUE)));resumen_arl$subsistema <- rep("ARL", nrow(resumen_arl))
  resumen_ccf<-data.frame(informacion%>%group_by(YEAR,MONTH,Tipologia)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE ), Monto_total = sum(valor_aportes_ccf_Max_Sum, na.rm = TRUE)));resumen_ccf$subsistema <- rep("CCF", nrow(resumen_ccf))
  resumen_icbf<-data.frame(informacion%>%group_by(YEAR,MONTH,Tipologia)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE ), Monto_total = sum(valor_aportes_parafiscales_icbf_Max_Sum, na.rm = TRUE)));resumen_icbf$subsistema <- rep("ICBF", nrow(resumen_icbf))
  resumen_sena<-data.frame(informacion%>%group_by(YEAR,MONTH,Tipologia)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE ), Monto_total = sum(valor_aportes_parafiscales_sena_Max_Sum, na.rm = TRUE)));resumen_sena$subsistema <- rep("SENA", nrow(resumen_sena))
  resumen_consolidado<-rbind(resumen_t,resumen_s,resumen_p,resumen_arl,resumen_ccf,resumen_icbf,resumen_sena)
  #valores<-c(0.9753,0.9822,0.9845,0.9891,0.9916,0.9931,0.9918,0.993,0.9947,0.9959,0.997,1,1.006,1.0118,1.0162,
  #         1.0212,1.0244,1.0271,1.0294,1.0303,1.0326,1.0343,1.0354,1.038,1.0424,1.0494,1.0553,1.057,1.0536,
  #         1.0497,1.0497,1.0496,1.0529,1.0523,1.0508,1.0548,1.0591,1.0658,1.0712,1.0776,1.0884,1.0878,1.0914,1.0962)
  #años<-c(rep(c(2018,2019,2020),c(12,12,12)),rep(2021,8))
  #meses<-c(rep(seq(1,12),3),seq(1,8))

  #info_temporal <- data.frame(años,meses)
  #resumen_consolidado<-merge(resumen_consolidado, info_temporal, by.x = c("YEAR","MONTH"), by.y = c("años","meses"), all.x = TRUE)
  resumen_consolidado
}

informacion_1_4_2018_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC1_4_2018.csv")
informacion_5_8_2018_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC5_8_2018.csv")
informacion_9_12_2018_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC9_12_2018.csv")
informacion_1_4_2019_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC1_4_2019.csv")
informacion_5_8_2019_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC5_8_2019.csv")
informacion_9_12_2019_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC9_12_2019.csv")
informacion_1_4_2020_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC1_4_2020.csv")
informacion_5_8_2020_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC5_8_2020.csv")
informacion_9_12_2020_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC9_12_2020.csv")
informacion_1_4_2021_2<-tabla_inicial2(tabla = "Informacion_general/Salidas_generales_rangoIBC1_4_2021.csv")
informacion_5_8_2021_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC5_8_2021.csv")
informacion_9_12_2021_2<-tabla_inicial2(tabla = "Informacion_general/salidas_generales_rangoIBC9_12_2021_.csv")

informacion_completa2<-rbind(informacion_1_4_2018_2,informacion_5_8_2018_2,informacion_9_12_2018_2,
                            informacion_1_4_2019_2,informacion_5_8_2019_2,informacion_9_12_2019_2,
                            informacion_1_4_2020_2,informacion_5_8_2020_2,informacion_9_12_2020_2,
                            informacion_1_4_2021_2,informacion_5_8_2021_2,informacion_9_12_2021_2)
informacion_completa2_dep <- data.frame(informacion_completa2%>%filter(Tipologia %in% c("Dep_No_priv","Dep_sec_priv"))%>%group_by(YEAR,MONTH,subsistema)%>%summarise(cotizantes = sum(cotizantes,na.rm = TRUE), Monto_total  = sum(Monto_total, na.rm = TRUE)))
informacion_completa2_dep$Tipologia<-rep("Dependientes", nrow(informacion_completa2_dep))
informacion_completa2_tot <- data.frame(informacion_completa2%>%filter(Tipologia %in% c("Dep_No_priv","Dep_sec_priv", "Independiente"))%>%group_by(YEAR,MONTH,subsistema)%>%summarise(cotizantes = sum(cotizantes,na.rm = TRUE), Monto_total  = sum(Monto_total, na.rm = TRUE)))
informacion_completa2_tot$Tipologia<-rep("Total", nrow(informacion_completa2_tot))
informacion_completa2<-rbind(informacion_completa2,informacion_completa2_dep,informacion_completa2_tot)

valores<-c(0.9753,0.9822,0.9845,0.9891,0.9916,0.9931,0.9918,0.993,0.9947,0.9959,0.997,1,
           1.006,1.0118,1.0162,1.0212,1.0244,1.0271,1.0294,1.0303,1.0326,1.0343,1.0354,1.038,
           1.0424,1.0494,1.0553,1.057,1.0536,1.0497,1.0497,1.0496,1.0529,1.0523,1.0508,1.0548,
           1.0591,1.0658,1.0712,1.0776,1.0884,1.0878,1.0914,1.0962,1.1004,1.1006,1.106,1.1141)
años<-rep(c(2018,2019,2020,2021),c(12,12,12,12))#cambiar
meses<-c(rep(seq(1,12),4))#cambiar
info_temporal <- data.frame(años,meses,valores)
informacion_completa2<-merge(informacion_completa2, info_temporal, by.x = c("YEAR","MONTH"), by.y = c("años","meses"), all.x = TRUE)

########################
###########    CAMBIAR
year_periodo_ana = 2021
Mes_periodo_ana = 12
####cambiar el valor del deflcatar
informacion_completa2$Monto_total_def<-informacion_completa2$Monto_total*(mean(informacion_completa2[informacion_completa2$YEAR == year_periodo_ana & informacion_completa2$MONTH == Mes_periodo_ana , "valores"])/informacion_completa2$valores)
informacion_completa2<-merge(informacion_completa2,mes_rep, by.x = "MONTH", by.y = "month", all.x = TRUE)
informacion_completa2<-informacion_completa2[order(informacion_completa2$Tipologia,informacion_completa2$YEAR, informacion_completa2$MONTH, informacion_completa2$subsistema),]

#salida3
tabla<-"Resumen_general/Informacion_general_SECCION1_3_21.csv"

tabla_inicial3<-function(tabla){
  informacion<-read.csv2(tabla)#cambiar para del 1 al 7 de 2021
  #informacion<-read.csv2(tabla,sep = ',',dec = '.')
  informacion$monto_total<-informacion$cot_obligatoria_salud_Max_Sum+informacion$aporte_cot_obligatoria_pension_Max_Sum+informacion$cot_obligatoria_arl_Max_Sum+informacion$valor_aportes_ccf_Max_Sum+informacion$valor_aportes_parafiscales_sena_Max_Sum+informacion$valor_aportes_parafiscales_icbf_Max_Sum
  informacion_res<-data.frame(informacion%>%group_by(Tipologia,YEAR,MONTH,Seccion_fn)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), pago_total = sum(monto_total, na.rm = TRUE)))
  informacion_res_dep<-data.frame(informacion%>%filter(Tipologia %in% c('Dep_No_priv','Dep_sec_priv'))%>%group_by(YEAR,MONTH,Seccion_fn)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), pago_total = sum(monto_total, na.rm = TRUE)))
  informacion_res_dep$Tipologia<-rep("Dependientes",nrow(informacion_res_dep))
  informacion_res_tot<-data.frame(informacion%>%filter(Tipologia %in% c('Dep_No_priv','Dep_sec_priv','Independiente'))%>%group_by(YEAR,MONTH,Seccion_fn)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), pago_total = sum(monto_total, na.rm = TRUE)))
  informacion_res_tot$Tipologia<-rep("Total",nrow(informacion_res_tot))
  informacion_res<-rbind(informacion_res,informacion_res_dep,informacion_res_tot)
  informacion_res
}

informacion_1_4_2018_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica1_4_2018.csv")
informacion_5_8_2018_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica5_8_2018.csv")
informacion_9_12_2018_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica9_12_2018.csv")
informacion_1_4_2019_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica1_4_2019.csv")
informacion_5_8_2019_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica5_8_2019.csv")
informacion_9_12_2019_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica9_12_2019.csv")
informacion_1_4_2020_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica1_4_2020.csv")
informacion_5_8_2020_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica5_8_2020.csv")
informacion_9_12_2020_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica9_12_2020.csv")
informacion_1_4_2021_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica1_4_2021.csv")
informacion_5_8_2021_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica5_8_2021.csv")
informacion_9_12_2021_3<-tabla_inicial3(tabla = "Informacion_general/Salidas_generales_Aeconomica9_12_2021.csv")

Consolidado_informa_seccion<-rbind(informacion_9_12_2021_3,informacion_5_8_2021_3,informacion_1_4_2021_3,informacion_9_12_2020_3,informacion_5_8_2020_3,informacion_1_4_2020_3,
                                   informacion_9_12_2019_3,informacion_5_8_2019_3,informacion_1_4_2019_3,informacion_9_12_2018_3,informacion_5_8_2018_3,informacion_1_4_2018_3)

tipologias<-data.frame(Consolidado_informa_seccion%>%group_by(Tipologia)%>%summarise(total_cot = n()))
tipologias<-tipologias[,1];total_tipologia<-length(tipologias)
Secciones<-data.frame(Consolidado_informa_seccion%>%group_by(Seccion_fn)%>%summarise(total_cot = n()))
Secciones<-Secciones[,1];total_seccinoes = length(Secciones)
time_datos<-data.frame(Consolidado_informa_seccion%>%group_by(YEAR,MONTH)%>%summarise(total_cot = n()))
time_datos<-time_datos[,1:2];total_periodos = nrow(time_datos)

tipologias_tot_consoldiado <- rep(tipologias,rep((total_periodos*total_seccinoes),total_tipologia))
Secciones_tot_consoldiado <- rep(rep(Secciones,rep((total_periodos),total_seccinoes)),total_tipologia)
Años_tot_consoldiado <- rep(time_datos$YEAR,(total_tipologia*total_seccinoes))
Mes_tot_consoldiado <- rep(time_datos$MONTH,(total_tipologia*total_seccinoes))

estructura_general<-data.frame(tipologias_tot_consoldiado,Secciones_tot_consoldiado,Años_tot_consoldiado,Mes_tot_consoldiado)
colnames(estructura_general)<-c("Tipologia","Seccion_fn","YEAR","MONTH")

Consolidado_informa_seccion_fn<-merge(estructura_general,Consolidado_informa_seccion, by = c("Tipologia","Seccion_fn","YEAR","MONTH"), all.x = TRUE)

Trans_seccion<-read.csv2("Secciones_economicas_desc.csv")
Consolidado_informa_seccion_fn<-merge(Consolidado_informa_seccion_fn,Trans_seccion, by.x = "Seccion_fn", by.y = "Seccion_PILA", all.x =TRUE)
Consolidado_informa_seccion_fn<-merge(Consolidado_informa_seccion_fn,mes_rep, by.x = "MONTH", by.y = "month", all.x = TRUE)
Consolidado_informa_seccion_fn<-Consolidado_informa_seccion_fn[order(Consolidado_informa_seccion_fn$Tipologia,Consolidado_informa_seccion_fn$Seccion_fn,Consolidado_informa_seccion_fn$YEAR,Consolidado_informa_seccion_fn$MONTH),]
Consolidado_informa_seccion_fn$var_mensual = rep(0, nrow(Consolidado_informa_seccion_fn))
Consolidado_informa_seccion_fn$var_anual = rep(0, nrow(Consolidado_informa_seccion_fn))
Consolidado_informa_seccion_fn$secuencia<-seq(1,nrow(Consolidado_informa_seccion_fn))
Consolidado_informa_seccion_fn[is.na(Consolidado_informa_seccion_fn$cotizantes),"cotizantes"]<-rep(0,nrow(Consolidado_informa_seccion_fn[is.na(Consolidado_informa_seccion_fn$cotizantes),]))

for(tip in tipologias){
  for(sec in Secciones){
    Subtabla<-Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$Tipologia == tip & Consolidado_informa_seccion_fn$Seccion_fn == sec,]
    Minimo = min(Subtabla$secuencia)
    Maximo = max(Subtabla$secuencia)
    for(linea in (Minimo+1):Maximo){
      Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$secuencia == linea, "var_mensual"]<-
        (Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$secuencia == linea, "cotizantes"]-Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$secuencia == (linea-1), "cotizantes"])/
        Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$secuencia == (linea-1), "cotizantes"]
    }
    for(años in c(2019,2020,2021)){
      Subtabla2<-Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$Tipologia == tip & Consolidado_informa_seccion_fn$Seccion_fn == sec & Consolidado_informa_seccion_fn$YEAR == años,]
      Minimo2 = min(Subtabla2$secuencia)
      Maximo2 = max(Subtabla2$secuencia)
      for(linea in Minimo2:Maximo2){
        Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$secuencia == linea, "var_anual"]<-
          (Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$secuencia == linea, "cotizantes"]-Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$secuencia == (linea-12), "cotizantes"])/
          Consolidado_informa_seccion_fn[Consolidado_informa_seccion_fn$secuencia == (linea-12), "cotizantes"]
      }    
    }
  }
}


#salida4
tabla<-"Informacion_general/Informacion_general_ubicacion_COT1_3_21.csv"
Trans_DPTO<-read.csv2("departamentos.csv")

tabla_inicial4<-function(tabla){
  informacion<-read.csv2(tabla)#cambiar por información del 1 al 
  #informacion<-read.csv2(tabla,sep = ',',dec = '.')
  informacion$monto_total<-informacion$cot_obligatoria_salud_Max_Sum+informacion$aporte_cot_obligatoria_pension_Max_Sum+informacion$cot_obligatoria_arl_Max_Sum+informacion$valor_aportes_ccf_Max_Sum+informacion$valor_aportes_parafiscales_sena_Max_Sum+informacion$valor_aportes_parafiscales_icbf_Max_Sum
  informacion<-merge(informacion,Trans_DPTO, by = "departamento_COT_Max", all.x =TRUE)
  informacion[is.na(informacion$Nombre_dpto),"departamento_COT_Max"]<-rep(0,nrow(informacion[is.na(informacion$Nombre_dpto),]))
  resultados_informa<-data.frame(informacion%>%group_by(Tipologia,departamento_COT_Max,YEAR,MONTH)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm =TRUE), monto_total = sum(monto_total, na.rm =TRUE)))
  resultados_informa_dep<-data.frame(informacion%>%filter(Tipologia %in% c('Dep_No_priv','Dep_sec_priv'))%>%group_by(departamento_COT_Max,YEAR,MONTH)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm =TRUE), monto_total = sum(monto_total, na.rm =TRUE)))
  resultados_informa_dep$Tipologia<-rep('Dependientes',nrow(resultados_informa_dep))
  resultados_informa_tot<-data.frame(informacion%>%filter(Tipologia %in% c('Dep_No_priv','Dep_sec_priv','Independiente'))%>%group_by(departamento_COT_Max,YEAR,MONTH)%>%summarise(cotizantes = sum(Total_relaciones_laborales, na.rm =TRUE), monto_total = sum(monto_total, na.rm =TRUE)))
  resultados_informa_tot$Tipologia<-rep('Total',nrow(resultados_informa_tot))
  resultados_informa<-rbind(resultados_informa,resultados_informa_dep,resultados_informa_tot)
  resultados_informa
}

informacion_1_4_2018_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO1_4_2018.csv")
informacion_5_8_2018_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO5_8_2018.csv")
informacion_9_12_2018_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO9_12_2018.csv")
informacion_1_4_2019_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO1_4_2019.csv")
informacion_5_8_2019_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO5_8_2019.csv")
informacion_9_12_2019_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO9_12_2019.csv")
informacion_1_4_2020_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO1_4_2020.csv")
informacion_5_8_2020_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO5_8_2020.csv")
informacion_9_12_2020_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO9_12_2020.csv")
informacion_1_4_2021_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO1_4_2021.csv")
informacion_5_8_2021_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO5_8_2021.csv")
informacion_9_12_2021_4<-tabla_inicial4(tabla = "Informacion_general/Salidas_generales_DPTO_MPIO9_12_2021_.csv")

informacion_completo_ubicacion<-rbind(informacion_1_4_2018_4,informacion_5_8_2018_4,informacion_9_12_2018_4,informacion_1_4_2019_4,informacion_5_8_2019_4,informacion_9_12_2019_4,
                                      informacion_1_4_2020_4,informacion_5_8_2020_4,informacion_9_12_2020_4,informacion_1_4_2021_4,informacion_5_8_2021_4,informacion_9_12_2021_4)

tipologias<-data.frame(informacion_completo_ubicacion%>%group_by(Tipologia)%>%summarise(total_cot = n()))
tipologias<-tipologias[,1];total_tipologia<-length(tipologias)
Departamentos<-data.frame(informacion_completo_ubicacion%>%group_by(departamento_COT_Max)%>%summarise(total_cot = n()))
Departamentos<-Departamentos[,1];total_departamentos = length(Departamentos)
time_datos<-data.frame(informacion_completo_ubicacion%>%group_by(YEAR,MONTH)%>%summarise(total_cot = n()))
time_datos<-time_datos[,1:2];total_periodos = nrow(time_datos)

tipologias_tot_consoldiado <- rep(tipologias,rep((total_periodos*total_departamentos),total_tipologia))

Departamentos_tot_consoldiado <- rep(rep(Departamentos,rep((total_periodos),total_departamentos)),total_tipologia)
Años_tot_consoldiado <- rep(time_datos$YEAR,(total_tipologia*total_departamentos))
Mes_tot_consoldiado <- rep(time_datos$MONTH,(total_tipologia*total_departamentos))

estructura_general_ubicacion<-data.frame(tipologias_tot_consoldiado,Departamentos_tot_consoldiado,Años_tot_consoldiado,Mes_tot_consoldiado)
colnames(estructura_general_ubicacion)<-c("Tipologia","departamento_COT_Max","YEAR","MONTH")
estructura_general_ubicacion_fn<-merge(estructura_general_ubicacion,informacion_completo_ubicacion, by = c("Tipologia","departamento_COT_Max","YEAR","MONTH"), all.x = TRUE)

Trans_DPTO<-read.csv2("departamentos.csv")
Consolidado_informa_DPTO_fn<-merge(estructura_general_ubicacion_fn,Trans_DPTO, by.x = "departamento_COT_Max", by.y = "departamento_COT_Max", all.x =TRUE)
Consolidado_informa_DPTO_fn<-merge(Consolidado_informa_DPTO_fn,mes_rep, by.x = "MONTH", by.y = "month", all.x = TRUE)
Consolidado_informa_DPTO_fn<-Consolidado_informa_DPTO_fn[order(Consolidado_informa_DPTO_fn$Tipologia,Consolidado_informa_DPTO_fn$departamento_COT_Max,Consolidado_informa_DPTO_fn$YEAR,Consolidado_informa_DPTO_fn$MONTH),]
Consolidado_informa_DPTO_fn$var_mensual = rep(0, nrow(Consolidado_informa_DPTO_fn))
Consolidado_informa_DPTO_fn$var_anual = rep(0, nrow(Consolidado_informa_DPTO_fn))
Consolidado_informa_DPTO_fn$secuencia<-seq(1,nrow(Consolidado_informa_DPTO_fn))
Consolidado_informa_DPTO_fn[is.na(Consolidado_informa_DPTO_fn$cotizantes),"cotizantes"]<-rep(0,nrow(Consolidado_informa_DPTO_fn[is.na(Consolidado_informa_DPTO_fn$cotizantes),]))

for(tip in tipologias){
  for(dpto in Departamentos){
    Subtabla<-Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$Tipologia == tip & Consolidado_informa_DPTO_fn$departamento_COT_Max == dpto,]
    Minimo = min(Subtabla$secuencia, na.rm = TRUE)
    Maximo = max(Subtabla$secuencia, na.rm = TRUE)
    for(linea in (Minimo+1):Maximo){
      Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$secuencia == linea, "var_mensual"]<-
        (Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$secuencia == linea, "cotizantes"]-Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$secuencia == (linea-1), "cotizantes"])/
        Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$secuencia == (linea-1), "cotizantes"]
    }
    for(años in c(2019,2020,2021)){
      Subtabla2<-Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$Tipologia == tip & Consolidado_informa_DPTO_fn$departamento_COT_Max == dpto & Consolidado_informa_DPTO_fn$YEAR == años,]
      Minimo2 = min(Subtabla2$secuencia, na.rm = TRUE)
      Maximo2 = max(Subtabla2$secuencia, na.rm = TRUE)
      for(linea in Minimo2:Maximo2){
        Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$secuencia == linea, "var_anual"]<-
          (Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$secuencia == linea, "cotizantes"]-Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$secuencia == (linea-12), "cotizantes"])/
          Consolidado_informa_DPTO_fn[Consolidado_informa_DPTO_fn$secuencia == (linea-12), "cotizantes"]
      }    
    }
  }
}

write.xlsx(informacion_completa,"Salidas/Parte1_12_.xlsx", sheetName = "cuadros")
write.xlsx(informacion_completa2,"Salidas/Parte1_12_.xlsx", sheetName = "grafico1", append = TRUE)
write.xlsx(Consolidado_informa_seccion_fn,"Salidas/Parte1_12_.xlsx", sheetName = "tabla_seccion_economica", append = TRUE)
write.xlsx(Consolidado_informa_DPTO_fn,"Salidas/Parte1_12_.xlsx", sheetName = "mapa", append = TRUE)

#######################################################3
#Ciclo economico

setwd("D:/Nuevos_Insumos_IMC/")

tabla = "Ciclo_economico/salidas_empleos_6_20.csv"
permanentes = "Ciclo_economico/salidas_permanentes_AE_6_20.csv"
permanentes2 = "Ciclo_economico/salidas_permanentes_size_6_20.csv"
empresas = "Ciclo_economico/salidas1_empleos_6_20.csv"
year = 2020
mes = 6
CIIU<-read.xlsx("DetalleCIIU.xlsx", sheetName = "Secciontotal")
Trans_seccion<-read.csv2("Secciones_economicas_desc.csv")

Size1<-c("0","Grande","Maediana","Micro","Micro1","Muy_grande","Pequeña","Total")
Size2<-c("0","Grande","Mediana","Micro (2-10 Empleados)","Micro (1 Empleado)","Muy grande","Pequeña","Total")
Size3<-c("0","Grande","Mediana","Micro","Micro","Muy grande","Pequeña","Total")
Size4<-c(0,5,4,2,1,6,3,7)
trans_size = data.frame(Size1,Size2,Size3,Size4);colnames(trans_size)<-c("Size_company","Size_company_1","Size_company_2","secuenciaSize")

cicloS1_new<-function(tabla,permanentes,permanentes2,empresas,mes,year){
  base_empleos<-read.csv2(tabla)#cambiar 
  #base_empleos<-read.csv2(tabla,sep = ',', dec = '.', na.strings = '$null$')
  colnames(base_empleos)<-c("codigo_depto_Max","codigo_ciudad_Max","codigo_actividad_economica_Max",    
  "Rango_IBC_Max","SEXO_Max","edad_fn","IBC_REFERENCIA_Max_Mean_salen","PAGOS_TOTALES_Max_Sum_salen","PAGOS_TOTALES_Max_Mean_salen",
  "Total_personas_salen","IBC_REFERENCIA_Max_Mean_entran","PAGOS_TOTALES_Max_Sum_entran","PAGOS_TOTALES_Max_Mean_entran", "Total_personas_nuevas" ,"IBC_REFERENCIA_Max_Mean_permanecen",
  "PAGOS_TOTALES_Max_Sum_permanecen","PAGOS_TOTALES_Max_Mean_permanecen","Total_personas_permanecen","periodo")
  base_empleos$edad_fin<-floor(as.numeric(base_empleos$edad_fn))
  base_empleos<-merge(base_empleos,CIIU,by.x = "codigo_actividad_economica_Max", by.y = "Clase",all.x = TRUE)
  
  recuadros<-data.frame(base_empleos%>%group_by(Seccion)%>%summarise(Total_personas_salen = sum(Total_personas_salen, na.rm = TRUE), Total_personas_nuevas = sum(Total_personas_nuevas, na.rm = TRUE), Total_personas_permanecen = sum(Total_personas_permanecen, na.rm = TRUE)))
  recuadrostot<-data.frame(base_empleos%>%summarise(Total_personas_salen = sum(Total_personas_salen, na.rm = TRUE), Total_personas_nuevas = sum(Total_personas_nuevas, na.rm = TRUE), Total_personas_permanecen = sum(Total_personas_permanecen, na.rm = TRUE)))
  recuadrostot$Seccion = "Tot"
  recuadros<-rbind(recuadros,recuadrostot)
  recuadros[is.na(recuadros$Seccion),"Seccion"]<-"NA"
  recuadros<-merge(recuadros,Trans_seccion, by.x = "Seccion", by.y = "Seccion_PILA", all.x = TRUE)
  recuadros$year_p<-rep(year,nrow(recuadros))
  recuadros$month<-rep(mes,nrow(recuadros))
  recuadros<-merge(recuadros,mes_rep, by = "month", all.x = TRUE)
  recuadros$Porcentaje_salen<-recuadros$Total_personas_salen/(recuadros$Total_personas_salen+recuadros$Total_personas_permanecen)
  recuadros$Porcentaje_nuevas<-recuadros$Total_personas_nuevas/(recuadros$Total_personas_nuevas+recuadros$Total_personas_permanecen)
  recuadros$Porcentaje_permanecen<-recuadros$Total_personas_permanecen/(recuadros$Total_personas_nuevas+recuadros$Total_personas_permanecen)
  #seccion 2 ciclo economico recuadros primera parte
  recuadros[is.na(recuadros$Seccion_PILA_nom),"Seccion_PILA_nom"]<-"Otros"
  
  #serie con los datos de entradas y salidas 
  grafico1<-recuadros
  grafico1$periodo<-paste0(grafico1$mes2,"_",grafico1$year_p)
  
  s_permanentes<-read.csv2(permanentes)
  #s_permanentes<-read.csv2(permanentes,sep = ',', dec = '.', na.strings = '$null$')
  colnames(s_permanentes)<-c("codigo_actividad_economica_Max","codigo_actividad_economica_Max_pre","Total_personas_permanecen","Periodo")
  s_permanentes<-merge(s_permanentes,CIIU, by.x = "codigo_actividad_economica_Max", by.y = "Clase", all.x = TRUE)
  s_permanentes<-merge(s_permanentes,CIIU, by.x = "codigo_actividad_economica_Max_pre", by.y = "Clase", all.x = TRUE)
  s_permanentes<-data.frame(s_permanentes%>%group_by(Seccion.x,Seccion.y)%>%summarise(total_cotizantes = sum(Total_personas_permanecen, na.rm =TRUE)))
  s_permanentes$mismo<-rep(0,nrow(s_permanentes))
  s_permanentes[is.na(s_permanentes$Seccion.x),"Seccion.x"]<-"NA"
  s_permanentes[is.na(s_permanentes$Seccion.y),"Seccion.y"]<-"NA"
  s_permanentes[s_permanentes$Seccion.x == s_permanentes$Seccion.y, "mismo"]<-rep(1, nrow(s_permanentes[s_permanentes$Seccion.x == s_permanentes$Seccion.y, ]))
  s_permanentes_mismo<-data.frame(s_permanentes%>%filter(mismo == 1)%>%group_by(Seccion.x)%>%summarise(total_cotizantes_igual = sum(total_cotizantes, na.rm = TRUE)))
  s_permanentes_cambia<-data.frame(s_permanentes%>%filter(mismo == 0)%>%group_by(Seccion.x)%>%summarise(total_cotizantes_cambia = sum(total_cotizantes, na.rm = TRUE)))
  s_permanentes_res<-merge(s_permanentes_mismo,s_permanentes_cambia, by = "Seccion.x", all = TRUE)
  s_permanentes_res_TOT<-data.frame(s_permanentes_res%>%summarise(total_cotizantes_igual = sum(total_cotizantes_igual, na.rm = TRUE),total_cotizantes_cambia = sum(total_cotizantes_cambia, na.rm = TRUE)))
  s_permanentes_res_TOT$Seccion.x<-"Tot"
  s_permanentes_res<-rbind(s_permanentes_res,s_permanentes_res_TOT)
  s_permanentes_res$por_igual <-s_permanentes_res$total_cotizantes_igual/(s_permanentes_res$total_cotizantes_igual+s_permanentes_res$total_cotizantes_cambia)
  s_permanentes_res$por_diferente <-s_permanentes_res$total_cotizantes_cambia/(s_permanentes_res$total_cotizantes_igual+s_permanentes_res$total_cotizantes_cambia)
  s_permanentes_res$year_p <- rep(year,nrow(s_permanentes_res))
  s_permanentes_res$month <- rep(mes,nrow(s_permanentes_res))
  s_permanentes_res<-merge(s_permanentes_res,Trans_seccion, by.x = "Seccion.x", by.y = "Seccion_PILA", all.x = TRUE)
  s_permanentes_res<-merge(s_permanentes_res,mes_rep, by = "month", all.x = TRUE)
  s_permanentes_res[is.na(s_permanentes_res$Seccion_PILA_nom),"Seccion_PILA_nom"]<-"Otros"
  
  s_permanentes_cambia2<-data.frame(s_permanentes%>%filter(mismo == 0)%>%group_by(Seccion.y,Seccion.x)%>%summarise(total_cotizantes_cambia = sum(total_cotizantes, na.rm = TRUE)))
  s_permanentes_cambiaT<-data.frame(s_permanentes%>%filter(mismo == 0)%>%group_by(Seccion.y)%>%summarise(total_cotizantes_C = sum(total_cotizantes, na.rm = TRUE)))
  s_permanentes_cambia2<-merge(s_permanentes_cambia2,s_permanentes_cambiaT, by = "Seccion.y", all.x = TRUE)
  s_permanentes_cambia2$participacion_porcentual<-s_permanentes_cambia2$total_cotizantes_cambia/s_permanentes_cambia2$total_cotizantes_C
  s_permanentes_cambia2<-merge(s_permanentes_cambia2,Trans_seccion, by.x = "Seccion.x", by.y = "Seccion_PILA", all.x = TRUE)
  s_permanentes_cambia2<-merge(s_permanentes_cambia2,Trans_seccion, by.x = "Seccion.y", by.y = "Seccion_PILA", all.x = TRUE)
  s_permanentes_cambia2$year_p <- rep(year,nrow(s_permanentes_cambia2))
  s_permanentes_cambia2$month <- rep(mes,nrow(s_permanentes_cambia2))
  s_permanentes_cambia2<-merge(s_permanentes_cambia2,mes_rep, by = "month", all.x = TRUE)
  s_permanentes_cambia2[is.na(s_permanentes_cambia2$Seccion_PILA_nom.y),"Seccion_PILA_nom.y"]<-rep("Otros", nrow(s_permanentes_cambia2[is.na(s_permanentes_cambia2$Seccion_PILA_nom.y),]))
  s_permanentes_cambia2[is.na(s_permanentes_cambia2$Seccion_PILA_nom.x),"Seccion_PILA_nom.x"]<-rep("Otros", nrow(s_permanentes_cambia2[is.na(s_permanentes_cambia2$Seccion_PILA_nom.x),]))
  
  informacion_empresas <- read.csv2(empresas)#cambiar 
  #informacion_empresas<-read.csv2("Ciclo_economico/salidas1_empleos_12_19.csv")#read.csv2(empresas)#read.csv2("Ciclo_economico/salidas1_empleos_11_20.csv")
  #informacion_empresas <- read.csv2(empresas,sep = ',', dec = '.', na.strings = '$null$')
  informacion_empresas<-merge(informacion_empresas,CIIU,by.x = "codigo_actividad_economica_Max", by.y = "Clase", all.x = TRUE)
  informacion_empresas[is.na(informacion_empresas$Seccion),"Seccion"]<-rep("NA",nrow(informacion_empresas[is.na(informacion_empresas$Seccion),]))
  #Empleados_empresas_general_Sum por Empleados_empresas_generan_Sum, Total_empresas por Total_empresas_pos, Total_empleados_previo_Sum por Total_empleados_general_previo_Sum, Total_empresas_previo por Total_empresas_pre
  if(ncol(informacion_empresas) < 19){#se tiene que bloquear para poder leer el 6 de 2021
    informacion_empresas<-data.frame(informacion_empresas%>%rename(
                                 Empleados_empresas_generan_Sum = Empleados_empresas_general_Sum, 
                                 Total_empresas_pos = Total_empresas,
                                 Total_empleados_general_previo_Sum = Total_empleados_previo_Sum,
                                 Total_empresas_pre = Total_empresas_previo))
  }
  if(ncol(informacion_empresas) >= 19){informacion_empresas = informacion_empresas}
  res_informacion_empresas<-data.frame(informacion_empresas%>%group_by(Seccion,Size_company)%>%summarise(
    Personas_entran = sum(Total_empleados_nuevos_Sum, na.rm = TRUE),
    Empresas_generan = sum(Total_empleados_nuevos_Count, na.rm = TRUE),
    Personas_general_priv = sum(Total_empleados_general_Sum, na.rm = TRUE),
    Personas_empresas_generan = sum(Empleados_empresas_generan_Sum,na.rm = TRUE),#Personas_empresas_generan = sum(Empleados_empresas_general_Sum,na.rm = TRUE) cambiar para otros años
    Total_empresas = sum(Total_empresas_pos,na.rm = TRUE),#Total_empresas = sum(Total_empresas,na.rm = TRUE),cambiar para otros años
    Personas_salen = sum(Total_empleados_salen_Sum,na.rm = TRUE), 
    Empresas_destruyen_empleo= sum(Total_empleados_salen_Count,na.rm = TRUE), 
    Total_personas_previo = sum(Total_empleados_general_previo_Sum,na.rm = TRUE),
    Total_empresas_previo = sum(Total_empresas_pre,na.rm = TRUE)))
  res_informacion_empresasTOT<-data.frame(informacion_empresas%>%group_by(Seccion)%>%summarise(
    Personas_entran = sum(Total_empleados_nuevos_Sum, na.rm = TRUE),
    Empresas_generan = sum(Total_empleados_nuevos_Count, na.rm = TRUE),
    Personas_general_priv = sum(Total_empleados_general_Sum, na.rm = TRUE),
    Personas_empresas_generan = sum(Empleados_empresas_generan_Sum,na.rm = TRUE),#Personas_empresas_generan = sum(Empleados_empresas_general_Sum,na.rm = TRUE) cambiar para otros años
    Total_empresas = sum(Total_empresas_pos,na.rm = TRUE),#Total_empresas = sum(Total_empresas,na.rm = TRUE),cambiar para otros años
    Personas_salen = sum(Total_empleados_salen_Sum,na.rm = TRUE), 
    Empresas_destruyen_empleo= sum(Total_empleados_salen_Count,na.rm = TRUE), 
    Total_personas_previo = sum(Total_empleados_general_previo_Sum,na.rm = TRUE),
    Total_empresas_previo = sum(Total_empresas_pre,na.rm = TRUE)))
  res_informacion_empresasTOT$Size_company<-rep("Total",nrow(res_informacion_empresasTOT))
  res_informacion_empresasTOTG<-data.frame(informacion_empresas%>%summarise(
    Personas_entran = sum(Total_empleados_nuevos_Sum, na.rm = TRUE),
    Empresas_generan = sum(Total_empleados_nuevos_Count, na.rm = TRUE),
    Personas_general_priv = sum(Total_empleados_general_Sum, na.rm = TRUE),
    Personas_empresas_generan = sum(Empleados_empresas_generan_Sum,na.rm = TRUE),#Personas_empresas_generan = sum(Empleados_empresas_general_Sum,na.rm = TRUE) cambiar para otros años
    Total_empresas = sum(Total_empresas_pos,na.rm = TRUE),#Total_empresas = sum(Total_empresas,na.rm = TRUE),cambiar para otros años
    Personas_salen = sum(Total_empleados_salen_Sum,na.rm = TRUE), 
    Empresas_destruyen_empleo= sum(Total_empleados_salen_Count,na.rm = TRUE), 
    Total_personas_previo = sum(Total_empleados_general_previo_Sum,na.rm = TRUE),
    Total_empresas_previo = sum(Total_empresas_pre,na.rm = TRUE)))
  res_informacion_empresasTOTG$Size_company<-rep("Total",nrow(res_informacion_empresasTOTG))
  res_informacion_empresasTOTG$Seccion<-rep("Tot",nrow(res_informacion_empresasTOTG))
  res_informacion_empresas<-rbind(res_informacion_empresas,res_informacion_empresasTOT,res_informacion_empresasTOTG)
  res_informacion_empresas$year_p <- rep(year,nrow(res_informacion_empresas))
  res_informacion_empresas$month <- rep(mes,nrow(res_informacion_empresas))
  res_informacion_empresas<-merge(res_informacion_empresas,trans_size,by = "Size_company", all.x = TRUE)
  res_informacion_empresas$Porcentaje_generan<-res_informacion_empresas$Empresas_generan/res_informacion_empresas$Total_empresas
  res_informacion_empresas$Porcentaje_no_generan<-1-res_informacion_empresas$Empresas_generan/res_informacion_empresas$Total_empresas
  
  #base_empleos[base_empleos$SEXO_Max == "", "SEXO_Max"]<-rep("N",nrow(base_empleos[base_empleos$SEXO_Max == "", ]))#cambiar
  
  base_empleos[is.na(base_empleos$SEXO_Max), "SEXO_Max"]<-rep("N",nrow(base_empleos[is.na(base_empleos$SEXO_Max), ]))#cambiar
  
  base_empleos[base_empleos$SEXO_Max == "NULL", "SEXO_Max"]<-rep("N",nrow(base_empleos[base_empleos$SEXO_Max == "NULL", ]))
  base_empleos[is.na(base_empleos$edad_fin), "edad_fin"]<-rep(999,nrow(base_empleos[is.na(base_empleos$edad_fin), ]))
  base_empleos[is.na(base_empleos$Seccion), "Seccion"]<-rep("NA",nrow(base_empleos[is.na(base_empleos$Seccion), ]))
  resultados_edad1<-data.frame(base_empleos%>%group_by(Seccion,SEXO_Max,edad_fin)%>%
                                 summarise(Total_personas_salen = sum(Total_personas_salen,na.rm = TRUE),Total_personas_nuevas = sum(Total_personas_nuevas,na.rm = TRUE),
                                           Total_personas_permanecen = sum(Total_personas_permanecen,na.rm = TRUE)))
  resultados_edad1_tot<-data.frame(resultados_edad1%>%group_by(SEXO_Max,edad_fin)%>%
                                     summarise(Total_personas_salen = sum(Total_personas_salen,na.rm = TRUE),Total_personas_nuevas = sum(Total_personas_nuevas,na.rm = TRUE),
                                               Total_personas_permanecen = sum(Total_personas_permanecen,na.rm = TRUE)))
  resultados_edad1_tot$Seccion<-rep("Tot",nrow(resultados_edad1_tot))
  resultados_edad1_c<-rbind(resultados_edad1,resultados_edad1_tot)
  resultados_edad1_c$month = rep(mes,nrow(resultados_edad1_c))
  resultados_edad1_c$year = rep(year,nrow(resultados_edad1_c))
  resultados_edad1_c<-merge(resultados_edad1_c,mes_rep, by = "month", all.x = TRUE)
  resultados_edad1_c<-merge(resultados_edad1_c,Trans_seccion, by.x = "Seccion", by.y = "Seccion_PILA", all.x = TRUE)
  resultados_edad1_c[is.na(resultados_edad1_c$Seccion_PILA_nom),"Seccion_PILA_nom"]<-rep("Otros",nrow(resultados_edad1_c[is.na(resultados_edad1_c$Seccion_PILA_nom),]))
  
  rm(base_empleos)
  salidas = list(Recuadros = recuadros,Grafico = grafico1,Permanentes=s_permanentes_res,Permanentes_cambia=s_permanentes_cambia2,Empresas=res_informacion_empresas,demografico = resultados_edad1_c)
  salidas
}

#salidas6_20<-salidas

salidas12_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_12_21_.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_12_21_.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_12_21_.csv",empresas = "Ciclo_economico/salidas1_empleos_12_21_.csv",year = 2021,mes = 12)
salidas11_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_11_21_.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_10_21_.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_11_21_.csv",empresas = "Ciclo_economico/salidas1_empleos_11_21_.csv",year = 2021,mes = 11)
salidas10_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_10_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_10_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_10_21.csv",empresas = "Ciclo_economico/salidas1_empleos_10_21.csv",year = 2021,mes = 10)
salidas9_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_9_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_9_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_9_21.csv",empresas = "Ciclo_economico/salidas1_empleos_9_21.csv",year = 2021,mes = 9)
salidas8_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_8_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_8_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_8_21.csv",empresas = "Ciclo_economico/salidas1_empleos_8_21.csv",year = 2021,mes = 8)
salidas7_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_7_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_7_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_7_21.csv",empresas = "Ciclo_economico/salidas1_empleos_7_21.csv",year = 2021,mes = 7)
salidas6_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_6_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_6_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_6_21.csv",empresas = "Ciclo_economico/salidas1_empleos_6_21.csv",year = 2021,mes = 6)
salidas5_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_5_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_5_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_5_21.csv",empresas = "Ciclo_economico/salidas1_empleos_5_21.csv",year = 2021,mes = 5)
salidas4_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_4_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_4_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_4_21.csv",empresas = "Ciclo_economico/salidas1_empleos_4_21.csv",year = 2021,mes = 4)
salidas3_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_3_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_3_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_3_21.csv",empresas = "Ciclo_economico/salidas1_empleos_3_21.csv",year = 2021,mes = 3)
salidas2_21<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_2_21.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_2_21.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_2_21.csv",empresas = "Ciclo_economico/salidas1_empleos_2_21.csv",year = 2021,mes = 2)
  
salidas12_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_12_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_12_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_12_20.csv",empresas = "Ciclo_economico/salidas1_empleos_12_20.csv",year = 2020,mes = 12)
salidas11_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_11_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_11_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_11_20.csv",empresas = "Ciclo_economico/salidas1_empleos_11_20.csv",year = 2020,mes = 11)
salidas10_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_10_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_9_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_9_20.csv",empresas = "Ciclo_economico/salidas1_empleos_9_20.csv",year = 2020,mes = 10)
salidas9_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_9_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_8_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_8_20.csv",empresas = "Ciclo_economico/salidas1_empleos_8_20.csv",year = 2020,mes = 9)
salidas8_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_8_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_8_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_8_20.csv",empresas = "Ciclo_economico/salidas1_empleos_8_20.csv",year = 2020,mes = 8)
salidas7_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_7_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_7_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_7_20.csv",empresas = "Ciclo_economico/salidas1_empleos_7_20.csv",year = 2020,mes = 7)
salidas6_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_6_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_6_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_6_20.csv",empresas = "Ciclo_economico/salidas1_empleos_6_20_.csv",year = 2020,mes = 6)
salidas5_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_5_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_5_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_5_20.csv",empresas = "Ciclo_economico/salidas1_empleos_5_20.csv",year = 2020,mes = 5)
salidas4_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_4_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_4_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_4_20.csv",empresas = "Ciclo_economico/salidas1_empleos_4_20.csv",year = 2020,mes = 4)
salidas3_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_3_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_3_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_3_20.csv",empresas = "Ciclo_economico/salidas1_empleos_3_20.csv",year = 2020,mes = 3)
salidas2_20<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_2_20.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_2_20.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_2_20.csv",empresas = "Ciclo_economico/salidas1_empleos_2_20.csv",year = 2020,mes = 2)
  
salidas12_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_12_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_12_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_12_19.csv",empresas = "Ciclo_economico/salidas1_empleos_12_19.csv",year = 2019,mes = 12)
salidas11_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_11_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_11_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_11_19.csv",empresas = "Ciclo_economico/salidas1_empleos_11_19.csv",year = 2019,mes = 11)
salidas10_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_10_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_9_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_9_19.csv",empresas = "Ciclo_economico/salidas1_empleos_9_19.csv",year = 2019,mes = 10)
salidas9_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_9_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_8_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_8_19.csv",empresas = "Ciclo_economico/salidas1_empleos_8_19.csv",year = 2019,mes = 9)
  salidas8_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_8_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_8_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_8_19.csv",empresas = "Ciclo_economico/salidas1_empleos_8_19.csv",year = 2019,mes = 8)
  salidas7_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_7_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_7_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_7_19.csv",empresas = "Ciclo_economico/salidas1_empleos_7_19.csv",year = 2019,mes = 7)
  salidas6_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_6_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_6_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_6_19.csv",empresas = "Ciclo_economico/salidas1_empleos_6_19.csv",year = 2019,mes = 6)
  salidas5_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_5_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_5_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_5_19.csv",empresas = "Ciclo_economico/salidas1_empleos_5_19.csv",year = 2019,mes = 5)
  salidas4_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_4_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_4_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_4_19.csv",empresas = "Ciclo_economico/salidas1_empleos_4_19.csv",year = 2019,mes = 4)
  salidas3_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_3_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_3_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_3_19.csv",empresas = "Ciclo_economico/salidas1_empleos_3_19.csv",year = 2019,mes = 3)
  salidas2_19<-cicloS1_new(tabla = "Ciclo_economico/salidas_empleos_2_19.csv",permanentes = "Ciclo_economico/salidas_permanentes_AE_2_19.csv",permanentes2 = "Ciclo_economico/salidas_permanentes_size_2_19.csv",empresas = "Ciclo_economico/salidas1_empleos_2_19.csv",year = 2019,mes = 2)

Recuadros_general<-rbind(salidas12_21$Recuadros,salidas11_21$Recuadros,salidas10_21$Recuadros,salidas9_21$Recuadros,salidas8_21$Recuadros,salidas7_21$Recuadros,salidas6_21$Recuadros,salidas5_21$Recuadros,salidas4_21$Recuadros,salidas3_21$Recuadros,salidas2_21$Recuadros,
                         salidas12_20$Recuadros,salidas11_20$Recuadros,salidas10_20$Recuadros,salidas9_20$Recuadros,salidas8_20$Recuadros,salidas7_20$Recuadros,salidas6_20$Recuadros,salidas5_20$Recuadros,salidas4_20$Recuadros,salidas3_20$Recuadros,salidas2_20$Recuadros,
                         salidas12_19$Recuadros,salidas11_19$Recuadros,salidas10_19$Recuadros,salidas9_19$Recuadros,salidas8_19$Recuadros,salidas7_19$Recuadros,salidas6_19$Recuadros,salidas5_19$Recuadros,salidas4_19$Recuadros,salidas3_19$Recuadros,salidas2_19$Recuadros)
Grafico_general<-rbind(salidas12_21$Grafico,salidas11_21$Grafico,salidas10_21$Grafico,salidas9_21$Grafico,salidas8_21$Grafico,salidas7_21$Grafico,salidas6_21$Grafico,salidas5_21$Grafico,salidas4_21$Grafico,salidas3_21$Grafico,salidas2_21$Grafico,
                         salidas12_20$Grafico,salidas11_20$Grafico,salidas10_20$Grafico,salidas9_20$Grafico,salidas8_20$Grafico,salidas7_20$Grafico,salidas6_20$Grafico,salidas5_20$Grafico,salidas4_20$Grafico,salidas3_20$Grafico,salidas2_20$Grafico,
                         salidas12_19$Grafico,salidas11_19$Grafico,salidas10_19$Grafico,salidas9_19$Grafico,salidas8_19$Grafico,salidas7_19$Grafico,salidas6_19$Grafico,salidas5_19$Grafico,salidas4_19$Grafico,salidas3_19$Grafico,salidas2_19$Grafico)
Permanentes_general<-rbind(salidas12_21$Permanentes,salidas11_21$Permanentes,salidas10_21$Permanentes,salidas9_21$Permanentes,salidas8_21$Permanentes,salidas7_21$Permanentes,salidas6_21$Permanentes,salidas5_21$Permanentes,salidas4_21$Permanentes,salidas3_21$Permanentes,salidas2_21$Permanentes,
                       salidas12_20$Permanentes,salidas11_20$Permanentes,salidas10_20$Permanentes,salidas9_20$Permanentes,salidas8_20$Permanentes,salidas7_20$Permanentes,salidas6_20$Permanentes,salidas5_20$Permanentes,salidas4_20$Permanentes,salidas3_20$Permanentes,salidas2_20$Permanentes,
                       salidas12_19$Permanentes,salidas11_19$Permanentes,salidas10_19$Permanentes,salidas9_19$Permanentes,salidas8_19$Permanentes,salidas7_19$Permanentes,salidas6_19$Permanentes,salidas5_19$Permanentes,salidas4_19$Permanentes,salidas3_19$Permanentes,salidas2_19$Permanentes)
Permanentes_cambia_general<-rbind(salidas12_21$Permanentes_cambia,salidas11_21$Permanentes_cambia,salidas10_21$Permanentes_cambia,salidas9_21$Permanentes_cambia,salidas8_21$Permanentes_cambia,salidas7_21$Permanentes_cambia,salidas6_21$Permanentes_cambia,salidas5_21$Permanentes_cambia,salidas4_21$Permanentes_cambia,salidas3_21$Permanentes_cambia,salidas2_21$Permanentes_cambia,
                           salidas12_20$Permanentes_cambia,salidas11_20$Permanentes_cambia,salidas10_20$Permanentes_cambia,salidas9_20$Permanentes_cambia,salidas8_20$Permanentes_cambia,salidas7_20$Permanentes_cambia,salidas6_20$Permanentes_cambia,salidas5_20$Permanentes_cambia,salidas4_20$Permanentes_cambia,salidas3_20$Permanentes_cambia,salidas2_20$Permanentes_cambia,
                           salidas12_19$Permanentes_cambia,salidas11_19$Permanentes_cambia,salidas10_19$Permanentes_cambia,salidas9_19$Permanentes_cambia,salidas8_19$Permanentes_cambia,salidas7_19$Permanentes_cambia,salidas6_19$Permanentes_cambia,salidas5_19$Permanentes_cambia,salidas4_19$Permanentes_cambia,salidas3_19$Permanentes_cambia,salidas2_19$Permanentes_cambia)
Empresas_general<-rbind(salidas12_21$Empresas,salidas11_21$Empresas,salidas10_21$Empresas,salidas9_21$Empresas,salidas8_21$Empresas,salidas7_21$Empresas,salidas6_21$Empresas,salidas5_21$Empresas,salidas4_21$Empresas,salidas3_21$Empresas,salidas2_21$Empresas,
                                  salidas12_20$Empresas,salidas11_20$Empresas,salidas10_20$Empresas,salidas9_20$Empresas,salidas8_20$Empresas,salidas7_20$Empresas,salidas6_20$Empresas,salidas5_20$Empresas,salidas4_20$Empresas,salidas3_20$Empresas,salidas2_20$Empresas,
                                  salidas12_19$Empresas,salidas11_19$Empresas,salidas10_19$Empresas,salidas9_19$Empresas,salidas8_19$Empresas,salidas7_19$Empresas,salidas6_19$Empresas,salidas5_19$Empresas,salidas4_19$Empresas,salidas3_19$Empresas,salidas2_19$Empresas)
demografico_general<-rbind(salidas12_21$demografico,salidas11_21$demografico,salidas10_21$demografico,salidas9_21$demografico,salidas8_21$demografico,salidas7_21$demografico,salidas6_21$demografico,salidas5_21$demografico,salidas4_21$demografico,salidas3_21$demografico,salidas2_21$demografico,
                        salidas12_20$demografico,salidas11_20$demografico,salidas10_20$demografico,salidas9_20$demografico,salidas8_20$demografico,salidas7_20$demografico,salidas6_20$demografico,salidas5_20$demografico,salidas4_20$demografico,salidas3_20$demografico,salidas2_20$demografico,
                        salidas12_19$demografico,salidas11_19$demografico,salidas10_19$demografico,salidas9_19$demografico,salidas8_19$demografico,salidas7_19$demografico,salidas6_19$demografico,salidas5_19$demografico,salidas4_19$demografico,salidas3_19$demografico,salidas2_19$demografico)

demografico_general_paralelo<-data.frame(demografico_general%>%filter(Seccion!="Tot")%>%group_by(year,month,mes1,mes2,edad_fin)%>%summarise(Total_personas_salen = sum(Total_personas_salen,na.rm = TRUE),Total_personas_nuevas = sum(Total_personas_nuevas,na.rm = TRUE),Total_personas_permanecen = sum(Total_personas_permanecen,na.rm = TRUE)))
  
write.csv2(Recuadros_general,"Salidas/recuadro_resumen_t_12_.csv")
write.csv2(Grafico_general,"Salidas/Grafico_12_.csv")
write.csv2(Permanentes_general,"Salidas/Permanentes_12_.csv")
write.csv2(Permanentes_cambia_general,"Salidas/Permanentes_cambia_12_.csv")
write.csv2(Empresas_general,"Salidas/Empresas_12_.csv")
write.csv2(demografico_general,"Salidas/Demografico_12_.csv")
write.csv2(demografico_general_paralelo,"Salidas/Demografico_paralelo_12_.csv")

##ejercicio para la diferencia por departamento y municipio 

setwd("D:/Nuevos_Insumos_IMC/empresas/")
Tabla = "empresas_2021_2.csv"
Mpios = read.csv2("D:/Aplicacion/tablero/Ejercicios adicionales/municipios.csv")
Dptos = read.csv2("D:/Aplicacion/tablero/Ejercicios adicionales/departamentos.csv")
Trans_seccion<-read.csv2("Secciones_economicas_desc.csv")
CIIU<-read.xlsx("DetalleCIIU.xlsx", sheetName = "Secciontotal")
year = 2021
month = 12

resumen_ubicacion<-function(Tabla,year,month){
  base<-read.csv2(Tabla)
  #base<-read.csv2(Tabla,sep = ',', dec = '.', na.strings = '$null$')
  
  base[base$Total_cotizantes == 1, "size"] <-rep("Micro_1", nrow(base[base$Total_cotizantes == 1,]))
  base[base$Total_cotizantes > 1 & base$Total_cotizantes <= 10, "size"] <-rep("Micro", nrow(base[base$Total_cotizantes > 1 & base$Total_cotizantes <= 10,]))
  base[base$Total_cotizantes > 10 & base$Total_cotizantes <= 50, "size"] <-rep("Pequeña", nrow(base[base$Total_cotizantes > 10 & base$Total_cotizantes <= 50,]))
  base[base$Total_cotizantes > 50 & base$Total_cotizantes <= 200, "size"] <-rep("Mediana", nrow(base[base$Total_cotizantes > 50  & base$Total_cotizantes <= 200,]))
  base[base$Total_cotizantes > 200 & base$Total_cotizantes <= 500, "size"] <-rep("Grande", nrow(base[base$Total_cotizantes > 200 & base$Total_cotizantes <= 500 ,]))
  base[base$Total_cotizantes > 500 , "size"] <-rep("Muy grande", nrow(base[base$Total_cotizantes > 500 ,]))
  
  base<-merge(base,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y= "Clase", all.x = TRUE)
  base[is.na(base$Seccion),"Seccion"]<-rep("NA", nrow(base[is.na(base$Seccion),]))

  Dptos_val = Dptos[,c("Dpto","val_dpto")]
  base$Divipola <- base$codigo_depto_Max_Max*1000+base$codigo_ciudad_Max_Max
  base<-merge(base,Dptos_val, by.x = "codigo_depto_Max_Max", by.y= "Dpto", all.x = TRUE)
  base[is.na(base$val_dpto),"codigo_depto_Max_Max"]<-rep(1,nrow(base[is.na(base$val_dpto),]))
  Mpios_val = Mpios[,c("Divipola","val_mpio")]
  base<-merge(base,Mpios_val, by = "Divipola", all.x = TRUE)
  base[is.na(base$val_mpio),"Divipola"]<-rep(99999,nrow(base[is.na(base$val_mpio),]))

  res_general_mpio<-data.frame(base%>%group_by(codigo_depto_Max_Max,Divipola,Seccion,size)%>%summarise(total_apt = n(),total_cot =sum(Total_cotizantes,na.rm = TRUE)))
  Dptos_nom = Dptos[,c("Dpto","nom_dpto")]
  Mpios_nom = Mpios[,c("Divipola","nom_mpio")]
  res_general_mpio<-merge(res_general_mpio,Dptos_nom,by.x = "codigo_depto_Max_Max",by.y = "Dpto", all.x = TRUE)
  res_general_mpio<-merge(res_general_mpio,Mpios_nom,by = "Divipola", all.x = TRUE)
  res_general_mpio[is.na(res_general_mpio$nom_dpto),"nom_dpto"]<-rep("No_rep.",nrow(res_general_mpio[is.na(res_general_mpio$nom_dpto),]))
  res_general_mpio[is.na(res_general_mpio$nom_mpio),"nom_mpio"]<-rep("No_rep.",nrow(res_general_mpio[is.na(res_general_mpio$nom_mpio),]))
  res_general_mpio<-merge(res_general_mpio,Trans_seccion,by.x = "Seccion",by.y = "Seccion_PILA", all.x = TRUE)
  res_general_mpio[is.na(res_general_mpio$Seccion_PILA_nom),"Seccion_PILA_nom"]<-rep("Otros",nrow(res_general_mpio[is.na(res_general_mpio$Seccion_PILA_nom),]))

  res_general_nal<-data.frame(res_general_mpio%>%summarise(total_apt = sum(total_apt,na.rm = TRUE),total_cot = sum(total_cot,na.rm = TRUE)))
  res_general_nal$codigo_depto_Max_Max<-0;res_general_nal$nom_dpto<-"Nacional"
  res_general_dpto_tot<-data.frame(res_general_mpio%>%group_by(codigo_depto_Max_Max,nom_dpto)%>%summarise(total_apt = sum(total_apt,na.rm = TRUE),total_cot = sum(total_cot,na.rm = TRUE)))
  res_dpto_SE_tot<-data.frame(res_general_mpio%>%group_by(codigo_depto_Max_Max,nom_dpto,Seccion,Seccion_PILA_nom)%>%summarise(total_apt = sum(total_apt,na.rm = TRUE),total_cot = sum(total_cot,na.rm = TRUE)))
  res_dpto_SE_tot<-merge(res_dpto_SE_tot,res_general_dpto_tot,by = c("codigo_depto_Max_Max","nom_dpto"),all.x = TRUE)
  res_dpto_SE_tot$dist_apt<-res_dpto_SE_tot$total_apt.x/res_dpto_SE_tot$total_apt.y
  res_dpto_SE_tot$dist_cot<-res_dpto_SE_tot$total_cot.x/res_dpto_SE_tot$total_cot.y
  res_dpto_size_tot<-data.frame(res_general_mpio%>%group_by(codigo_depto_Max_Max,nom_dpto,size)%>%summarise(total_apt = sum(total_apt,na.rm = TRUE),total_cot = sum(total_cot,na.rm = TRUE)))
  res_dpto_size_tot<-merge(res_dpto_size_tot,res_general_dpto_tot,by = c("codigo_depto_Max_Max","nom_dpto"),all.x = TRUE)
  res_dpto_size_tot$dist_apt<-res_dpto_size_tot$total_apt.x/res_dpto_size_tot$total_apt.y
  res_dpto_size_tot$dist_cot<-res_dpto_size_tot$total_cot.x/res_dpto_size_tot$total_cot.y
  
  res_general_mpio_tot<-data.frame(res_general_mpio%>%group_by(codigo_depto_Max_Max,nom_dpto,Divipola,nom_mpio)%>%summarise(total_apt = sum(total_apt,na.rm = TRUE),total_cot = sum(total_cot,na.rm = TRUE)))
  res_mpio_SE_tot<-data.frame(res_general_mpio%>%group_by(codigo_depto_Max_Max,nom_dpto,Divipola,nom_mpio,Seccion,Seccion_PILA_nom)%>%summarise(total_apt = sum(total_apt,na.rm = TRUE),total_cot = sum(total_cot,na.rm = TRUE)))
  res_mpio_SE_tot<-merge(res_mpio_SE_tot,res_general_mpio_tot,by = c("codigo_depto_Max_Max","nom_dpto","Divipola","nom_mpio"),all.x = TRUE)
  res_mpio_SE_tot$dist_apt<-res_mpio_SE_tot$total_apt.x/res_mpio_SE_tot$total_apt.y
  res_mpio_SE_tot$dist_cot<-res_mpio_SE_tot$total_cot.x/res_mpio_SE_tot$total_cot.y
  res_mpio_size_tot<-data.frame(res_general_mpio%>%group_by(codigo_depto_Max_Max,nom_dpto,Divipola,nom_mpio,size)%>%summarise(total_apt = sum(total_apt,na.rm = TRUE),total_cot = sum(total_cot,na.rm = TRUE)))
  res_mpio_size_tot<-merge(res_mpio_size_tot,res_general_mpio_tot,by = c("codigo_depto_Max_Max","nom_dpto","Divipola","nom_mpio"),all.x = TRUE)
  res_mpio_size_tot$dist_apt<-res_mpio_size_tot$total_apt.x/res_mpio_size_tot$total_apt.y
  res_mpio_size_tot$dist_cot<-res_mpio_size_tot$total_cot.x/res_mpio_size_tot$total_cot.y
  res_general_mpio_tot<-merge(res_general_mpio_tot,res_general_dpto_tot, by = c("codigo_depto_Max_Max","nom_dpto"), all.x = TRUE)
  res_general_mpio_tot$dist_apt<-res_general_mpio_tot$total_apt.x/res_general_mpio_tot$total_apt.y
  res_general_mpio_tot$dist_cot<-res_general_mpio_tot$total_cot.x/res_general_mpio_tot$total_cot.y
  
  res_general_dpto_tot<-rbind(res_general_dpto_tot,res_general_nal)
  res_general_dpto_tot$dist_apt<-res_general_dpto_tot$total_apt/res_general_nal$total_apt
  res_general_dpto_tot$dist_cot<-res_general_dpto_tot$total_cot/res_general_nal$total_cot
  
  res_general_dpto_tot$year<-rep(year,nrow(res_general_dpto_tot))
  res_general_dpto_tot$month<-rep(month,nrow(res_general_dpto_tot));res_general_dpto_tot<-merge(res_general_dpto_tot,mes_rep, by = "month", all.x = TRUE)
  res_dpto_SE_tot$year<-rep(year,nrow(res_dpto_SE_tot))
  res_dpto_SE_tot$month<-rep(month,nrow(res_dpto_SE_tot));res_dpto_SE_tot<-merge(res_dpto_SE_tot,mes_rep, by = "month", all.x = TRUE)
  res_dpto_size_tot$year<-rep(year,nrow(res_dpto_size_tot))
  res_dpto_size_tot$month<-rep(month,nrow(res_dpto_size_tot));res_dpto_size_tot<-merge(res_dpto_size_tot,mes_rep, by = "month", all.x = TRUE)

  res_general_mpio_tot$year<-rep(year,nrow(res_general_mpio_tot))
  res_general_mpio_tot$month<-rep(month,nrow(res_general_mpio_tot));res_general_mpio_tot<-merge(res_general_mpio_tot,mes_rep, by = "month", all.x = TRUE)
  res_mpio_SE_tot$year<-rep(year,nrow(res_mpio_SE_tot))
  res_mpio_SE_tot$month<-rep(month,nrow(res_mpio_SE_tot));res_mpio_SE_tot<-merge(res_mpio_SE_tot,mes_rep, by = "month", all.x = TRUE)
  res_mpio_size_tot$year<-rep(year,nrow(res_mpio_size_tot))
  res_mpio_size_tot$month<-rep(month,nrow(res_mpio_size_tot));res_mpio_size_tot<-merge(res_mpio_size_tot,mes_rep, by = "month", all.x = TRUE)
  
  salida = list(res_dpto = res_general_dpto_tot,res_dpto_AE = res_dpto_SE_tot, res_dpto_size = res_dpto_size_tot,res_mpio = res_general_mpio_tot,res_mpio_AE = res_mpio_SE_tot, res_mpio_size=res_mpio_size_tot)
  salida
}

res_12_21 <- resumen_ubicacion(Tabla = "empresas_2021_12_new.csv",year = 2021,month = 12)
res_11_21 <- resumen_ubicacion(Tabla = "empresas_2021_11_new.csv",year = 2021,month = 11)
res_10_21 <- resumen_ubicacion(Tabla = "empresas_2021_10_new.csv",year = 2021,month = 10)
res_9_21 <- resumen_ubicacion(Tabla = "empresas_2021_9_new.csv",year = 2021,month = 9)
res_8_21 <- resumen_ubicacion(Tabla = "empresas_2021_8.csv",year = 2021,month = 8)
res_7_21 <- resumen_ubicacion(Tabla = "empresas_2021_7.csv",year = 2021,month = 7)
res_6_21 <- resumen_ubicacion(Tabla = "empresas_2021_6.csv",year = 2021,month = 6)
res_5_21 <- resumen_ubicacion(Tabla = "empresas_2021_5.csv",year = 2021,month = 5)
res_4_21 <- resumen_ubicacion(Tabla = "empresas_2021_4.csv",year = 2021,month = 4)
res_3_21 <- resumen_ubicacion(Tabla = "empresas_2021_3.csv",year = 2021,month = 3)
res_2_21 <- resumen_ubicacion(Tabla = "empresas_2021_2.csv",year = 2021,month = 2)
res_1_21 <- resumen_ubicacion(Tabla = "empresas_2021_1.csv",year = 2021,month = 1)

res_12_20 <- resumen_ubicacion(Tabla = "empresas_2020_12.csv",year = 2020,month = 12)
res_11_20 <- resumen_ubicacion(Tabla = "empresas_2020_11.csv",year = 2020,month = 11)
res_10_20 <- resumen_ubicacion(Tabla = "empresas_2020_10.csv",year = 2020,month = 10)
res_9_20 <- resumen_ubicacion(Tabla = "empresas_2020_9.csv",year = 2020,month = 9)
res_8_20 <- resumen_ubicacion(Tabla = "empresas_2020_8.csv",year = 2020,month = 8)
res_7_20 <- resumen_ubicacion(Tabla = "empresas_2020_7.csv",year = 2020,month = 7)
res_6_20 <- resumen_ubicacion(Tabla = "empresas_2020_6.csv",year = 2020,month = 6)
res_5_20 <- resumen_ubicacion(Tabla = "empresas_2020_5.csv",year = 2020,month = 5)
res_4_20 <- resumen_ubicacion(Tabla = "empresas_2020_4.csv",year = 2020,month = 4)
res_3_20 <- resumen_ubicacion(Tabla = "empresas_2020_3.csv",year = 2020,month = 3)
res_2_20 <- resumen_ubicacion(Tabla = "empresas_2020_2.csv",year = 2020,month = 2)
res_1_20 <- resumen_ubicacion(Tabla = "empresas_2020_1.csv",year = 2020,month = 1)

res_12_19 <- resumen_ubicacion(Tabla = "empresas_2019_12.csv",year = 2019,month = 12)
res_11_19 <- resumen_ubicacion(Tabla = "empresas_2019_11.csv",year = 2019,month = 11)
res_10_19 <- resumen_ubicacion(Tabla = "empresas_2019_10.csv",year = 2019,month = 10)
res_9_19 <- resumen_ubicacion(Tabla = "empresas_2019_9.csv",year = 2019,month = 9)
res_8_19 <- resumen_ubicacion(Tabla = "empresas_2019_8.csv",year = 2019,month = 8)
res_7_19 <- resumen_ubicacion(Tabla = "empresas_2019_7.csv",year = 2019,month = 7)
res_6_19 <- resumen_ubicacion(Tabla = "empresas_2019_6.csv",year = 2019,month = 6)
res_5_19 <- resumen_ubicacion(Tabla = "empresas_2019_5.csv",year = 2019,month = 5)
res_4_19 <- resumen_ubicacion(Tabla = "empresas_2019_4.csv",year = 2019,month = 4)
res_3_19 <- resumen_ubicacion(Tabla = "empresas_2019_3.csv",year = 2019,month = 3)
res_2_19 <- resumen_ubicacion(Tabla = "empresas_2019_2.csv",year = 2019,month = 2)
res_1_19 <- resumen_ubicacion(Tabla = "empresas_2019_1.csv",year = 2019,month = 1)

res_12_18 <- resumen_ubicacion(Tabla = "empresas_2018_12.csv",year = 2018,month = 12)
res_11_18 <- resumen_ubicacion(Tabla = "empresas_2018_11.csv",year = 2018,month = 11)
res_10_18 <- resumen_ubicacion(Tabla = "empresas_2018_10.csv",year = 2018,month = 10)
res_9_18 <- resumen_ubicacion(Tabla = "empresas_2018_9.csv",year = 2018,month = 9)
res_8_18 <- resumen_ubicacion(Tabla = "empresas_2018_8.csv",year = 2018,month = 8)
res_7_18 <- resumen_ubicacion(Tabla = "empresas_2018_7.csv",year = 2018,month = 7)
res_6_18 <- resumen_ubicacion(Tabla = "empresas_2018_6.csv",year = 2018,month = 6)
res_5_18 <- resumen_ubicacion(Tabla = "empresas_2018_5.csv",year = 2018,month = 5)
res_4_18 <- resumen_ubicacion(Tabla = "empresas_2018_4.csv",year = 2018,month = 4)
res_3_18 <- resumen_ubicacion(Tabla = "empresas_2018_3.csv",year = 2018,month = 3)
res_2_18 <- resumen_ubicacion(Tabla = "empresas_2018_2.csv",year = 2018,month = 2)
res_1_18 <- resumen_ubicacion(Tabla = "empresas_2018_1.csv",year = 2018,month = 1)

res_general_dpto_fn<-rbind(res_12_21$res_dpto,res_11_21$res_dpto,res_10_21$res_dpto,res_9_21$res_dpto,res_8_21$res_dpto,res_7_21$res_dpto,res_6_21$res_dpto,res_5_21$res_dpto,res_4_21$res_dpto,res_3_21$res_dpto,res_2_21$res_dpto,res_1_21$res_dpto,
                        res_12_20$res_dpto,res_11_20$res_dpto,res_10_20$res_dpto,res_9_20$res_dpto,res_8_20$res_dpto,res_7_20$res_dpto,res_6_20$res_dpto,res_5_20$res_dpto,res_4_20$res_dpto,res_3_20$res_dpto,res_2_20$res_dpto,res_1_20$res_dpto,
                        res_12_19$res_dpto,res_11_19$res_dpto,res_10_19$res_dpto,res_9_19$res_dpto,res_8_19$res_dpto,res_7_19$res_dpto,res_6_19$res_dpto,res_5_19$res_dpto,res_4_19$res_dpto,res_3_19$res_dpto,res_2_19$res_dpto,res_1_19$res_dpto,
                        res_12_18$res_dpto,res_11_18$res_dpto,res_10_18$res_dpto,res_9_18$res_dpto,res_8_18$res_dpto,res_7_18$res_dpto,res_6_18$res_dpto,res_5_18$res_dpto,res_4_18$res_dpto,res_3_18$res_dpto,res_2_18$res_dpto,res_1_18$res_dpto)
res_general_dpto_AE_fn<-rbind(res_12_21$res_dpto_AE,res_11_21$res_dpto_AE,res_10_21$res_dpto_AE,res_9_21$res_dpto_AE,res_8_21$res_dpto_AE,res_7_21$res_dpto_AE,res_6_21$res_dpto_AE,res_5_21$res_dpto_AE,res_4_21$res_dpto_AE,res_3_21$res_dpto_AE,res_2_21$res_dpto_AE,res_1_21$res_dpto_AE,
                        res_12_20$res_dpto_AE,res_11_20$res_dpto_AE,res_10_20$res_dpto_AE,res_9_20$res_dpto_AE,res_8_20$res_dpto_AE,res_7_20$res_dpto_AE,res_6_20$res_dpto_AE,res_5_20$res_dpto_AE,res_4_20$res_dpto_AE,res_3_20$res_dpto_AE,res_2_20$res_dpto_AE,res_1_20$res_dpto_AE,
                        res_12_19$res_dpto_AE,res_11_19$res_dpto_AE,res_10_19$res_dpto_AE,res_9_19$res_dpto_AE,res_8_19$res_dpto_AE,res_7_19$res_dpto_AE,res_6_19$res_dpto_AE,res_5_19$res_dpto_AE,res_4_19$res_dpto_AE,res_3_19$res_dpto_AE,res_2_19$res_dpto_AE,res_1_19$res_dpto_AE,
                        res_12_18$res_dpto_AE,res_11_18$res_dpto_AE,res_10_18$res_dpto_AE,res_9_18$res_dpto_AE,res_8_18$res_dpto_AE,res_7_18$res_dpto_AE,res_6_18$res_dpto_AE,res_5_18$res_dpto_AE,res_4_18$res_dpto_AE,res_3_18$res_dpto_AE,res_2_18$res_dpto_AE,res_1_18$res_dpto_AE)
res_general_dpto_size_fn<-rbind(res_12_21$res_dpto_size,res_11_21$res_dpto_size,res_10_21$res_dpto_size,res_9_21$res_dpto_size,res_8_21$res_dpto_size,res_7_21$res_dpto_size,res_6_21$res_dpto_size,res_5_21$res_dpto_size,res_4_21$res_dpto_size,res_3_21$res_dpto_size,res_2_21$res_dpto_size,res_1_21$res_dpto_size,
                           res_12_20$res_dpto_size,res_11_20$res_dpto_size,res_10_20$res_dpto_size,res_9_20$res_dpto_size,res_8_20$res_dpto_size,res_7_20$res_dpto_size,res_6_20$res_dpto_size,res_5_20$res_dpto_size,res_4_20$res_dpto_size,res_3_20$res_dpto_size,res_2_20$res_dpto_size,res_1_20$res_dpto_size,
                           res_12_19$res_dpto_size,res_11_19$res_dpto_size,res_10_19$res_dpto_size,res_9_19$res_dpto_size,res_8_19$res_dpto_size,res_7_19$res_dpto_size,res_6_19$res_dpto_size,res_5_19$res_dpto_size,res_4_19$res_dpto_size,res_3_19$res_dpto_size,res_2_19$res_dpto_size,res_1_19$res_dpto_size,
                           res_12_18$res_dpto_size,res_11_18$res_dpto_size,res_10_18$res_dpto_size,res_9_18$res_dpto_size,res_8_18$res_dpto_size,res_7_18$res_dpto_size,res_6_18$res_dpto_size,res_5_18$res_dpto_size,res_4_18$res_dpto_size,res_3_18$res_dpto_size,res_2_18$res_dpto_size,res_1_18$res_dpto_size)

res_general_mpio_fn<-rbind(res_12_21$res_mpio,res_11_21$res_mpio,res_10_21$res_mpio,res_9_21$res_mpio,res_8_21$res_mpio,res_7_21$res_mpio,res_6_21$res_mpio,res_5_21$res_mpio,res_4_21$res_mpio,res_3_21$res_mpio,res_2_21$res_mpio,res_1_21$res_mpio,
                        res_12_20$res_mpio,res_11_20$res_mpio,res_10_20$res_mpio,res_9_20$res_mpio,res_8_20$res_mpio,res_7_20$res_mpio,res_6_20$res_mpio,res_5_20$res_mpio,res_4_20$res_mpio,res_3_20$res_mpio,res_2_20$res_mpio,res_1_20$res_mpio,
                        res_12_19$res_mpio,res_11_19$res_mpio,res_10_19$res_mpio,res_9_19$res_mpio,res_8_19$res_mpio,res_7_19$res_mpio,res_6_19$res_mpio,res_5_19$res_mpio,res_4_19$res_mpio,res_3_19$res_mpio,res_2_19$res_mpio,res_1_19$res_mpio,
                        res_12_18$res_mpio,res_11_18$res_mpio,res_10_18$res_mpio,res_9_18$res_mpio,res_8_18$res_mpio,res_7_18$res_mpio,res_6_18$res_mpio,res_5_18$res_mpio,res_4_18$res_mpio,res_3_18$res_mpio,res_2_18$res_mpio,res_1_18$res_mpio)
res_general_mpio_AE_fn<-rbind(res_12_21$res_mpio_AE,res_11_21$res_mpio_AE,res_10_21$res_mpio_AE,res_9_21$res_mpio_AE,res_8_21$res_mpio_AE,res_7_21$res_mpio_AE,res_6_21$res_mpio_AE,res_5_21$res_mpio_AE,res_4_21$res_mpio_AE,res_3_21$res_mpio_AE,res_2_21$res_mpio_AE,res_1_21$res_mpio_AE,
                        res_12_20$res_mpio_AE,res_11_20$res_mpio_AE,res_10_20$res_mpio_AE,res_9_20$res_mpio_AE,res_8_20$res_mpio_AE,res_7_20$res_mpio_AE,res_6_20$res_mpio_AE,res_5_20$res_mpio_AE,res_4_20$res_mpio_AE,res_3_20$res_mpio_AE,res_2_20$res_mpio_AE,res_1_20$res_mpio_AE,
                        res_12_19$res_mpio_AE,res_11_19$res_mpio_AE,res_10_19$res_mpio_AE,res_9_19$res_mpio_AE,res_8_19$res_mpio_AE,res_7_19$res_mpio_AE,res_6_19$res_mpio_AE,res_5_19$res_mpio_AE,res_4_19$res_mpio_AE,res_3_19$res_mpio_AE,res_2_19$res_mpio_AE,res_1_19$res_mpio_AE,
                        res_12_18$res_mpio_AE,res_11_18$res_mpio_AE,res_10_18$res_mpio_AE,res_9_18$res_mpio_AE,res_8_18$res_mpio_AE,res_7_18$res_mpio_AE,res_6_18$res_mpio_AE,res_5_18$res_mpio_AE,res_4_18$res_mpio_AE,res_3_18$res_mpio_AE,res_2_18$res_mpio_AE,res_1_18$res_mpio_AE)
res_general_mpio_size_fn<-rbind(res_12_21$res_mpio_size,res_11_21$res_mpio_size,res_10_21$res_mpio_size,res_9_21$res_mpio_size,res_8_21$res_mpio_size,res_7_21$res_mpio_size,res_6_21$res_mpio_size,res_5_21$res_mpio_size,res_4_21$res_mpio_size,res_3_21$res_mpio_size,res_2_21$res_mpio_size,res_1_21$res_mpio_size,
                        res_12_20$res_mpio_size,res_11_20$res_mpio_size,res_10_20$res_mpio_size,res_9_20$res_mpio_size,res_8_20$res_mpio_size,res_7_20$res_mpio_size,res_6_20$res_mpio_size,res_5_20$res_mpio_size,res_4_20$res_mpio_size,res_3_20$res_mpio_size,res_2_20$res_mpio_size,res_1_20$res_mpio_size,
                        res_12_19$res_mpio_size,res_11_19$res_mpio_size,res_10_19$res_mpio_size,res_9_19$res_mpio_size,res_8_19$res_mpio_size,res_7_19$res_mpio_size,res_6_19$res_mpio_size,res_5_19$res_mpio_size,res_4_19$res_mpio_size,res_3_19$res_mpio_size,res_2_19$res_mpio_size,res_1_19$res_mpio_size,
                        res_12_18$res_mpio_size,res_11_18$res_mpio_size,res_10_18$res_mpio_size,res_9_18$res_mpio_size,res_8_18$res_mpio_size,res_7_18$res_mpio_size,res_6_18$res_mpio_size,res_5_18$res_mpio_size,res_4_18$res_mpio_size,res_3_18$res_mpio_size,res_2_18$res_mpio_size,res_1_18$res_mpio_size)

res_general_dpto_fn<-res_general_dpto_fn[order(res_general_dpto_fn$codigo_depto_Max_Max,res_general_dpto_fn$year,res_general_dpto_fn$month),]
res_general_dpto_fn$var_mensua_cot<-rep(0,nrow(res_general_dpto_fn))
res_general_dpto_fn$var_anual_cot<-rep(0,nrow(res_general_dpto_fn))
res_general_dpto_fn$var_mensua_apt<-rep(0,nrow(res_general_dpto_fn))
res_general_dpto_fn$var_anual_apt<-rep(0,nrow(res_general_dpto_fn))
res_general_dpto_fn$secuencia<-1:nrow(res_general_dpto_fn)
departamentos<-c(Dptos[,1],0,1)

for(dpto in departamentos){
  filtro_dpto<-res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto,]
  for(i in (min(filtro_dpto$secuencia)+1):max(filtro_dpto$secuencia)){
    res_general_dpto_fn[res_general_dpto_fn$secuencia == i,"var_mensua_cot"]<-
      (res_general_dpto_fn[res_general_dpto_fn$secuencia == i,"total_cot"]-res_general_dpto_fn[res_general_dpto_fn$secuencia == (i-1),"total_cot"])/
      res_general_dpto_fn[res_general_dpto_fn$secuencia == (i-1),"total_cot"]
    res_general_dpto_fn[res_general_dpto_fn$secuencia == i,"var_mensua_apt"]<-
      (res_general_dpto_fn[res_general_dpto_fn$secuencia == i,"total_apt"]-res_general_dpto_fn[res_general_dpto_fn$secuencia == (i-1),"total_apt"])/
      res_general_dpto_fn[res_general_dpto_fn$secuencia == (i-1),"total_apt"]
  }
  for(j in c(2019,2020,2021)){
    meses_entran<-res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == j, "month"]
    for(mes_ana in meses_entran){
      res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == j & res_general_dpto_fn$month == mes_ana,"var_anual_cot"]<-
        (res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == j & res_general_dpto_fn$month == mes_ana,"total_cot"]-res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == (j-1) & res_general_dpto_fn$month == mes_ana,"total_cot"])/
        res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == (j-1) & res_general_dpto_fn$month == mes_ana,"total_cot"]
      res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == j & res_general_dpto_fn$month == mes_ana,"var_anual_apt"]<-
        (res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == j & res_general_dpto_fn$month == mes_ana,"total_apt"]-res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == (j-1) & res_general_dpto_fn$month == mes_ana,"total_apt"])/
        res_general_dpto_fn[res_general_dpto_fn$codigo_depto_Max_Max == dpto & res_general_dpto_fn$year == (j-1) & res_general_dpto_fn$month == mes_ana,"total_apt"]
    }
  }
}

res_general_mpio_fn<-res_general_mpio_fn[order(res_general_mpio_fn$codigo_depto_Max_Max,res_general_mpio_fn$Divipola,res_general_mpio_fn$year,res_general_mpio_fn$month),]
#res_general_mpio_fn$var_mensua_cot<-rep(0,nrow(res_general_mpio_fn))
#res_general_mpio_fn$var_anual_cot<-rep(0,nrow(res_general_mpio_fn))
#res_general_mpio_fn$var_mensua_apt<-rep(0,nrow(res_general_mpio_fn))
#res_general_mpio_fn$var_anual_apt<-rep(0,nrow(res_general_mpio_fn))
#res_general_mpio_fn$secuencia<-1:nrow(res_general_mpio_fn)

#for(dpto in departamentos){
#  municipio<-data.frame(res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto,]%>%group_by(Divipola)%>%summarise(total_uni = n()))[,1]
#  for(muni in municipio){
#    filtro_datos = res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == res_general_mpio_fn & res_general_mpio_fn$Divipola == muni,]
#    for(i in (min(filtro_datos$secuencia)+1):max(filtro_datos$secuencia)){
#      res_general_mpio_fn[res_general_mpio_fn$secuencia == i,"var_mensua_cot"]<-
#        (res_general_mpio_fn[res_general_mpio_fn$secuencia == i,"total_cot"]-res_general_mpio_fn[res_general_mpio_fn$secuencia == (i-1),"total_cot"])/
#        res_general_mpio_fn[res_general_mpio_fn$secuencia == (i-1),"total_cot"]
#      res_general_mpio_fn[res_general_mpio_fn$secuencia == i,"var_mensua_apt"]<-
#        (res_general_mpio_fn[res_general_mpio_fn$secuencia == i,"total_apt"]-res_general_mpio_fn[res_general_mpio_fn$secuencia == (i-1),"total_apt"])/
#        res_general_mpio_fn[res_general_mpio_fn$secuencia == (i-1),"total_apt"]
#    }
#    for(años in c(2019,2020,2021)){
#      meses_entran<-res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto &res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == j, "month"]
#      for(mes_ana in meses_entran){
#        res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto & res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == años &res_general_mpio_fn$month ==mes_ana,"var_anual_cot"]<-
#          (res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto & res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == años &res_general_mpio_fn$month ==mes_ana,"total_cot"]-
#             res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto & res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == (años-1) &res_general_mpio_fn$month ==mes_ana,"total_cot"])/
#              res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto & res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == (años-1) &res_general_mpio_fn$month ==mes_ana,"total_cot"]
#        res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto & res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == años &res_general_mpio_fn$month ==mes_ana,"var_anual_apt"]<-
#          (res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto & res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == años &res_general_mpio_fn$month ==mes_ana,"total_apt"]-
#             res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto & res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == (años-1) &res_general_mpio_fn$month ==mes_ana,"total_apt"])/
#               res_general_mpio_fn[res_general_mpio_fn$codigo_depto_Max_Max == dpto & res_general_mpio_fn$Divipola == muni & res_general_mpio_fn$year == (años-1) &res_general_mpio_fn$month ==mes_ana,"total_apt"]
#        }
#      }
#    }
#  }
#}

setwd("D:/Nuevos_Insumos_IMC/salidas/")
write.csv2(res_general_dpto_fn,"Departamental_fn_12_.csv")
write.csv2(res_general_dpto_AE_fn,"Departamental_fn_AE_12_.csv")
write.csv2(res_general_dpto_size_fn,"Departamental_fn_size_12_.csv")
write.csv2(res_general_mpio_fn,"Municipal_fn_12_.csv")
write.csv2(res_general_mpio_AE_fn,"Municipal_fn_AE_12_.csv")
write.csv2(res_general_mpio_size_fn,"Municipal_fn_size_12_.csv")

########################################################################
#######################################################################
########Cuadro de control 

setwd("D:/Aplicacion/tablero/Resumen_general/")
febrero20_ibc<-read.csv2("Informacion_general_IBC1_4_20.csv")

#######################################################################
#######informacion asobares 
setwd("D:/Aplicacion/tablero/")
tabla1<-"Resumen_general/Informacion_general_SECCION.csv"
tabla2<-"Resumen_general/Informacion_general_edad_sexo.csv"

res_asobaresgen<-function(tabla1,tabla2){
  informacion<-read.csv2(tabla1)
  informacion$division<-as.numeric(substr(informacion$codigo_actividad_economica_Max,1,2))
  resultadost<-data.frame(informacion%>%group_by(Tipologia,YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), total_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE), total_retiros = sum(retiro_Max_Sum, na.rm = TRUE),suspension_temporal_Max_Sum = sum(suspension_temporal_Max_Sum,na.rm = TRUE),vacaciones_Max_Sum = sum(vacaciones_Max_Sum,na.rm = TRUE),licencia_maternidad_Max_Sum = sum(licencia_maternidad_Max_Sum,na.rm = TRUE)))
  resultadosdiv<-data.frame(informacion%>%filter(division == 56)%>%group_by(Tipologia,YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), total_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE), total_retiros = sum(retiro_Max_Sum, na.rm = TRUE),suspension_temporal_Max_Sum = sum(suspension_temporal_Max_Sum,na.rm = TRUE),vacaciones_Max_Sum = sum(vacaciones_Max_Sum,na.rm = TRUE),licencia_maternidad_Max_Sum = sum(licencia_maternidad_Max_Sum,na.rm = TRUE)))
  resultadosclase<-data.frame(informacion%>%filter(codigo_actividad_economica_Max == 5630)%>%group_by(Tipologia,YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), total_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE), total_retiros = sum(retiro_Max_Sum, na.rm = TRUE),suspension_temporal_Max_Sum = sum(suspension_temporal_Max_Sum,na.rm = TRUE),vacaciones_Max_Sum = sum(vacaciones_Max_Sum,na.rm = TRUE),licencia_maternidad_Max_Sum = sum(licencia_maternidad_Max_Sum,na.rm = TRUE)))
  informacion_dem<-read.csv2(tabla2)
  resultadost_demt<-data.frame(informacion_dem%>%group_by(Tipologia,YEAR,MONTH,Sexo_fin,Edad_fin)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
  resultados_gen<-list(resultadost = resultadost,resultadosdiv= resultadosdiv,resultadost_demt = resultadost_demt,resultadosclase = resultadosclase,resultadost_demt = resultadost_demt)
  resultados_gen
}

empresas_gen_21_6_8<-res_asobaresgen("Resumen_general/Informacion_general_SECCION.csv","Resumen_general/Informacion_general_edad_sexo.csv")
empresas_gen_21_3_5<-res_asobaresgen("Resumen_general/Informacion_general_SECCION3_5.csv","Resumen_general/Informacion_general_edad_sexo3_5.csv")
empresas_gen_21_1_2<-res_asobaresgen("Resumen_general/Informacion_general_SECCION1_2.csv","Resumen_general/Informacion_general_edad_sexo1_2.csv")
empresas_gen_20_9_12<-res_asobaresgen("Resumen_general/Informacion_general_SECCION8_12_20.csv","Resumen_general/Informacion_general_edad_sexo8_12_20.csv")
empresas_gen_20_5_8<-res_asobaresgen("Resumen_general/Informacion_general_SECCION5_8_20.csv","Resumen_general/Informacion_general_edad_sexo5_8_20.csv")
empresas_gen_20_1_4<-res_asobaresgen("Resumen_general/Informacion_general_SECCION1_4_20.csv","Resumen_general/Informacion_general_edad_sexo1_4_20.csv")
empresas_gen_19_9_12<-res_asobaresgen("Resumen_general/Informacion_general_SECCION8_12_19.csv","Resumen_general/Informacion_general_edad_sexo8_12_19.csv")
empresas_gen_19_5_8<-res_asobaresgen("Resumen_general/Informacion_general_SECCION5_8_19.csv","Resumen_general/Informacion_general_edad_sexo5_8_19.csv")
empresas_gen_19_1_4<-res_asobaresgen("Resumen_general/Informacion_general_SECCION1_4_19.csv","Resumen_general/Informacion_general_edad_sexo1_4_19.csv")
empresas_gen_18_9_12<-res_asobaresgen("Resumen_general/Informacion_general_SECCION9_12_18.csv","Resumen_general/Informacion_general_edad_sexo9_12_18.csv")
empresas_gen_18_5_8<-res_asobaresgen("Resumen_general/Informacion_general_SECCION5_8_18.csv","Resumen_general/Informacion_general_edad_sexo5_8_18.csv")
empresas_gen_18_1_4<-res_asobaresgen("Resumen_general/Informacion_general_SECCION1_4_18.csv","Resumen_general/Informacion_general_edad_sexo1_4_18.csv")


resumen_general<-rbind(empresas_gen_21_6_8$resultadost,empresas_gen_21_3_5$resultadost,empresas_gen_21_1_2$resultadost,empresas_gen_20_9_12$resultadost,empresas_gen_20_5_8$resultadost,
                       empresas_gen_20_1_4$resultadost,empresas_gen_19_9_12$resultadost,empresas_gen_19_5_8$resultadost,empresas_gen_19_1_4$resultadost,empresas_gen_18_9_12$resultadost,empresas_gen_18_5_8$resultadost,empresas_gen_18_1_4$resultadost)
resumen_division<-rbind(empresas_gen_21_6_8$resultadosdiv,empresas_gen_21_3_5$resultadosdiv,empresas_gen_21_1_2$resultadosdiv,empresas_gen_20_9_12$resultadosdiv,empresas_gen_20_5_8$resultadosdiv,
                       empresas_gen_20_1_4$resultadosdiv,empresas_gen_19_9_12$resultadosdiv,empresas_gen_19_5_8$resultadosdiv,empresas_gen_19_1_4$resultadosdiv,empresas_gen_18_9_12$resultadosdiv,empresas_gen_18_5_8$resultadosdiv,empresas_gen_18_1_4$resultadosdiv)
resumen_clase<-rbind(empresas_gen_21_6_8$resultadosclase,empresas_gen_21_3_5$resultadosclase,empresas_gen_21_1_2$resultadosclase,empresas_gen_20_9_12$resultadosclase,empresas_gen_20_5_8$resultadosclase,
                        empresas_gen_20_1_4$resultadosclase,empresas_gen_19_9_12$resultadosclase,empresas_gen_19_5_8$resultadosclase,empresas_gen_19_1_4$resultadosclase,empresas_gen_18_9_12$resultadosclase,empresas_gen_18_5_8$resultadosclase,empresas_gen_18_1_4$resultadosclase)
resumen_t_demo<-rbind(empresas_gen_21_6_8$resultadost_demt,empresas_gen_21_3_5$resultadost_demt,empresas_gen_21_1_2$resultadost_demt,empresas_gen_20_9_12$resultadost_demt,empresas_gen_20_5_8$resultadost_demt,
                     empresas_gen_20_1_4$resultadost_demt,empresas_gen_19_9_12$resultadost_demt,empresas_gen_19_5_8$resultadost_demt,empresas_gen_19_1_4$resultadost_demt,empresas_gen_18_9_12$resultadost_demt,empresas_gen_18_5_8$resultadost_demt,empresas_gen_18_1_4$resultadost_demt)

#tabla<-"empresas/empresas_2021_8.csv"

res_asobares<-function(tabla){
  informacion<-read.csv2(tabla)
  informacion$division<-as.numeric(substr(informacion$codigo_actividad_economica_Max_Max,1,2))
  informacion[informacion$Total_cotizantes == 1,"size"]<-rep(1,nrow(informacion[informacion$Total_cotizantes == 1,]))
  informacion[informacion$Total_cotizantes > 1 & informacion$Total_cotizantes <= 10,"size"]<-rep(2,nrow(informacion[informacion$Total_cotizantes  > 1 & informacion$Total_cotizantes <= 10,]))
  informacion[informacion$Total_cotizantes > 10 & informacion$Total_cotizantes <= 50,"size"]<-rep(3,nrow(informacion[informacion$Total_cotizantes  > 10 & informacion$Total_cotizantes <= 50,]))
  informacion[informacion$Total_cotizantes > 50 & informacion$Total_cotizantes <= 200,"size"]<-rep(4,nrow(informacion[informacion$Total_cotizantes  > 50 & informacion$Total_cotizantes <= 200,]))
  informacion[informacion$Total_cotizantes > 200 & informacion$Total_cotizantes <= 500,"size"]<-rep(5,nrow(informacion[informacion$Total_cotizantes  > 200 & informacion$Total_cotizantes <= 500,]))
  informacion[informacion$Total_cotizantes > 500,"size"]<-rep(6,nrow(informacion[informacion$Total_cotizantes > 500,]))
  resultadost_apt<-data.frame(informacion%>%group_by(YEAR,MONTH,size)%>%summarise(Total_aportantes = n(),Total_cotizantes = sum(Total_cotizantes, na.rm = TRUE)))
  resultadost_apt_cancela<-data.frame(informacion%>%filter(EstadoMatricula == 3)%>%group_by(YEAR,MONTH,size)%>%summarise(Total_aportantes_cancela = n(),Total_cotizantes_cancela = sum(Total_cotizantes, na.rm = TRUE)))
  resultadost_apt<-merge(resultadost_apt,resultadost_apt_cancela,by =c("YEAR","MONTH","size"), all.x=TRUE)
  resultadost_apt_div<-data.frame(informacion%>%filter(division == 56)%>%group_by(YEAR,MONTH,size)%>%summarise(Total_aportantes = n(),Total_cotizantes = sum(Total_cotizantes, na.rm = TRUE)))
  resultadost_apt_cancela_div<-data.frame(informacion%>%filter(EstadoMatricula == 3,division == 56)%>%group_by(YEAR,MONTH,size)%>%summarise(Total_aportantes_cancela = n(),Total_cotizantes_cancela = sum(Total_cotizantes, na.rm = TRUE)))
  resultadost_apt_div<-merge(resultadost_apt_div,resultadost_apt_cancela_div,by =c("YEAR","MONTH","size"), all.x=TRUE)
  resultadost_apt_clase<-data.frame(informacion%>%filter(codigo_actividad_economica_Max_Max == 5630)%>%group_by(YEAR,MONTH,size)%>%summarise(Total_aportantes = n(),Total_cotizantes = sum(Total_cotizantes, na.rm = TRUE)))
  resultadost_apt_cancela_clase<-data.frame(informacion%>%filter(EstadoMatricula == 3,codigo_actividad_economica_Max_Max == 5630)%>%group_by(YEAR,MONTH,size)%>%summarise(Total_aportantes_cancela = n(),Total_cotizantes_cancela = sum(Total_cotizantes, na.rm = TRUE)))
  resultadost_apt_clase<-merge(resultadost_apt_clase,resultadost_apt_cancela_clase,by =c("YEAR","MONTH","size"), all.x=TRUE)
  resultados_a_aprotante = list(resultado_total = resultadost_apt,resultado_div =  resultadost_apt_div,resultado_clase = resultadost_apt_clase)
  resultados_a_aprotante
}
  
res_general_apt21_8<-res_asobares("empresas/empresas_2021_8.csv")
res_general_apt21_7<-res_asobares("empresas/empresas_2021_7.csv")
res_general_apt21_6<-res_asobares("empresas/empresas_2021_6.csv")
res_general_apt21_5<-res_asobares("empresas/empresas_2021_5.csv")
res_general_apt21_4<-res_asobares("empresas/empresas_2021_4.csv")
res_general_apt21_3<-res_asobares("empresas/empresas_2021_3.csv")
res_general_apt21_2<-res_asobares("empresas/empresas_2021_2.csv")
res_general_apt21_1<-res_asobares("empresas/empresas_2021_1.csv")

res_general_apt20_12<-res_asobares("empresas/empresas_2020_12.csv")
res_general_apt20_11<-res_asobares("empresas/empresas_2020_11.csv")
res_general_apt20_10<-res_asobares("empresas/empresas_2020_10.csv")
res_general_apt20_9<-res_asobares("empresas/empresas_2020_9.csv")
res_general_apt20_8<-res_asobares("empresas/empresas_2020_8.csv")
res_general_apt20_7<-res_asobares("empresas/empresas_2020_7.csv")
res_general_apt20_6<-res_asobares("empresas/empresas_2020_6.csv")
res_general_apt20_5<-res_asobares("empresas/empresas_2020_5.csv")
res_general_apt20_4<-res_asobares("empresas/empresas_2020_4.csv")
res_general_apt20_3<-res_asobares("empresas/empresas_2020_3.csv")
res_general_apt20_2<-res_asobares("empresas/empresas_2020_2.csv")
res_general_apt20_1<-res_asobares("empresas/empresas_2020_1.csv")

res_general_apt19_12<-res_asobares("empresas/empresas_2019_12.csv")
res_general_apt19_11<-res_asobares("empresas/empresas_2019_11.csv")
res_general_apt19_10<-res_asobares("empresas/empresas_2019_10.csv")
res_general_apt19_9<-res_asobares("empresas/empresas_2019_9.csv")
res_general_apt19_8<-res_asobares("empresas/empresas_2019_8.csv")
res_general_apt19_7<-res_asobares("empresas/empresas_2019_7.csv")
res_general_apt19_6<-res_asobares("empresas/empresas_2019_6.csv")
res_general_apt19_5<-res_asobares("empresas/empresas_2019_5.csv")
res_general_apt19_4<-res_asobares("empresas/empresas_2019_4.csv")
res_general_apt19_3<-res_asobares("empresas/empresas_2019_3.csv")
res_general_apt19_2<-res_asobares("empresas/empresas_2019_2.csv")
res_general_apt19_1<-res_asobares("empresas/empresas_2019_1.csv")

resumen_emp_total<-rbind(res_general_apt21_8$resultado_total,
                         res_general_apt21_7$resultado_total,
                         res_general_apt21_6$resultado_total,
                         res_general_apt21_5$resultado_total,
                         res_general_apt21_4$resultado_total,
                         res_general_apt21_3$resultado_total,
                         res_general_apt21_2$resultado_total,
                         res_general_apt21_1$resultado_total,
                         res_general_apt20_12$resultado_total,
                         res_general_apt20_11$resultado_total,
                         res_general_apt20_10$resultado_total,
                         res_general_apt20_9$resultado_total,
                         res_general_apt20_8$resultado_total,
                         res_general_apt20_7$resultado_total,
                         res_general_apt20_6$resultado_total,
                         res_general_apt20_5$resultado_total,
                         res_general_apt20_4$resultado_total,
                         res_general_apt20_3$resultado_total,
                         res_general_apt20_2$resultado_total,
                         res_general_apt20_1$resultado_total,
                         res_general_apt19_12$resultado_total,
                         res_general_apt19_11$resultado_total,
                         res_general_apt19_10$resultado_total,
                         res_general_apt19_9$resultado_total,
                         res_general_apt19_8$resultado_total,
                         res_general_apt19_7$resultado_total,
                         res_general_apt19_6$resultado_total,
                         res_general_apt19_5$resultado_total,
                         res_general_apt19_4$resultado_total,
                         res_general_apt19_3$resultado_total,
                         res_general_apt19_2$resultado_total,
                         res_general_apt19_1$resultado_total)
resumen_emp_div<-rbind(res_general_apt21_8$resultado_div,
                         res_general_apt21_7$resultado_div,
                         res_general_apt21_6$resultado_div,
                         res_general_apt21_5$resultado_div,
                         res_general_apt21_4$resultado_div,
                         res_general_apt21_3$resultado_div,
                         res_general_apt21_2$resultado_div,
                         res_general_apt21_1$resultado_div,
                         res_general_apt20_12$resultado_div,
                         res_general_apt20_11$resultado_div,
                         res_general_apt20_10$resultado_div,
                         res_general_apt20_9$resultado_div,
                         res_general_apt20_8$resultado_div,
                         res_general_apt20_7$resultado_div,
                         res_general_apt20_6$resultado_div,
                         res_general_apt20_5$resultado_div,
                         res_general_apt20_4$resultado_div,
                         res_general_apt20_3$resultado_div,
                         res_general_apt20_2$resultado_div,
                         res_general_apt20_1$resultado_div,
                         res_general_apt19_12$resultado_div,
                         res_general_apt19_11$resultado_div,
                         res_general_apt19_10$resultado_div,
                         res_general_apt19_9$resultado_div,
                         res_general_apt19_8$resultado_div,
                         res_general_apt19_7$resultado_div,
                         res_general_apt19_6$resultado_div,
                         res_general_apt19_5$resultado_div,
                         res_general_apt19_4$resultado_div,
                         res_general_apt19_3$resultado_div,
                         res_general_apt19_2$resultado_div,
                         res_general_apt19_1$resultado_div)
resumen_emp_clase<-rbind(res_general_apt21_8$resultado_clase,
                       res_general_apt21_7$resultado_clase,
                       res_general_apt21_6$resultado_clase,
                       res_general_apt21_5$resultado_clase,
                       res_general_apt21_4$resultado_clase,
                       res_general_apt21_3$resultado_clase,
                       res_general_apt21_2$resultado_clase,
                       res_general_apt21_1$resultado_clase,
                       res_general_apt20_12$resultado_clase,
                       res_general_apt20_11$resultado_clase,
                       res_general_apt20_10$resultado_clase,
                       res_general_apt20_9$resultado_clase,
                       res_general_apt20_8$resultado_clase,
                       res_general_apt20_7$resultado_clase,
                       res_general_apt20_6$resultado_clase,
                       res_general_apt20_5$resultado_clase,
                       res_general_apt20_4$resultado_clase,
                       res_general_apt20_3$resultado_clase,
                       res_general_apt20_2$resultado_clase,
                       res_general_apt20_1$resultado_clase,
                       res_general_apt19_12$resultado_clase,
                       res_general_apt19_11$resultado_clase,
                       res_general_apt19_10$resultado_clase,
                       res_general_apt19_9$resultado_clase,
                       res_general_apt19_8$resultado_clase,
                       res_general_apt19_7$resultado_clase,
                       res_general_apt19_6$resultado_clase,
                       res_general_apt19_5$resultado_clase,
                       res_general_apt19_4$resultado_clase,
                       res_general_apt19_3$resultado_clase,
                       res_general_apt19_2$resultado_clase,
                       res_general_apt19_1$resultado_clase)

write.csv(resumen_general,"C:/Users/aemendoza/Documents/2021/Noviembre/PAEF/resumen_general.csv")
write.csv(resumen_division,"C:/Users/aemendoza/Documents/2021/Noviembre/PAEF/resumen_division.csv")
write.csv(resumen_clase,"C:/Users/aemendoza/Documents/2021/Noviembre/PAEF/resumen_clase.csv")
write.csv(resumen_t_demo,"C:/Users/aemendoza/Documents/2021/Noviembre/PAEF/resumen_t_demo.csv")
write.csv(resumen_emp_total,"C:/Users/aemendoza/Documents/2021/Noviembre/PAEF/resumen_general_empresas.csv")
write.csv(resumen_emp_div,"C:/Users/aemendoza/Documents/2021/Noviembre/PAEF/resumen_general_empresas_div.csv")
write.csv(resumen_emp_clase,"C:/Users/aemendoza/Documents/2021/Noviembre/PAEF/resumen_general_empresas_clase.csv")

####################################################################
###################piso de proteccion social########################################

setwd("D:/PisoMinimo_proteccionSocial/")
tabal_piso<-read.csv2("PISO_tipo.csv")
CIIU<-read.xlsx("D:/Aplicacion/tablero/DetalleCIIU.xlsx", sheetName = "Secciontotal")
tabal_piso$mes=substr(tabal_piso$periodo_resto,6,7)
tabal_piso$IBC_max<-apply(tabal_piso[,c(6,7,8,9)],1,max)
tabla_piso_sep<-tabal_piso[tabal_piso$mes == "09" ,-c(3,33)]
tabla_piso_ago<-tabal_piso[tabal_piso$mes == "08" ,-c(3,33)]
tabla_piso_jul<-tabal_piso[tabal_piso$mes == "07" ,-c(3,33)]
tabla_piso_jun<-tabal_piso[tabal_piso$mes == "06" ,-c(3,33)]
tabla_piso_may<-tabal_piso[tabal_piso$mes == "05" ,-c(3,33)]

Septiembre21<-read.csv2("Sep21_piso_minimo.csv")
Septiembre21$IBC_max<-apply(Septiembre21[,c(5,6,7,8)],1,max)
Septiembre21_min<-Septiembre21[Septiembre21$IBC_max < 908526,];nrow(Septiembre21_min)
Septiembre21_min<-rbind(Septiembre21_min,tabla_piso_sep);nrow(Septiembre21_min)
Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max %in% c(3,16,42,57,59),"tipologia"]<-rep(1,nrow(Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max %in% c(3,16,42,57,59),]))
Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max == 65,"tipologia"]<-rep(2,nrow(Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max == 65,]))
Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max == 66,"tipologia"]<-rep(3,nrow(Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max == 66,]))
Septiembre21_min[is.na(Septiembre21_min$tipologia),"tipologia"]<-rep(4,nrow(Septiembre21_min[is.na(Septiembre21_min$tipologia),]))
table(Septiembre21_min$tipologia)
Septiembre21_min_2<-Septiembre21_min[Septiembre21_min$ingreso_Max_Max == 0 & Septiembre21_min$retiro_Max_Max == 0 & Septiembre21_min$suspension_temporal_Max_Max == 0 & Septiembre21_min$vacaciones_Max_Max ==0,];nrow(Septiembre21_min_2)
table(Septiembre21_min_2$tipologia)

par(mfrow = c(1,3))
plot(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 1,"ibc_salud_Max_Max"]), main = "Distribución de personas IBC salud", bty = "n", xlab = "IBC salud", ylab = "% personas", xlim = c(0,909000),lty = 1,ylim = c(0,0.000006))
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 4,"ibc_salud_Max_Max"]), lty = 2)
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 2,"ibc_salud_Max_Max"]), lty = 3)
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 3,"ibc_salud_Max_Max"]), lty = 4)
legend("topleft",legend = c("Ind","Dep","Piso_dep","Piso_Ind"), lty = c(1,2,3,4),bty = "n", cex = 0.9)

plot(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 1,"ibc_pension_Max_Max"]), main = "Distribución de personas IBC pensión", bty = "n", xlab = "IBC pensión", ylab = "% personas", xlim = c(0,909000),lty = 1,ylim = c(0,0.000006))
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 4,"ibc_pension_Max_Max"]), lty = 2)
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 2,"ibc_pension_Max_Max"]), lty = 3)
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 3,"ibc_pension_Max_Max"]), lty = 4)
legend("topleft",legend = c("Ind","Dep","Piso_dep","Piso_Ind"), lty = c(1,2,3,4),bty = "n", cex = 0.9)

plot(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 1,"ibc_arl_Max_Max"]), main = "Distribución de personas IBC ARL", bty = "n", xlab = "IBC ARL", ylab = "% personas", xlim = c(0,909000),lty = 1,ylim = c(0,0.000006))
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 4,"ibc_arl_Max_Max"]), lty = 2)
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 2,"ibc_arl_Max_Max"]), lty = 3)
lines(density(Septiembre21_min_2[Septiembre21_min_2$tipologia == 3,"ibc_arl_Max_Max"]), lty = 4)
legend("topleft",legend = c("Ind","Dep","Piso_dep","Piso_Ind"), lty = c(1,2,3,4),bty = "n", cex = 0.9)

resumen_estadistico<-data.frame(Septiembre21_min_2%>%group_by(tipologia)%>%summarise(total_personas = n(),
                    mean_salud = mean(ibc_salud_Max_Max,na.rm = TRUE), median_salud = median(ibc_salud_Max_Max,na.rm = TRUE),sd_salud = sd(ibc_salud_Max_Max,na.rm = TRUE),
                    mean_pension = mean(ibc_pension_Max_Max,na.rm = TRUE), median_pension = median(ibc_pension_Max_Max,na.rm = TRUE),sd_pension = sd(ibc_pension_Max_Max,na.rm = TRUE),
                    mean_arl = mean(ibc_arl_Max_Max,na.rm = TRUE), median_arl = median(ibc_arl_Max_Max,na.rm = TRUE),sd_arl = sd(ibc_arl_Max_Max,na.rm = TRUE)))

write.xlsx(resumen_estadistico,"C:/Users/aemendoza/Documents/2021/Noviembre/PISO_ppt/salida_resumen.xlsx")

Septiembre21_min_2<-merge(Septiembre21_min_2,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x =TRUE)
resumen_estadistico2<-data.frame(Septiembre21_min_2%>%group_by(tipologia,Seccion)%>%summarise(total_personas = n(),
                                                                                     mean_salud = mean(ibc_salud_Max_Max,na.rm = TRUE), median_salud = median(ibc_salud_Max_Max,na.rm = TRUE),sd_salud = sd(ibc_salud_Max_Max,na.rm = TRUE),
                                                                                     mean_pension = mean(ibc_pension_Max_Max,na.rm = TRUE), median_pension = median(ibc_pension_Max_Max,na.rm = TRUE),sd_pension = sd(ibc_pension_Max_Max,na.rm = TRUE),
                                                                                     mean_arl = mean(ibc_arl_Max_Max,na.rm = TRUE), median_arl = median(ibc_arl_Max_Max,na.rm = TRUE),sd_arl = sd(ibc_arl_Max_Max,na.rm = TRUE)))

write.xlsx(resumen_estadistico2,"C:/Users/aemendoza/Documents/2021/Noviembre/PISO_ppt/salida_resumen2.xlsx")
lm(Septiembre21)

Septiembre21<-read.csv2("May21_piso_minimo.csv")
Septiembre21$IBC_max<-apply(Septiembre21[,c(5,6,7,8)],1,max)
Septiembre21_min<-Septiembre21[Septiembre21$IBC_max < 908526,];nrow(Septiembre21_min)
Septiembre21_min<-rbind(Septiembre21_min,tabla_piso_sep);nrow(Septiembre21_min)
Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max %in% c(3,16,42,57,59),"tipologia"]<-rep(1,nrow(Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max %in% c(3,16,42,57,59),]))
Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max == 65,"tipologia"]<-rep(2,nrow(Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max == 65,]))
Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max == 66,"tipologia"]<-rep(3,nrow(Septiembre21_min[Septiembre21_min$tipo_cotizante_Max_Max == 66,]))
Septiembre21_min[is.na(Septiembre21_min$tipologia),"tipologia"]<-rep(4,nrow(Septiembre21_min[is.na(Septiembre21_min$tipologia),]))
table(Septiembre21_min$tipologia)
Agosto21_min_2<-Septiembre21_min[Septiembre21_min$ingreso_Max_Max == 0 & Septiembre21_min$retiro_Max_Max == 0 & Septiembre21_min$suspension_temporal_Max_Max == 0 & Septiembre21_min$vacaciones_Max_Max ==0,];nrow(Septiembre21_min_2)
table(Agosto21_min_2$tipologia)
Julio21_min_2<-Septiembre21_min[Septiembre21_min$ingreso_Max_Max == 0 & Septiembre21_min$retiro_Max_Max == 0 & Septiembre21_min$suspension_temporal_Max_Max == 0 & Septiembre21_min$vacaciones_Max_Max ==0,];nrow(Septiembre21_min_2)
table(Julio21_min_2$tipologia)
Junio21_min_2<-Septiembre21_min[Septiembre21_min$ingreso_Max_Max == 0 & Septiembre21_min$retiro_Max_Max == 0 & Septiembre21_min$suspension_temporal_Max_Max == 0 & Septiembre21_min$vacaciones_Max_Max ==0,];nrow(Septiembre21_min_2)
table(Julio21_min_2$tipologia)
Mayo21_min_2<-Septiembre21_min[Septiembre21_min$ingreso_Max_Max == 0 & Septiembre21_min$retiro_Max_Max == 0 & Septiembre21_min$suspension_temporal_Max_Max == 0 & Septiembre21_min$vacaciones_Max_Max ==0,];nrow(Septiembre21_min_2)
table(Julio21_min_2$tipologia)

Agosto21_min_2_3<-Agosto21_min_2[,c(1,2,33)];colnames(Agosto21_min_2_3)<-c("tipo_identificacion_COT","numero_identificacion_COT","Tipo_Ago")
Julio21_min_2_3<-Julio21_min_2[,c(1,2,33)];colnames(Julio21_min_2_3)<-c("tipo_identificacion_COT","numero_identificacion_COT","Tipo_Jul")
Junio21_min_2_3<-Junio21_min_2[,c(1,2,33)];colnames(Junio21_min_2_3)<-c("tipo_identificacion_COT","numero_identificacion_COT","Tipo_Jun")
Mayo21_min_2_3<-Mayo21_min_2[,c(1,2,33)];colnames(Mayo21_min_2_3)<-c("tipo_identificacion_COT","numero_identificacion_COT","Tipo_May")

Septiembre21_min_2_2<-Septiembre21_min_2[Septiembre21_min_2$tipologia%in% c(1,4),]
Septiembre21_min_2_2<-merge(Septiembre21_min_2_2,Agosto21_min_2_3, by = c("tipo_identificacion_COT","numero_identificacion_COT"), all.x = TRUE)
Septiembre21_min_2_2<-merge(Septiembre21_min_2_2,Julio21_min_2_3, by = c("tipo_identificacion_COT","numero_identificacion_COT"), all.x = TRUE)
Septiembre21_min_2_2<-merge(Septiembre21_min_2_2,Junio21_min_2_3, by = c("tipo_identificacion_COT","numero_identificacion_COT"), all.x = TRUE)
Septiembre21_min_2_2<-merge(Septiembre21_min_2_2,Mayo21_min_2_3, by = c("tipo_identificacion_COT","numero_identificacion_COT"), all.x = TRUE)
Septiembre21_min_2_2[is.na(Septiembre21_min_2_2$Tipo_Ago), "Tipo_Ago1"]<-rep(0,nrow(Septiembre21_min_2_2[is.na(Septiembre21_min_2_2$Tipo_Ago),]))
Septiembre21_min_2_2[is.na(Septiembre21_min_2_2$Tipo_Jul), "Tipo_jul1"]<-rep(0,nrow(Septiembre21_min_2_2[is.na(Septiembre21_min_2_2$Tipo_Jul),]))
Septiembre21_min_2_2[is.na(Septiembre21_min_2_2$Tipo_Jun), "Tipo_Jun1"]<-rep(0,nrow(Septiembre21_min_2_2[is.na(Septiembre21_min_2_2$Tipo_Jun),]))
Septiembre21_min_2_2[is.na(Septiembre21_min_2_2$Tipo_May), "Tipo_May1"]<-rep(0,nrow(Septiembre21_min_2_2[is.na(Septiembre21_min_2_2$Tipo_May),]))

Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_Ago), "Tipo_Ago1"]<-rep(1,nrow(Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_Ago),]))
Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_Jul), "Tipo_jul1"]<-rep(1,nrow(Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_Jul),]))
Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_Jun), "Tipo_Jun1"]<-rep(1,nrow(Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_Jun),]))
Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_May), "Tipo_May1"]<-rep(1,nrow(Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_May),]))

Septiembre21_min_2_2$totalidad<-apply(Septiembre21_min_2_2[,c(39,40,41,42)], 1, sum)

Septiembre21_min_2_2recu<-Septiembre21_min_2_2[!is.na(Septiembre21_min_2_2$Tipo_Ago) & !is.na(Septiembre21_min_2_2$Tipo_Jul) & !is.na(Septiembre21_min_2_2$Tipo_Jun) & !is.na(Septiembre21_min_2_2$Tipo_May),];nrow(Septiembre21_min_2_2recu)

resumen_estadistico4<-data.frame(Septiembre21_min_2_2recu%>%group_by(tipologia,Seccion)%>%summarise(total_personas = n(),
                                                                                              mean_salud = mean(ibc_salud_Max_Max,na.rm = TRUE), median_salud = median(ibc_salud_Max_Max,na.rm = TRUE),sd_salud = sd(ibc_salud_Max_Max,na.rm = TRUE),
                                                                                              mean_pension = mean(ibc_pension_Max_Max,na.rm = TRUE), median_pension = median(ibc_pension_Max_Max,na.rm = TRUE),sd_pension = sd(ibc_pension_Max_Max,na.rm = TRUE),
                                                                                              mean_arl = mean(ibc_arl_Max_Max,na.rm = TRUE), median_arl = median(ibc_arl_Max_Max,na.rm = TRUE),sd_arl = sd(ibc_arl_Max_Max,na.rm = TRUE)))
write.xlsx(resumen_estadistico4,"C:/Users/aemendoza/Documents/2021/Noviembre/PISO_ppt/salida_resumen2_recur.xlsx")

resumen_estadistico2<-data.frame(Septiembre21_min_2%>%filter(tipologia =)%>%group_by(tipologia,Seccion)%>%summarise(total_personas = n(),
                                                                                              mean_salud = mean(ibc_salud_Max_Max,na.rm = TRUE), median_salud = median(ibc_salud_Max_Max,na.rm = TRUE),sd_salud = sd(ibc_salud_Max_Max,na.rm = TRUE),
                                                                                              mean_pension = mean(ibc_pension_Max_Max,na.rm = TRUE), median_pension = median(ibc_pension_Max_Max,na.rm = TRUE),sd_pension = sd(ibc_pension_Max_Max,na.rm = TRUE),
                                                                                              mean_arl = mean(ibc_arl_Max_Max,na.rm = TRUE), median_arl = median(ibc_arl_Max_Max,na.rm = TRUE),sd_arl = sd(ibc_arl_Max_Max,na.rm = TRUE)))
#########################################################################################
#########################################################################################
######geografico - geovisor##############################################################

setwd("D:/Nuevos_Insumos_IMC/empresas/")

#tabla_1 = "empresas_2021_12.csv";tabla_2 = "empresas_2021_11.csv";tabla_3 = "empresas_2021_10.csv";

#ultimo trimestre 
consolidado_APT<-function(tabla_1,tabla_2,tabla_3){
  base1_<-read.csv2(tabla_1, dec = ".");nrow(base1_);base1_$pos<-rep(1,nrow(base1_))
  base2_<-read.csv2(tabla_2, dec = ".");nrow(base2_)
  colnames(base2_)<-colnames(base2_)<-c("tipo_identificacion_APT","numero_identificacion_APT",paste0(colnames(base2_[,c(3:ncol(base2_))]),"pos2"));base2_$pos2<-rep(1,nrow(base2_))
  base3_<-read.csv2(tabla_3, dec = ".");nrow(base3_)
  colnames(base3_)<-colnames(base3_)<-c("tipo_identificacion_APT","numero_identificacion_APT",paste0(colnames(base3_[,c(3:ncol(base3_))]),"pos3"));base3_$pos3<-rep(1,nrow(base3_))

  consolidado<-merge(base1_,base2_, by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE);nrow(consolidado)
  consolidado[is.na(consolidado$pos),"ingreso_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"ingreso_Max_Sumpos2" ];consolidado[is.na(consolidado$pos),"ibc_salud_Max_Mean" ]<-consolidado[is.na(consolidado$pos),"ibc_salud_Max_Meanpos2" ];
  consolidado[is.na(consolidado$pos),"retiro_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"retiro_Max_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"suspension_temporal_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"suspension_temporal_Max_Sumpos2" ];consolidado[is.na(consolidado$pos),"vacaciones_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"vacaciones_Max_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"cot_obligatoria_salud_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"cot_obligatoria_salud_Max_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"codigo_ciudad_Max_Max" ]<-consolidado[is.na(consolidado$pos),"codigo_ciudad_Max_Maxpos2" ];consolidado[is.na(consolidado$pos),"codigo_depto_Max_Max" ]<-consolidado[is.na(consolidado$pos),"codigo_depto_Max_Maxpos2" ];
  consolidado[is.na(consolidado$pos),"codigo_actividad_economica_Max_Max" ]<-consolidado[is.na(consolidado$pos),"codigo_actividad_economica_Max_Maxpos2" ];
  consolidado[is.na(consolidado$pos),"Sexo_hombres_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"Sexo_hombres_Max_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"Sexo_mujeres_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"Sexo_mujeres_Max_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"jovenes_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"jovenes_Max_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"mayores_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"mayores_Max_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Sum" ]<-consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Mean" ]<-consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Meanpos2" ];
  consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Median" ]<-consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Medianpos2" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Sum" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Mean" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Meanpos2" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Median" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Medianpos2" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Sum" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Sumpos2" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Mean" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Meanpos2" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Median" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Medianpos2" ];
  consolidado[is.na(consolidado$pos),"Total_cotizantes" ]<-consolidado[is.na(consolidado$pos),"Total_cotizantespos2" ];consolidado[is.na(consolidado$pos),"FechaRenovacion" ]<-consolidado[is.na(consolidado$pos),"FechaRenovacionpos2" ];
  consolidado[is.na(consolidado$pos),"FechaCancelacion" ]<-consolidado[is.na(consolidado$pos),"FechaCancelacionpos2" ];consolidado[is.na(consolidado$pos),"FechaMatricula" ]<-consolidado[is.na(consolidado$pos),"FechaMatriculapos2" ];
  consolidado[is.na(consolidado$pos),"EstadoMatricula" ]<-consolidado[is.na(consolidado$pos),"EstadoMatriculapos2" ];consolidado[is.na(consolidado$pos),"Empleados" ]<-consolidado[is.na(consolidado$pos),"Empleadospos2" ];
  consolidado[is.na(consolidado$pos),"YEAR" ]<-consolidado[is.na(consolidado$pos),"YEARpos2" ];consolidado[is.na(consolidado$pos),"MONTH" ]<-consolidado[is.na(consolidado$pos),"MONTHpos2" ];
  consolidado<-consolidado[,1:33];consolidado$pos<-rep(1,nrow(consolidado))

  consolidado<-merge(consolidado,base3_, by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE);nrow(consolidado)
  consolidado[is.na(consolidado$pos),"ingreso_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"ingreso_Max_Sumpos3" ];consolidado[is.na(consolidado$pos),"ibc_salud_Max_Mean" ]<-consolidado[is.na(consolidado$pos),"ibc_salud_Max_Meanpos3" ];
  consolidado[is.na(consolidado$pos),"retiro_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"retiro_Max_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"suspension_temporal_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"suspension_temporal_Max_Sumpos3" ];consolidado[is.na(consolidado$pos),"vacaciones_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"vacaciones_Max_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"cot_obligatoria_salud_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"cot_obligatoria_salud_Max_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"codigo_ciudad_Max_Max" ]<-consolidado[is.na(consolidado$pos),"codigo_ciudad_Max_Maxpos3" ];consolidado[is.na(consolidado$pos),"codigo_depto_Max_Max" ]<-consolidado[is.na(consolidado$pos),"codigo_depto_Max_Maxpos3" ];
  consolidado[is.na(consolidado$pos),"codigo_actividad_economica_Max_Max" ]<-consolidado[is.na(consolidado$pos),"codigo_actividad_economica_Max_Maxpos3" ];
  consolidado[is.na(consolidado$pos),"Sexo_hombres_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"Sexo_hombres_Max_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"Sexo_mujeres_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"Sexo_mujeres_Max_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"jovenes_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"jovenes_Max_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"mayores_Max_Sum" ]<-consolidado[is.na(consolidado$pos),"mayores_Max_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Sum" ]<-consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Mean" ]<-consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Meanpos3" ];
  consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Median" ]<-consolidado[is.na(consolidado$pos),"IBC_REFERENCIA_Medianpos3" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Sum" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Mean" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Meanpos3" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Median" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_Medianpos3" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Sum" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Sumpos3" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Mean" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Meanpos3" ];
  consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Median" ]<-consolidado[is.na(consolidado$pos),"PAGOS_TOTALES_SP_Medianpos3" ];
  consolidado[is.na(consolidado$pos),"Total_cotizantes" ]<-consolidado[is.na(consolidado$pos),"Total_cotizantespos3" ];consolidado[is.na(consolidado$pos),"FechaRenovacion" ]<-consolidado[is.na(consolidado$pos),"FechaRenovacionpos3" ];
  consolidado[is.na(consolidado$pos),"FechaCancelacion" ]<-consolidado[is.na(consolidado$pos),"FechaCancelacionpos3" ];consolidado[is.na(consolidado$pos),"FechaMatricula" ]<-consolidado[is.na(consolidado$pos),"FechaMatriculapos3" ];
  consolidado[is.na(consolidado$pos),"EstadoMatricula" ]<-consolidado[is.na(consolidado$pos),"EstadoMatriculapos3" ];consolidado[is.na(consolidado$pos),"Empleados" ]<-consolidado[is.na(consolidado$pos),"Empleadospos3" ];
  consolidado[is.na(consolidado$pos),"YEAR" ]<-consolidado[is.na(consolidado$pos),"YEARpos3" ];consolidado[is.na(consolidado$pos),"MONTH" ]<-consolidado[is.na(consolidado$pos),"MONTHpos3" ];
  consolidado<-consolidado[,1:33]
  consolidado
}

###############################################
# seleccione el trimestre que esta trabajando #
###############################################
trimestre_analisis<-consolidado_APT("empresas_2021_12_new.csv","empresas_2021_11_new.csv","empresas_2021_10_new.csv")
trimestre_analisis_pre<-consolidado_APT("empresas_2021_9.csv","empresas_2021_8.csv","empresas_2021_7.csv")

Divipola_dpto<-read_excel("C:/Users/aemendoza/Documents/Información/Valida_ubicacion.xlsx", sheet = "departamento")
Divipola_mpio<-read_excel("C:/Users/aemendoza/Documents/Información/Valida_ubicacion.xlsx", sheet = "municipio")
CIIU<-read.xlsx("DetalleCIIU.xlsx", sheetName = "Secciontotal")

Estado_matricula<-function(base){
  base$Activa<-rep(0,nrow(base))
  base$Cancelada_inactiva<-rep(0,nrow(base))
  base[base$EstadoMatricula %in% c(1,7,8),"Activa"]<-rep(1,nrow(base[base$EstadoMatricula %in% c(1,7,8),]))
  base[base$EstadoMatricula %in% c(2,3,4,5,6,9,10,11,12,13,14,15,16,17),"Cancelada_inactiva"]<-rep(1,nrow(base[base$EstadoMatricula %in% c(2,3,4,5,6,9,10,11,12,13,14,15,16,17),]))
  base
}
tamaño_empresa<-function(base){
  base$Micro<-rep(0,nrow(base));
  base[base$Total_cotizantes <= 10,"Micro"]<-rep(1,nrow(base[base$Total_cotizantes <= 10,]))
  base$Pequeña<-rep(0,nrow(base));
  base[base$Total_cotizantes > 10 & base$Total_cotizantes <= 50,"Pequeña"]<-rep(1,nrow(base[base$Total_cotizantes > 10 & base$Total_cotizantes <= 50,]))
  base$Mediana<-rep(0,nrow(base));
  base[base$Total_cotizantes > 50 & base$Total_cotizantes <= 200,"Mediana"]<-rep(1,nrow(base[base$Total_cotizantes > 50 & base$Total_cotizantes <= 200,]))
  base$Grande<-rep(0,nrow(base));
  base[base$Total_cotizantes > 200 & base$Total_cotizantes <= 500,"Grande"]<-rep(1,nrow(base[base$Total_cotizantes > 200 & base$Total_cotizantes <= 500,]))
  base$Muy_grande<-rep(0,nrow(base));
  base[base$Total_cotizantes > 500,"Muy_grande"]<-rep(1,nrow(base[base$Total_cotizantes > 500,]))
  base
}
panel_fun<-function(tabla,periodo){
  informa<-read.csv2(tabla)#cambiar
  #informa<-read.csv2(tabla,sep = ',', dec = '.', na.strings = '$null$')#cambiar
  informa$divipola<-informa$codigo_depto_Max_Max*1000+informa$codigo_ciudad_Max_Max
  resinforma_dpto<-data.frame(informa%>%group_by(codigo_depto_Max_Max)%>%summarise(Total_apt = n(), Total_cot = sum(Total_cotizantes, na.rm = TRUE)))
  resinforma_mpio<-data.frame(informa%>%group_by(divipola)%>%summarise(Total_apt = n(), Total_cot = sum(Total_cotizantes, na.rm = TRUE)))
  colnames(resinforma_dpto)<-c("Dpto",paste0("apt",periodo), paste0("cot",periodo))
  colnames(resinforma_mpio)<-c("Mpio",paste0("apt",periodo), paste0("cot",periodo))
  resultado<-list(departamental = resinforma_dpto, municipal = resinforma_mpio)
  resultado
}
Actividad_economica<-function(base){
  #base<-merge(base,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
  base$Seccion_A<-rep(0,nrow(base));base[base$Seccion == "A","Seccion_A"]<-rep(1,nrow(base[base$Seccion == "A",]))
  base$Seccion_B<-rep(0,nrow(base));base[base$Seccion == "B","Seccion_B"]<-rep(1,nrow(base[base$Seccion == "B",]))   
  base$Seccion_C<-rep(0,nrow(base));base[base$Seccion == "C","Seccion_C"]<-rep(1,nrow(base[base$Seccion == "C",]))
  base$Seccion_D<-rep(0,nrow(base));base[base$Seccion == "D","Seccion_D"]<-rep(1,nrow(base[base$Seccion == "D",]))
  base$Seccion_E<-rep(0,nrow(base));base[base$Seccion == "E","Seccion_E"]<-rep(1,nrow(base[base$Seccion == "E",]))
  base$Seccion_F<-rep(0,nrow(base));base[base$Seccion == "F","Seccion_F"]<-rep(1,nrow(base[base$Seccion == "F",]))  
  base$Seccion_G<-rep(0,nrow(base));base[base$Seccion == "G","Seccion_G"]<-rep(1,nrow(base[base$Seccion == "G",])) 
  base$Seccion_H<-rep(0,nrow(base));base[base$Seccion == "H","Seccion_H"]<-rep(1,nrow(base[base$Seccion == "H",]))   
  base$Seccion_I<-rep(0,nrow(base));base[base$Seccion == "I","Seccion_I"]<-rep(1,nrow(base[base$Seccion == "I",]))   
  base$Seccion_J<-rep(0,nrow(base));base[base$Seccion == "J","Seccion_J"]<-rep(1,nrow(base[base$Seccion == "J",]))   
  base$Seccion_K<-rep(0,nrow(base));base[base$Seccion == "K","Seccion_K"]<-rep(1,nrow(base[base$Seccion == "K",]))   
  base$Seccion_L<-rep(0,nrow(base));base[base$Seccion == "L","Seccion_L"]<-rep(1,nrow(base[base$Seccion == "L",]))   
  base$Seccion_M<-rep(0,nrow(base));base[base$Seccion == "M","Seccion_M"]<-rep(1,nrow(base[base$Seccion == "M",]))   
  base$Seccion_N<-rep(0,nrow(base));base[base$Seccion == "N","Seccion_N"]<-rep(1,nrow(base[base$Seccion == "N",]))   
  base$Seccion_O<-rep(0,nrow(base));base[base$Seccion == "O","Seccion_O"]<-rep(1,nrow(base[base$Seccion == "O",]))   
  base$Seccion_P<-rep(0,nrow(base));base[base$Seccion == "P","Seccion_P"]<-rep(1,nrow(base[base$Seccion == "P",]))   
  base$Seccion_Q<-rep(0,nrow(base));base[base$Seccion == "Q","Seccion_Q"]<-rep(1,nrow(base[base$Seccion == "Q",]))   
  base$Seccion_R<-rep(0,nrow(base));base[base$Seccion == "R","Seccion_R"]<-rep(1,nrow(base[base$Seccion == "R",]))   
  base$Seccion_S<-rep(0,nrow(base));base[base$Seccion == "S","Seccion_S"]<-rep(1,nrow(base[base$Seccion == "S",]))   
  base$Seccion_T<-rep(0,nrow(base));base[base$Seccion == "T","Seccion_T"]<-rep(1,nrow(base[base$Seccion == "T",]))   
  base$Seccion_U<-rep(0,nrow(base));base[base$Seccion == "U","Seccion_U"]<-rep(1,nrow(base[base$Seccion == "U",]))
  base
}

##################################################
#  Modulo de incio, resultados generales         #
##################################################

trimestre_analisis<-merge(trimestre_analisis,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
trimestre_analisis[is.na(trimestre_analisis$Seccion),"Seccion"]<-rep("Otra",nrow(trimestre_analisis[is.na(trimestre_analisis$Seccion),]))
trimestre_analisis<-Estado_matricula(trimestre_analisis)
trimestre_analisis<-tamaño_empresa(trimestre_analisis)
trimestre_analisis<-Actividad_economica(trimestre_analisis)
trimestre_analisis<-trimestre_analisis[,c("tipo_identificacion_APT","numero_identificacion_APT","retiro_Max_Sum","ingreso_Max_Sum",
                              "codigo_ciudad_Max_Max","codigo_depto_Max_Max","codigo_actividad_economica_Max_Max",
                              "Total_cotizantes","EstadoMatricula","Activa","Cancelada_inactiva","Micro","Pequeña","Mediana","Grande","Muy_grande",
                              "IBC_REFERENCIA_Mean","IBC_REFERENCIA_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Sum",
                              "Seccion","Seccion_A","Seccion_B","Seccion_C","Seccion_D","Seccion_E","Seccion_F","Seccion_G","Seccion_H","Seccion_I","Seccion_J","Seccion_K","Seccion_L","Seccion_M","Seccion_N","Seccion_O","Seccion_P","Seccion_Q","Seccion_R","Seccion_S","Seccion_T","Seccion_U",
                              "Sexo_hombres_Max_Sum","Sexo_mujeres_Max_Sum","jovenes_Max_Sum","mayores_Max_Sum","cot_obligatoria_salud_Max_Sum")]
trimestre_analisis<-merge(trimestre_analisis,Divipola_dpto,by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
trimestre_analisis$divipola<-trimestre_analisis$codigo_depto_Max_Max*1000+trimestre_analisis$codigo_ciudad_Max_Max
trimestre_analisis<-merge(trimestre_analisis,Divipola_mpio,by.x = "divipola", by.y = "Municipio", all.x = TRUE)
trimestre_analisis$Dpto1<-trimestre_analisis$codigo_depto_Max_Max
trimestre_analisis$Divipola1<-trimestre_analisis$divipola
trimestre_analisis[is.na(trimestre_analisis$validacion_dpto),"Dpto1"]<-rep(0,nrow(trimestre_analisis[is.na(trimestre_analisis$validacion_dpto),]))
trimestre_analisis[is.na(trimestre_analisis$valida),"Divipola1"]<-rep(0,nrow(trimestre_analisis[is.na(trimestre_analisis$valida),]))
trimestre_analisis$pos = rep(1,nrow(trimestre_analisis))

trimestre_analisis_pre<-merge(trimestre_analisis_pre,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
trimestre_analisis_pre[is.na(trimestre_analisis_pre$Seccion),"Seccion"]<-rep("Otra",nrow(trimestre_analisis_pre[is.na(trimestre_analisis_pre$Seccion),]))
trimestre_analisis_pre<-Estado_matricula(trimestre_analisis_pre)
trimestre_analisis_pre<-tamaño_empresa(trimestre_analisis_pre)
trimestre_analisis_pre<-Actividad_economica(trimestre_analisis_pre)
trimestre_analisis_pre<-trimestre_analisis_pre[,c("tipo_identificacion_APT","numero_identificacion_APT","retiro_Max_Sum","ingreso_Max_Sum",
                                  "codigo_ciudad_Max_Max","codigo_depto_Max_Max","codigo_actividad_economica_Max_Max",
                                  "Total_cotizantes","EstadoMatricula","Activa","Cancelada_inactiva","Micro","Pequeña","Mediana","Grande","Muy_grande",
                                  "IBC_REFERENCIA_Mean","IBC_REFERENCIA_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Sum",
                                  "Seccion","Seccion_A","Seccion_B","Seccion_C","Seccion_D","Seccion_E","Seccion_F","Seccion_G","Seccion_H","Seccion_I","Seccion_J","Seccion_K","Seccion_L","Seccion_M","Seccion_N","Seccion_O","Seccion_P","Seccion_Q","Seccion_R","Seccion_S","Seccion_T","Seccion_U",
                                  "Sexo_hombres_Max_Sum","Sexo_mujeres_Max_Sum","jovenes_Max_Sum","mayores_Max_Sum","cot_obligatoria_salud_Max_Sum")]
trimestre_analisis_pre<-merge(trimestre_analisis_pre,Divipola_dpto,by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
trimestre_analisis_pre$divipola<-trimestre_analisis_pre$codigo_depto_Max_Max*1000+trimestre_analisis_pre$codigo_ciudad_Max_Max
trimestre_analisis_pre<-merge(trimestre_analisis_pre,Divipola_mpio,by.x = "divipola", by.y = "Municipio", all.x = TRUE)
trimestre_analisis_pre$Dpto1<-trimestre_analisis_pre$codigo_depto_Max_Max
trimestre_analisis_pre$Divipola1<-trimestre_analisis_pre$divipola
trimestre_analisis_pre[is.na(trimestre_analisis_pre$validacion_dpto),"Dpto1"]<-rep(0,nrow(trimestre_analisis_pre[is.na(trimestre_analisis_pre$validacion_dpto),]))
trimestre_analisis_pre[is.na(trimestre_analisis_pre$valida),"Divipola1"]<-rep(0,nrow(trimestre_analisis_pre[is.na(trimestre_analisis_pre$valida),]))
colnames(trimestre_analisis_pre)<-c("divipola2","codigo_depto_Max_Max2","tipo_identificacion_APT","numero_identificacion_APT", paste0(colnames(trimestre_analisis_pre[,5:ncol(trimestre_analisis_pre)]),"2"))
trimestre_analisis_pre$pre = rep(1,nrow(trimestre_analisis_pre))

panel_trimestre<-merge(trimestre_analisis,trimestre_analisis_pre, by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE)

Resumen_Muni_tr<-data.frame(panel_trimestre%>%group_by(Divipola1)%>%
  summarise(Total_apt = sum(pos,na.rm = TRUE), 
                Var_apt_trim = (sum(pos, na.rm = TRUE)-sum(pre, na.rm = TRUE))/sum(pre, na.rm = TRUE),
            Total_cot = sum(Total_cotizantes, na.rm = TRUE),
                Cot_medio_por_empresa = sum(Total_cotizantes, na.rm = TRUE)/sum(pos,na.rm = TRUE),
                Var_cot_trim = (sum(Total_cotizantes, na.rm = TRUE)-sum(Total_cotizantes2, na.rm = TRUE))/sum(Total_cotizantes2, na.rm = TRUE),
                Participacion_cot_retiro = sum(retiro_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),Participacion_cot_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),
            Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
            Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE),
            IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),
            IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),#IBC_max = max(IBC_REFERENCIA_Mean,na.rm = TRUE), IBC_min = min(IBC_REFERENCIA_Mean,na.rm = TRUE),
            Total_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Max_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Min_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),
            Total_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Max_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Min_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),
            Relacion_sexos_pos = sum(Sexo_hombres_Max_Sum, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum, na.rm = TRUE) * 100, total_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE), participacion_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE) * 100,
            Relacion_sexos_pre = sum(Sexo_hombres_Max_Sum2, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum2, na.rm = TRUE) * 100, total_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE),participacion_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE)/sum(Total_cotizantes2, na.rm = TRUE) * 100))
Resumen_Dpto_tr<-data.frame(panel_trimestre%>%group_by(Dpto1)%>%
  summarise(Total_apt = sum(pos,na.rm = TRUE), 
            Var_apt_trim = (sum(pos, na.rm = TRUE)-sum(pre, na.rm = TRUE))/sum(pre, na.rm = TRUE),
            Total_cot = sum(Total_cotizantes, na.rm = TRUE),
            Cot_medio_por_empresa = sum(Total_cotizantes, na.rm = TRUE)/sum(pos,na.rm = TRUE),
            Var_cot_trim = (sum(Total_cotizantes, na.rm = TRUE)-sum(Total_cotizantes2, na.rm = TRUE))/sum(Total_cotizantes2, na.rm = TRUE),
            Participacion_cot_retiro = sum(retiro_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),Participacion_cot_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),
            Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
            Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE),
            IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),
            IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),#IBC_max = max(IBC_REFERENCIA_Mean,na.rm = TRUE), IBC_min = min(IBC_REFERENCIA_Mean,na.rm = TRUE),
            Total_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Max_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Min_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),
            Total_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Max_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Min_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),
            Relacion_sexos_pos = sum(Sexo_hombres_Max_Sum, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum, na.rm = TRUE) * 100, total_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE), participacion_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE) * 100,
            Relacion_sexos_pre = sum(Sexo_hombres_Max_Sum2, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum2, na.rm = TRUE) * 100, total_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE),participacion_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE)/sum(Total_cotizantes2, na.rm = TRUE) * 100))

#salen
Total_info_pre_salida<-data.frame(panel_trimestre%>%group_by(Divipola12)%>%summarise(Total_apt_pre = sum(pre,na.rm = TRUE), Total_cot_pre = sum(Total_cotizantes2, na.rm = TRUE)))
Total_info_salen_salida<-data.frame(panel_trimestre%>%filter(is.na(panel_trimestre$pos) & panel_trimestre$pre == 1)%>%group_by(Divipola12)%>%summarise(Total_apt_salen = sum(pre,na.rm = TRUE), Total_cot_salen = sum(Total_cotizantes2, na.rm = TRUE)))
Total_info_salen_salida<-merge(Total_info_salen_salida,Total_info_pre_salida, by = "Divipola12", all  = TRUE)
Total_info_salen_salida$por_salen_apt<-Total_info_salen_salida$Total_apt_salen/Total_info_salen_salida$Total_apt_pre
Total_info_salen_salida$por_salen_cot<-Total_info_salen_salida$Total_cot_salen/Total_info_salen_salida$Total_cot_pre
panel_trimestre_salen<-panel_trimestre[(is.na(panel_trimestre$pos) & panel_trimestre$pre == 1),c(1,2,56:108)];nrow(panel_trimestre_salen)
panel_trimestre_salen$Retira_todo<-panel_trimestre_salen$Total_cotizantes2-panel_trimestre_salen$retiro_Max_Sum2
panel_trimestre_salen_tot<-panel_trimestre_salen[(panel_trimestre_salen$Retira_todo == 0 | (panel_trimestre_salen$Retira_todo != 0 & panel_trimestre_salen$Cancelada_inactiva2 == 1)),];nrow(panel_trimestre_salen_tot)
panel_trimestre_salen_mora<-panel_trimestre_salen[(panel_trimestre_salen$Retira_todo != 0 & panel_trimestre_salen$Activa2 == 1),];nrow(panel_trimestre_salen_mora)
res_panel_trimestre_salen_tot<-data.frame(panel_trimestre_salen_tot%>%group_by(Divipola12)%>%summarise(Total_apt_salen_sistema = n(),Total_cot_salen_sistema = sum(Total_cotizantes2, na.rm = TRUE), 
        Total_apt_Micro = sum(Micro2, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña2, na.rm = TRUE),Total_apt_Mediana = sum(Mediana2, na.rm = TRUE),Total_apt_Grande = sum(Grande2, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande2, na.rm = TRUE),
        Total_apt_sec_A = sum(Seccion_A2, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B2, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C2, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D2, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E2, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F2, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G2, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H2, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I2, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J2, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K2, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L2, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M2, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N2, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O2, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P2, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q2, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R2, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S2, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T2, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U2, na.rm = TRUE)))
res_panel_trimestre_salen_mora<-data.frame(panel_trimestre_salen_mora%>%group_by(Divipola12)%>%summarise(Total_apt_salen_mora = n(),Total_cot_salen_mora = sum(Total_cotizantes2, na.rm = TRUE)))
Total_info_salen_salida_muni<-merge(Total_info_salen_salida,res_panel_trimestre_salen_tot, by = "Divipola12", all = TRUE)
Total_info_salen_salida_muni<-merge(Total_info_salen_salida_muni,res_panel_trimestre_salen_mora, by = "Divipola12", all = TRUE)
Total_info_salen_salida_muni$Por_salen_apt_sistema<-Total_info_salen_salida_muni$Total_apt_salen_sistema/Total_info_salen_salida_muni$Total_apt_salen
Total_info_salen_salida_muni$Por_salen_cot_sistema<-Total_info_salen_salida_muni$Total_cot_salen_sistema/Total_info_salen_salida_muni$Total_cot_salen
Total_info_salen_salida_muni$Por_salen_apt_mora<-Total_info_salen_salida_muni$Total_apt_salen_mora/Total_info_salen_salida_muni$Total_apt_salen
Total_info_salen_salida_muni<-Total_info_salen_salida_muni[,c("Divipola12","Total_apt_salen","por_salen_apt","Total_apt_salen_sistema","Por_salen_apt_sistema","Total_apt_salen_mora","Por_salen_apt_mora","Total_cot_salen","por_salen_cot","Total_cot_salen_sistema","Por_salen_cot_sistema",
      "Total_apt_Micro","Total_apt_Pequena","Total_apt_Mediana","Total_apt_Grande","Total_apt_Muy_grande",
      "Total_apt_sec_A","Total_apt_sec_B","Total_apt_sec_C","Total_apt_sec_D","Total_apt_sec_E","Total_apt_sec_F","Total_apt_sec_G" , "Total_apt_sec_H","Total_apt_sec_I","Total_apt_sec_J","Total_apt_sec_K", "Total_apt_sec_L", "Total_apt_sec_M" ,"Total_apt_sec_N","Total_apt_sec_O" , "Total_apt_sec_P", "Total_apt_sec_Q","Total_apt_sec_R","Total_apt_sec_S","Total_apt_sec_T","Total_apt_sec_U")]

Total_info_pre_salida<-data.frame(panel_trimestre%>%group_by(Dpto12)%>%summarise(Total_apt_pre = sum(pre,na.rm = TRUE), Total_cot_pre = sum(Total_cotizantes2, na.rm = TRUE)))
Total_info_salen_salida<-data.frame(panel_trimestre%>%filter(is.na(panel_trimestre$pos) & panel_trimestre$pre == 1)%>%group_by(Dpto12)%>%summarise(Total_apt_salen = sum(pre,na.rm = TRUE), Total_cot_salen = sum(Total_cotizantes2, na.rm = TRUE)))
Total_info_salen_salida<-merge(Total_info_salen_salida,Total_info_pre_salida, by = "Dpto12", all  = TRUE)
Total_info_salen_salida$por_salen_apt<-Total_info_salen_salida$Total_apt_salen/Total_info_salen_salida$Total_apt_pre
Total_info_salen_salida$por_salen_cot<-Total_info_salen_salida$Total_cot_salen/Total_info_salen_salida$Total_cot_pre
panel_trimestre_salen<-panel_trimestre[(is.na(panel_trimestre$pos) & panel_trimestre$pre == 1),c(1,2,56:108)];nrow(panel_trimestre_salen)
panel_trimestre_salen$Retira_todo<-panel_trimestre_salen$Total_cotizantes2-panel_trimestre_salen$retiro_Max_Sum2
panel_trimestre_salen_tot<-panel_trimestre_salen[(panel_trimestre_salen$Retira_todo == 0 | (panel_trimestre_salen$Retira_todo != 0 & panel_trimestre_salen$Cancelada_inactiva2 == 1)),];nrow(panel_trimestre_salen_tot)
panel_trimestre_salen_mora<-panel_trimestre_salen[(panel_trimestre_salen$Retira_todo != 0 & panel_trimestre_salen$Activa2 == 1),];nrow(panel_trimestre_salen_mora)
res_panel_trimestre_salen_tot<-data.frame(panel_trimestre_salen_tot%>%group_by(Dpto12)%>%summarise(Total_apt_salen_sistema = n(),Total_cot_salen_sistema = sum(Total_cotizantes2, na.rm = TRUE), 
                                                                                                       Total_apt_Micro = sum(Micro2, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña2, na.rm = TRUE),Total_apt_Mediana = sum(Mediana2, na.rm = TRUE),Total_apt_Grande = sum(Grande2, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande2, na.rm = TRUE),
                                                                                                       Total_apt_sec_A = sum(Seccion_A2, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B2, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C2, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D2, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E2, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F2, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G2, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H2, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I2, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J2, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K2, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L2, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M2, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N2, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O2, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P2, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q2, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R2, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S2, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T2, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U2, na.rm = TRUE)))
res_panel_trimestre_salen_mora<-data.frame(panel_trimestre_salen_mora%>%group_by(Dpto12)%>%summarise(Total_apt_salen_mora = n(),Total_cot_salen_mora = sum(Total_cotizantes2, na.rm = TRUE)))
Total_info_salen_salida_dpto<-merge(Total_info_salen_salida,res_panel_trimestre_salen_tot, by = "Dpto12", all = TRUE)
Total_info_salen_salida_dpto<-merge(Total_info_salen_salida_dpto,res_panel_trimestre_salen_mora, by = "Dpto12", all = TRUE)
Total_info_salen_salida_dpto$Por_salen_apt_sistema<-Total_info_salen_salida_dpto$Total_apt_salen_sistema/Total_info_salen_salida_dpto$Total_apt_salen
Total_info_salen_salida_dpto$Por_salen_cot_sistema<-Total_info_salen_salida_dpto$Total_cot_salen_sistema/Total_info_salen_salida_dpto$Total_cot_salen
Total_info_salen_salida_dpto$Por_salen_apt_mora<-Total_info_salen_salida_dpto$Total_apt_salen_mora/Total_info_salen_salida_dpto$Total_apt_salen
Total_info_salen_salida_dpto<-Total_info_salen_salida_dpto[,c("Dpto12","Total_apt_salen","por_salen_apt","Total_apt_salen_sistema","Por_salen_apt_sistema","Total_apt_salen_mora","Por_salen_apt_mora","Total_cot_salen","por_salen_cot","Total_cot_salen_sistema","Por_salen_cot_sistema",
                                                              "Total_apt_Micro","Total_apt_Pequena","Total_apt_Mediana","Total_apt_Grande","Total_apt_Muy_grande",
                                                              "Total_apt_sec_A","Total_apt_sec_B","Total_apt_sec_C","Total_apt_sec_D","Total_apt_sec_E","Total_apt_sec_F","Total_apt_sec_G" , "Total_apt_sec_H","Total_apt_sec_I","Total_apt_sec_J","Total_apt_sec_K", "Total_apt_sec_L", "Total_apt_sec_M" ,"Total_apt_sec_N","Total_apt_sec_O" , "Total_apt_sec_P", "Total_apt_sec_Q","Total_apt_sec_R","Total_apt_sec_S","Total_apt_sec_T","Total_apt_sec_U")]

write.xlsx(Total_info_salen_salida_muni, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_salen_2021_IV.xlsx",sheetName = "Municipal",showNA = FALSE)
write.xlsx(Total_info_salen_salida_dpto, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_salen_2021_IV.xlsx",sheetName = "Departamental",append = TRUE, showNA = FALSE)

#Entran

Total_info_entran<-data.frame(panel_trimestre%>%group_by(Divipola1)%>%summarise(Total_apt_pos = sum(pos,na.rm = TRUE), Total_cot_pos = sum(Total_cotizantes, na.rm = TRUE)))
Total_info_entran_in<-data.frame(panel_trimestre%>%filter(is.na(panel_trimestre$pre) & panel_trimestre$pos == 1)%>%group_by(Divipola1)%>%summarise(Total_apt_entran = sum(pos,na.rm = TRUE), Total_cot_entran = sum(Total_cotizantes, na.rm = TRUE),
                  Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
                  Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE)))
Total_info_entran_muni<-merge(Total_info_entran_in,Total_info_entran, by = "Divipola1", all  = TRUE)
Total_info_entran_muni$por_entran_apt<-Total_info_entran_muni$Total_apt_entran/Total_info_entran_muni$Total_apt_pos
Total_info_entran_muni$por_entran_cot<-Total_info_entran_muni$Total_cot_entran/Total_info_entran_muni$Total_cot_pos

Total_info_entran<-data.frame(panel_trimestre%>%group_by(Dpto1)%>%summarise(Total_apt_pos = sum(pos,na.rm = TRUE), Total_cot_pos = sum(Total_cotizantes, na.rm = TRUE)))
Total_info_entran_in<-data.frame(panel_trimestre%>%filter(is.na(panel_trimestre$pre) & panel_trimestre$pos == 1)%>%group_by(Dpto1)%>%summarise(Total_apt_entran = sum(pos,na.rm = TRUE), Total_cot_entran = sum(Total_cotizantes, na.rm = TRUE),
                  Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
                  Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE)))
Total_info_entran_dpto<-merge(Total_info_entran_in,Total_info_entran, by = "Dpto1", all  = TRUE)
Total_info_entran_dpto$por_entran_apt<-Total_info_entran_dpto$Total_apt_entran/Total_info_entran_dpto$Total_apt_pos
Total_info_entran_dpto$por_entran_cot<-Total_info_entran_dpto$Total_cot_entran/Total_info_entran_dpto$Total_cot_pos

write.xlsx(Total_info_entran_muni, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_entran_2021_IV.xlsx",sheetName = "Municipal",showNA = FALSE)
write.xlsx(Total_info_entran_dpto, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_entran_2021_IV.xlsx",sheetName = "Departamental",append = TRUE, showNA = FALSE)

#permanecen

Total_info_permanencen<-data.frame(panel_trimestre%>%group_by(Divipola1)%>%summarise(Total_apt_pos = sum(pos,na.rm = TRUE)))
Total_info_permanecen_1_muni<-data.frame(panel_trimestre%>%filter(panel_trimestre$pre == 1 & panel_trimestre$pos == 1)%>%group_by(Divipola1)%>%summarise(Total_apt_permanecen = sum(pos,na.rm = TRUE), 
                    Total_cot_per_pre = sum(Total_cotizantes, na.rm = TRUE),Total_cot_per_pos = sum(Total_cotizantes2,na.rm = TRUE),
                    Var_cot_permanecen = (sum(Total_cotizantes, na.rm = TRUE)-sum(Total_cotizantes2,na.rm = TRUE))/sum(Total_cotizantes2,na.rm = TRUE),
                    Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
                    Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE)))
Total_info_permanecen_1_muni<-merge(Total_info_permanencen,Total_info_permanecen_1_muni, by = "Divipola1", all  = TRUE)
Total_info_permanecen_1_muni$por_permanecen_apt<-Total_info_permanecen_1_muni$Total_apt_permanecen/Total_info_permanecen_1_muni$Total_apt_pos

Total_info_permanencen<-data.frame(panel_trimestre%>%group_by(Dpto1)%>%summarise(Total_apt_pos = sum(pos,na.rm = TRUE)))
Total_info_permanecen_1_dpto<-data.frame(panel_trimestre%>%filter(panel_trimestre$pre == 1 & panel_trimestre$pos == 1)%>%group_by(Dpto1)%>%summarise(Total_apt_permanecen = sum(pos,na.rm = TRUE), 
                              Total_cot_per_pre = sum(Total_cotizantes, na.rm = TRUE),Total_cot_per_pos = sum(Total_cotizantes2,na.rm = TRUE),
                              Var_cot_permanecen = (sum(Total_cotizantes, na.rm = TRUE)-sum(Total_cotizantes2,na.rm = TRUE))/sum(Total_cotizantes2,na.rm = TRUE),
                              Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
                              Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE)))
Total_info_permanecen_1_dpto<-merge(Total_info_permanencen,Total_info_permanecen_1_dpto, by = "Dpto1", all  = TRUE)
Total_info_permanecen_1_dpto$por_permanecen_apt<-Total_info_permanecen_1_dpto$Total_apt_permanecen/Total_info_permanecen_1_dpto$Total_apt_pos

write.xlsx(Total_info_permanecen_1_muni, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_permanecen_2021_IV.xlsx",sheetName = "Municipal",showNA = FALSE)
write.xlsx(Total_info_permanecen_1_dpto, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_permanecen_2021_IV.xlsx",sheetName = "Departamental",append = TRUE, showNA = FALSE)

#cambial por trimestre movil
trimestre_analisis_anual20<-consolidado_APT("empresas_2020_12.csv","empresas_2020_11.csv","empresas_2020_10.csv")
trimestre_analisis_anual20_pre<-consolidado_APT("empresas_2020_9.csv","empresas_2020_8.csv","empresas_2020_7.csv")
trimestre_analisis_anual19<-consolidado_APT("empresas_2019_12.csv","empresas_2019_11.csv","empresas_2019_10.csv")
trimestre_analisis_anual19_pre<-consolidado_APT("empresas_2019_9.csv","empresas_2019_8.csv","empresas_2019_7.csv")

valores<-c(0.9753,0.9822,0.9845,0.9891,0.9916,0.9931,0.9918,0.993,0.9947,0.9959,0.997,1,
           1.006,1.0118,1.0162,1.0212,1.0244,1.0271,1.0294,1.0303,1.0326,1.0343,1.0354,1.038,
           1.0424,1.0494,1.0553,1.057,1.0536,1.0497,1.0497,1.0496,1.0529,1.0523,1.0508,1.0548,
           1.0591,1.0658,1.0712,1.0776,1.0884,1.0878,1.0914,1.0962,1.1004,1.1006,1.106,1.1141)
años<-c(rep(c(2018,2019,2020,2021),c(12,12,12,12)))#cambiar
meses<-c(rep(seq(1,12),4))#cambiar
info_temporal <- data.frame(años,meses,valores)

#cambiar para deflactar los valores
year_periodo_analisis<-2021;month_periodo_analisis<-12;

trimestre_analisis_anual20$IBC_REFERENCIA_Mean<-trimestre_analisis_anual20$IBC_REFERENCIA_Mean*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20$IBC_REFERENCIA_Median<-trimestre_analisis_anual20$IBC_REFERENCIA_Median*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20$PAGOS_TOTALES_Mean<-trimestre_analisis_anual20$PAGOS_TOTALES_Mean*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20$PAGOS_TOTALES_Median<-trimestre_analisis_anual20$PAGOS_TOTALES_Median*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20$PAGOS_TOTALES_Sum<-trimestre_analisis_anual20$PAGOS_TOTALES_Sum*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20_pre$IBC_REFERENCIA_Mean<-trimestre_analisis_anual20_pre$IBC_REFERENCIA_Mean*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20_pre$IBC_REFERENCIA_Median<-trimestre_analisis_anual20_pre$IBC_REFERENCIA_Median*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20_pre$PAGOS_TOTALES_Mean<-trimestre_analisis_anual20_pre$PAGOS_TOTALES_Mean*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20_pre$PAGOS_TOTALES_Median<-trimestre_analisis_anual20_pre$PAGOS_TOTALES_Median*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual20_pre$PAGOS_TOTALES_Sum<-trimestre_analisis_anual20_pre$PAGOS_TOTALES_Sum*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-1) & info_temporal$meses == month_periodo_analisis, "valores"])

trimestre_analisis_anual19$IBC_REFERENCIA_Mean<-trimestre_analisis_anual19$IBC_REFERENCIA_Mean*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19$IBC_REFERENCIA_Median<-trimestre_analisis_anual19$IBC_REFERENCIA_Median*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19$PAGOS_TOTALES_Mean<-trimestre_analisis_anual19$PAGOS_TOTALES_Mean*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19$PAGOS_TOTALES_Median<-trimestre_analisis_anual19$PAGOS_TOTALES_Median*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19$PAGOS_TOTALES_Sum<-trimestre_analisis_anual19$PAGOS_TOTALES_Sum*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19_pre$IBC_REFERENCIA_Mean<-trimestre_analisis_anual19_pre$IBC_REFERENCIA_Mean*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19_pre$IBC_REFERENCIA_Median<-trimestre_analisis_anual19_pre$IBC_REFERENCIA_Median*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19_pre$PAGOS_TOTALES_Mean<-trimestre_analisis_anual19_pre$PAGOS_TOTALES_Mean*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19_pre$PAGOS_TOTALES_Median<-trimestre_analisis_anual19_pre$PAGOS_TOTALES_Median*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])
trimestre_analisis_anual19_pre$PAGOS_TOTALES_Sum<-trimestre_analisis_anual19_pre$PAGOS_TOTALES_Sum*(info_temporal[info_temporal$años == year_periodo_analisis & info_temporal$meses == month_periodo_analisis, "valores"]/info_temporal[info_temporal$años == (year_periodo_analisis-2) & info_temporal$meses == month_periodo_analisis, "valores"])

trimestre_analisis_anual20<-merge(trimestre_analisis_anual20,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
trimestre_analisis_anual20[is.na(trimestre_analisis_anual20$Seccion),"Seccion"]<-rep("Otra",nrow(trimestre_analisis_anual20[is.na(trimestre_analisis_anual20$Seccion),]))
trimestre_analisis_anual20<-Estado_matricula(trimestre_analisis_anual20)
trimestre_analisis_anual20<-tamaño_empresa(trimestre_analisis_anual20)
trimestre_analisis_anual20<-Actividad_economica(trimestre_analisis_anual20)
trimestre_analisis_anual20<-trimestre_analisis_anual20[,c("tipo_identificacion_APT","numero_identificacion_APT","retiro_Max_Sum","ingreso_Max_Sum",
                                                          "codigo_ciudad_Max_Max","codigo_depto_Max_Max","codigo_actividad_economica_Max_Max",
                                                          "Total_cotizantes","EstadoMatricula","Activa","Cancelada_inactiva","Micro","Pequeña","Mediana","Grande","Muy_grande",
                                                          "IBC_REFERENCIA_Mean","IBC_REFERENCIA_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Sum",
                                                          "Seccion","Seccion_A","Seccion_B","Seccion_C","Seccion_D","Seccion_E","Seccion_F","Seccion_G","Seccion_H","Seccion_I","Seccion_J","Seccion_K","Seccion_L","Seccion_M","Seccion_N","Seccion_O","Seccion_P","Seccion_Q","Seccion_R","Seccion_S","Seccion_T","Seccion_U",
                                                          "Sexo_hombres_Max_Sum","Sexo_mujeres_Max_Sum","jovenes_Max_Sum","mayores_Max_Sum","cot_obligatoria_salud_Max_Sum")]
trimestre_analisis_anual20<-merge(trimestre_analisis_anual20,Divipola_dpto,by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
trimestre_analisis_anual20$divipola<-trimestre_analisis_anual20$codigo_depto_Max_Max*1000+trimestre_analisis_anual20$codigo_ciudad_Max_Max
trimestre_analisis_anual20<-merge(trimestre_analisis_anual20,Divipola_mpio,by.x = "divipola", by.y = "Municipio", all.x = TRUE)
trimestre_analisis_anual20$Dpto1<-trimestre_analisis_anual20$codigo_depto_Max_Max
trimestre_analisis_anual20$Divipola1<-trimestre_analisis_anual20$divipola
trimestre_analisis_anual20[is.na(trimestre_analisis_anual20$validacion_dpto),"Dpto1"]<-rep(0,nrow(trimestre_analisis_anual20[is.na(trimestre_analisis_anual20$validacion_dpto),]))
trimestre_analisis_anual20[is.na(trimestre_analisis_anual20$valida),"Divipola1"]<-rep(0,nrow(trimestre_analisis_anual20[is.na(trimestre_analisis_anual20$valida),]))
trimestre_analisis_anual20$pos = rep(1,nrow(trimestre_analisis_anual20))

trimestre_analisis_anual20_pre<-merge(trimestre_analisis_anual20_pre,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
trimestre_analisis_anual20_pre[is.na(trimestre_analisis_anual20_pre$Seccion),"Seccion"]<-rep("Otra",nrow(trimestre_analisis_anual20_pre[is.na(trimestre_analisis_anual20_pre$Seccion),]))
trimestre_analisis_anual20_pre<-Estado_matricula(trimestre_analisis_anual20_pre)
trimestre_analisis_anual20_pre<-tamaño_empresa(trimestre_analisis_anual20_pre)
trimestre_analisis_anual20_pre<-Actividad_economica(trimestre_analisis_anual20_pre)
trimestre_analisis_anual20_pre<-trimestre_analisis_anual20_pre[,c("tipo_identificacion_APT","numero_identificacion_APT","retiro_Max_Sum","ingreso_Max_Sum",
                                                                  "codigo_ciudad_Max_Max","codigo_depto_Max_Max","codigo_actividad_economica_Max_Max",
                                                                  "Total_cotizantes","EstadoMatricula","Activa","Cancelada_inactiva","Micro","Pequeña","Mediana","Grande","Muy_grande",
                                                                  "IBC_REFERENCIA_Mean","IBC_REFERENCIA_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Sum",
                                                                  "Seccion","Seccion_A","Seccion_B","Seccion_C","Seccion_D","Seccion_E","Seccion_F","Seccion_G","Seccion_H","Seccion_I","Seccion_J","Seccion_K","Seccion_L","Seccion_M","Seccion_N","Seccion_O","Seccion_P","Seccion_Q","Seccion_R","Seccion_S","Seccion_T","Seccion_U",
                                                                  "Sexo_hombres_Max_Sum","Sexo_mujeres_Max_Sum","jovenes_Max_Sum","mayores_Max_Sum","cot_obligatoria_salud_Max_Sum")]
trimestre_analisis_anual20_pre<-merge(trimestre_analisis_anual20_pre,Divipola_dpto,by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
trimestre_analisis_anual20_pre$divipola<-trimestre_analisis_anual20_pre$codigo_depto_Max_Max*1000+trimestre_analisis_anual20_pre$codigo_ciudad_Max_Max
trimestre_analisis_anual20_pre<-merge(trimestre_analisis_anual20_pre,Divipola_mpio,by.x = "divipola", by.y = "Municipio", all.x = TRUE)
trimestre_analisis_anual20_pre$Dpto1<-trimestre_analisis_anual20_pre$codigo_depto_Max_Max
trimestre_analisis_anual20_pre$Divipola1<-trimestre_analisis_anual20_pre$divipola
trimestre_analisis_anual20_pre[is.na(trimestre_analisis_anual20_pre$validacion_dpto),"Dpto1"]<-rep(0,nrow(trimestre_analisis_anual20_pre[is.na(trimestre_analisis_anual20_pre$validacion_dpto),]))
trimestre_analisis_anual20_pre[is.na(trimestre_analisis_anual20_pre$valida),"Divipola1"]<-rep(0,nrow(trimestre_analisis_anual20_pre[is.na(trimestre_analisis_anual20_pre$valida),]))
colnames(trimestre_analisis_anual20_pre)<-c("divipola2","codigo_depto_Max_Max2","tipo_identificacion_APT","numero_identificacion_APT", paste0(colnames(trimestre_analisis_anual20_pre[,5:ncol(trimestre_analisis_anual20_pre)]),"2"))
trimestre_analisis_anual20_pre$pre = rep(1,nrow(trimestre_analisis_anual20_pre))

panel_trimestre_anual20<-merge(trimestre_analisis_anual20,trimestre_analisis_anual20_pre, by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE)

trimestre_analisis_anual19<-merge(trimestre_analisis_anual19,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
trimestre_analisis_anual19[is.na(trimestre_analisis_anual19$Seccion),"Seccion"]<-rep("Otra",nrow(trimestre_analisis_anual19[is.na(trimestre_analisis_anual19$Seccion),]))
trimestre_analisis_anual19<-Estado_matricula(trimestre_analisis_anual19)
trimestre_analisis_anual19<-tamaño_empresa(trimestre_analisis_anual19)
trimestre_analisis_anual19<-Actividad_economica(trimestre_analisis_anual19)
trimestre_analisis_anual19<-trimestre_analisis_anual19[,c("tipo_identificacion_APT","numero_identificacion_APT","retiro_Max_Sum","ingreso_Max_Sum",
                                                          "codigo_ciudad_Max_Max","codigo_depto_Max_Max","codigo_actividad_economica_Max_Max",
                                                          "Total_cotizantes","EstadoMatricula","Activa","Cancelada_inactiva","Micro","Pequeña","Mediana","Grande","Muy_grande",
                                                          "IBC_REFERENCIA_Mean","IBC_REFERENCIA_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Sum",
                                                          "Seccion","Seccion_A","Seccion_B","Seccion_C","Seccion_D","Seccion_E","Seccion_F","Seccion_G","Seccion_H","Seccion_I","Seccion_J","Seccion_K","Seccion_L","Seccion_M","Seccion_N","Seccion_O","Seccion_P","Seccion_Q","Seccion_R","Seccion_S","Seccion_T","Seccion_U",
                                                          "Sexo_hombres_Max_Sum","Sexo_mujeres_Max_Sum","jovenes_Max_Sum","mayores_Max_Sum","cot_obligatoria_salud_Max_Sum")]
trimestre_analisis_anual19<-merge(trimestre_analisis_anual19,Divipola_dpto,by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
trimestre_analisis_anual19$divipola<-trimestre_analisis_anual19$codigo_depto_Max_Max*1000+trimestre_analisis_anual19$codigo_ciudad_Max_Max
trimestre_analisis_anual19<-merge(trimestre_analisis_anual19,Divipola_mpio,by.x = "divipola", by.y = "Municipio", all.x = TRUE)
trimestre_analisis_anual19$Dpto1<-trimestre_analisis_anual19$codigo_depto_Max_Max
trimestre_analisis_anual19$Divipola1<-trimestre_analisis_anual19$divipola
trimestre_analisis_anual19[is.na(trimestre_analisis_anual19$validacion_dpto),"Dpto1"]<-rep(0,nrow(trimestre_analisis_anual19[is.na(trimestre_analisis_anual19$validacion_dpto),]))
trimestre_analisis_anual19[is.na(trimestre_analisis_anual19$valida),"Divipola1"]<-rep(0,nrow(trimestre_analisis_anual19[is.na(trimestre_analisis_anual19$valida),]))
trimestre_analisis_anual19$pos = rep(1,nrow(trimestre_analisis_anual19))

trimestre_analisis_anual19_pre<-merge(trimestre_analisis_anual19_pre,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
trimestre_analisis_anual19_pre[is.na(trimestre_analisis_anual19_pre$Seccion),"Seccion"]<-rep("Otra",nrow(trimestre_analisis_anual19_pre[is.na(trimestre_analisis_anual19_pre$Seccion),]))
trimestre_analisis_anual19_pre<-Estado_matricula(trimestre_analisis_anual19_pre)
trimestre_analisis_anual19_pre<-tamaño_empresa(trimestre_analisis_anual19_pre)
trimestre_analisis_anual19_pre<-Actividad_economica(trimestre_analisis_anual19_pre)
trimestre_analisis_anual19_pre<-trimestre_analisis_anual19_pre[,c("tipo_identificacion_APT","numero_identificacion_APT","retiro_Max_Sum","ingreso_Max_Sum",
                                                                  "codigo_ciudad_Max_Max","codigo_depto_Max_Max","codigo_actividad_economica_Max_Max",
                                                                  "Total_cotizantes","EstadoMatricula","Activa","Cancelada_inactiva","Micro","Pequeña","Mediana","Grande","Muy_grande",
                                                                  "IBC_REFERENCIA_Mean","IBC_REFERENCIA_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Median","PAGOS_TOTALES_Mean","PAGOS_TOTALES_Sum",
                                                                  "Seccion","Seccion_A","Seccion_B","Seccion_C","Seccion_D","Seccion_E","Seccion_F","Seccion_G","Seccion_H","Seccion_I","Seccion_J","Seccion_K","Seccion_L","Seccion_M","Seccion_N","Seccion_O","Seccion_P","Seccion_Q","Seccion_R","Seccion_S","Seccion_T","Seccion_U",
                                                                  "Sexo_hombres_Max_Sum","Sexo_mujeres_Max_Sum","jovenes_Max_Sum","mayores_Max_Sum","cot_obligatoria_salud_Max_Sum")]
trimestre_analisis_anual19_pre<-merge(trimestre_analisis_anual19_pre,Divipola_dpto,by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
trimestre_analisis_anual19_pre$divipola<-trimestre_analisis_anual19_pre$codigo_depto_Max_Max*1000+trimestre_analisis_anual19_pre$codigo_ciudad_Max_Max
trimestre_analisis_anual19_pre<-merge(trimestre_analisis_anual19_pre,Divipola_mpio,by.x = "divipola", by.y = "Municipio", all.x = TRUE)
trimestre_analisis_anual19_pre$Dpto1<-trimestre_analisis_anual19_pre$codigo_depto_Max_Max
trimestre_analisis_anual19_pre$Divipola1<-trimestre_analisis_anual19_pre$divipola
trimestre_analisis_anual19_pre[is.na(trimestre_analisis_anual19_pre$validacion_dpto),"Dpto1"]<-rep(0,nrow(trimestre_analisis_anual19_pre[is.na(trimestre_analisis_anual19_pre$validacion_dpto),]))
trimestre_analisis_anual19_pre[is.na(trimestre_analisis_anual19_pre$valida),"Divipola1"]<-rep(0,nrow(trimestre_analisis_anual19_pre[is.na(trimestre_analisis_anual19_pre$valida),]))
colnames(trimestre_analisis_anual19_pre)<-c("divipola2","codigo_depto_Max_Max2","tipo_identificacion_APT","numero_identificacion_APT", paste0(colnames(trimestre_analisis_anual19_pre[,5:ncol(trimestre_analisis_anual19_pre)]),"2"))
trimestre_analisis_anual19_pre$pre = rep(1,nrow(trimestre_analisis_anual19_pre))

panel_trimestre_anual19<-merge(trimestre_analisis_anual19,trimestre_analisis_anual19_pre, by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE)

Resumen_Muni_Tr_anual20<-data.frame(panel_trimestre_anual20%>%group_by(Divipola1)%>%
  summarise(Total_apt = sum(pos,na.rm = TRUE), 
            Var_apt_trim = (sum(pos, na.rm = TRUE)-sum(pre, na.rm = TRUE))/sum(pre, na.rm = TRUE),
            Total_cot = sum(Total_cotizantes, na.rm = TRUE),
            Cot_medio_por_empresa = sum(Total_cotizantes, na.rm = TRUE)/sum(pos,na.rm = TRUE),
            Var_cot_trim = (sum(Total_cotizantes, na.rm = TRUE)-sum(Total_cotizantes2, na.rm = TRUE))/sum(Total_cotizantes2, na.rm = TRUE),
            Participacion_cot_retiro = sum(retiro_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),Participacion_cot_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),
            Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
            Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE),
            IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),
            IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),#IBC_max = max(IBC_REFERENCIA_Mean,na.rm = TRUE), IBC_min = min(IBC_REFERENCIA_Mean,na.rm = TRUE),
            Total_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Max_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Min_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),
            Total_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Max_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Min_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),
            Relacion_sexos_pos = sum(Sexo_hombres_Max_Sum, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum, na.rm = TRUE) * 100, total_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE), participacion_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE) * 100,
            Relacion_sexos_pre = sum(Sexo_hombres_Max_Sum2, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum2, na.rm = TRUE) * 100, total_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE),participacion_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE)/sum(Total_cotizantes2, na.rm = TRUE) * 100))

colnames(Resumen_Muni_Tr_anual20)<-c("Divipola1", paste0(colnames(Resumen_Muni_Tr_anual20[,c(2:ncol(Resumen_Muni_Tr_anual20))]),"_anual20"))

Resumen_Dpto_Tr_anual20<-data.frame(panel_trimestre_anual20%>%group_by(Dpto1)%>%
                                      summarise(Total_apt = sum(pos,na.rm = TRUE), 
                                                Var_apt_trim = (sum(pos, na.rm = TRUE)-sum(pre, na.rm = TRUE))/sum(pre, na.rm = TRUE),
                                                Total_cot = sum(Total_cotizantes, na.rm = TRUE),
                                                Cot_medio_por_empresa = sum(Total_cotizantes, na.rm = TRUE)/sum(pos,na.rm = TRUE),
                                                Var_cot_trim = (sum(Total_cotizantes, na.rm = TRUE)-sum(Total_cotizantes2, na.rm = TRUE))/sum(Total_cotizantes2, na.rm = TRUE),
                                                Participacion_cot_retiro = sum(retiro_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),Participacion_cot_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),
                                                Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
                                                Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE),
                                                IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),
                                                #IBC_max = max(IBC_REFERENCIA_Mean,na.rm = TRUE), IBC_min = min(IBC_REFERENCIA_Mean,na.rm = TRUE),
                                                Total_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Max_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Min_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),
                                                Total_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Max_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Min_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),
                                                Relacion_sexos_pos = sum(Sexo_hombres_Max_Sum, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum, na.rm = TRUE) * 100, total_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE), participacion_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE) * 100,
                                                Relacion_sexos_pre = sum(Sexo_hombres_Max_Sum2, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum2, na.rm = TRUE) * 100, total_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE),participacion_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE)/sum(Total_cotizantes2, na.rm = TRUE) * 100))

colnames(Resumen_Dpto_Tr_anual20)<-c("Dpto1", paste0(colnames(Resumen_Dpto_Tr_anual20[,c(2:ncol(Resumen_Dpto_Tr_anual20))]),"_anual20"))

Resumen_Muni_Tr_anual19<-data.frame(panel_trimestre_anual19%>%group_by(Divipola1)%>%
                                      summarise(Total_apt = sum(pos,na.rm = TRUE), 
                                                Var_apt_trim = (sum(pos, na.rm = TRUE)-sum(pre, na.rm = TRUE))/sum(pre, na.rm = TRUE),
                                                Total_cot = sum(Total_cotizantes, na.rm = TRUE),
                                                Cot_medio_por_empresa = sum(Total_cotizantes, na.rm = TRUE)/sum(pos,na.rm = TRUE),
                                                Var_cot_trim = (sum(Total_cotizantes, na.rm = TRUE)-sum(Total_cotizantes2, na.rm = TRUE))/sum(Total_cotizantes2, na.rm = TRUE),
                                                Participacion_cot_retiro = sum(retiro_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),Participacion_cot_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),
                                                Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
                                                Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE),
                                                IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),
                                                IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),#IBC_max = max(IBC_REFERENCIA_Mean,na.rm = TRUE), IBC_min = min(IBC_REFERENCIA_Mean,na.rm = TRUE),
                                                Total_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Max_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Min_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),
                                                Total_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Max_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Min_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),
                                                Relacion_sexos_pos = sum(Sexo_hombres_Max_Sum, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum, na.rm = TRUE) * 100, total_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE), participacion_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE) * 100,
                                                Relacion_sexos_pre = sum(Sexo_hombres_Max_Sum2, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum2, na.rm = TRUE) * 100, total_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE),participacion_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE)/sum(Total_cotizantes2, na.rm = TRUE) * 100))

colnames(Resumen_Muni_Tr_anual19)<-c("Divipola1", paste0(colnames(Resumen_Muni_Tr_anual19[,c(2:ncol(Resumen_Muni_Tr_anual19))]),"_anual19"))

Resumen_Dpto_Tr_anual19<-data.frame(panel_trimestre_anual19%>%group_by(Dpto1)%>%
                                      summarise(Total_apt = sum(pos,na.rm = TRUE), 
                                                Var_apt_trim = (sum(pos, na.rm = TRUE)-sum(pre, na.rm = TRUE))/sum(pre, na.rm = TRUE),
                                                Total_cot = sum(Total_cotizantes, na.rm = TRUE),
                                                Cot_medio_por_empresa = sum(Total_cotizantes, na.rm = TRUE)/sum(pos,na.rm = TRUE),
                                                Var_cot_trim = (sum(Total_cotizantes, na.rm = TRUE)-sum(Total_cotizantes2, na.rm = TRUE))/sum(Total_cotizantes2, na.rm = TRUE),
                                                Participacion_cot_retiro = sum(retiro_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),Participacion_cot_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE),
                                                Total_apt_Micro = sum(Micro, na.rm = TRUE),Total_apt_Pequena = sum(Pequeña, na.rm = TRUE),Total_apt_Mediana = sum(Mediana, na.rm = TRUE),Total_apt_Grande = sum(Grande, na.rm = TRUE),Total_apt_Muy_grande = sum(Muy_grande, na.rm = TRUE),
                                                Total_apt_sec_A = sum(Seccion_A, na.rm = TRUE),Total_apt_sec_B = sum(Seccion_B, na.rm = TRUE),Total_apt_sec_C = sum(Seccion_C, na.rm = TRUE),Total_apt_sec_D = sum(Seccion_D, na.rm = TRUE),Total_apt_sec_E = sum(Seccion_E, na.rm = TRUE),Total_apt_sec_F = sum(Seccion_F, na.rm = TRUE),Total_apt_sec_G = sum(Seccion_G, na.rm = TRUE),Total_apt_sec_H = sum(Seccion_H, na.rm = TRUE),Total_apt_sec_I = sum(Seccion_I, na.rm = TRUE),Total_apt_sec_J = sum(Seccion_J, na.rm = TRUE),Total_apt_sec_K = sum(Seccion_K, na.rm = TRUE),Total_apt_sec_L = sum(Seccion_L, na.rm = TRUE),Total_apt_sec_M = sum(Seccion_M, na.rm = TRUE),Total_apt_sec_N = sum(Seccion_N, na.rm = TRUE),Total_apt_sec_O = sum(Seccion_O, na.rm = TRUE),Total_apt_sec_P = sum(Seccion_P, na.rm = TRUE),Total_apt_sec_Q = sum(Seccion_Q, na.rm = TRUE),Total_apt_sec_R = sum(Seccion_R, na.rm = TRUE),Total_apt_sec_S = sum(Seccion_S, na.rm = TRUE),Total_apt_sec_T = sum(Seccion_T, na.rm = TRUE),Total_apt_sec_U = sum(Seccion_U, na.rm = TRUE),
                                                IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),
                                                IBC_mean = mean(IBC_REFERENCIA_Mean, na.rm = TRUE),IBC_mean_pre = mean(IBC_REFERENCIA_Mean2, na.rm = TRUE),#IBC_max = max(IBC_REFERENCIA_Mean,na.rm = TRUE), IBC_min = min(IBC_REFERENCIA_Mean,na.rm = TRUE),
                                                Total_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Max_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),Min_pagos = sum(PAGOS_TOTALES_Sum, na.rm = TRUE),
                                                Total_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Max_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),Min_pagos_pre = sum(PAGOS_TOTALES_Sum2, na.rm = TRUE),
                                                Relacion_sexos_pos = sum(Sexo_hombres_Max_Sum, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum, na.rm = TRUE) * 100, total_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE), participacion_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE)/sum(Total_cotizantes, na.rm = TRUE) * 100,
                                                Relacion_sexos_pre = sum(Sexo_hombres_Max_Sum2, na.rm = TRUE)/ sum(Sexo_mujeres_Max_Sum2, na.rm = TRUE) * 100, total_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE),participacion_jovenes_pre = sum(jovenes_Max_Sum2, na.rm = TRUE)/sum(Total_cotizantes2, na.rm = TRUE) * 100))

colnames(Resumen_Dpto_Tr_anual19)<-c("Dpto1", paste0(colnames(Resumen_Dpto_Tr_anual19[,c(2:ncol(Resumen_Dpto_Tr_anual19))]),"_anual19"))

Resumen_Muni_tr_C<-merge(Resumen_Muni_tr,Resumen_Muni_Tr_anual20, by = "Divipola1", all = TRUE)
Resumen_Muni_tr_C<-merge(Resumen_Muni_tr_C,Resumen_Muni_Tr_anual19, by = "Divipola1", all = TRUE)
Resumen_Muni_tr_C$Var_apt_anual<-(Resumen_Muni_tr_C$Total_apt-Resumen_Muni_tr_C$Total_apt_anual20)/Resumen_Muni_tr_C$Total_apt_anual20
Resumen_Muni_tr_C$Var_apt_anual20<-(Resumen_Muni_tr_C$Total_apt_anual20-Resumen_Muni_tr_C$Total_apt_anual19)/Resumen_Muni_tr_C$Total_apt_anual19
Resumen_Muni_tr_C$Var_cot_anual<-(Resumen_Muni_tr_C$Total_cot-Resumen_Muni_tr_C$Total_cot_anual20)/Resumen_Muni_tr_C$Total_cot_anual20
Resumen_Muni_tr_C$var_IBC_mean_pre<-(Resumen_Muni_tr_C$IBC_mean-Resumen_Muni_tr_C$IBC_mean_pre)/Resumen_Muni_tr_C$IBC_mean_pre
Resumen_Muni_tr_C$var_IBC_mean_anual<-(Resumen_Muni_tr_C$IBC_mean-Resumen_Muni_tr_C$IBC_mean_anual20)/Resumen_Muni_tr_C$IBC_mean_anual20
Resumen_Muni_tr_C$var_Pagos_total_pre<-(Resumen_Muni_tr_C$Total_pagos-Resumen_Muni_tr_C$Total_pagos_pre)/Resumen_Muni_tr_C$Total_pagos_pre
Resumen_Muni_tr_C$var_Pagos_total_anual<-(Resumen_Muni_tr_C$Total_pagos-Resumen_Muni_tr_C$Total_pagos_anual20)/Resumen_Muni_tr_C$Total_pagos_anual20

Resumen_Muni_tr_C_mosulo1<-Resumen_Muni_tr_C[,c("Divipola1","Total_apt","Var_apt_anual","Var_apt_trim","Var_apt_anual20","Var_apt_trim_anual20","Var_apt_trim_anual19","Total_cot","Var_cot_anual","Var_cot_trim","Cot_medio_por_empresa","Cot_medio_por_empresa_anual20","Cot_medio_por_empresa_anual19","Participacion_cot_retiro","Participacion_cot_ingresos","Participacion_cot_retiro_anual20","Participacion_cot_ingresos_anual20","Relacion_sexos_pos","Relacion_sexos_pre","Relacion_sexos_pos_anual20","participacion_jovenes","participacion_jovenes_pre","participacion_jovenes_anual20",
                                                "IBC_mean","var_IBC_mean_pre","var_IBC_mean_anual","Total_pagos","var_Pagos_total_pre","var_Pagos_total_anual","Max_pagos","Min_pagos",
                                                "Total_apt_Micro","Total_apt_Pequena","Total_apt_Mediana","Total_apt_Grande","Total_apt_Muy_grande",
                                                "Total_apt_sec_A","Total_apt_sec_B","Total_apt_sec_C","Total_apt_sec_D","Total_apt_sec_E","Total_apt_sec_F","Total_apt_sec_G","Total_apt_sec_H","Total_apt_sec_I","Total_apt_sec_J","Total_apt_sec_K","Total_apt_sec_L","Total_apt_sec_M","Total_apt_sec_N","Total_apt_sec_O","Total_apt_sec_P","Total_apt_sec_Q","Total_apt_sec_R","Total_apt_sec_S","Total_apt_sec_T","Total_apt_sec_U",
                                                "Total_apt_Micro_anual20","Total_apt_Pequena_anual20","Total_apt_Mediana_anual20","Total_apt_Grande_anual20","Total_apt_Muy_grande_anual20",
                                                "Total_apt_sec_A_anual20","Total_apt_sec_B_anual20","Total_apt_sec_C_anual20","Total_apt_sec_D_anual20","Total_apt_sec_E_anual20","Total_apt_sec_F_anual20","Total_apt_sec_G_anual20","Total_apt_sec_H_anual20","Total_apt_sec_I_anual20","Total_apt_sec_J_anual20","Total_apt_sec_K_anual20","Total_apt_sec_L_anual20","Total_apt_sec_M_anual20","Total_apt_sec_N_anual20","Total_apt_sec_O_anual20","Total_apt_sec_P_anual20","Total_apt_sec_Q_anual20","Total_apt_sec_R_anual20","Total_apt_sec_S_anual20","Total_apt_sec_T_anual20","Total_apt_sec_U_anual20",
                                                "Total_apt_Micro_anual19","Total_apt_Pequena_anual19","Total_apt_Mediana_anual19","Total_apt_Grande_anual19","Total_apt_Muy_grande_anual19",
                                                "Total_apt_sec_A_anual19","Total_apt_sec_B_anual19","Total_apt_sec_C_anual19","Total_apt_sec_D_anual19","Total_apt_sec_E_anual19","Total_apt_sec_F_anual19","Total_apt_sec_G_anual19","Total_apt_sec_H_anual19","Total_apt_sec_I_anual19","Total_apt_sec_J_anual19","Total_apt_sec_K_anual19","Total_apt_sec_L_anual19","Total_apt_sec_M_anual19","Total_apt_sec_N_anual19","Total_apt_sec_O_anual19","Total_apt_sec_P_anual19","Total_apt_sec_Q_anual19","Total_apt_sec_R_anual19","Total_apt_sec_S_anual19","Total_apt_sec_T_anual19","Total_apt_sec_U_anual19")]

Resumen_Dpto_tr_C<-merge(Resumen_Dpto_tr,Resumen_Dpto_Tr_anual20, by = "Dpto1", all = TRUE)
Resumen_Dpto_tr_C<-merge(Resumen_Dpto_tr_C,Resumen_Dpto_Tr_anual19, by = "Dpto1", all = TRUE)
Resumen_Dpto_tr_C$Var_apt_anual<-(Resumen_Dpto_tr_C$Total_apt-Resumen_Dpto_tr_C$Total_apt_anual20)/Resumen_Dpto_tr_C$Total_apt_anual20
Resumen_Dpto_tr_C$Var_apt_anual20<-(Resumen_Dpto_tr_C$Total_apt_anual20-Resumen_Dpto_tr_C$Total_apt_anual19)/Resumen_Dpto_tr_C$Total_apt_anual19
Resumen_Dpto_tr_C$Var_cot_anual<-(Resumen_Dpto_tr_C$Total_cot-Resumen_Dpto_tr_C$Total_cot_anual20)/Resumen_Dpto_tr_C$Total_cot_anual20
Resumen_Dpto_tr_C$var_IBC_mean_pre<-(Resumen_Dpto_tr_C$IBC_mean-Resumen_Dpto_tr_C$IBC_mean_pre)/Resumen_Dpto_tr_C$IBC_mean_pre
Resumen_Dpto_tr_C$var_IBC_mean_anual<-(Resumen_Dpto_tr_C$IBC_mean-Resumen_Dpto_tr_C$IBC_mean_anual20)/Resumen_Dpto_tr_C$IBC_mean_anual20
Resumen_Dpto_tr_C$var_Pagos_total_pre<-(Resumen_Dpto_tr_C$Total_pagos-Resumen_Dpto_tr_C$Total_pagos_pre)/Resumen_Dpto_tr_C$Total_pagos_pre
Resumen_Dpto_tr_C$var_Pagos_total_anual<-(Resumen_Dpto_tr_C$Total_pagos-Resumen_Dpto_tr_C$Total_pagos_anual20)/Resumen_Dpto_tr_C$Total_pagos_anual20

Resumen_Dpto_tr_C_mosulo1<-Resumen_Dpto_tr_C[,c("Dpto1","Total_apt","Var_apt_anual","Var_apt_trim","Var_apt_anual20","Var_apt_trim_anual20","Var_apt_trim_anual19","Total_cot","Var_cot_anual","Var_cot_trim","Cot_medio_por_empresa","Cot_medio_por_empresa_anual20","Cot_medio_por_empresa_anual19","Participacion_cot_retiro","Participacion_cot_ingresos","Participacion_cot_retiro_anual20","Participacion_cot_ingresos_anual20",
                                                "IBC_mean","var_IBC_mean_pre","var_IBC_mean_anual","Total_pagos","var_Pagos_total_pre","var_Pagos_total_anual","Max_pagos","Min_pagos",
                                                "Total_apt_Micro","Total_apt_Pequena","Total_apt_Mediana","Total_apt_Grande","Total_apt_Muy_grande",
                                                "Total_apt_sec_A","Total_apt_sec_B","Total_apt_sec_C","Total_apt_sec_D","Total_apt_sec_E","Total_apt_sec_F","Total_apt_sec_G","Total_apt_sec_H","Total_apt_sec_I","Total_apt_sec_J","Total_apt_sec_K","Total_apt_sec_L","Total_apt_sec_M","Total_apt_sec_N","Total_apt_sec_O","Total_apt_sec_P","Total_apt_sec_Q","Total_apt_sec_R","Total_apt_sec_S","Total_apt_sec_T","Total_apt_sec_U",
                                                "Total_apt_Micro_anual20","Total_apt_Pequena_anual20","Total_apt_Mediana_anual20","Total_apt_Grande_anual20","Total_apt_Muy_grande_anual20",
                                                "Total_apt_sec_A_anual20","Total_apt_sec_B_anual20","Total_apt_sec_C_anual20","Total_apt_sec_D_anual20","Total_apt_sec_E_anual20","Total_apt_sec_F_anual20","Total_apt_sec_G_anual20","Total_apt_sec_H_anual20","Total_apt_sec_I_anual20","Total_apt_sec_J_anual20","Total_apt_sec_K_anual20","Total_apt_sec_L_anual20","Total_apt_sec_M_anual20","Total_apt_sec_N_anual20","Total_apt_sec_O_anual20","Total_apt_sec_P_anual20","Total_apt_sec_Q_anual20","Total_apt_sec_R_anual20","Total_apt_sec_S_anual20","Total_apt_sec_T_anual20","Total_apt_sec_U_anual20",
                                                "Total_apt_Micro_anual19","Total_apt_Pequena_anual19","Total_apt_Mediana_anual19","Total_apt_Grande_anual19","Total_apt_Muy_grande_anual19",
                                                "Total_apt_sec_A_anual19","Total_apt_sec_B_anual19","Total_apt_sec_C_anual19","Total_apt_sec_D_anual19","Total_apt_sec_E_anual19","Total_apt_sec_F_anual19","Total_apt_sec_G_anual19","Total_apt_sec_H_anual19","Total_apt_sec_I_anual19","Total_apt_sec_J_anual19","Total_apt_sec_K_anual19","Total_apt_sec_L_anual19","Total_apt_sec_M_anual19","Total_apt_sec_N_anual19","Total_apt_sec_O_anual19","Total_apt_sec_P_anual19","Total_apt_sec_Q_anual19","Total_apt_sec_R_anual19","Total_apt_sec_S_anual19","Total_apt_sec_T_anual19","Total_apt_sec_U_anual19")]
#MES 
setwd("D:/Nuevos_Insumos_IMC/empresas/")

#tabla_Tot = "empresas_2021_12.csv";periodo = "2021_12"

mensual_salida<-function(tabla_Tot,periodo){
  base1_<-read.csv2(tabla_Tot, dec = ".");
  base_datos<-merge(base1_,Divipola_dpto,by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
  base_datos$divipola<-base_datos$codigo_depto_Max_Max*1000+base_datos$codigo_ciudad_Max_Max
  base_datos<-merge(base_datos,Divipola_mpio,by.x = "divipola", by.y = "Municipio", all.x = TRUE)
  base_datos$Dpto1<-base_datos$codigo_depto_Max_Max
  base_datos$Divipola1<-base_datos$divipola
  base_datos[is.na(base_datos$validacion_dpto),"Dpto1"]<-rep(0,nrow(base_datos[is.na(base_datos$validacion_dpto),]))
  base_datos[is.na(base_datos$valida),"Divipola1"]<-rep(0,nrow(base_datos[is.na(base_datos$valida),]))
  res_base_datos_muni<-data.frame(base_datos%>%group_by(Divipola1)%>%summarise(Total_aportantes = n(), total_cotizantes = sum(Total_cotizantes, na.rm = TRUE), Total_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE)))
  res_base_datos_dpto<-data.frame(base_datos%>%group_by(Dpto1)%>%summarise(Total_aportantes = n(), total_cotizantes = sum(Total_cotizantes, na.rm = TRUE), Total_jovenes = sum(jovenes_Max_Sum, na.rm = TRUE)))
  colnames(res_base_datos_muni)<-c("Divipola1", paste0(colnames(res_base_datos_muni[,c(2:ncol(res_base_datos_muni))]), periodo)) 
  colnames(res_base_datos_dpto)<-c("Dpto1", paste0(colnames(res_base_datos_dpto[,c(2:ncol(res_base_datos_dpto))]), periodo)) 
  salidas <- list(municipal = res_base_datos_muni, departamental = res_base_datos_dpto)
  salidas
}

Mes1<-mensual_salida("empresas_2021_12.csv","_12_21")
Mes2<-mensual_salida("empresas_2021_11.csv","_11_21")
Mes3<-mensual_salida("empresas_2021_10.csv","_10_21")

Mes1_20<-mensual_salida("empresas_2020_12.csv","_12_20")
Mes2_20<-mensual_salida("empresas_2020_11.csv","_11_20")
Mes3_20<-mensual_salida("empresas_2020_10.csv","_10_20")

Mes1_19<-mensual_salida("empresas_2019_12.csv","_12_19")
Mes2_19<-mensual_salida("empresas_2019_11.csv","_11_19")
Mes3_19<-mensual_salida("empresas_2019_10.csv","_10_19")

Consolidado_mes_trimestre_muni<-merge(Mes1$municipal,Mes2$municipal, by = "Divipola1",all = TRUE)
Consolidado_mes_trimestre_muni<-merge(Consolidado_mes_trimestre_muni,Mes3$municipal, by = "Divipola1",all = TRUE)
Consolidado_mes_trimestre_muni<-merge(Consolidado_mes_trimestre_muni,Mes1_20$municipal, by = "Divipola1",all = TRUE)
Consolidado_mes_trimestre_muni<-merge(Consolidado_mes_trimestre_muni,Mes2_20$municipal, by = "Divipola1",all = TRUE)
Consolidado_mes_trimestre_muni<-merge(Consolidado_mes_trimestre_muni,Mes3_20$municipal, by = "Divipola1",all = TRUE)
Consolidado_mes_trimestre_muni<-merge(Consolidado_mes_trimestre_muni,Mes1_19$municipal, by = "Divipola1",all = TRUE)
Consolidado_mes_trimestre_muni<-merge(Consolidado_mes_trimestre_muni,Mes2_19$municipal, by = "Divipola1",all = TRUE)
Consolidado_mes_trimestre_muni<-merge(Consolidado_mes_trimestre_muni,Mes3_19$municipal, by = "Divipola1",all = TRUE)

Consolidado_mes_trimestre_dpto<-merge(Mes1$departamental,Mes2$departamental, by = "Dpto1",all = TRUE)
Consolidado_mes_trimestre_dpto<-merge(Consolidado_mes_trimestre_dpto,Mes3$departamental, by = "Dpto1",all = TRUE)
Consolidado_mes_trimestre_dpto<-merge(Consolidado_mes_trimestre_dpto,Mes1_20$departamental, by = "Dpto1",all = TRUE)
Consolidado_mes_trimestre_dpto<-merge(Consolidado_mes_trimestre_dpto,Mes2_20$departamental, by = "Dpto1",all = TRUE)
Consolidado_mes_trimestre_dpto<-merge(Consolidado_mes_trimestre_dpto,Mes3_20$departamental, by = "Dpto1",all = TRUE)
Consolidado_mes_trimestre_dpto<-merge(Consolidado_mes_trimestre_dpto,Mes1_19$departamental, by = "Dpto1",all = TRUE)
Consolidado_mes_trimestre_dpto<-merge(Consolidado_mes_trimestre_dpto,Mes2_19$departamental, by = "Dpto1",all = TRUE)
Consolidado_mes_trimestre_dpto<-merge(Consolidado_mes_trimestre_dpto,Mes3_19$departamental, by = "Dpto1",all = TRUE)

library(readxl)

PAEF <- read_excel("D:/Informacion PROGRAMAS/Consolidado_programas.xlsx", sheet = "PAEF")
PAP <- read_excel("D:/Informacion PROGRAMAS/Consolidado_programas.xlsx", sheet = "PAP")
MICRO  <- read_excel("D:/Informacion PROGRAMAS/Consolidado_programas.xlsx", sheet = "MICRO")
PARO  <- read_excel("D:/Informacion PROGRAMAS/Consolidado_programas.xlsx", sheet = "PARO")
JOVENES <- read_excel("D:/Informacion PROGRAMAS/Consolidado_programas.xlsx", sheet = "Jovenes")
NUEVO_EMPLEO <- read_excel("D:/Informacion PROGRAMAS/Consolidado_programas.xlsx", sheet = "Nuevo_empleo")

#CONSOLIDADO PROGRAMAS
PAEF_ben<-PAEF[PAEF$ESTADO_TOTAL == "Aprobado con subsidio",c("Tipo_identificacion","Numero_identificacion")];PAEF_ben$PAEF <- rep(1,nrow(PAEF_ben));nrow(PAEF_ben)
PAP_ben<-PAP[PAP$Estado == "Aprobado con subsidio",c("Tipo_documento","Numero_docuento")];PAP_ben$PAP <- rep(1,nrow(PAP_ben))
PARO_ben<-PARO[PARO$Recursos_asociados_Sum > 0,c("TIPO_IDENTIFICA","NUM_EMPRESA1")];PARO_ben$PARO <- rep(1,nrow(PARO_ben))
MICRO_ben<-MICRO[MICRO$U_Recursos_Total_Sum > 0,c("TIPO_IDENTIFICACION","NUM_IDENTIFICACION")];MICRO_ben$MICRO <- rep(1,nrow(MICRO_ben))

nrow(PAEF_ben);
PAP_ben1<-data.frame(PAP_ben%>%group_by(Tipo_documento,Numero_docuento)%>%summarise(PAP = sum(PAP, na.rm = TRUE)));nrow(PAP_ben1)
PAEF_PAP<-merge(PAEF_ben, PAP_ben1, by.x = c("Tipo_identificacion","Numero_identificacion"), by.y = c("Tipo_documento","Numero_docuento"), all = TRUE);nrow(PAEF_PAP)
nrow(PARO_ben)
PARO_ben1<-data.frame(PARO_ben%>%group_by(TIPO_IDENTIFICA,NUM_EMPRESA1)%>%summarise(PARO = sum(PARO, na.rm = TRUE)));nrow(PARO_ben1)
PAEF_PAP_PARO<-merge(PAEF_PAP, PARO_ben1, by.x = c("Tipo_identificacion","Numero_identificacion"), by.y = c("TIPO_IDENTIFICA","NUM_EMPRESA1"), all = TRUE);nrow(PAEF_PAP_PARO)
MICRO_ben1<-data.frame(MICRO_ben%>%group_by(TIPO_IDENTIFICACION,NUM_IDENTIFICACION)%>%summarise(MICRO = sum(MICRO, na.rm = TRUE)));nrow(MICRO_ben1)
PAEF_PAP_PARO_MICRO<-merge(PAEF_PAP_PARO, MICRO_ben1, by.x = c("Tipo_identificacion","Numero_identificacion"), by.y = c("TIPO_IDENTIFICACION","NUM_IDENTIFICACION"), all = TRUE);nrow(PAEF_PAP_PARO_MICRO)

######################
PAEF$Cotizantes_conformida<-as.numeric(PAEF$Cotizantes_conformida)
res_PAEF_muni <- data.frame(PAEF%>%filter(ESTADO_TOTAL == "Aprobado con subsidio")%>%group_by(Divipola1)%>%summarise(total_beneficados_PAEF = n(), total_cot_beneficiados_PAEF = sum(Cotizantes_conformida, na.rm = TRUE)))
res_PAEF_dpto <- data.frame(PAEF%>%filter(ESTADO_TOTAL == "Aprobado con subsidio")%>%group_by(Dpto1)%>%summarise(total_beneficados_PAEF = n(), total_cot_beneficiados_PAEF = sum(Cotizantes_conformida, na.rm = TRUE)))

PAP$Cotizantes_conformida<-as.numeric(PAP$Total_conformes)
res_PAP_muni <- data.frame(PAP%>%filter(Estado == "Aprobado con subsidio")%>%group_by(DIVIPOLA)%>%summarise(total_beneficados_PAP = n(), total_cot_beneficiados_PAP = sum(Cotizantes_conformida, na.rm = TRUE)))
res_PAP_dpto <- data.frame(PAP%>%filter(Estado == "Aprobado con subsidio")%>%group_by(Departamento)%>%summarise(total_beneficados_PAP = n(), total_cot_beneficiados_PAP = sum(Cotizantes_conformida, na.rm = TRUE)))

MICRO$Cotizantes_conformida<-as.numeric(MICRO$`Total cotizantes conformes`)
res_MICRO_muni <- data.frame(MICRO%>%filter(U_Recursos_Total_Sum > 0)%>%group_by(STRCODPAR_MUNICIPIO)%>%summarise(total_beneficados_MICRO = n(), total_cot_beneficiados_MICRO = sum(Cotizantes_conformida, na.rm = TRUE)))
res_MICRO_dpto <- data.frame(MICRO%>%filter(U_Recursos_Total_Sum > 0)%>%group_by(STRCODPAR_DEPARTAMENTO)%>%summarise(total_beneficados_MICRO = n(), total_cot_beneficiados_MICRO = sum(Cotizantes_conformida, na.rm = TRUE)))

PARO$Cotizantes_conformida<-as.numeric(PARO$`Total Cotizantes conformes`)
res_PARO_muni <- data.frame(PARO%>%filter(Recursos_asociados_Sum > 0)%>%group_by(STRCODPAR_CIUDAD_EMPRESA)%>%summarise(total_beneficados_PARO = n(), total_cot_beneficiados_PARO = sum(Cotizantes_conformida, na.rm = TRUE)))
res_PARO_dpto <- data.frame(PARO%>%filter(Recursos_asociados_Sum > 0)%>%group_by(STRCODPAR_DEPTO_EMPRESA)%>%summarise(total_beneficados_PARO = n(), total_cot_beneficiados_PARO = sum(Cotizantes_conformida, na.rm = TRUE)))

JOVENES$Cotizantes_conformida<-as.numeric(JOVENES$Total_Cotizantes_conformes)
res_JOVENES_muni <- data.frame(JOVENES%>%filter(Numero_recursos_Sum > 0)%>%group_by(STRCODPAR_CIUDAD)%>%summarise(total_beneficados_JOVENES = n(), total_cot_beneficiados_JOVENES = sum(Cotizantes_conformida, na.rm = TRUE)))
res_JOVENES_dpto <- data.frame(JOVENES%>%filter(Numero_recursos_Sum > 0)%>%group_by(DEPARTAMENTO)%>%summarise(total_beneficados_JOVENES = n(), total_cot_beneficiados_JOVENES = sum(Cotizantes_conformida, na.rm = TRUE)))

NUEVO_EMPLEO$Cotizantes_conformida<-as.numeric(NUEVO_EMPLEO$Total_conformes)
res_NUEVO_EMPLEO_muni <- data.frame(NUEVO_EMPLEO%>%filter(RECURSOS_CONSOLIDADO_Sum > 0)%>%group_by(STRCODPAR_CIUDAD)%>%summarise(total_beneficados_NUEVO_EMPLEO = n(), total_cot_beneficiados_NUEVO_EMPLEO = sum(Cotizantes_conformida, na.rm = TRUE)))
res_NUEVO_EMPLEO_dpto <- data.frame(NUEVO_EMPLEO%>%filter(RECURSOS_CONSOLIDADO_Sum > 0)%>%group_by(DEPARTAMENTO)%>%summarise(total_beneficados_NUEVO_EMPLEO = n(), total_cot_beneficiados_NUEVO_EMPLEO = sum(Cotizantes_conformida, na.rm = TRUE)))

Resumen_Muni_tr_C_mosulo2<-merge(Resumen_Muni_tr_C_mosulo1,Consolidado_mes_trimestre_muni, by = "Divipola1", all = TRUE)
Resumen_Dpto_tr_C_mosulo2<-merge(Resumen_Dpto_tr_C_mosulo1,Consolidado_mes_trimestre_dpto, by = "Dpto1", all = TRUE)

Resumen_Muni_tr_C_mosulo2<-merge(Resumen_Muni_tr_C_mosulo2,res_PAEF_muni, by.x = "Divipola1", by.y = "Divipola1", all = TRUE)
Resumen_Muni_tr_C_mosulo2<-merge(Resumen_Muni_tr_C_mosulo2,res_PAP_muni, by.x = "Divipola1", by.y = "DIVIPOLA", all = TRUE)
Resumen_Muni_tr_C_mosulo2<-merge(Resumen_Muni_tr_C_mosulo2,res_MICRO_muni, by.x = "Divipola1", by.y = "STRCODPAR_MUNICIPIO", all = TRUE)
Resumen_Muni_tr_C_mosulo2<-merge(Resumen_Muni_tr_C_mosulo2,res_PARO_muni, by.x = "Divipola1", by.y = "STRCODPAR_CIUDAD_EMPRESA", all = TRUE)
Resumen_Muni_tr_C_mosulo2<-merge(Resumen_Muni_tr_C_mosulo2,res_JOVENES_muni, by.x = "Divipola1", by.y = "STRCODPAR_CIUDAD", all = TRUE)
Resumen_Muni_tr_C_mosulo2<-merge(Resumen_Muni_tr_C_mosulo2,res_NUEVO_EMPLEO_muni, by.x = "Divipola1", by.y = "STRCODPAR_CIUDAD", all = TRUE)

Resumen_Dpto_tr_C_mosulo2<-merge(Resumen_Dpto_tr_C_mosulo2,res_PAEF_dpto, by.x = "Dpto1", by.y = "Dpto1", all = TRUE)
Resumen_Dpto_tr_C_mosulo2<-merge(Resumen_Dpto_tr_C_mosulo2,res_PAP_dpto, by.x = "Dpto1", by.y = "Departamento", all = TRUE)
Resumen_Dpto_tr_C_mosulo2<-merge(Resumen_Dpto_tr_C_mosulo2,res_MICRO_dpto, by.x = "Dpto1", by.y = "STRCODPAR_DEPARTAMENTO", all = TRUE)
Resumen_Dpto_tr_C_mosulo2<-merge(Resumen_Dpto_tr_C_mosulo2,res_PARO_dpto, by.x = "Dpto1", by.y = "STRCODPAR_DEPTO_EMPRESA", all = TRUE)
Resumen_Dpto_tr_C_mosulo2<-merge(Resumen_Dpto_tr_C_mosulo2,res_JOVENES_dpto, by.x = "Dpto1", by.y = "DEPARTAMENTO", all = TRUE)
Resumen_Dpto_tr_C_mosulo2<-merge(Resumen_Dpto_tr_C_mosulo2,res_NUEVO_EMPLEO_dpto, by.x = "Dpto1", by.y = "DEPARTAMENTO", all = TRUE)

##independientes y total personas en proyecciones de poblacion 

base_poblacion_muni<-read_xlsx("Proyecciones_municipal_PET.xlsx",sheet = "resumen")
base_poblacion_dpto<-read_xlsx("Proyecciones_departamental_PET.xlsx",sheet = "resumen")
total_cot_ubicaccion_muni<-read_xlsx("Total_cotizantes_por_municipio.xlsx",sheet = "Hoja1")
total_cot_ubicaccion_dpto<-read_xlsx("Total_cotizantes_por_departamento.xlsx",sheet = "Hoja1")

total_cot_ubicaccion_muni<-merge(total_cot_ubicaccion_muni,base_poblacion_muni,by.x = "Divipola", by.y = "divipola", all = TRUE)
total_cot_ubicaccion_DPTO<-merge(total_cot_ubicaccion_dpto,base_poblacion_dpto,by.x = "dpto", by.y = "Dpto", all = TRUE)
total_cot_ubicaccion_muni$participa_PILA_Proy12_19<-total_cot_ubicaccion_muni$Cot_12_19/total_cot_ubicaccion_muni$Total_2019_proy
total_cot_ubicaccion_muni$participa_PILA_Proy12_20<-total_cot_ubicaccion_muni$Cot_12_20/total_cot_ubicaccion_muni$Total_2020_proy
total_cot_ubicaccion_muni$participa_PILA_Proy10_21<-total_cot_ubicaccion_muni$Cot_10_21/total_cot_ubicaccion_muni$Total_2021_proy
total_cot_ubicaccion_muni$participa_PILA_Proy11_21<-total_cot_ubicaccion_muni$Cot_11_21/total_cot_ubicaccion_muni$Total_2021_proy
total_cot_ubicaccion_muni$participa_PILA_Proy12_21<-total_cot_ubicaccion_muni$Cot_12_21/total_cot_ubicaccion_muni$Total_2021_proy
total_cot_ubicaccion_muni1<-total_cot_ubicaccion_muni[,c("Divipola","Total_2021_proy","Cot_12_21","participa_PILA_Proy12_19","participa_PILA_Proy12_20","participa_PILA_Proy10_21","participa_PILA_Proy11_21","participa_PILA_Proy12_21")]
total_cot_ubicaccion_DPTO$participa_PILA_Proy12_19<-total_cot_ubicaccion_DPTO$Cot_12_19/total_cot_ubicaccion_DPTO$proy_Total_2019
total_cot_ubicaccion_DPTO$participa_PILA_Proy12_20<-total_cot_ubicaccion_DPTO$Cot_12_20/total_cot_ubicaccion_DPTO$proy_Total_2020
total_cot_ubicaccion_DPTO$participa_PILA_Proy10_21<-total_cot_ubicaccion_DPTO$Cot_10_21/total_cot_ubicaccion_DPTO$proy_Total_2021
total_cot_ubicaccion_DPTO$participa_PILA_Proy11_21<-total_cot_ubicaccion_DPTO$Cot_11_21/total_cot_ubicaccion_DPTO$proy_Total_2021
total_cot_ubicaccion_DPTO$participa_PILA_Proy12_21<-total_cot_ubicaccion_DPTO$Cot_12_21/total_cot_ubicaccion_DPTO$proy_Total_2021
total_cot_ubicaccion_DPTO1<-total_cot_ubicaccion_DPTO[,c("dpto","proy_Total_2021","Cot_12_21","participa_PILA_Proy12_19","participa_PILA_Proy12_20","participa_PILA_Proy10_21","participa_PILA_Proy11_21","participa_PILA_Proy12_21")]

Resumen_Muni_tr_C_mosulo2<-merge(Resumen_Muni_tr_C_mosulo2,total_cot_ubicaccion_muni1,by.x = "Divipola1", by.y = "Divipola", all = TRUE)
Resumen_Dpto_tr_C_mosulo2<-merge(Resumen_Dpto_tr_C_mosulo2,total_cot_ubicaccion_DPTO1,by.x = "Dpto1", by.y = "dpto", all = TRUE)

write.xlsx(Resumen_Muni_tr_C_mosulo2, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_mod1_2021_IV.xlsx",sheetName = "Municipal",showNA = FALSE)
write.xlsx(Resumen_Dpto_tr_C_mosulo2, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_mod1_2021_IV.xlsx",sheetName = "Departamental",append = TRUE, showNA = FALSE)

##################################################
#  Modulo de las empresas que salen              #
##################################################

Salen_tr_analisis<-panel_trimestre[!is.na(panel_trimestre$pre) & is.na(panel_trimestre$pos),];nrow(Salen_tr_analisis)
info_general_muni<-Resumen_Muni[,c("Divipola1","Total_apt", "Total_cot")]
trimestre_IV_20<-consolidado_APT("empresas_2020_8.csv","empresas_2020_7.csv","empresas_2020_6.csv")
trimestre_IV_19<-consolidado_APT("empresas_2019_8.csv","empresas_2019_7.csv","empresas_2019_6.csv")

##################################################
#  Modulo de las empresas que entran             #
##################################################

write.xlsx(Resumen_Muni, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_mod1_beta.xlsx",sheetName = "Municipal")
write.xlsx(Resumen_Dpto, "D:/Nuevos_Insumos_IMC/Geovisor/resrumen_geovisor_mod1_beta.xlsx",sheetName = "Departamental",append = TRUE)

###########################################################################
#geovisor Bogota 

library(stringr)
setwd("D:/Nuevos_Insumos_IMC/empresas/")

tabla = "empresas_2021_1.csv"
mes = 1
year = 2021

empresas1_21<-read.csv2(tabla)
empresas1_21$divipola1_21<-empresas1_21$codigo_depto_Max_Max*1000+empresas1_21$codigo_ciudad_Max_Max
empresas1_21<-empresas1_21[,c("tipo_identificacion_APT","numero_identificacion_APT","divipola1_21","Total_cotizantes")]
colnames(empresas1_21)<-c("tipo_identificacion_APT","numero_identificacion_APT","divipola1_21","Total_cotizantes1_21")

tabla = "empresas_2021_2.csv"
mes = 2
year = 21

empresas<-read.csv2(tabla)
empresas$divipola<-empresas$codigo_depto_Max_Max*1000+empresas$codigo_ciudad_Max_Max
empresas<-empresas[,c("tipo_identificacion_APT","numero_identificacion_APT","divipola","Total_cotizantes")]
colnames(empresas)<-c("tipo_identificacion_APT","numero_identificacion_APT","divipola",paste0("Total_cotizantes",mes,"_",year))
empresas_consolidado<-merge(empresas1_21,empresas, by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE)
empresas_consolidado$Divipola_general<-empresas_consolidado$divipola
empresas_consolidado[is.na(empresas_consolidado$divipola) & !is.na(empresas_consolidado$divipola1_21),"Divipola_general"]<-empresas_consolidado[is.na(empresas_consolidado$divipola) & !is.na(empresas_consolidado$divipola1_21),"divipola1_21"]
empresas_consolidado<-empresas_consolidado[,-c(3,5)]

tabla = "empresas_2021_4.csv"
mes = 4
year = 21

empresas_longitudinal<-function(tabla,mes,year){
  empresas<-read.csv2(tabla)
  empresas$divipola<-empresas$codigo_depto_Max_Max*1000+empresas$codigo_ciudad_Max_Max
  empresas<-empresas[,c("tipo_identificacion_APT","numero_identificacion_APT","divipola","Total_cotizantes")]
  colnames(empresas)<-c("tipo_identificacion_APT","numero_identificacion_APT","divipola",paste0("Total_cotizantes",mes,"_",year))
  empresas_consolidado<-merge(empresas_consolidado,empresas, by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE)
  empresas_consolidado[is.na(empresas_consolidado$divipola) & !is.na(empresas_consolidado$Divipola_general),"divipola"]<-empresas_consolidado[is.na(empresas_consolidado$divipola) & !is.na(empresas_consolidado$Divipola_general),"Divipola_general"]
  empresas_consolidado<-select(empresas_consolidado, - Divipola_general)
  empresas_consolidado<-rename(empresas_consolidado, Divipola_general = divipola)
  empresas_consolidado
}

empresas_consolidado<-empresas_longitudinal("empresas_2021_3.csv",3,21)















colnames(empresas)<-c("tipo_identificacion_APT","numero_identificacion_APT",paste0("divipola",mes,"_",year),paste0("Total_cotizantes",mes,"_",year))
info_var_mes_completo<-merge(info_var_mes,info_var_mes_pre,by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE)
resumen_1<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pos))%>%group_by(Dpto1)%>%summarise(Total_aportantes = n(),total_cotizantes = sum(Total_cotizantes_pos,na.rm = TRUE),
                      total_apt_MICRO = sum(Micro_pos,na.rm = TRUE),total_apt_PEQUEÑA = sum(Pequeña_pos,na.rm = TRUE),total_apt_MEDIANA = sum(Mediana_pos,na.rm = TRUE),total_apt_GRANDE = sum(Grande_pos,na.rm = TRUE),total_apt_MUY_GRANDE = sum(Muy_grande_pos,na.rm = TRUE),
                      total_Seccion_A = sum(Seccion_A_pos,na.rm=TRUE),total_Seccion_B = sum(Seccion_B_pos,na.rm=TRUE),total_Seccion_C = sum(Seccion_C_pos,na.rm=TRUE),total_Seccion_D = sum(Seccion_D_pos,na.rm=TRUE),total_Seccion_E = sum(Seccion_E_pos,na.rm=TRUE),
                      total_Seccion_F = sum(Seccion_F_pos,na.rm=TRUE),total_Seccion_G = sum(Seccion_G_pos,na.rm=TRUE),total_Seccion_H = sum(Seccion_H_pos,na.rm=TRUE),total_Seccion_I = sum(Seccion_I_pos,na.rm=TRUE),total_Seccion_J = sum(Seccion_J_pos,na.rm=TRUE),
                      total_Seccion_K = sum(Seccion_K_pos,na.rm=TRUE),total_Seccion_L = sum(Seccion_L_pos,na.rm=TRUE),total_Seccion_M = sum(Seccion_M_pos,na.rm=TRUE),total_Seccion_N = sum(Seccion_N_pos,na.rm=TRUE),total_Seccion_O = sum(Seccion_O_pos,na.rm=TRUE),
                      total_Seccion_P = sum(Seccion_P_pos,na.rm=TRUE),total_Seccion_Q = sum(Seccion_Q_pos,na.rm=TRUE),total_Seccion_R = sum(Seccion_R_pos,na.rm=TRUE),total_Seccion_S = sum(Seccion_S_pos,na.rm=TRUE),total_Seccion_T = sum(Seccion_T_pos,na.rm=TRUE),
                      total_Seccion_U = sum(Seccion_U_pos,na.rm=TRUE)))
resumen_1_mpio<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pos))%>%group_by(Divipola1)%>%summarise(Total_aportantes = n(),total_cotizantes = sum(Total_cotizantes_pos,na.rm = TRUE),
                                                                                                                                        total_apt_MICRO = sum(Micro_pos,na.rm = TRUE),total_apt_PEQUEÑA = sum(Pequeña_pos,na.rm = TRUE),total_apt_MEDIANA = sum(Mediana_pos,na.rm = TRUE),total_apt_GRANDE = sum(Grande_pos,na.rm = TRUE),total_apt_MUY_GRANDE = sum(Muy_grande_pos,na.rm = TRUE),
                                                                                                                                       total_Seccion_A = sum(Seccion_A_pos,na.rm=TRUE),total_Seccion_B = sum(Seccion_B_pos,na.rm=TRUE),total_Seccion_C = sum(Seccion_C_pos,na.rm=TRUE),total_Seccion_D = sum(Seccion_D_pos,na.rm=TRUE),total_Seccion_E = sum(Seccion_E_pos,na.rm=TRUE),
                                                                                                                                       total_Seccion_F = sum(Seccion_F_pos,na.rm=TRUE),total_Seccion_G = sum(Seccion_G_pos,na.rm=TRUE),total_Seccion_H = sum(Seccion_H_pos,na.rm=TRUE),total_Seccion_I = sum(Seccion_I_pos,na.rm=TRUE),total_Seccion_J = sum(Seccion_J_pos,na.rm=TRUE),
                                                                                                                                       total_Seccion_K = sum(Seccion_K_pos,na.rm=TRUE),total_Seccion_L = sum(Seccion_L_pos,na.rm=TRUE),total_Seccion_M = sum(Seccion_M_pos,na.rm=TRUE),total_Seccion_N = sum(Seccion_N_pos,na.rm=TRUE),total_Seccion_O = sum(Seccion_O_pos,na.rm=TRUE),
                                                                                                                                       total_Seccion_P = sum(Seccion_P_pos,na.rm=TRUE),total_Seccion_Q = sum(Seccion_Q_pos,na.rm=TRUE),total_Seccion_R = sum(Seccion_R_pos,na.rm=TRUE),total_Seccion_S = sum(Seccion_S_pos,na.rm=TRUE),total_Seccion_T = sum(Seccion_T_pos,na.rm=TRUE),
                                                                                                                                       total_Seccion_U = sum(Seccion_U_pos,na.rm=TRUE)))
resumen_1_PRE<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pre))%>%group_by(Dpto1_pre)%>%summarise(Total_aportantes_pre = n(),total_cotizantes_pre = sum(Total_cotizantes_pre,na.rm = TRUE)))
resumen_1_PRE_mpio<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pre))%>%group_by(Divipola1_pre)%>%summarise(Total_aportantes_pre = n(),total_cotizantes_pre = sum(Total_cotizantes_pre,na.rm = TRUE)))
resumen_1<-merge(resumen_1,resumen_1_PRE,by.x = "Dpto1",by.y = "Dpto1_pre", all = TRUE)
resumen_1$var_mensual_apt<-(resumen_1$Total_aportantes-resumen_1$Total_aportantes_pre)/resumen_1$Total_aportantes_pre
resumen_1$var_mensual_cot<-(resumen_1$total_cotizantes-resumen_1$total_cotizantes_pre)/resumen_1$total_cotizantes_pre
resumen_1_mpio<-merge(resumen_1_mpio,resumen_1_PRE_mpio,by.x = "Divipola1",by.y = "Divipola1_pre", all = TRUE)
resumen_1_mpio$var_mensual_apt<-(resumen_1_mpio$Total_aportantes-resumen_1_mpio$Total_aportantes_pre)/resumen_1_mpio$Total_aportantes_pre
resumen_1_mpio$var_mensual_cot<-(resumen_1_mpio$total_cotizantes-resumen_1_mpio$total_cotizantes_pre)/resumen_1_mpio$total_cotizantes_pre

#variacon anual cambia periodo
tabla_anual<-"empresas_2020_10.csv"
info_var_mes_anual<-read.csv2(tabla_anual)#,sep = ',', dec = '.', na.strings = '$null$')#cambiar
info_var_mes_anual<-merge(info_var_mes_anual,Divipola_dpto,by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
info_var_mes_anual$divipola<-info_var_mes_anual$codigo_depto_Max_Max*1000+info_var_mes_anual$codigo_ciudad_Max_Max
info_var_mes_anual<-merge(info_var_mes_anual,Divipola_mpio,by.x = "divipola", by.y = "Municipio", all.x = TRUE)
info_var_mes_anual$Dpto1<-info_var_mes_anual$codigo_depto_Max_Max
info_var_mes_anual$Divipola1<-info_var_mes_anual$divipola
info_var_mes_anual[is.na(info_var_mes_anual$validacion_dpto),"Dpto1"]<-rep(0,nrow(info_var_mes_anual[is.na(info_var_mes_anual$validacion_dpto),]))
info_var_mes_anual[is.na(info_var_mes_anual$valida),"Divipola1"]<-rep(0,nrow(info_var_mes_anual[is.na(info_var_mes_anual$valida),]))
resumen_1_anual<-data.frame(info_var_mes_anual%>%group_by(Dpto1)%>%summarise(Total_aportantes_pre_anual = n(),total_cotizantes_pre_anual = sum(Total_cotizantes,na.rm = TRUE)))
resumen_1_anual_muni<-data.frame(info_var_mes_anual%>%group_by(Divipola1)%>%summarise(Total_aportantes_pre_anual = n(),total_cotizantes_pre_anual = sum(Total_cotizantes,na.rm = TRUE)))
#continua

resumen_1<-merge(resumen_1,resumen_1_anual,by = "Dpto1", all.x =TRUE)
resumen_1$var_anual_apt<-(resumen_1$Total_aportantes-resumen_1$Total_aportantes_pre_anual)/resumen_1$Total_aportantes_pre_anual
resumen_1$var_anual_cot<-(resumen_1$total_cotizantes-resumen_1$total_cotizantes_pre_anual)/resumen_1$total_cotizantes_pre_anual
resumen_1_mpio<-merge(resumen_1_mpio,resumen_1_anual_muni,by = "Divipola1", all.x =TRUE)
resumen_1_mpio$var_anual_apt<-(resumen_1_mpio$Total_aportantes-resumen_1_mpio$Total_aportantes_pre_anual)/resumen_1_mpio$Total_aportantes_pre_anual
resumen_1_mpio$var_anual_cot<-(resumen_1_mpio$total_cotizantes-resumen_1_mpio$total_cotizantes_pre_anual)/resumen_1_mpio$total_cotizantes_pre_anual

resumen_1_entran<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pos),is.na(info_var_mes_completo$Total_cotizantes_pre))%>%group_by(Dpto1)%>%summarise(Total_aportantes_in = n(),total_cotizantes_in = sum(Total_cotizantes_pos,na.rm = TRUE),
                      total_apt_MICRO = sum(Micro_pos,na.rm = TRUE),total_apt_PEQUEÑA = sum(Pequeña_pos,na.rm = TRUE),total_apt_MEDIANA = sum(Mediana_pos,na.rm = TRUE),total_apt_GRANDE = sum(Grande_pos,na.rm = TRUE),total_apt_MUY_GRANDE = sum(Muy_grande_pos,na.rm = TRUE),
                      total_Seccion_A = sum(Seccion_A_pos,na.rm=TRUE),total_Seccion_B = sum(Seccion_B_pos,na.rm=TRUE),total_Seccion_C = sum(Seccion_C_pos,na.rm=TRUE),total_Seccion_D = sum(Seccion_D_pos,na.rm=TRUE),total_Seccion_E = sum(Seccion_E_pos,na.rm=TRUE),
                      total_Seccion_F = sum(Seccion_F_pos,na.rm=TRUE),total_Seccion_G = sum(Seccion_G_pos,na.rm=TRUE),total_Seccion_H = sum(Seccion_H_pos,na.rm=TRUE),total_Seccion_I = sum(Seccion_I_pos,na.rm=TRUE),total_Seccion_J = sum(Seccion_J_pos,na.rm=TRUE),
                      total_Seccion_K = sum(Seccion_K_pos,na.rm=TRUE),total_Seccion_L = sum(Seccion_L_pos,na.rm=TRUE),total_Seccion_M = sum(Seccion_M_pos,na.rm=TRUE),total_Seccion_N = sum(Seccion_N_pos,na.rm=TRUE),total_Seccion_O = sum(Seccion_O_pos,na.rm=TRUE),
                      total_Seccion_P = sum(Seccion_P_pos,na.rm=TRUE),total_Seccion_Q = sum(Seccion_Q_pos,na.rm=TRUE),total_Seccion_R = sum(Seccion_R_pos,na.rm=TRUE),total_Seccion_S = sum(Seccion_S_pos,na.rm=TRUE),total_Seccion_T = sum(Seccion_T_pos,na.rm=TRUE),
                      total_Seccion_U = sum(Seccion_U_pos,na.rm=TRUE)))
resumen_1_salen<-data.frame(info_var_mes_completo%>%filter(is.na(info_var_mes_completo$Total_cotizantes_pos),!is.na(info_var_mes_completo$Total_cotizantes_pre))%>%group_by(Dpto1_pre)%>%summarise(Total_aportantes_out = n(),total_cotizantes_out = sum(Total_cotizantes_pre,na.rm = TRUE)))
resumen_1_permanecen<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pos),!is.na(info_var_mes_completo$Total_cotizantes_pre))%>%group_by(Dpto1)%>%summarise(Total_aportantes_permanecen = n(),total_cotizantes_per_pre = sum(Total_cotizantes_pre,na.rm = TRUE),total_cotizantes_per_pos = sum(Total_cotizantes_pos,na.rm = TRUE),
                     total_apt_MICRO = sum(Micro_pos,na.rm = TRUE),total_apt_PEQUEÑA = sum(Pequeña_pos,na.rm = TRUE),total_apt_MEDIANA = sum(Mediana_pos,na.rm = TRUE),total_apt_GRANDE = sum(Grande_pos,na.rm = TRUE),total_apt_MUY_GRANDE = sum(Muy_grande_pos,na.rm = TRUE)))

resumen_1_entran_1<-resumen_1_entran[,c("Dpto1","Total_aportantes_in","total_cotizantes_in")]
resumen_1_salen_1<-resumen_1_salen[,c("Dpto1_pre","Total_aportantes_out","total_cotizantes_out")]
resumen_1_permanecen_1<-resumen_1_permanecen[,c("Dpto1","Total_aportantes_permanecen","total_cotizantes_per_pre","total_cotizantes_per_pos")]
resumen_1<-merge(resumen_1,resumen_1_entran_1, by = "Dpto1", all = TRUE)
resumen_1<-merge(resumen_1,resumen_1_salen_1, by.x = "Dpto1", by.y = "Dpto1_pre", all = TRUE)
resumen_1<-merge(resumen_1,resumen_1_permanecen_1, by = "Dpto1", all = TRUE)
resumen_1$por_entran<-resumen_1$Total_aportantes_in/resumen_1$Total_aportantes
resumen_1$por_permanecen<-resumen_1$Total_aportantes_permanecen/resumen_1$Total_aportantes

resumen_1_entran_muni<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pos),is.na(info_var_mes_completo$Total_cotizantes_pre))%>%group_by(Divipola1)%>%summarise(Total_aportantes_in = n(),total_cotizantes_in = sum(Total_cotizantes_pos,na.rm = TRUE),
                                                                                                                                                                                                total_apt_MICRO = sum(Micro_pos,na.rm = TRUE),total_apt_PEQUEÑA = sum(Pequeña_pos,na.rm = TRUE),total_apt_MEDIANA = sum(Mediana_pos,na.rm = TRUE),total_apt_GRANDE = sum(Grande_pos,na.rm = TRUE),total_apt_MUY_GRANDE = sum(Muy_grande_pos,na.rm = TRUE),
                                                                                                                                                                                                total_Seccion_A = sum(Seccion_A_pos,na.rm=TRUE),total_Seccion_B = sum(Seccion_B_pos,na.rm=TRUE),total_Seccion_C = sum(Seccion_C_pos,na.rm=TRUE),total_Seccion_D = sum(Seccion_D_pos,na.rm=TRUE),total_Seccion_E = sum(Seccion_E_pos,na.rm=TRUE),
                                                                                                                                                                                                total_Seccion_F = sum(Seccion_F_pos,na.rm=TRUE),total_Seccion_G = sum(Seccion_G_pos,na.rm=TRUE),total_Seccion_H = sum(Seccion_H_pos,na.rm=TRUE),total_Seccion_I = sum(Seccion_I_pos,na.rm=TRUE),total_Seccion_J = sum(Seccion_J_pos,na.rm=TRUE),
                                                                                                                                                                                                total_Seccion_K = sum(Seccion_K_pos,na.rm=TRUE),total_Seccion_L = sum(Seccion_L_pos,na.rm=TRUE),total_Seccion_M = sum(Seccion_M_pos,na.rm=TRUE),total_Seccion_N = sum(Seccion_N_pos,na.rm=TRUE),total_Seccion_O = sum(Seccion_O_pos,na.rm=TRUE),
                                                                                                                                                                                                total_Seccion_P = sum(Seccion_P_pos,na.rm=TRUE),total_Seccion_Q = sum(Seccion_Q_pos,na.rm=TRUE),total_Seccion_R = sum(Seccion_R_pos,na.rm=TRUE),total_Seccion_S = sum(Seccion_S_pos,na.rm=TRUE),total_Seccion_T = sum(Seccion_T_pos,na.rm=TRUE),
                                                                                                                                                                                                total_Seccion_U = sum(Seccion_U_pos,na.rm=TRUE)))
resumen_1_salen_muni<-data.frame(info_var_mes_completo%>%filter(is.na(info_var_mes_completo$Total_cotizantes_pos),!is.na(info_var_mes_completo$Total_cotizantes_pre))%>%group_by(Divipola1_pre)%>%summarise(Total_aportantes_out = n(),total_cotizantes_out = sum(Total_cotizantes_pre,na.rm = TRUE)))
resumen_1_permanecen_muni<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pos),!is.na(info_var_mes_completo$Total_cotizantes_pre))%>%group_by(Divipola1)%>%summarise(Total_aportantes_permanecen = n(),total_cotizantes_per_pre = sum(Total_cotizantes_pre,na.rm = TRUE),total_cotizantes_per_pos = sum(Total_cotizantes_pos,na.rm = TRUE),
                            total_apt_MICRO = sum(Micro_pos,na.rm = TRUE),total_apt_PEQUEÑA = sum(Pequeña_pos,na.rm = TRUE),total_apt_MEDIANA = sum(Mediana_pos,na.rm = TRUE),total_apt_GRANDE = sum(Grande_pos,na.rm = TRUE),total_apt_MUY_GRANDE = sum(Muy_grande_pos,na.rm = TRUE)))

resumen_1_entran_muni_1<-resumen_1_entran_muni[,c("Divipola1","Total_aportantes_in","total_cotizantes_in")]
resumen_1_salen_muni_1<-resumen_1_salen_muni[,c("Divipola1_pre","Total_aportantes_out","total_cotizantes_out")]
resumen_1_permanecen_muni_1<-resumen_1_permanecen_muni[,c("Divipola1","Total_aportantes_permanecen","total_cotizantes_per_pre","total_cotizantes_per_pos")]
resumen_1_mpio<-merge(resumen_1_mpio,resumen_1_entran_muni_1, by = "Divipola1", all = TRUE)
resumen_1_mpio<-merge(resumen_1_mpio,resumen_1_salen_muni_1, by.x = "Divipola1", by.y = "Divipola1_pre", all = TRUE)
resumen_1_mpio<-merge(resumen_1_mpio,resumen_1_permanecen_muni_1, by = "Divipola1", all = TRUE)
resumen_1_mpio$por_entran<-resumen_1_mpio$Total_aportantes_in/resumen_1_mpio$Total_aportantes
resumen_1_mpio$por_permanecen<-resumen_1_mpio$Total_aportantes_permanecen/resumen_1_mpio$Total_aportantes

#primera salida##################################
resumen_1$por_salen<-resumen_1$Total_aportantes_out/resumen_1$Total_aportantes_pre
resumen_1_mpio$por_salen<-resumen_1_mpio$Total_aportantes_out/resumen_1_mpio$Total_aportantes_pre
#continua

info_var_mes_completo$retiran_todo<-info_var_mes_completo$Total_cotizantes_pre-info_var_mes_completo$retiro_Max_Sum_pre
resumen_2_salen_1<-data.frame(info_var_mes_completo%>%filter(is.na(info_var_mes_completo$Total_cotizantes_pos),!is.na(info_var_mes_completo$Total_cotizantes_pre),retiran_todo == 0)%>%group_by(Dpto1_pre)%>%summarise(Total_aportantes_out_retiran = n(),total_cotizantes_out_retiran = sum(Total_cotizantes_pre,na.rm = TRUE)))
resumen_2_salen_2<-data.frame(info_var_mes_completo%>%filter(is.na(info_var_mes_completo$Total_cotizantes_pos),!is.na(info_var_mes_completo$Total_cotizantes_pre),retiran_todo != 0)%>%group_by(Dpto1_pre)%>%summarise(Total_aportantes_out_NOretiran = n(),total_cotizantes_out_NOretiran = sum(Total_cotizantes_pre,na.rm = TRUE),Total_aportantes_out_NOretiran_cancelan = sum(Cancelada_inactiva_pre,na.rm = TRUE),Total_aportantes_out_NOretiran_activa = sum(Activa_pre,na.rm = TRUE)))
resumen_2_salen_0<-resumen_1[,c("Dpto1","Total_aportantes_out","total_cotizantes_out","por_salen")]
resumen_2<-merge(resumen_2_salen_0,resumen_2_salen_1, by.x = "Dpto1", by.y = "Dpto1_pre", all = TRUE)
resumen_2$por_salen_retiran<-resumen_2$Total_aportantes_out_retiran/resumen_2$Total_aportantes_out
resumen_2<-merge(resumen_2,resumen_2_salen_2, by.x = "Dpto1", by.y = "Dpto1_pre", all = TRUE)
resumen_2$por_salen_NO_retiran<-1-resumen_2$Total_aportantes_out_retiran/resumen_2$Total_aportantes_out
resumen_2$por_salen_NO_retiran_Cancelan<-resumen_2$Total_aportantes_out_NOretiran_cancelan/resumen_2$Total_aportantes_out_NOretiran
resumen_2$por_salen_NO_retiran_Activa<-resumen_2$Total_aportantes_out_NOretiran_activa/resumen_2$Total_aportantes_out_NOretiran
resumen_2<-resumen_2[,c("Dpto1","Total_aportantes_out","por_salen","total_cotizantes_out","Total_aportantes_out_retiran","por_salen_retiran","total_cotizantes_out_retiran","Total_aportantes_out_NOretiran","por_salen_NO_retiran","total_cotizantes_out_NOretiran","Total_aportantes_out_NOretiran_cancelan","por_salen_NO_retiran_Cancelan","Total_aportantes_out_NOretiran_activa","por_salen_NO_retiran_Activa")]

info_var_mes_completo_retira<-info_var_mes_completo[(is.na(info_var_mes_completo$Total_cotizantes_pos) & !is.na(info_var_mes_completo$Total_cotizantes_pre) & info_var_mes_completo$retiran_todo == 0),]
resumen2_salidas_retiran<-data.frame(info_var_mes_completo%>%filter(retiran_todo == 0)%>%group_by(Dpto1_pre)%>%summarise(Total_aportantes_retiran_todo = n(), Total_cotizantes_retiran_todo = sum(Total_cotizantes_pre)))
resumen2_salidas_NO_retiran<-data.frame(info_var_mes_completo%>%filter(retiran_todo != 0)%>%group_by(Dpto1_pre)%>%summarise(Total_aportantes_NO_retiran_todo = n(), 
                                        Total_cotizantes_NO_retiran_todo = sum(Total_cotizantes_pre, na.rm = TRUE),Total_aportante_cancelan = sum(Cancelada_inactiva_pre, na.rm =TRUE),Total_aportante_Activa = sum(Cancelada_inactiva_pre, na.rm =TRUE) ))
resumen2_salidas_NO_retiran_activa<-data.frame(info_var_mes_completo%>%filter(retiran_todo != 0,)%>%group_by(Dpto1_pre)%>%summarise(Total_aportantes_NO_retiran_todo = n(), Total_cotizantes_NO_retiran_todo = sum(Total_cotizantes_pre)))

informacion_1_19<-panel_fun("empresas_2019_1.csv","19_1")
informacion_2_19<-panel_fun("empresas_2019_2.csv","19_2")
informacion_3_19<-panel_fun("empresas_2019_3.csv","19_3")
informacion_4_19<-panel_fun("empresas_2019_4.csv","19_4")
informacion_5_19<-panel_fun("empresas_2019_5.csv","19_5")
informacion_6_19<-panel_fun("empresas_2019_6.csv","19_6")
informacion_7_19<-panel_fun("empresas_2019_7.csv","19_7")
informacion_8_19<-panel_fun("empresas_2019_8.csv","19_8")
informacion_9_19<-panel_fun("empresas_2019_9.csv","19_9")
informacion_10_19<-panel_fun("empresas_2019_10.csv","19_10")
informacion_11_19<-panel_fun("empresas_2019_11.csv","19_11")
informacion_12_19<-panel_fun("empresas_2019_12.csv","19_12")

informacion_1_20<-panel_fun("empresas_2020_1.csv","20_1")
informacion_2_20<-panel_fun("empresas_2020_2.csv","20_2")
informacion_3_20<-panel_fun("empresas_2020_3.csv","20_3")
informacion_4_20<-panel_fun("empresas_2020_4.csv","20_4")
informacion_5_20<-panel_fun("empresas_2020_5.csv","20_5")
informacion_6_20<-panel_fun("empresas_2020_6.csv","20_6")
informacion_7_20<-panel_fun("empresas_2020_7.csv","20_7")
informacion_8_20<-panel_fun("empresas_2020_8.csv","20_8")
informacion_9_20<-panel_fun("empresas_2020_9.csv","20_9")
informacion_10_20<-panel_fun("empresas_2020_10.csv","20_10")
informacion_11_20<-panel_fun("empresas_2020_11.csv","20_11")
informacion_12_20<-panel_fun("empresas_2020_12.csv","20_12")

informacion_1_21<-panel_fun("empresas_2021_1.csv","21_1")
informacion_2_21<-panel_fun("empresas_2021_2.csv","21_2")
informacion_3_21<-panel_fun("empresas_2021_3.csv","21_3")
informacion_4_21<-panel_fun("empresas_2021_4.csv","21_4")
informacion_5_21<-panel_fun("empresas_2021_5.csv","21_5")
informacion_6_21<-panel_fun("empresas_2021_6.csv","21_6")
informacion_7_21<-panel_fun("empresas_2021_7.csv","21_7")
informacion_8_21<-panel_fun("empresas_2021_8.csv","21_8")
informacion_9_21<-panel_fun("empresas_2021_9.csv","21_9")
informacion_10_21<-panel_fun("empresas_2021_10.csv","21_10")

consolidado_mpio<-merge(informacion_1_19$municipal,informacion_2_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_3_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_4_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_5_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_6_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_7_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_8_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_9_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_10_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_11_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_12_19$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_1_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_2_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_3_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_4_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_5_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_6_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_7_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_8_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_9_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_10_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_11_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_12_20$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_1_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_2_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_3_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_4_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_5_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_6_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_7_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_8_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_9_21$municipal, by = "Mpio", all = TRUE)
consolidado_mpio<-merge(consolidado_mpio,informacion_10_21$municipal, by = "Mpio", all = TRUE)

consolidado_dpto<-merge(informacion_1_19$departamental,informacion_2_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_3_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_4_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_5_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_6_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_7_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_8_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_9_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_10_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_11_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_12_19$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_1_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_2_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_3_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_4_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_5_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_6_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_7_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_8_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_9_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_10_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_11_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_12_20$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_1_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_2_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_3_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_4_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_5_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_6_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_7_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_8_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_9_21$departamental, by = "Dpto", all = TRUE)
consolidado_dpto<-merge(consolidado_dpto,informacion_10_21$departamental, by = "Dpto", all = TRUE)

consolidado_mpio$var_mensual_2_20_apt<-(consolidado_mpio$apt20_2-consolidado_mpio$apt20_1)/consolidado_mpio$apt20_1
consolidado_mpio$var_mensual_3_20_apt<-(consolidado_mpio$apt20_3-consolidado_mpio$apt20_2)/consolidado_mpio$apt20_2
consolidado_mpio$var_mensual_4_20_apt<-(consolidado_mpio$apt20_4-consolidado_mpio$apt20_3)/consolidado_mpio$apt20_3
consolidado_mpio$var_mensual_5_20_apt<-(consolidado_mpio$apt20_5-consolidado_mpio$apt20_4)/consolidado_mpio$apt20_4
consolidado_mpio$var_mensual_6_20_apt<-(consolidado_mpio$apt20_6-consolidado_mpio$apt20_5)/consolidado_mpio$apt20_5
consolidado_mpio$var_mensual_7_20_apt<-(consolidado_mpio$apt20_7-consolidado_mpio$apt20_6)/consolidado_mpio$apt20_6
consolidado_mpio$var_mensual_8_20_apt<-(consolidado_mpio$apt20_8-consolidado_mpio$apt20_7)/consolidado_mpio$apt20_7
consolidado_mpio$var_mensual_9_20_apt<-(consolidado_mpio$apt20_9-consolidado_mpio$apt20_8)/consolidado_mpio$apt20_8
consolidado_mpio$var_mensual_10_20_apt<-(consolidado_mpio$apt20_10-consolidado_mpio$apt20_9)/consolidado_mpio$apt20_9
consolidado_mpio$var_mensual_11_20_apt<-(consolidado_mpio$apt20_11-consolidado_mpio$apt20_10)/consolidado_mpio$apt20_10
consolidado_mpio$var_mensual_12_20_apt<-(consolidado_mpio$apt20_12-consolidado_mpio$apt20_11)/consolidado_mpio$apt20_11
consolidado_mpio$var_mensual_1_21_apt<-(consolidado_mpio$apt21_1-consolidado_mpio$apt20_12)/consolidado_mpio$apt20_12
consolidado_mpio$var_mensual_2_21_apt<-(consolidado_mpio$apt21_2-consolidado_mpio$apt21_1)/consolidado_mpio$apt21_1
consolidado_mpio$var_mensual_3_21_apt<-(consolidado_mpio$apt21_3-consolidado_mpio$apt21_2)/consolidado_mpio$apt21_2
consolidado_mpio$var_mensual_4_21_apt<-(consolidado_mpio$apt21_4-consolidado_mpio$apt21_3)/consolidado_mpio$apt21_3
consolidado_mpio$var_mensual_5_21_apt<-(consolidado_mpio$apt21_5-consolidado_mpio$apt21_4)/consolidado_mpio$apt21_4
consolidado_mpio$var_mensual_6_21_apt<-(consolidado_mpio$apt21_6-consolidado_mpio$apt21_5)/consolidado_mpio$apt21_5
consolidado_mpio$var_mensual_7_21_apt<-(consolidado_mpio$apt21_7-consolidado_mpio$apt21_6)/consolidado_mpio$apt21_6
consolidado_mpio$var_mensual_8_21_apt<-(consolidado_mpio$apt21_8-consolidado_mpio$apt21_7)/consolidado_mpio$apt21_7
consolidado_mpio$var_mensual_9_21_apt<-(consolidado_mpio$apt21_9-consolidado_mpio$apt21_8)/consolidado_mpio$apt21_8

consolidado_mpio$var_mensual_2_20_cot<-(consolidado_mpio$cot20_2-consolidado_mpio$cot20_1)/consolidado_mpio$cot20_1
consolidado_mpio$var_mensual_3_20_cot<-(consolidado_mpio$cot20_3-consolidado_mpio$cot20_2)/consolidado_mpio$cot20_2
consolidado_mpio$var_mensual_4_20_cot<-(consolidado_mpio$cot20_4-consolidado_mpio$cot20_3)/consolidado_mpio$cot20_3
consolidado_mpio$var_mensual_5_20_cot<-(consolidado_mpio$cot20_5-consolidado_mpio$cot20_4)/consolidado_mpio$cot20_4
consolidado_mpio$var_mensual_6_20_cot<-(consolidado_mpio$cot20_6-consolidado_mpio$cot20_5)/consolidado_mpio$cot20_5
consolidado_mpio$var_mensual_7_20_cot<-(consolidado_mpio$cot20_7-consolidado_mpio$cot20_6)/consolidado_mpio$cot20_6
consolidado_mpio$var_mensual_8_20_cot<-(consolidado_mpio$cot20_8-consolidado_mpio$cot20_7)/consolidado_mpio$cot20_7
consolidado_mpio$var_mensual_9_20_cot<-(consolidado_mpio$cot20_9-consolidado_mpio$cot20_8)/consolidado_mpio$cot20_8
consolidado_mpio$var_mensual_10_20_cot<-(consolidado_mpio$cot20_10-consolidado_mpio$cot20_9)/consolidado_mpio$cot20_9
consolidado_mpio$var_mensual_11_20_cot<-(consolidado_mpio$cot20_11-consolidado_mpio$cot20_10)/consolidado_mpio$cot20_10
consolidado_mpio$var_mensual_12_20_cot<-(consolidado_mpio$cot20_12-consolidado_mpio$cot20_11)/consolidado_mpio$cot20_11
consolidado_mpio$var_mensual_1_21_cot<-(consolidado_mpio$cot21_1-consolidado_mpio$cot20_12)/consolidado_mpio$cot20_12
consolidado_mpio$var_mensual_2_21_cot<-(consolidado_mpio$cot21_2-consolidado_mpio$cot21_1)/consolidado_mpio$cot21_1
consolidado_mpio$var_mensual_3_21_cot<-(consolidado_mpio$cot21_3-consolidado_mpio$cot21_2)/consolidado_mpio$cot21_2
consolidado_mpio$var_mensual_4_21_cot<-(consolidado_mpio$cot21_4-consolidado_mpio$cot21_3)/consolidado_mpio$cot21_3
consolidado_mpio$var_mensual_5_21_cot<-(consolidado_mpio$cot21_5-consolidado_mpio$cot21_4)/consolidado_mpio$cot21_4
consolidado_mpio$var_mensual_6_21_cot<-(consolidado_mpio$cot21_6-consolidado_mpio$cot21_5)/consolidado_mpio$cot21_5
consolidado_mpio$var_mensual_7_21_cot<-(consolidado_mpio$cot21_7-consolidado_mpio$cot21_6)/consolidado_mpio$cot21_6
consolidado_mpio$var_mensual_8_21_cot<-(consolidado_mpio$cot21_8-consolidado_mpio$cot21_7)/consolidado_mpio$cot21_7
consolidado_mpio$var_mensual_9_21_cot<-(consolidado_mpio$cot21_9-consolidado_mpio$cot21_8)/consolidado_mpio$cot21_8
consolidado_mpio_gen<-consolidado_mpio[,c(1,68:107)]

consolidado_dpto$var_mensual_2_20_apt<-(consolidado_dpto$apt20_2-consolidado_dpto$apt20_1)/consolidado_dpto$apt20_1
consolidado_dpto$var_mensual_3_20_apt<-(consolidado_dpto$apt20_3-consolidado_dpto$apt20_2)/consolidado_dpto$apt20_2
consolidado_dpto$var_mensual_4_20_apt<-(consolidado_dpto$apt20_4-consolidado_dpto$apt20_3)/consolidado_dpto$apt20_3
consolidado_dpto$var_mensual_5_20_apt<-(consolidado_dpto$apt20_5-consolidado_dpto$apt20_4)/consolidado_dpto$apt20_4
consolidado_dpto$var_mensual_6_20_apt<-(consolidado_dpto$apt20_6-consolidado_dpto$apt20_5)/consolidado_dpto$apt20_5
consolidado_dpto$var_mensual_7_20_apt<-(consolidado_dpto$apt20_7-consolidado_dpto$apt20_6)/consolidado_dpto$apt20_6
consolidado_dpto$var_mensual_8_20_apt<-(consolidado_dpto$apt20_8-consolidado_dpto$apt20_7)/consolidado_dpto$apt20_7
consolidado_dpto$var_mensual_9_20_apt<-(consolidado_dpto$apt20_9-consolidado_dpto$apt20_8)/consolidado_dpto$apt20_8
consolidado_dpto$var_mensual_10_20_apt<-(consolidado_dpto$apt20_10-consolidado_dpto$apt20_9)/consolidado_dpto$apt20_9
consolidado_dpto$var_mensual_11_20_apt<-(consolidado_dpto$apt20_11-consolidado_dpto$apt20_10)/consolidado_dpto$apt20_10
consolidado_dpto$var_mensual_12_20_apt<-(consolidado_dpto$apt20_12-consolidado_dpto$apt20_11)/consolidado_dpto$apt20_11
consolidado_dpto$var_mensual_1_21_apt<-(consolidado_dpto$apt21_1-consolidado_dpto$apt20_12)/consolidado_dpto$apt20_12
consolidado_dpto$var_mensual_2_21_apt<-(consolidado_dpto$apt21_2-consolidado_dpto$apt21_1)/consolidado_dpto$apt21_1
consolidado_dpto$var_mensual_3_21_apt<-(consolidado_dpto$apt21_3-consolidado_dpto$apt21_2)/consolidado_dpto$apt21_2
consolidado_dpto$var_mensual_4_21_apt<-(consolidado_dpto$apt21_4-consolidado_dpto$apt21_3)/consolidado_dpto$apt21_3
consolidado_dpto$var_mensual_5_21_apt<-(consolidado_dpto$apt21_5-consolidado_dpto$apt21_4)/consolidado_dpto$apt21_4
consolidado_dpto$var_mensual_6_21_apt<-(consolidado_dpto$apt21_6-consolidado_dpto$apt21_5)/consolidado_dpto$apt21_5
consolidado_dpto$var_mensual_7_21_apt<-(consolidado_dpto$apt21_7-consolidado_dpto$apt21_6)/consolidado_dpto$apt21_6
consolidado_dpto$var_mensual_8_21_apt<-(consolidado_dpto$apt21_8-consolidado_dpto$apt21_7)/consolidado_dpto$apt21_7
consolidado_dpto$var_mensual_9_21_apt<-(consolidado_dpto$apt21_9-consolidado_dpto$apt21_8)/consolidado_dpto$apt21_8
consolidado_dpto$var_mensual_10_21_apt<-(consolidado_dpto$apt21_10-consolidado_dpto$apt21_9)/consolidado_dpto$apt21_9

consolidado_dpto$var_mensual_2_20_cot<-(consolidado_dpto$cot20_2-consolidado_dpto$cot20_1)/consolidado_dpto$cot20_1
consolidado_dpto$var_mensual_3_20_cot<-(consolidado_dpto$cot20_3-consolidado_dpto$cot20_2)/consolidado_dpto$cot20_2
consolidado_dpto$var_mensual_4_20_cot<-(consolidado_dpto$cot20_4-consolidado_dpto$cot20_3)/consolidado_dpto$cot20_3
consolidado_dpto$var_mensual_5_20_cot<-(consolidado_dpto$cot20_5-consolidado_dpto$cot20_4)/consolidado_dpto$cot20_4
consolidado_dpto$var_mensual_6_20_cot<-(consolidado_dpto$cot20_6-consolidado_dpto$cot20_5)/consolidado_dpto$cot20_5
consolidado_dpto$var_mensual_7_20_cot<-(consolidado_dpto$cot20_7-consolidado_dpto$cot20_6)/consolidado_dpto$cot20_6
consolidado_dpto$var_mensual_8_20_cot<-(consolidado_dpto$cot20_8-consolidado_dpto$cot20_7)/consolidado_dpto$cot20_7
consolidado_dpto$var_mensual_9_20_cot<-(consolidado_dpto$cot20_9-consolidado_dpto$cot20_8)/consolidado_dpto$cot20_8
consolidado_dpto$var_mensual_10_20_cot<-(consolidado_dpto$cot20_10-consolidado_dpto$cot20_9)/consolidado_dpto$cot20_9
consolidado_dpto$var_mensual_11_20_cot<-(consolidado_dpto$cot20_11-consolidado_dpto$cot20_10)/consolidado_dpto$cot20_10
consolidado_dpto$var_mensual_12_20_cot<-(consolidado_dpto$cot20_12-consolidado_dpto$cot20_11)/consolidado_dpto$cot20_11
consolidado_dpto$var_mensual_1_21_cot<-(consolidado_dpto$cot21_1-consolidado_dpto$cot20_12)/consolidado_dpto$cot20_12
consolidado_dpto$var_mensual_2_21_cot<-(consolidado_dpto$cot21_2-consolidado_dpto$cot21_1)/consolidado_dpto$cot21_1
consolidado_dpto$var_mensual_3_21_cot<-(consolidado_dpto$cot21_3-consolidado_dpto$cot21_2)/consolidado_dpto$cot21_2
consolidado_dpto$var_mensual_4_21_cot<-(consolidado_dpto$cot21_4-consolidado_dpto$cot21_3)/consolidado_dpto$cot21_3
consolidado_dpto$var_mensual_5_21_cot<-(consolidado_dpto$cot21_5-consolidado_dpto$cot21_4)/consolidado_dpto$cot21_4
consolidado_dpto$var_mensual_6_21_cot<-(consolidado_dpto$cot21_6-consolidado_dpto$cot21_5)/consolidado_dpto$cot21_5
consolidado_dpto$var_mensual_7_21_cot<-(consolidado_dpto$cot21_7-consolidado_dpto$cot21_6)/consolidado_dpto$cot21_6
consolidado_dpto$var_mensual_8_21_cot<-(consolidado_dpto$cot21_8-consolidado_dpto$cot21_7)/consolidado_dpto$cot21_7
consolidado_dpto$var_mensual_9_21_cot<-(consolidado_dpto$cot21_9-consolidado_dpto$cot21_8)/consolidado_dpto$cot21_8
consolidado_dpto$var_mensual_10_21_cot<-(consolidado_dpto$cot21_10-consolidado_dpto$cot21_9)/consolidado_dpto$cot21_9
consolidado_dpto_gen<-consolidado_dpto[,c(1,68:107)]

#primera salida, serie
#consolidado_dpto_gen

info_var_mes<-read.csv2(tabla)
info_var_mes<-Estado_matricula(info_var_mes)
info_var_mes<-tamaño_empresa(info_var_mes)
info_var_mes<-Actividad_economica(info_var_mes)
info_var_mes<-info_var_mes[,c("tipo_identificacion_APT","numero_identificacion_APT","retiro_Max_Sum","ingreso_Max_Sum",
                              "codigo_ciudad_Max_Max","codigo_depto_Max_Max","codigo_actividad_economica_Max_Max",
                              "Total_cotizantes","EstadoMatricula","Activa","Cancelada_inactiva","Micro","Pequeña","Mediana","Grande","Muy_grande","Seccion","Seccion_A","Seccion_B","Seccion_C","Seccion_D","Seccion_E",
                              "Seccion_F","Seccion_G","Seccion_H","Seccion_I","Seccion_J","Seccion_K","Seccion_L","Seccion_M","Seccion_N","Seccion_O","Seccion_P","Seccion_Q","Seccion_R","Seccion_S","Seccion_T")]
colnames(info_var_mes)<-c("tipo_identificacion_APT","numero_identificacion_APT",paste0(colnames(info_var_mes[,3:ncol(info_var_mes)]),"_pos"))
info_var_mes<-merge(info_var_mes,Divipola_dpto,by.x = "codigo_depto_Max_Max_pos", by.y = "departamento", all.x = TRUE)
info_var_mes$divipola<-info_var_mes$codigo_depto_Max_Max_pos*1000+info_var_mes$codigo_ciudad_Max_Max_pos
info_var_mes<-merge(info_var_mes,Divipola_mpio,by.x = "codigo_ciudad_Max_Max_pos", by.y = "Municipio", all.x = TRUE)
info_var_mes$Dpto1<-info_var_mes$codigo_depto_Max_Max_pos
info_var_mes$Divipola1<-info_var_mes$divipola
info_var_mes[is.na(info_var_mes$validacion_dpto),"Dpto1"]<-rep(0,nrow(info_var_mes[is.na(info_var_mes$validacion_dpto),]))
info_var_mes[is.na(info_var_mes$valida),"Divipola1"]<-rep(0,nrow(info_var_mes[is.na(info_var_mes$valida),]))

info_var_mes_pre<-read.csv2(tabla_pre)
info_var_mes_pre<-Estado_matricula(info_var_mes_pre)
info_var_mes_pre<-tamaño_empresa(info_var_mes_pre)
info_var_mes_pre<-Actividad_economica(info_var_mes_pre)
info_var_mes_pre<-info_var_mes_pre[,c("tipo_identificacion_APT","numero_identificacion_APT","retiro_Max_Sum","ingreso_Max_Sum",
                              "codigo_ciudad_Max_Max","codigo_depto_Max_Max","codigo_actividad_economica_Max_Max",
                              "Total_cotizantes","EstadoMatricula","Activa","Cancelada_inactiva","Micro","Pequeña","Mediana","Grande","Muy_grande","Seccion","Seccion_A","Seccion_B","Seccion_C","Seccion_D","Seccion_E",
                              "Seccion_F","Seccion_G","Seccion_H","Seccion_I","Seccion_J","Seccion_K","Seccion_L","Seccion_M","Seccion_N","Seccion_O","Seccion_P","Seccion_Q","Seccion_R","Seccion_S","Seccion_T")]
colnames(info_var_mes_pre)<-c("tipo_identificacion_APT","numero_identificacion_APT",paste0(colnames(info_var_mes_pre[,3:ncol(info_var_mes_pre)]),"_pre"))

info_var_mes_completo<-merge(info_var_mes,info_var_mes_pre,by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE)
res_total_per<-data.frame(info_var_mes_completo%>%filter(!is.na(info_var_mes_completo$Total_cotizantes_pos),!is.na(info_var_mes_completo$Total_cotizantes_pos)%>%group_by()))

#Entran

resumen_in<-data.frame(entran%>%group_by())

info_var_mes<-merge(info_var_mes,Divipola_dpto, by.x = "codigo_depto_Max_Max", by.y = "departamento", all.x = TRUE)
info_var_mes<-merge(info_var_mes,Divipola_mpio, by.x = "divipola", by.y = "Municipio", all.x = TRUE)
info_var_mes$dpto_validacion<-info_var_mes$validacion_dpto*info_var_mes$codigo_depto_Max_Max
info_var_mes$mpio_validacion<-info_var_mes$valida*info_var_mes$divipola

resumen_dpto<-data.frame(info_var_mes%>%group_by(mpio_validacion)%>%summarise(Total_aportante))


resumen_muni<-data.frame(info_var_mes%>%group_by())


###Banrep 

#setwd("C:/Users/aemendoza/Documents/2021/tablero/Información/")
setwd("D:/Aplicacion/tablero/")
tabla<-"Resumen_general/Informacion_general_IBC8_10_21.csv"

informacion<-read.csv2("Resumen_general/Informacion_general_IBC8_10_21.csv")#,sep = ',', dec = '.')#version 2
resultado3_t_21<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count),Total_cotizantes_tot = sum(Total_relaciones_laborales, na.rm = TRUE),total_monto_general = ))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC4_7_21.csv",sep = ',', dec = '.')#version 2
resultado2_t_21<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/informe_general_IBC1_3_21.csv",sep = ',', dec = '.')#version 2
resultado1_t_21<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC8_12_20.csv")#,sep = ',', dec = '.')#version 2
resultado4_t_20<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC5_8_20.csv")#,sep = ',', dec = '.')#version 2
resultado3_t_20<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC1_4_20.csv")#,sep = ',', dec = '.')#version 2
resultado2_t_20<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC8_12_19.csv")#,sep = ',', dec = '.')#version 2
resultado4_t_19<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC5_8_19.csv")#,sep = ',', dec = '.')#version 2
resultado3_t_19<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC1_4_19.csv")#,sep = ',', dec = '.')#version 2
resultado2_t_19<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC9_12_18.csv")#,sep = ',', dec = '.')#version 2
resultado4_t_18<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC5_8_18.csv")#,sep = ',', dec = '.')#version 2
resultado3_t_18<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
informacion<-read.csv2("Resumen_general/Informacion_general_IBC1_4_18.csv")#,sep = ',', dec = '.')#version 2
resultado2_t_18<-data.frame(informacion%>%group_by(YEAR,MONTH,Rango_IBC)%>%summarise(Total_cotizantes = sum(cot_obligatoria_salud_Max_Sum, na.rm = TRUE),Total_monto_salud = sum(cot_obligatoria_salud_Max_Count)))
consolidado<-rbind(resultado3_t_21,resultado2_t_21,resultado1_t_21,resultado4_t_20,resultado3_t_20,resultado2_t_20,resultado4_t_19,resultado3_t_19,resultado2_t_19,resultado4_t_18,resultado3_t_18,resultado2_t_18)

write.xlsx(consolidado,"salida_serie_salud.xlsx")
tabla_incial<-function(tabla){
  informacion<-read.csv2(tabla) #CAMBIAR POR EXPORTACION MANUAL DE MODELER, PARA LOS CASOS DE 4 A 7 EN GENERAL
  informacion<-read.csv2(tabla,sep = ',', dec = '.')#version 2
  #informacion<-read.csv2(tabla)#version enero21
  resultados<-data.frame(informacion%>%group_by(Tipologia,YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
  resultados_dependientes<-data.frame(informacion%>%filter(Tipologia %in% c("Dep_No_priv","Dep_sec_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
  resultados_dependientes$Tipologia<-rep("Dependientes",nrow(resultados_dependientes))
  resultados_total<-data.frame(informacion%>%filter(Tipologia %in% c("Dep_No_priv","Dep_sec_priv","Independiente"))%>%group_by(YEAR,MONTH)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
  resultados_total$Tipologia<-rep("Total",nrow(resultados_dependientes))
  resultados<-rbind(resultados,resultados_dependientes,resultados_total)
  resultados
}


##
library(dplyr)
library(xlsx)
junio<-read.csv2("D:/Aplicacion/tablero/empresas/empresas_2021_6.csv",sep = ',', dec = '.', na.strings = '$null$')
mayo<-read.csv2("D:/Aplicacion/tablero/empresas/empresas_2021_5.csv",sep = ',', dec = '.', na.strings = '$null$')
abril<-read.csv2("D:/Aplicacion/tablero/empresas/empresas_2021_4.csv",sep = ',', dec = '.', na.strings = '$null$')
marzo<-read.csv2("D:/Aplicacion/tablero/empresas/empresas_2021_3.csv",sep = ',', dec = '.', na.strings = '$null$')

junio19<-read.csv2("D:/Aplicacion/tablero/empresas/empresas_2019_6.csv")#,sep = ',', dec = '.', na.strings = '$null$')
mayo19<-read.csv2("D:/Aplicacion/tablero/empresas/empresas_2019_5.csv")#,sep = ',', dec = '.', na.strings = '$null$')
abril19<-read.csv2("D:/Aplicacion/tablero/empresas/empresas_2019_4.csv")#,sep = ',', dec = '.', na.strings = '$null$')
marzo19<-read.csv2("D:/Aplicacion/tablero/empresas/empresas_2019_3.csv")#,sep = ',', dec = '.', na.strings = '$null$')

res_junio<-data.frame(junio%>%group_by(codigo_depto_Max_Max)%>%summarise(total_apt_junio = n(), total_cot_jun = sum(Total_cotizantes,na.rm = TRUE)))
res_mayo<-data.frame(mayo%>%group_by(codigo_depto_Max_Max)%>%summarise(total_apt_mayo = n(), total_cot_may = sum(Total_cotizantes,na.rm = TRUE)))
res_abril<-data.frame(abril%>%group_by(codigo_depto_Max_Max)%>%summarise(total_apt_abril = n(), total_cot_abr = sum(Total_cotizantes,na.rm = TRUE)))
res_marzo<-data.frame(marzo%>%group_by(codigo_depto_Max_Max)%>%summarise(total_apt_marzo = n(), total_cot_mar = sum(Total_cotizantes,na.rm = TRUE)))

res_junio19<-data.frame(junio19%>%group_by(codigo_depto_Max_Max)%>%summarise(total_apt_junio = n(), total_cot_jun = sum(Total_cotizantes,na.rm = TRUE)))
res_mayo19<-data.frame(mayo19%>%group_by(codigo_depto_Max_Max)%>%summarise(total_apt_mayo = n(), total_cot_may = sum(Total_cotizantes,na.rm = TRUE)))
res_abril19<-data.frame(abril19%>%group_by(codigo_depto_Max_Max)%>%summarise(total_apt_abril = n(), total_cot_abr = sum(Total_cotizantes,na.rm = TRUE)))
res_marzo19<-data.frame(marzo19%>%group_by(codigo_depto_Max_Max)%>%summarise(total_apt_marzo = n(), total_cot_mar = sum(Total_cotizantes,na.rm = TRUE)))

consolida_dpto<-merge(res_marzo,res_abril, by = "codigo_depto_Max_Max", all = TRUE)
consolida_dpto<-merge(consolida_dpto,res_mayo, by = "codigo_depto_Max_Max", all = TRUE)
consolida_dpto<-merge(consolida_dpto,res_junio, by = "codigo_depto_Max_Max", all = TRUE)

consolida_dpto19<-merge(res_marzo19,res_abril19, by = "codigo_depto_Max_Max", all = TRUE)
consolida_dpto19<-merge(consolida_dpto19,res_mayo19, by = "codigo_depto_Max_Max", all = TRUE)
consolida_dpto19<-merge(consolida_dpto19,res_junio19, by = "codigo_depto_Max_Max", all = TRUE)

write.xlsx(consolida_dpto,"D:/Aplicacion/tablero/empresas/salida_analisis.xlsx")
write.xlsx(consolida_dpto19,"D:/Aplicacion/tablero/empresas/salida_analisis19.xlsx")

seccion_mayo<-data.frame(mayo%>%group_by(codigo_actividad_economica_Max_Max)%>%summarise(total_aportantes_mayo = n(), total_cotizantes_mayo = sum(Total_cotizantes, na.rm = TRUE)))
seccion_junio<-data.frame(junio%>%group_by(codigo_actividad_economica_Max_Max)%>%summarise(total_aportantes_junio = n(), total_cotizantes_junio = sum(Total_cotizantes, na.rm = TRUE)))

seccion_56<-merge(seccion_mayo,seccion_junio, by = "codigo_actividad_economica_Max_Max",all = TRUE)
write.xlsx(seccion_56,"D:/Aplicacion/tablero/empresas/seccion_56.xlsx")

##################################
#SOLICITUD ANIF 
#APORTANTES
setwd("D:/Aplicacion/tablero/empresas/")
DATOS<-"empresas_2021_9.csv"
periodo_DATOS<-"_pos"
periodo_DATOS_pre<-"_pre"
DATOS_pre<-"empresas_2021_8.csv"
periodo<-"_2021_9"

solicitud_APT<-function(DATOS,DATOS_pre,periodo_DATOS,periodo_DATOS_pre,periodo){
  #informacion<-read.csv2(DATOS)#cambiar por segundas lecturas
  informacion<-read.csv2(DATOS,sep = ',', dec = '.', na.strings = '$null$')
  
  informacion[informacion$Total_cotizantes <= 10,"Size"]<-rep("Micro",nrow(informacion[informacion$Total_cotizantes <= 10,]))
  informacion[informacion$Total_cotizantes > 10 & informacion$Total_cotizantes <= 50,"Size"]<-rep("Pequeña",nrow(informacion[informacion$Total_cotizantes > 10 & informacion$Total_cotizantes <= 50,]))
  informacion[informacion$Total_cotizantes > 50 & informacion$Total_cotizantes <= 200,"Size"]<-rep("Mediana",nrow(informacion[informacion$Total_cotizantes > 50 & informacion$Total_cotizantes <= 200,]))
  informacion[informacion$Total_cotizantes > 200 & informacion$Total_cotizantes <= 500,"Size"]<-rep("Grande",nrow(informacion[informacion$Total_cotizantes > 200 & informacion$Total_cotizantes <= 500,]))
  informacion[informacion$Total_cotizantes > 500,"Size"]<-rep("Muy_grande",nrow(informacion[informacion$Total_cotizantes > 500,]))
  informacion<-merge(informacion,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
  informacion<-informacion[,c("tipo_identificacion_APT","numero_identificacion_APT","codigo_actividad_economica_Max_Max","Size","Seccion","Total_cotizantes","codigo_depto_Max_Max")]
  colnames(informacion)<-c(colnames(informacion[,c(1,2)]),paste0(colnames(informacion[,c(3,4,5,6,7)]),periodo_DATOS))
  #informacion_pre<-read.csv2(DATOS_pre)#cambiar en segunda lectura
  informacion_pre<-read.csv2(DATOS_pre,sep = ',', dec = '.', na.strings = '$null$')
  informacion_pre[informacion_pre$Total_cotizantes <= 10,"Size"]<-rep("Micro",nrow(informacion_pre[informacion_pre$Total_cotizantes <= 10,]))
  informacion_pre[informacion_pre$Total_cotizantes > 10 & informacion_pre$Total_cotizantes <= 50,"Size"]<-rep("Pequeña",nrow(informacion_pre[informacion_pre$Total_cotizantes > 10 & informacion_pre$Total_cotizantes <= 50,]))
  informacion_pre[informacion_pre$Total_cotizantes > 50 & informacion_pre$Total_cotizantes <= 200,"Size"]<-rep("Mediana",nrow(informacion_pre[informacion_pre$Total_cotizantes > 50 & informacion_pre$Total_cotizantes <= 200,]))
  informacion_pre[informacion_pre$Total_cotizantes > 200 & informacion_pre$Total_cotizantes <= 500,"Size"]<-rep("Grande",nrow(informacion_pre[informacion_pre$Total_cotizantes > 200 & informacion_pre$Total_cotizantes <= 500,]))
  informacion_pre[informacion_pre$Total_cotizantes > 500,"Size"]<-rep("Muy_grande",nrow(informacion_pre[informacion_pre$Total_cotizantes > 500,]))
  informacion_pre<-merge(informacion_pre,CIIU, by.x = "codigo_actividad_economica_Max_Max", by.y = "Clase", all.x = TRUE)
  informacion_pre<-informacion_pre[,c("tipo_identificacion_APT","numero_identificacion_APT","codigo_actividad_economica_Max_Max","Size","Seccion","Total_cotizantes","codigo_depto_Max_Max")]
  colnames(informacion_pre)<-c(colnames(informacion_pre[,c(1,2)]),paste0(colnames(informacion_pre[,c(3,4,5,6,7)]),periodo_DATOS_pre))
  completo<-merge(informacion,informacion_pre, by = c("tipo_identificacion_APT","numero_identificacion_APT"), all = TRUE)

  Entran<-completo[!is.na(completo$Total_cotizantes_pos) & is.na(completo$Total_cotizantes_pre),];nrow(Entran)
  Permanecen<-completo[!is.na(completo$Total_cotizantes_pos) & !is.na(completo$Total_cotizantes_pre),];nrow(Permanecen)
  Salen<-completo[is.na(completo$Total_cotizantes_pos) & !is.na(completo$Total_cotizantes_pre),];nrow(Salen)

  res_entran<-data.frame(Entran%>%group_by(Size_pos,Seccion_pos,codigo_actividad_economica_Max_Max_pos)%>%summarise(Total_apt_in = n()))
  res_permanecen<-data.frame(Permanecen%>%group_by(Size_pos,Seccion_pos,codigo_actividad_economica_Max_Max_pos)%>%summarise(Total_apt_per = n()))
  res_salen<-data.frame(Salen%>%group_by(Size_pre,Seccion_pre,codigo_actividad_economica_Max_Max_pre)%>%summarise(Total_apt_out = n()))
  resumen_panel<-merge(res_entran,res_permanecen, by = c("Size_pos","Seccion_pos","codigo_actividad_economica_Max_Max_pos"), all = TRUE)
  resumen_panel<-merge(resumen_panel,res_salen, by.x = c("Size_pos","Seccion_pos","codigo_actividad_economica_Max_Max_pos"), by.y = c("Size_pre","Seccion_pre","codigo_actividad_economica_Max_Max_pre"), all = TRUE)
  colnames(resumen_panel)<-c("Tamaño", "Seccion","Actividad_economica",paste0("Total_entran",periodo),paste0("Total_permanecen",periodo),paste0("Total_salen",periodo))

  res_entran_BTA<-data.frame(Entran%>%filter(codigo_depto_Max_Max_pos == 11)%>%group_by(Size_pos,Seccion_pos,codigo_actividad_economica_Max_Max_pos)%>%summarise(Total_apt_in = n()))
  res_permanecen_BTA<-data.frame(Permanecen%>%filter(codigo_depto_Max_Max_pos == 11)%>%group_by(Size_pos,Seccion_pos,codigo_actividad_economica_Max_Max_pos)%>%summarise(Total_apt_per = n()))
  res_salen_BTA<-data.frame(Salen%>%group_by(Size_pre,Seccion_pre,codigo_actividad_economica_Max_Max_pre)%>%summarise(Total_apt_out = n()))
  resumen_panel_BTA<-merge(res_entran_BTA,res_permanecen_BTA, by = c("Size_pos","Seccion_pos","codigo_actividad_economica_Max_Max_pos"), all = TRUE)
  resumen_panel_BTA<-merge(resumen_panel_BTA,res_salen_BTA, by.x = c("Size_pos","Seccion_pos","codigo_actividad_economica_Max_Max_pos"), by.y = c("Size_pre","Seccion_pre","codigo_actividad_economica_Max_Max_pre"), all = TRUE)
  colnames(resumen_panel_BTA)<-c("Tamaño", "Seccion","Actividad_economica",paste0("Total_entran",periodo),paste0("Total_permanecen",periodo),paste0("Total_salen",periodo))
  
  SALIDA_PANEL<-list(Nacional = resumen_panel, Bogota = resumen_panel_BTA)
  SALIDA_PANEL
}

salida2_19_ANIF<-solicitud_APT("empresas_2019_2.csv","empresas_2019_1.csv","_pos","_pre","_2019_2")
salida3_19_ANIF<-solicitud_APT("empresas_2019_3.csv","empresas_2019_2.csv","_pos","_pre","_2019_3")
salida4_19_ANIF<-solicitud_APT("empresas_2019_4.csv","empresas_2019_3.csv","_pos","_pre","_2019_4")
salida5_19_ANIF<-solicitud_APT("empresas_2019_5.csv","empresas_2019_4.csv","_pos","_pre","_2019_5")
salida6_19_ANIF<-solicitud_APT("empresas_2019_6.csv","empresas_2019_5.csv","_pos","_pre","_2019_6")
salida7_19_ANIF<-solicitud_APT("empresas_2019_7.csv","empresas_2019_6.csv","_pos","_pre","_2019_7")
salida8_19_ANIF<-solicitud_APT("empresas_2019_8.csv","empresas_2019_7.csv","_pos","_pre","_2019_8")
salida9_19_ANIF<-solicitud_APT("empresas_2019_9.csv","empresas_2019_8.csv","_pos","_pre","_2019_9")
salida10_19_ANIF<-solicitud_APT("empresas_2019_10.csv","empresas_2019_9.csv","_pos","_pre","_2019_10")
salida11_19_ANIF<-solicitud_APT("empresas_2019_11.csv","empresas_2019_10.csv","_pos","_pre","_2019_11")
salida12_19_ANIF<-solicitud_APT("empresas_2019_12.csv","empresas_2019_11.csv","_pos","_pre","_2019_12")

salida1_20_ANIF<-solicitud_APT("empresas_2020_1.csv","empresas_2019_12.csv","_pos","_pre","_2020_1")
salida2_20_ANIF<-solicitud_APT("empresas_2020_2.csv","empresas_2020_1.csv","_pos","_pre","_2020_2")
salida3_20_ANIF<-solicitud_APT("empresas_2020_3.csv","empresas_2020_2.csv","_pos","_pre","_2020_3")
salida4_20_ANIF<-solicitud_APT("empresas_2020_4.csv","empresas_2020_3.csv","_pos","_pre","_2020_4")
salida5_20_ANIF<-solicitud_APT("empresas_2020_5.csv","empresas_2020_4.csv","_pos","_pre","_2020_5")
salida6_20_ANIF<-solicitud_APT("empresas_2020_6.csv","empresas_2020_5.csv","_pos","_pre","_2020_6")
salida7_20_ANIF<-solicitud_APT("empresas_2020_7.csv","empresas_2020_6.csv","_pos","_pre","_2020_7")
salida8_20_ANIF<-solicitud_APT("empresas_2020_8.csv","empresas_2020_7.csv","_pos","_pre","_2020_8")
salida9_20_ANIF<-solicitud_APT("empresas_2020_9.csv","empresas_2020_8.csv","_pos","_pre","_2020_9")
salida10_20_ANIF<-solicitud_APT("empresas_2020_10.csv","empresas_2020_9.csv","_pos","_pre","_2020_10")
salida11_20_ANIF<-solicitud_APT("empresas_2020_11.csv","empresas_2020_10.csv","_pos","_pre","_2020_11")
salida12_20_ANIF<-solicitud_APT("empresas_2020_12.csv","empresas_2020_11.csv","_pos","_pre","_2020_12")

salida1_21_ANIF<-solicitud_APT("empresas_2021_1.csv","empresas_2020_12.csv","_pos","_pre","_2021_1")
salida2_21_ANIF<-solicitud_APT("empresas_2021_2.csv","empresas_2021_1.csv","_pos","_pre","_2021_2")
salida3_21_ANIF<-SALIDA_PANEL
salida4_21_ANIF<-SALIDA_PANEL#solicitud_APT("empresas_2021_4.csv","empresas_2021_3.csv","_pos","_pre","_2021_4")
salida5_21_ANIF<-SALIDA_PANEL#solicitud_APT("empresas_2021_5.csv","empresas_2021_4.csv","_pos","_pre","_2021_5")
salida6_21_ANIF<-SALIDA_PANEL#solicitud_APT("empresas_2021_6.csv","empresas_2021_5.csv","_pos","_pre","_2021_6")
salida7_21_ANIF<-SALIDA_PANEL#solicitud_APT("empresas_2021_7.csv","empresas_2021_6.csv","_pos","_pre","_2021_7")
salida8_21_ANIF<-SALIDA_PANEL#solicitud_APT("empresas_2021_8.csv","empresas_2021_7.csv","_pos","_pre","_2021_8")
salida9_21_ANIF<-SALIDA_PANEL#solicitud_APT("empresas_2021_9.csv","empresas_2021_8.csv","_pos","_pre","_2021_9")
salida10_21_ANIF<-SALIDA_PANEL#solicitud_APT("empresas_2021_10.csv","empresas_2021_9.csv","_pos","_pre","_2021_10")

salidas_APT_anifnal<-merge(salida2_19_ANIF$Nacional,salida3_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salida2_19_ANIF$Bogota,salida3_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida4_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida4_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida5_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida5_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida6_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida6_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida7_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida7_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida8_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida8_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida9_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida9_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida10_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida10_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida11_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida11_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida12_19_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida12_19_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);

salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida1_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida1_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida2_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida2_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida3_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida3_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida4_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida4_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida5_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida5_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida6_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida6_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida7_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida7_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida8_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida8_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida9_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida9_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida10_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida10_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida11_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida11_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida12_20_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida12_20_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);

salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida1_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida1_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida2_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida2_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida3_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida3_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida4_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida4_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida5_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida5_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida6_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida6_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida7_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida7_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida8_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida8_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida9_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida9_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);
salidas_APT_anifnal<-merge(salidas_APT_anifnal,salida10_21_ANIF$Nacional, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);salidas_APT_anifBTA<-merge(salidas_APT_anifBTA,salida10_21_ANIF$Bogota, by = c("Tamaño","Seccion","Actividad_economica"), all = TRUE);

salidas_APT_anifnal[is.na(salidas_APT_anifnal)]<-0
salidas_APT_anifBTA[is.na(salidas_APT_anifBTA)]<-0

write.xlsx(salidas_APT_anifnal,"C:/Users/aemendoza/Documents/2022/Enero/Cotizaciones/solicitud_aportantes.xlsx",sheetName = "Nacional")
write.xlsx(salidas_APT_anifBTA,"C:/Users/aemendoza/Documents/2022/Enero/Cotizaciones/solicitud_aportantes.xlsx",sheetName = "Bogota", append = TRUE)

#COTIZANTES
setwd("D:/Aplicacion/tablero/Resumen_general/")
DATOS<-"informe_general_IBC1_3_21_.csv"

informacion<-read.csv2(DATOS)#,sep = ',', dec = '.')#version 2


#########################################################################################################################
#############presentacion 

library(xlsx)
library(dplyr)
library(tidyverse)
library(DT)
library(kableExtra)
library(knitr)

datos_tipo<-read.xlsx("D:/Nuevos_Insumos_IMC/Insumos_general_presentacion.xlsx",sheetName = "type_year")
informacion9_11_21<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC9_11_2021.csv")
informacion5_8_21<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC5_8_2021.csv")
informacion1_4_21<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC1_4_2021.csv")
informacion9_12_20<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC9_12_2020.csv")
informacion5_8_20<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC5_8_2020.csv")
informacion1_4_20<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC1_4_2020.csv")
informacion9_12_19<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC9_12_2019.csv")
informacion5_8_19<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC5_8_2019.csv")
informacion1_4_19<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC1_4_2019.csv")
informacion9_12_18<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC9_12_2018.csv")
informacion5_8_18<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC5_8_2018.csv")
informacion1_4_18<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC1_4_2018.csv")

independientes<-rbind(data.frame(informacion9_11_21%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_21%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_21%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_20%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_20%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_20%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_19%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_19%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_19%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_18%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_18%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_18%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))))
dependientes<-rbind(data.frame(informacion9_11_21%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_21%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_21%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_20%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_20%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_20%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_19%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_19%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_19%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_18%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_18%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_18%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))))
Dep_sec_priv<-rbind(data.frame(informacion9_11_21%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_21%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_21%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_20%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_20%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_20%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_19%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_19%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_19%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion9_12_18%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion5_8_18%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
                      data.frame(informacion1_4_18%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))))
consolidado_tot<-merge(independientes,dependientes, by = c("YEAR","MONTH"), all = TRUE)
consolidado_tot<-merge(consolidado_tot,Dep_sec_priv, by = c("YEAR","MONTH"), all = TRUE)
consolidado_tot$TOTAL_COT<-consolidado_tot$Total_ind+consolidado_tot$Total_dep
consolidado_tot<-consolidado_tot[order(consolidado_tot$YEAR,consolidado_tot$MONTH),]
consolidado_tot$var_anual_tot<-rep(0,nrow(consolidado_tot))
consolidado_tot$var_mensual_tot<-rep(0,nrow(consolidado_tot))
consolidado_tot$var_anual_ind<-rep(0,nrow(consolidado_tot))
consolidado_tot$var_mensual_ind<-rep(0,nrow(consolidado_tot))
consolidado_tot$var_anual_dep<-rep(0,nrow(consolidado_tot))
consolidado_tot$var_mensual_dep<-rep(0,nrow(consolidado_tot))
consolidado_tot$var_anual_dep_sec_priv<-rep(0,nrow(consolidado_tot))
consolidado_tot$var_mensual_dep_sec_priv<-rep(0,nrow(consolidado_tot))

for(p in 2:nrow(consolidado_tot)){
  consolidado_tot$var_mensual_tot[p]<-(consolidado_tot$TOTAL_COT[p]-consolidado_tot$TOTAL_COT[(p-1)])/(consolidado_tot$TOTAL_COT[(p-1)])
  consolidado_tot$var_mensual_ind[p]<-(consolidado_tot$Total_ind[p]-consolidado_tot$Total_ind[(p-1)])/(consolidado_tot$Total_ind[(p-1)])
  consolidado_tot$var_mensual_dep[p]<-(consolidado_tot$Total_dep[p]-consolidado_tot$Total_dep[(p-1)])/(consolidado_tot$Total_dep[(p-1)])
  consolidado_tot$var_mensual_dep_sec_priv[p]<-(consolidado_tot$Total_sec_priv[p]-consolidado_tot$Total_sec_priv[(p-1)])/(consolidado_tot$Total_sec_priv[(p-1)])
}
for(y in c(2019,2020,2021)){
  for(m in 1:12){
    consolidado_tot[consolidado_tot$YEAR == y & consolidado_tot$MONTH == m,"var_anual_tot"]<-(consolidado_tot[consolidado_tot$YEAR == y & consolidado_tot$MONTH == m,"TOTAL_COT"]-consolidado_tot[consolidado_tot$YEAR == (y-1) & consolidado_tot$MONTH == m,"TOTAL_COT"])/consolidado_tot[consolidado_tot$YEAR == (y-1) & consolidado_tot$MONTH == m,"TOTAL_COT"]
    consolidado_tot[consolidado_tot$YEAR == y & consolidado_tot$MONTH == m,"var_anual_ind"]<-(consolidado_tot[consolidado_tot$YEAR == y & consolidado_tot$MONTH == m,"Total_ind"]-consolidado_tot[consolidado_tot$YEAR == (y-1) & consolidado_tot$MONTH == m,"Total_ind"])/consolidado_tot[consolidado_tot$YEAR == (y-1) & consolidado_tot$MONTH == m,"Total_ind"]
    consolidado_tot[consolidado_tot$YEAR == y & consolidado_tot$MONTH == m,"var_anual_dep"]<-(consolidado_tot[consolidado_tot$YEAR == y & consolidado_tot$MONTH == m,"Total_dep"]-consolidado_tot[consolidado_tot$YEAR == (y-1) & consolidado_tot$MONTH == m,"Total_dep"])/consolidado_tot[consolidado_tot$YEAR == (y-1) & consolidado_tot$MONTH == m,"Total_dep"]
    consolidado_tot[consolidado_tot$YEAR == y & consolidado_tot$MONTH == m,"var_anual_dep_sec_priv"]<-(consolidado_tot[consolidado_tot$YEAR == y & consolidado_tot$MONTH == m,"Total_sec_priv"]-consolidado_tot[consolidado_tot$YEAR == (y-1) & consolidado_tot$MONTH == m,"Total_sec_priv"])/consolidado_tot[consolidado_tot$YEAR == (y-1) & consolidado_tot$MONTH == m,"Total_sec_priv"]
  }
}

consolidado_tot$Total_indp<-formatC(consolidado_tot$Total_ind/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
consolidado_tot$Total_depp<-formatC(consolidado_tot$Total_dep/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
consolidado_tot$Total_sec_privp<-formatC(consolidado_tot1$Total_sec_priv/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
consolidado_tot$TOTAL_COTp<-formatC(consolidado_tot$TOTAL_COT/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
consolidado_tot$var_anual_totp<-paste0(formatC(consolidado_tot$var_anual_tot*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%")
consolidado_tot$var_mensual_totp<-paste0(formatC(consolidado_tot$var_mensual_tot*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%")
consolidado_tot$var_anual_indp<-paste0(formatC(consolidado_tot$var_anual_ind*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%")
consolidado_tot$var_mensual_indp<-paste0(formatC(consolidado_tot$var_mensual_ind*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%")
consolidado_tot$var_anual_depp<-paste0(formatC(consolidado_tot$var_anual_dep*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%")
consolidado_tot$var_mensual_depp<-paste0(formatC(consolidado_tot$var_mensual_dep*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%")
consolidado_tot$var_anual_dep_sec_privp<-paste0(formatC(consolidado_tot$var_anual_dep_sec_priv*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%")
consolidado_tot$var_mensual_dep_sec_privp<-paste0(formatC(consolidado_tot$var_mensual_dep_sec_priv*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%")
mes2<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mes_rep<-data.frame(c(1:12),mes2);colnames(mes_rep)<-c("month","mes2")
consolidado_tot<-merge(consolidado_tot,mes_rep, by.x = "MONTH", by.y = "month", all.x = TRUE)

mes_ref = 11

#tabla1_1
tabla1_1<-consolidado_tot%>%filter(MONTH == mes_ref)%>%
  select(YEAR,TOTAL_COTp,var_anual_totp,var_mensual_totp,Total_depp,var_anual_depp,var_mensual_depp,Total_indp,var_anual_indp,var_mensual_indp)%>%
  arrange(desc(YEAR))%>%
  kbl(caption = "Comportamiento del periodo de análisis", align = "lccccccccc", 
    col.names = c("Año","Total cotizantes","Var anual","Var mensual","Total cotizantes","Var anual","Var mensual","Total cotizantes","Var anual","Var mensual"))%>%
  kable_classic(full_width = F, html_font = "Arial")%>%
  row_spec(row = 1,bold = TRUE)%>%
  add_header_above(c(" ","Total cotizantes" = 3,"Dependientes" = 3,"Independientes" = 3))


tabla1_2<-
consolidado_tot<-consolidado_tot[order(consolidado_tot$YEAR,consolidado_tot$MONTH),]
consolidado_tot$x<-seq(1,nrow(consolidado_tot))
var_anual<-consolidado_tot[,c("YEAR","MONTH","mes2","var_anual_tot","var_anual_dep","var_anual_ind","var_anual_dep_sec_priv")]
var_anual$var_anual_totp<-var_anual$var_anual_tot*100;var_anual$var_anual_depp<-var_anual$var_anual_dep*100;var_anual$var_anual_indp<-var_anual$var_anual_ind*100
var_anual$Año<-paste0(var_anual$YEAR,".")
var_mensual<-consolidado_tot[,c("YEAR","MONTH","mes2","var_mensual_tot","var_mensual_dep","var_mensual_ind","var_mensual_dep_sec_priv")]
var_mensual$var_mensual_totp<-var_mensual$var_mensual_tot*100;var_mensual$var_mensual_depp<-var_mensual$var_mensual_dep*100;var_mensual$var_mensual_indp<-var_mensual$var_mensual_ind*100
var_mensual$Año<-paste0(var_mensual$YEAR,".")

library(reshape)
library(ggplot2)
library(gridExtra)
min_var_anual = floor(min(var_anual[,c("var_anual_totp","var_anual_depp","var_anual_indp")]));max_var_anual = ceiling(max(var_anual[,c("var_anual_totp","var_anual_depp","var_anual_indp")]))
var_anualt<-ggplot(var_anual[var_anual$YEAR>=2019,],aes(x = MONTH, y = var_anual_totp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 6), text = element_text(size = 8))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion Anual (%)",limits = c(min_var_anual,max_var_anual))+
  labs(title ="Variaciones anuales", subtitle = "Todos los cotizantes")+
  scale_color_manual(values = c("grey60","black", "blue"))
var_anuald<-ggplot(var_anual[var_anual$YEAR>=2019,],aes(x = MONTH, y = var_anual_depp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 6), text = element_text(size = 8))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion Anual (%)",limits = c(min_var_anual,max_var_anual))+
  labs(title ="Variaciones anuales", subtitle = "Dependientes")+
  scale_color_manual(values = c("grey60","black", "blue"))
var_anuali<-ggplot(var_anual[var_anual$YEAR>=2019,],aes(x = MONTH, y = var_anual_indp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 6), text = element_text(size = 8))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion Anual (%)",limits = c(min_var_anual,max_var_anual))+
  labs(title ="Variaciones anuales", subtitle = "Independientes")+
  scale_color_manual(values = c("grey60","black", "blue"))

min_var_mensual = floor(min(var_mensual[,c("var_mensual_totp","var_mensual_depp","var_mensual_indp")]));max_var_mensual = ceiling(max(var_mensual[,c("var_mensual_totp","var_mensual_depp","var_mensual_indp")]))
var_mensualt<-ggplot(var_mensual,aes(x = MONTH, y = var_mensual_totp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 6),text = element_text(size = 8))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion mensual (%)",limits = c(min_var_mensual,max_var_mensual))+
  labs(title ="Variaciones mensuales", subtitle = "Todos los cotizantes")+
  scale_color_manual(values = c("grey70","grey40","black", "red"))
var_mensuald<-ggplot(var_mensual,aes(x = MONTH, y = var_mensual_depp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 6),text = element_text(size = 8))+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion mensual (%)",limits = c(min_var_mensual,max_var_mensual))+
  labs(title ="Variaciones mensuales", subtitle = "Dependientes")+
  scale_color_manual(values = c("grey70","grey40","black", "red"))
var_mensuali<-ggplot(var_mensual,aes(x = MONTH, y = var_mensual_indp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 6), text = element_text(size = 8))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion mensual (%)",limits = c(min_var_mensual,max_var_mensual))+
  labs(title ="Variaciones mensuales", subtitle = "Independientes")+
  scale_color_manual(values = c("grey70","grey40","black", "red"))

grid.arrange(var_anualt,var_anuald,var_anuali,var_mensualt,var_mensuald,var_mensuali,ncol = 3)

####serie con los total totales 

datos_tipo<-read.xlsx("D:/Nuevos_Insumos_IMC/Insumos_general_presentacion.xlsx",sheetName = "type_year")

## Panel de empresas 

setwd("D:/Nuevos_Insumos_IMC/empresas/")
Tabla = "empresas_2021_12.csv"
Mpios = read.csv2("D:/Aplicacion/tablero/Ejercicios adicionales/municipios.csv")
Dptos = read.csv2("D:/Aplicacion/tablero/Ejercicios adicionales/departamentos.csv")
Trans_seccion<-read.csv2("Secciones_economicas_desc.csv")
CIIU<-read.xlsx("DetalleCIIU.xlsx", sheetName = "Secciontotal")
year = 2021
month = 12

salida_company<-function(Tabla,year,month){
tabla_empresa<-read.csv2(Tabla)
resumen_empresa_mpio<-data.frame(tabla_empresa%>%group_by(codigo_depto_Max_Max,codigo_ciudad_Max_Max)%>%summarise(Total_apt = n(), total_cot = sum(Total_cotizantes), total_retiro = sum(retiro_Max_Sum,na.rm = TRUE), total_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE)))
colnames(resumen_empresa_mpio)<-c("codigo_depto_Max_Max","codigo_ciudad_Max_Max", paste0(colnames(resumen_empresa_mpio[,3:ncol(resumen_empresa_mpio)]),"_",month,"_",year))
resumen_empresa_dpto<-data.frame(tabla_empresa%>%group_by(codigo_depto_Max_Max)%>%summarise(Total_apt = n(), total_cot = sum(Total_cotizantes), total_retiro = sum(retiro_Max_Sum,na.rm = TRUE), total_ingresos = sum(ingreso_Max_Sum,na.rm = TRUE)))
colnames(resumen_empresa_dpto)<-c("codigo_depto_Max_Max", paste0(colnames(resumen_empresa_dpto[,2:ncol(resumen_empresa_dpto)]),"_",month,"_",year))
salida<-list(municipal = resumen_empresa_mpio, departamental = resumen_empresa_dpto)
salida
}

empresas_12_21<-salida_company("empresas_2021_12.csv",2021,12)
empresas_11_21<-salida_company("empresas_2021_11.csv",2021,11)
empresas_10_21<-salida_company("empresas_2021_10.csv",2021,10)
empresas_9_21<-salida_company("empresas_2021_9.csv",2021,9)

panel_mpio<-merge(empresas_9_21$municipal,empresas_10_21$municipal, by = c("codigo_depto_Max_Max","codigo_ciudad_Max_Max"), all = TRUE)
panel_mpio<-merge(panel_mpio,empresas_11_21$municipal, by = c("codigo_depto_Max_Max","codigo_ciudad_Max_Max"), all = TRUE)
panel_mpio<-merge(panel_mpio,empresas_12_21$municipal, by = c("codigo_depto_Max_Max","codigo_ciudad_Max_Max"), all = TRUE)

panel_DPTO<-merge(empresas_9_21$departamental,empresas_10_21$departamental, by = "codigo_depto_Max_Max", all = TRUE)
panel_DPTO<-merge(panel_DPTO,empresas_11_21$departamental, by = "codigo_depto_Max_Max", all = TRUE)
panel_DPTO<-merge(panel_DPTO,empresas_12_21$departamental, by = "codigo_depto_Max_Max", all = TRUE)

write.xlsx(panel_mpio,"C:/Users/aemendoza/Documents/2022/Marzo/cotizacion/informacion_panel.xlsx",sheetName = "Municipal")
write.xlsx(panel_DPTO,"C:/Users/aemendoza/Documents/2022/Marzo/cotizacion/informacion_panel.xlsx",sheetName = "Departamental", append = TRUE)

##################################################################################################
##############formato 

setwd("D:/Nuevos_Insumos_IMC/Informacion_general/")
orden_salida_rango<-data.frame(Rango_IBC2 = c("<1SMMLV","1SMMLV","1_2SMMLV","2_3SMMLV","3_4SMMLV","4_5SMMLV",">5SMMLV"),salida = 1:7)
departamentos<-read.csv2("D:/Nuevos_Insumos_IMC/departamentos.csv")

#febrero20
febrero_2020demo<-read.csv2("Salidas_generales_edad_sexo1_4_2020.csv");nrow(febrero_2020demo)
mujeres_FEB_20<-data.frame(caracteristica = "Mujer", Total_cotizantes_feb_20 = febrero_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == 2020, MONTH == 2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_FEB_20<-data.frame(caracteristica = "Hombre", Total_cotizantes_feb_20 = febrero_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == 2020, MONTH == 2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_FEB_20<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_feb_20 = febrero_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 2, Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_FEB_20<-data.frame(caracteristica = "Mayores40", Total_cotizantes_feb_20 = febrero_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 2, Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_feb_20<-data.frame(caracteristica = "SLN", Total_cotizantes_feb_20 = febrero_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 2)%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_feb_20<-data.frame(caracteristica = "Ingresos", Total_cotizantes_feb_20 = febrero_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 2)%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_feb_20<-data.frame(caracteristica = "Retiros", Total_cotizantes_feb_20 = febrero_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 2)%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
febrero_2020ibc<-read.csv2("Salidas_generales_rangoIBC1_4_2020.csv");nrow(febrero_2020demo)
febrero_2020ibc$Rango_IBC2<-NULL
febrero_2020ibc[febrero_2020ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(febrero_2020ibc[febrero_2020ibc$Rango_IBC == "0.500000",]))
febrero_2020ibc[febrero_2020ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(febrero_2020ibc[febrero_2020ibc$Rango_IBC == "1.000000",]))
febrero_2020ibc[febrero_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(febrero_2020ibc[febrero_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
febrero_2020ibc[febrero_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(febrero_2020ibc[febrero_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
febrero_2020ibc[febrero_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(febrero_2020ibc[febrero_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
febrero_2020ibc[febrero_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(febrero_2020ibc[febrero_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
febrero_2020ibc[febrero_2020ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(febrero_2020ibc[febrero_2020ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_feb_20<-data.frame(febrero_2020ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 2)%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_feb_20<-merge(res_rango_salarial_feb_20,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_feb_20<-res_rango_salarial_feb_20[order(res_rango_salarial_feb_20$salida),-3]
colnames(res_rango_salarial_feb_20)<-c("caracteristica","Total_cotizantes")
febrero_2020dpto<-read.csv2("Salidas_generales_DPTO_MPIO1_4_2020.csv");nrow(febrero_2020dpto)
res_deparamentos_feb_20<-data.frame(febrero_2020dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 2)%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_feb_20<-merge(res_deparamentos_feb_20,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_feb_20<-res_deparamentos_feb_20%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_feb_20)<-c("caracteristica","Total_cotizantes")
Consolidado_feb_20<-rbind(mujeres_FEB_20,hombres_FEB_20,jovenes_FEB_20,mayores_FEB_20,res_rango_salarial_feb_20,res_deparamentos_feb_20,SLN_feb_20,ingresos_feb_20,retiros_feb_20)
colnames(Consolidado_feb_20)<-c("caracteristica","Total_cot_02_20")

#marzo20
marzo_2020demo<-read.csv2("Salidas_generales_edad_sexo1_4_2020.csv");nrow(marzo_2020demo)
mujeres_mar_20<-data.frame(caracteristica = "Mujer", Total_cotizantes_mar_20 = marzo_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == 2020, MONTH == 3)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_mar_20<-data.frame(caracteristica = "Hombre", Total_cotizantes_mar_20 = marzo_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == 2020, MONTH == 3)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_mar_20<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_mar_20 = marzo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 3, Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_mar_20<-data.frame(caracteristica = "Mayores40", Total_cotizantes_mar_20 = marzo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 3, Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_mar_20<-data.frame(caracteristica = "SLN", Total_cotizantes_mar_20 = marzo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 3)%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_mar_20<-data.frame(caracteristica = "Ingresos", Total_cotizantes_mar_20 = marzo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 3)%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_mar_20<-data.frame(caracteristica = "Retiros", Total_cotizantes_mar_20 = marzo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 3)%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
marzo_2020ibc<-read.csv2("Salidas_generales_rangoIBC1_4_2020.csv");nrow(marzo_2020demo)
marzo_2020ibc$Rango_IBC2<-NULL
marzo_2020ibc[marzo_2020ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(marzo_2020ibc[marzo_2020ibc$Rango_IBC == "0.500000",]))
marzo_2020ibc[marzo_2020ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(marzo_2020ibc[marzo_2020ibc$Rango_IBC == "1.000000",]))
marzo_2020ibc[marzo_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(marzo_2020ibc[marzo_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
marzo_2020ibc[marzo_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(marzo_2020ibc[marzo_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
marzo_2020ibc[marzo_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(marzo_2020ibc[marzo_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
marzo_2020ibc[marzo_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(marzo_2020ibc[marzo_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
marzo_2020ibc[marzo_2020ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(marzo_2020ibc[marzo_2020ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_mar_20<-data.frame(marzo_2020ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 3)%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_mar_20<-merge(res_rango_salarial_mar_20,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_mar_20<-res_rango_salarial_mar_20[order(res_rango_salarial_mar_20$salida),-3]
colnames(res_rango_salarial_mar_20)<-c("caracteristica","Total_cotizantes")
marzo_2020dpto<-read.csv2("Salidas_generales_DPTO_MPIO1_4_2020.csv");nrow(marzo_2020dpto)
res_deparamentos_mar_20<-data.frame(marzo_2020dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 3)%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_mar_20<-merge(res_deparamentos_mar_20,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_mar_20<-res_deparamentos_mar_20%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_mar_20)<-c("caracteristica","Total_cotizantes")
Consolidado_mar_20<-rbind(mujeres_mar_20,hombres_mar_20,jovenes_mar_20,mayores_mar_20,res_rango_salarial_mar_20,res_deparamentos_mar_20,SLN_mar_20,ingresos_mar_20,retiros_mar_20)
colnames(Consolidado_mar_20)<-c("caracteristica","Total_cot_03_20")

#abril20
abril_2020demo<-read.csv2("Salidas_generales_edad_sexo1_4_2020.csv");nrow(abril_2020demo)
mujeres_abr_20<-data.frame(caracteristica = "Mujer", Total_cotizantes_abr_20 = abril_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == 2020, MONTH == 4)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_abr_20<-data.frame(caracteristica = "Hombre", Total_cotizantes_abr_20 = abril_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == 2020, MONTH == 4)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_abr_20<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_abr_20 = abril_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 4, Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_abr_20<-data.frame(caracteristica = "Mayores40", Total_cotizantes_abr_20 = abril_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 4, Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_abr_20<-data.frame(caracteristica = "SLN", Total_cotizantes_abr_20 = abril_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 4)%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_abr_20<-data.frame(caracteristica = "Ingresos", Total_cotizantes_abr_20 = abril_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 4)%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_abr_20<-data.frame(caracteristica = "Retiros", Total_cotizantes_abr_20 = abril_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 4)%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
abril_2020ibc<-read.csv2("Salidas_generales_rangoIBC1_4_2020.csv");nrow(abril_2020demo)
abril_2020ibc$Rango_IBC2<-NULL
abril_2020ibc[abril_2020ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(abril_2020ibc[abril_2020ibc$Rango_IBC == "0.500000",]))
abril_2020ibc[abril_2020ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(abril_2020ibc[abril_2020ibc$Rango_IBC == "1.000000",]))
abril_2020ibc[abril_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(abril_2020ibc[abril_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
abril_2020ibc[abril_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(abril_2020ibc[abril_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
abril_2020ibc[abril_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(abril_2020ibc[abril_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
abril_2020ibc[abril_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(abril_2020ibc[abril_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
abril_2020ibc[abril_2020ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(abril_2020ibc[abril_2020ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_abr_20<-data.frame(abril_2020ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 4)%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_abr_20<-merge(res_rango_salarial_abr_20,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_abr_20<-res_rango_salarial_abr_20[order(res_rango_salarial_abr_20$salida),-3]
colnames(res_rango_salarial_abr_20)<-c("caracteristica","Total_cotizantes")
abril_2020dpto<-read.csv2("Salidas_generales_DPTO_MPIO1_4_2020.csv");nrow(abril_2020dpto)
res_deparamentos_abr_20<-data.frame(abril_2020dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 4)%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_abr_20<-merge(res_deparamentos_abr_20,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_abr_20<-res_deparamentos_abr_20%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_abr_20)<-c("caracteristica","Total_cotizantes")
Consolidado_abr_20<-rbind(mujeres_abr_20,hombres_abr_20,jovenes_abr_20,mayores_abr_20,res_rango_salarial_abr_20,res_deparamentos_abr_20,SLN_abr_20,ingresos_abr_20,retiros_abr_20)
colnames(Consolidado_abr_20)<-c("caracteristica","Total_cot_04_20")

#MAYO20
mayo_2020demo<-read.csv2("Salidas_generales_edad_sexo5_8_2020.csv");nrow(mayo_2020demo)
mujeres_may_20<-data.frame(caracteristica = "Mujer", Total_cotizantes_may_20 = mayo_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == 2020, MONTH == 5)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_may_20<-data.frame(caracteristica = "Hombre", Total_cotizantes_may_20 = mayo_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == 2020, MONTH == 5)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_may_20<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_may_20 = mayo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 5, Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_may_20<-data.frame(caracteristica = "Mayores40", Total_cotizantes_may_20 = mayo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 5, Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_may_20<-data.frame(caracteristica = "SLN", Total_cotizantes_may_20 = mayo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 5)%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_may_20<-data.frame(caracteristica = "Ingresos", Total_cotizantes_may_20 = mayo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 5)%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_may_20<-data.frame(caracteristica = "Retiros", Total_cotizantes_may_20 = mayo_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 5)%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
mayo_2020ibc<-read.csv2("Salidas_generales_rangoIBC5_8_2020.csv");nrow(mayo_2020demo)
mayo_2020ibc$Rango_IBC2<-NULL
mayo_2020ibc[mayo_2020ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(mayo_2020ibc[mayo_2020ibc$Rango_IBC == "0.500000",]))
mayo_2020ibc[mayo_2020ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(mayo_2020ibc[mayo_2020ibc$Rango_IBC == "1.000000",]))
mayo_2020ibc[mayo_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(mayo_2020ibc[mayo_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
mayo_2020ibc[mayo_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(mayo_2020ibc[mayo_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
mayo_2020ibc[mayo_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(mayo_2020ibc[mayo_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
mayo_2020ibc[mayo_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(mayo_2020ibc[mayo_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
mayo_2020ibc[mayo_2020ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(mayo_2020ibc[mayo_2020ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_may_20<-data.frame(mayo_2020ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 5)%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_may_20<-merge(res_rango_salarial_may_20,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_may_20<-res_rango_salarial_may_20[order(res_rango_salarial_may_20$salida),-3]
colnames(res_rango_salarial_may_20)<-c("caracteristica","Total_cotizantes")
mayo_2020dpto<-read.csv2("Salidas_generales_DPTO_MPIO5_8_2020.csv");nrow(mayo_2020dpto)
res_deparamentos_may_20<-data.frame(mayo_2020dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 5)%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_may_20<-merge(res_deparamentos_may_20,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_may_20<-res_deparamentos_may_20%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_may_20)<-c("caracteristica","Total_cotizantes")
Consolidado_may_20<-rbind(mujeres_may_20,hombres_may_20,jovenes_may_20,mayores_may_20,res_rango_salarial_may_20,res_deparamentos_may_20,SLN_may_20,ingresos_may_20,retiros_may_20)
colnames(Consolidado_may_20)<-c("caracteristica","Total_cot_05_20")

#junio20
junio_2020demo<-read.csv2("Salidas_generales_edad_sexo5_8_2020.csv");nrow(junio_2020demo)
mujeres_jun_20<-data.frame(caracteristica = "Mujer", Total_cotizantes_jun_20 = junio_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == 2020, MONTH == 6)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_jun_20<-data.frame(caracteristica = "Hombre", Total_cotizantes_jun_20 = junio_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == 2020, MONTH == 6)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_jun_20<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_jun_20 = junio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 6, Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_jun_20<-data.frame(caracteristica = "Mayores40", Total_cotizantes_jun_20 = junio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 6, Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_jun_20<-data.frame(caracteristica = "SLN", Total_cotizantes_jun_20 = junio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 6)%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_jun_20<-data.frame(caracteristica = "Ingresos", Total_cotizantes_jun_20 = junio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 6)%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_jun_20<-data.frame(caracteristica = "Retiros", Total_cotizantes_jun_20 = junio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 6)%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
junio_2020ibc<-read.csv2("Salidas_generales_rangoIBC5_8_2020.csv");nrow(junio_2020demo)
junio_2020ibc$Rango_IBC2<-NULL
junio_2020ibc[junio_2020ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(junio_2020ibc[junio_2020ibc$Rango_IBC == "0.500000",]))
junio_2020ibc[junio_2020ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(junio_2020ibc[junio_2020ibc$Rango_IBC == "1.000000",]))
junio_2020ibc[junio_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(junio_2020ibc[junio_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
junio_2020ibc[junio_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(junio_2020ibc[junio_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
junio_2020ibc[junio_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(junio_2020ibc[junio_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
junio_2020ibc[junio_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(junio_2020ibc[junio_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
junio_2020ibc[junio_2020ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(junio_2020ibc[junio_2020ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_jun_20<-data.frame(junio_2020ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 6)%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_jun_20<-merge(res_rango_salarial_jun_20,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_jun_20<-res_rango_salarial_jun_20[order(res_rango_salarial_jun_20$salida),-3]
colnames(res_rango_salarial_jun_20)<-c("caracteristica","Total_cotizantes")
junio_2020dpto<-read.csv2("Salidas_generales_DPTO_MPIO5_8_2020.csv");nrow(junio_2020dpto)
res_deparamentos_jun_20<-data.frame(junio_2020dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 6)%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_jun_20<-merge(res_deparamentos_jun_20,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_jun_20<-res_deparamentos_jun_20%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_jun_20)<-c("caracteristica","Total_cotizantes")
Consolidado_jun_20<-rbind(mujeres_jun_20,hombres_jun_20,jovenes_jun_20,mayores_jun_20,res_rango_salarial_jun_20,res_deparamentos_jun_20,SLN_jun_20,ingresos_jun_20,retiros_jun_20)
colnames(Consolidado_jun_20)<-c("caracteristica","Total_cot_06_20")

#julio20
julio_2020demo<-read.csv2("Salidas_generales_edad_sexo5_8_2020.csv");nrow(julio_2020demo)
mujeres_jul_20<-data.frame(caracteristica = "Mujer", Total_cotizantes_jul_20 = julio_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == 2020, MONTH == 7)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_jul_20<-data.frame(caracteristica = "Hombre", Total_cotizantes_jul_20 = julio_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == 2020, MONTH == 7)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_jul_20<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_jul_20 = julio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 7, Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_jul_20<-data.frame(caracteristica = "Mayores40", Total_cotizantes_jul_20 = julio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 7, Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_jul_20<-data.frame(caracteristica = "SLN", Total_cotizantes_jul_20 = julio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 7)%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_jul_20<-data.frame(caracteristica = "Ingresos", Total_cotizantes_jul_20 = julio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 7)%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_jul_20<-data.frame(caracteristica = "Retiros", Total_cotizantes_jul_20 = julio_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 7)%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
julio_2020ibc<-read.csv2("Salidas_generales_rangoIBC5_8_2020.csv");nrow(julio_2020demo)
julio_2020ibc$Rango_IBC2<-NULL
julio_2020ibc[julio_2020ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(julio_2020ibc[julio_2020ibc$Rango_IBC == "0.500000",]))
julio_2020ibc[julio_2020ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(julio_2020ibc[julio_2020ibc$Rango_IBC == "1.000000",]))
julio_2020ibc[julio_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(julio_2020ibc[julio_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
julio_2020ibc[julio_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(julio_2020ibc[julio_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
julio_2020ibc[julio_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(julio_2020ibc[julio_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
julio_2020ibc[julio_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(julio_2020ibc[julio_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
julio_2020ibc[julio_2020ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(julio_2020ibc[julio_2020ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_jul_20<-data.frame(julio_2020ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 7)%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_jul_20<-merge(res_rango_salarial_jul_20,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_jul_20<-res_rango_salarial_jul_20[order(res_rango_salarial_jul_20$salida),-3]
colnames(res_rango_salarial_jul_20)<-c("caracteristica","Total_cotizantes")
julio_2020dpto<-read.csv2("Salidas_generales_DPTO_MPIO5_8_2020.csv");nrow(julio_2020dpto)
res_deparamentos_jul_20<-data.frame(julio_2020dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 7)%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_jul_20<-merge(res_deparamentos_jul_20,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_jul_20<-res_deparamentos_jul_20%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_jul_20)<-c("caracteristica","Total_cotizantes")
Consolidado_jul_20<-rbind(mujeres_jul_20,hombres_jul_20,jovenes_jul_20,mayores_jul_20,res_rango_salarial_jul_20,res_deparamentos_jul_20,SLN_jul_20,ingresos_jul_20,retiros_jul_20)
colnames(Consolidado_jul_20)<-c("caracteristica","Total_cot_07_20")

#agosto20
agosto_2020demo<-read.csv2("Salidas_generales_edad_sexo5_8_2020.csv");nrow(agosto_2020demo)
mujeres_ago_20<-data.frame(caracteristica = "Mujer", Total_cotizantes_ago_20 = agosto_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == 2020, MONTH == 8)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_ago_20<-data.frame(caracteristica = "Hombre", Total_cotizantes_ago_20 = agosto_2020demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == 2020, MONTH == 8)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_ago_20<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_ago_20 = agosto_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 8, Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_ago_20<-data.frame(caracteristica = "Mayores40", Total_cotizantes_ago_20 = agosto_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 8, Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_ago_20<-data.frame(caracteristica = "SLN", Total_cotizantes_ago_20 = agosto_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 8)%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_ago_20<-data.frame(caracteristica = "Ingresos", Total_cotizantes_ago_20 = agosto_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 8)%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_ago_20<-data.frame(caracteristica = "Retiros", Total_cotizantes_ago_20 = agosto_2020demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 8)%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
agosto_2020ibc<-read.csv2("Salidas_generales_rangoIBC5_8_2020.csv");nrow(agosto_2020demo)
agosto_2020ibc$Rango_IBC2<-NULL
agosto_2020ibc[agosto_2020ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(agosto_2020ibc[agosto_2020ibc$Rango_IBC == "0.500000",]))
agosto_2020ibc[agosto_2020ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(agosto_2020ibc[agosto_2020ibc$Rango_IBC == "1.000000",]))
agosto_2020ibc[agosto_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(agosto_2020ibc[agosto_2020ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
agosto_2020ibc[agosto_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(agosto_2020ibc[agosto_2020ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
agosto_2020ibc[agosto_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(agosto_2020ibc[agosto_2020ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
agosto_2020ibc[agosto_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(agosto_2020ibc[agosto_2020ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
agosto_2020ibc[agosto_2020ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(agosto_2020ibc[agosto_2020ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_ago_20<-data.frame(agosto_2020ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 8)%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_ago_20<-merge(res_rango_salarial_ago_20,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_ago_20<-res_rango_salarial_ago_20[order(res_rango_salarial_ago_20$salida),-3]
colnames(res_rango_salarial_ago_20)<-c("caracteristica","Total_cotizantes")
agosto_2020dpto<-read.csv2("Salidas_generales_DPTO_MPIO5_8_2020.csv");nrow(agosto_2020dpto)
res_deparamentos_ago_20<-data.frame(agosto_2020dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == 2020, MONTH == 8)%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_ago_20<-merge(res_deparamentos_ago_20,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_ago_20<-res_deparamentos_ago_20%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_ago_20)<-c("caracteristica","Total_cotizantes")
Consolidado_ago_20<-rbind(mujeres_ago_20,hombres_ago_20,jovenes_ago_20,mayores_ago_20,res_rango_salarial_ago_20,res_deparamentos_ago_20,SLN_ago_20,ingresos_ago_20,retiros_ago_20)
colnames(Consolidado_ago_20)<-c("caracteristica","Total_cot_08_20")

####situacion actual
año = 2021
mes = 12
#octubre2021
octubre_year21demo<-read.csv2("Salidas_generales_edad_sexo9_12_2021_.csv");nrow(octubre_year21demo)
mujeres_oct_year<-data.frame(caracteristica = "Mujer", Total_cotizantes_oct_year = octubre_year21demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == año, MONTH == (mes-2))%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_oct_year<-data.frame(caracteristica = "Hombre", Total_cotizantes_oct_year = octubre_year21demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == año, MONTH == (mes-2))%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_oct_year<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_oct_year = octubre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-2), Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_oct_year<-data.frame(caracteristica = "Mayores40", Total_cotizantes_oct_year = octubre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-2), Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_oct_year<-data.frame(caracteristica = "SLN", Total_cotizantes_oct_year = octubre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-2))%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_oct_year<-data.frame(caracteristica = "Ingresos", Total_cotizantes_oct_year = octubre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-2))%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_oct_year<-data.frame(caracteristica = "Retiros", Total_cotizantes_oct_year = octubre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-2))%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
octubre_year21ibc<-read.csv2("Salidas_generales_rangoIBC9_12_2021_.csv");nrow(octubre_year21demo)
octubre_year21ibc$Rango_IBC2<-NULL
octubre_year21ibc[octubre_year21ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(octubre_year21ibc[octubre_year21ibc$Rango_IBC == "0.500000",]))
octubre_year21ibc[octubre_year21ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(octubre_year21ibc[octubre_year21ibc$Rango_IBC == "1.000000",]))
octubre_year21ibc[octubre_year21ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(octubre_year21ibc[octubre_year21ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
octubre_year21ibc[octubre_year21ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(octubre_year21ibc[octubre_year21ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
octubre_year21ibc[octubre_year21ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(octubre_year21ibc[octubre_year21ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
octubre_year21ibc[octubre_year21ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(octubre_year21ibc[octubre_year21ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
octubre_year21ibc[octubre_year21ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(octubre_year21ibc[octubre_year21ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_oct_year<-data.frame(octubre_year21ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-2))%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_oct_year<-merge(res_rango_salarial_oct_year,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_oct_year<-res_rango_salarial_oct_year[order(res_rango_salarial_oct_year$salida),-3]
colnames(res_rango_salarial_oct_year)<-c("caracteristica","Total_cotizantes")
octubre_year21dpto<-read.csv2("Salidas_generales_DPTO_MPIO9_12_2021_.csv");nrow(octubre_year21dpto)
res_deparamentos_oct_year<-data.frame(octubre_year21dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-2))%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_oct_year<-merge(res_deparamentos_oct_year,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_oct_year<-res_deparamentos_oct_year%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_oct_year)<-c("caracteristica","Total_cotizantes")
Consolidado_oct_year<-rbind(mujeres_oct_year,hombres_oct_year,jovenes_oct_year,mayores_oct_year,res_rango_salarial_oct_year,res_deparamentos_oct_year,SLN_oct_year,ingresos_oct_year,retiros_oct_year)
colnames(Consolidado_oct_year)<-c("caracteristica","Total_cot_10_2021")

#noviembre2021
noviembre_year21demo<-read.csv2("Salidas_generales_edad_sexo9_12_2021_.csv");nrow(noviembre_year21demo)
mujeres_nov_year<-data.frame(caracteristica = "Mujer", Total_cotizantes_nov_year = noviembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == año, MONTH == (mes-1))%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_nov_year<-data.frame(caracteristica = "Hombre", Total_cotizantes_nov_year = noviembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == año, MONTH == (mes-1))%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_nov_year<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_nov_year = noviembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-1), Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_nov_year<-data.frame(caracteristica = "Mayores40", Total_cotizantes_nov_year = noviembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-1), Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_nov_year<-data.frame(caracteristica = "SLN", Total_cotizantes_nov_year = noviembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-1))%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_nov_year<-data.frame(caracteristica = "Ingresos", Total_cotizantes_nov_year = noviembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-1))%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_nov_year<-data.frame(caracteristica = "Retiros", Total_cotizantes_nov_year = noviembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-1))%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
noviembre_year21ibc<-read.csv2("Salidas_generales_rangoIBC9_12_2021_.csv");nrow(noviembre_year21demo)
noviembre_year21ibc$Rango_IBC2<-NULL
noviembre_year21ibc[noviembre_year21ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(noviembre_year21ibc[noviembre_year21ibc$Rango_IBC == "0.500000",]))
noviembre_year21ibc[noviembre_year21ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(noviembre_year21ibc[noviembre_year21ibc$Rango_IBC == "1.000000",]))
noviembre_year21ibc[noviembre_year21ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(noviembre_year21ibc[noviembre_year21ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
noviembre_year21ibc[noviembre_year21ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(noviembre_year21ibc[noviembre_year21ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
noviembre_year21ibc[noviembre_year21ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(noviembre_year21ibc[noviembre_year21ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
noviembre_year21ibc[noviembre_year21ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(noviembre_year21ibc[noviembre_year21ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
noviembre_year21ibc[noviembre_year21ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(noviembre_year21ibc[noviembre_year21ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_nov_year<-data.frame(noviembre_year21ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-1))%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_nov_year<-merge(res_rango_salarial_nov_year,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_nov_year<-res_rango_salarial_nov_year[order(res_rango_salarial_nov_year$salida),-3]
colnames(res_rango_salarial_nov_year)<-c("caracteristica","Total_cotizantes")
noviembre_year21dpto<-read.csv2("Salidas_generales_DPTO_MPIO9_12_2021_.csv");nrow(noviembre_year21dpto)
res_deparamentos_nov_year<-data.frame(noviembre_year21dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-1))%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_nov_year<-merge(res_deparamentos_nov_year,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_nov_year<-res_deparamentos_nov_year%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_nov_year)<-c("caracteristica","Total_cotizantes")
Consolidado_nov_year<-rbind(mujeres_nov_year,hombres_nov_year,jovenes_nov_year,mayores_nov_year,res_rango_salarial_nov_year,res_deparamentos_nov_year,SLN_nov_year,ingresos_nov_year,retiros_nov_year)
colnames(Consolidado_nov_year)<-c("caracteristica","Total_cot_11_2021")

#diciembre2021
diciembre_year21demo<-read.csv2("Salidas_generales_edad_sexo9_12_2021_.csv");nrow(diciembre_year21demo)
mujeres_dic_year<-data.frame(caracteristica = "Mujer", Total_cotizantes_dic_year = diciembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "F", YEAR == año, MONTH == (mes-0))%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
hombres_dic_year<-data.frame(caracteristica = "Hombre", Total_cotizantes_dic_year = diciembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", Sexo_fin == "M", YEAR == año, MONTH == (mes-0))%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
jovenes_dic_year<-data.frame(caracteristica = "Jovenes(18-28)", Total_cotizantes_dic_year = diciembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-0), Edad_fin>= 18 , Edad_fin<= 28)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
mayores_dic_year<-data.frame(caracteristica = "Mayores40", Total_cotizantes_dic_year = diciembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-0), Edad_fin> 40)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
SLN_dic_year<-data.frame(caracteristica = "SLN", Total_cotizantes_dic_year = diciembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-0))%>%summarise(Total_cotizantes = sum(suspension_temporal_Max_Sum, na.rm = TRUE)))
ingresos_dic_year<-data.frame(caracteristica = "Ingresos", Total_cotizantes_dic_year = diciembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-0))%>%summarise(Total_cotizantes = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiros_dic_year<-data.frame(caracteristica = "Retiros", Total_cotizantes_dic_year = diciembre_year21demo%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-0))%>%summarise(Total_cotizantes = sum(retiro_Max_Sum, na.rm = TRUE)))
diciembre_year21ibc<-read.csv2("Salidas_generales_rangoIBC9_12_2021_.csv");nrow(diciembre_year21demo)
diciembre_year21ibc$Rango_IBC2<-NULL
diciembre_year21ibc[diciembre_year21ibc$Rango_IBC == "0.500000","Rango_IBC2"]<-rep("<1SMMLV",nrow(diciembre_year21ibc[diciembre_year21ibc$Rango_IBC == "0.500000",]))
diciembre_year21ibc[diciembre_year21ibc$Rango_IBC == "1.000000","Rango_IBC2"]<-rep("1SMMLV",nrow(diciembre_year21ibc[diciembre_year21ibc$Rango_IBC == "1.000000",]))
diciembre_year21ibc[diciembre_year21ibc$Rango_IBC %in% c("1.500000","2.000000"),"Rango_IBC2"]<-rep("1_2SMMLV",nrow(diciembre_year21ibc[diciembre_year21ibc$Rango_IBC %in% c("1.500000","2.000000"),]))
diciembre_year21ibc[diciembre_year21ibc$Rango_IBC %in% c("2.500000","3.000000"),"Rango_IBC2"]<-rep("2_3SMMLV",nrow(diciembre_year21ibc[diciembre_year21ibc$Rango_IBC %in% c("2.500000","3.000000"),]))
diciembre_year21ibc[diciembre_year21ibc$Rango_IBC %in% c("3.500000","4.000000"),"Rango_IBC2"]<-rep("3_4SMMLV",nrow(diciembre_year21ibc[diciembre_year21ibc$Rango_IBC %in% c("3.500000","4.000000"),]))
diciembre_year21ibc[diciembre_year21ibc$Rango_IBC %in% c("4.500000","5.000000"),"Rango_IBC2"]<-rep("4_5SMMLV",nrow(diciembre_year21ibc[diciembre_year21ibc$Rango_IBC %in% c("4.500000","5.000000"),]))
diciembre_year21ibc[diciembre_year21ibc$Rango_IBC == "5.500000","Rango_IBC2"]<-rep(">5SMMLV",nrow(diciembre_year21ibc[diciembre_year21ibc$Rango_IBC == "5.500000",]))
res_rango_salarial_dic_year<-data.frame(diciembre_year21ibc%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-0))%>%group_by(Rango_IBC2)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_rango_salarial_dic_year<-merge(res_rango_salarial_dic_year,orden_salida_rango,by = "Rango_IBC2", all.x = TRUE)
res_rango_salarial_dic_year<-res_rango_salarial_dic_year[order(res_rango_salarial_dic_year$salida),-3]
colnames(res_rango_salarial_dic_year)<-c("caracteristica","Total_cotizantes")
diciembre_year21dpto<-read.csv2("Salidas_generales_DPTO_MPIO9_12_2021_.csv");nrow(diciembre_year21dpto)
res_deparamentos_dic_year<-data.frame(diciembre_year21dpto%>%filter(Tipologia == "Dep_sec_priv", YEAR == año, MONTH == (mes-0))%>%group_by(departamento_COT_Max)%>%summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE)))
res_deparamentos_dic_year<-merge(res_deparamentos_dic_year,departamentos, by = "departamento_COT_Max", all.x = TRUE)
res_deparamentos_dic_year<-res_deparamentos_dic_year%>%select(Nombre_dpto,Total_cotizantes)
colnames(res_deparamentos_dic_year)<-c("caracteristica","Total_cotizantes")
Consolidado_dic_year<-rbind(mujeres_dic_year,hombres_dic_year,jovenes_dic_year,mayores_dic_year,res_rango_salarial_dic_year,res_deparamentos_dic_year,SLN_dic_year,ingresos_dic_year,retiros_dic_year)
colnames(Consolidado_dic_year)<-c("caracteristica","Total_cot_12_2021")

Consolidado_general<-merge(Consolidado_feb_20,Consolidado_mar_20, by = "caracteristica", all = TRUE)
Consolidado_general<-merge(Consolidado_general,Consolidado_abr_20, by = "caracteristica", all = TRUE)
Consolidado_general<-merge(Consolidado_general,Consolidado_may_20, by = "caracteristica", all = TRUE)
Consolidado_general<-merge(Consolidado_general,Consolidado_jun_20, by = "caracteristica", all = TRUE)
Consolidado_general<-merge(Consolidado_general,Consolidado_jul_20, by = "caracteristica", all = TRUE)
Consolidado_general<-merge(Consolidado_general,Consolidado_ago_20, by = "caracteristica", all = TRUE)
Consolidado_general<-merge(Consolidado_general,Consolidado_oct_year, by = "caracteristica", all = TRUE)
Consolidado_general<-merge(Consolidado_general,Consolidado_nov_year, by = "caracteristica", all = TRUE)
Consolidado_general<-merge(Consolidado_general,Consolidado_dic_year, by = "caracteristica", all = TRUE)

Order_salida<-data.frame(secuencia_or = 1:46,
              caracteristica = c("Mujer","Hombre","Jovenes(18-28)","Mayores40","<1SMMLV","1SMMLV","1_2SMMLV","2_3SMMLV","3_4SMMLV","4_5SMMLV",">5SMMLV","Antioquia","Arauca","Archipiélago de san andrés, providencia y santa catalina","Atlántico",
                                 "Bogotá, d.c.", "Bolívar","Boyacá","Caldas", "Caquetá","Casanare","Cauca","Cesar","Chocó","Córdoba" ,"Cundinamarca","Guainía","Guaviare","Huila", "La guajira","Magdalena" ,"Meta", "Nariño", "Norte de santander",
                                 "Putumayo","Quindío", "Risaralda","Santander","Sucre","Tolima","Valle del cauca","Vaupés","Vichada","SLN","Ingresos","Retiros"))
Consolidado_general<-merge(Consolidado_general,Order_salida, by = "caracteristica", all.y = TRUE)
Consolidado_general<-Consolidado_general[order(Consolidado_general$secuencia_or),-12]
write.xlsx(Consolidado_general,"D:/Resultados_IMC/resumen_general/formato_1221.xlsx")
