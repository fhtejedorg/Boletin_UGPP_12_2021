

#estimado de cotizantes 2021 Enero 12.403.028, lim_inf 12.093.822 y lim_sup 12.712.234
#estimado de cotizantes 2022 Enero 12.785.428, lim_inf 12.094.021 y lim_sup 13.476.835
rm(list=ls())
library(xlsx)
library(dplyr);
Yes
library(tidyverse)
library(DT)
library(kableExtra)
library(knitr)
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(ggfortify)

Serie_completa<-read.csv2("D:/Resultados_IMC/Presentaciones/Serie_cotizantes.csv")
Serie_total_ts<-ts(c(Serie_completa[Serie_completa$Year %in% c(2015,2016,2017,2018,2019),"Tot_cotizantes"],Serie_completa[Serie_completa$Year == 2020 & Serie_completa$Mes == 1,"Tot_cotizantes"]),start = c(2015,1), frequency = 12)
modelo_ser_tot<-auto.arima(Serie_total_ts,stepwise = FALSE,approximation = FALSE)
pronostico_total<-forecast(modelo_ser_tot,level = c(95),h = 15)
serie_total_pro_tot<-autoplot(pronostico_total, ts.colout = "black",predict.colour = 'grey50',predict.linetype = 'dashed',conf.int.colour = "#7FFF00" )+
  labs(title = "Serie de cotizantes", x = "Años ", y = "Total cotizantes")

Serie_total_ts_dep<-ts(c(Serie_completa[Serie_completa$Year %in% c(2015,2016,2017,2018,2019),"Cot_dependientes"],Serie_completa[Serie_completa$Year == 2020 & Serie_completa$Mes == 1,"Cot_dependientes"]),start = c(2015,1), frequency = 12)
modelo_ser_dep<-auto.arima(Serie_total_ts_dep,stepwise = FALSE,approximation = FALSE)
pronostico_dep<-forecast(modelo_ser_dep,level = c(95),h = 15)
serie_total_pro_dep<-autoplot(pronostico_dep, predict.colour = 'blue', ts.colour = "blue",predict.linetype = 'dashed',conf.int.colour = "#7FFF00" )+
  labs(title = "Serie de cotizantes",subtitle = "Cotizantes dependientes", x = "Años ", y = "Total cotizantes")

Serie_total_ts_ind<-ts(c(Serie_completa[Serie_completa$Year %in% c(2014,2015,2016,2017,2018,2019),"Cot_independientes"],Serie_completa[Serie_completa$Year == 2020 & Serie_completa$Mes == 1,"Cot_independientes"]),start = c(2014,1), frequency = 12)
ymodelo_ser_ind<-auto.arima(Serie_total_ts_ind,stepwise = FALSE,approximation = FALSE)
pronostico_ind<-forecast(modelo_ser_ind,level = c(95),h = 15)
serie_total_pro_ind<-autoplot(pronostico_ind, predict.colour = 'darkred', ts.colour = "red",predict.linetype = 'dashed',conf.int.colour = "#7FFF00" )+
  labs(title = "Serie de cotizantes",subtitle = "Cotizantes independientes", x = "Años ", y = "Total cotizantes")

grid.arrange(serie_total_pro_ind,serie_total_pro_dep,serie_total_pro_tot, nrow = 1)

tabla0_2<- data.frame(c("2.018","2.019","2.020","2.021"),
    c(formatC(9171320/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(9361354/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(9634045/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),
      paste0(formatC(9980334/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1)," (",formatC(9313716/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),")")),
    c(paste0(2.42,"%"),paste0(2.07,"%"),paste0(2.91,"%"),paste0(2.83,"%")),
    c(formatC(2037402/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(1998738/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(2042867/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),
      paste0(formatC(2129681/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1)," (",formatC(2044477/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),")")),
    c(paste0(8.22,"%"),paste0(-1.9,"%"),paste0(2.21,"%"),paste0(4.25,"%")),
    c(formatC(11208722/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(11360092/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(11676912/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),
      paste0(formatC(11993732/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1)," (",formatC(11358193/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),")")),
    c(paste0(3.43,"%"),paste0(1.35,"%"),paste0(2.79,"%"),paste0(2.71,"%"))); colnames(tabla0_2)<- c("Año","Cot.Dependientes","Var anual","Cot.Independientes","Var anual","Total_Cotizantes","Var anual")
tabla0_2_1<-ggtexttable(tabla0_2,rows = NULL,cols  = c("Año","Cot. Dependientes", "Var anual","Cot. Independientes","Var anual","Total cotizantes","Var anual"),theme = ttheme("light"))

#primera diapositiva 

tabla_pronostico<-read.xlsx("C:/Users/aemendoza/Documents/2022/Febrero/cotizaciones/ajuste del modelo/estimacion_enero.xlsx",sheetName = "estimacion")
tabla_pronostico$Var_anual_O<-rep(0,nrow(tabla_pronostico))
tabla_pronostico$var_anual_P<-rep(0,nrow(tabla_pronostico))
for(i in 2:nrow(tabla_pronostico)){
  tabla_pronostico$Var_anual_O[i]<-paste0(formatC((tabla_pronostico$Total_cotizantes[i]-tabla_pronostico$Total_cotizantes[i-1])/tabla_pronostico$Total_cotizantes[i-1]*100,big.mark = ".",decimal.mark = ",",digits = 2),"%")
  tabla_pronostico$var_anual_P[i]<-paste0(formatC((tabla_pronostico$lambda     [i]-tabla_pronostico$lambda     [i-1])/tabla_pronostico$lambda     [i-1]*100,big.mark = ".",decimal.mark = ",",digits = 2),"%")}
tabla_pronostico$Total_cotizantes1<-formatC(tabla_pronostico$Total_cotizantes/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
tabla_pronostico$Total_cotizantes_p1<-formatC(tabla_pronostico$lambda/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
tabla1_1<-ggtexttable(data.frame(tabla_pronostico%>%select(Year,Total_cotizantes1,Var_anual_O,Total_cotizantes_p1,var_anual_P)),rows = NULL,
                      cols  = c("Año","Total Cot", "Var anual","Total Cot #Est","Var anual"),theme = ttheme("light"))#%>%

graficos1_1<-ggplot(tabla_pronostico)+
  geom_ribbon(aes(x = Year, ymin = q2.5, ymax = q97.5),fill = "grey90")+
  geom_path(aes(x = Year, y = lambda), color = "red")+
  geom_point(aes(x = Year, y = Total_cotizantes))+
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "darkorange", size = 1)+
  geom_text(aes(x = Year, y = lambda, label = formatC(tabla_pronostico$lambda/1000,digits = 2,
                                                      format = "f", decimal.mark = ",", big.mark = ".")),size = 2,nudge_x = 0.5)+
  labs(title = "Estimación de los cotizantes en Enero",subtitle = "Años de 2011 a 2021", x = "Año", y = "Total Cotizantes")
ggarrange(graficos1_1,tabla1_1,nrow = 1, ncol =2,heights = c(0.5, 0.6))

#####################################################################
#adicionar los nuevos meses cuando se tengan unos adicionales 
####slide del total y el año tipo

datos_tipo<-read.xlsx("D:/Nuevos_Insumos_IMC/Insumos_general_presentacion.xlsx",sheetName = "type_year")
informacion9_12_21<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_rangoIBC9_12_2021.csv")
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

independientes<-rbind(data.frame(informacion9_12_21%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(Total_ind = sum(Total_relaciones_laborales,na.rm = TRUE))),
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
dependientes<-rbind(data.frame(informacion9_12_21%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(Total_dep = sum(Total_relaciones_laborales,na.rm = TRUE))),
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
Dep_sec_priv<-rbind(data.frame(informacion9_12_21%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(Total_sec_priv = sum(Total_relaciones_laborales,na.rm = TRUE))),
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
#actualizar con 2022
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
consolidado_tot$Total_sec_privp<-formatC(consolidado_tot$Total_sec_priv/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
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

##########cambiar
mes_ref = 12
year_ref = 2021

#tabla1_1
consolidado_tot%>%filter(MONTH == mes_ref)%>%
  select(YEAR,TOTAL_COTp,var_anual_totp,var_mensual_totp,Total_depp,var_anual_depp,var_mensual_depp,Total_indp,var_anual_indp,var_mensual_indp)%>%
  arrange(desc(YEAR))%>%
  kbl(caption = "Comportamiento del periodo de análisis", align = "lccccccccc", 
      col.names = c("Año","Total cotizantes","Var anual","Var mensual","Total cotizantes","Var anual","Var mensual","Total cotizantes","Var anual","Var mensual"))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  kable_styling(full_width = F, font_size = 10)%>%
   #row_spec(row = 1,bold = TRUE)%>%
  row_spec(row = 0, bold = TRUE,# color = "white", background = "#00BFFF",
           font_size = "large")%>%
  add_header_above(c(" ","Total cotizantes" = 3,"Dependientes" = 3,"Independientes" = 3))

tipo_general<-c(1,1.040,1.040,1.059,1.071,1.071,1.056,1.071,1.096,1.087,1.079,1.057)
tipo_dependientes<-c(1,1.042,1.039,1.061,1.073,1.069,1.05,1.068,1.095,1.082,1.077,1.053)
tipo_independientes<-c(1,1.028,1.045,1.048,1.062,1.078,1.085,1.085,1.097,1.113,1.088,1.081)
consolidado_tipo_19<-consolidado_tot[consolidado_tot$YEAR == 2019,c("MONTH","TOTAL_COT")]
consolidado_tipo_20<-consolidado_tot[consolidado_tot$YEAR == 2020,c("MONTH","TOTAL_COT")]
consolidado_tipo_19d<-consolidado_tot[consolidado_tot$YEAR == 2019,c("MONTH","Total_dep")]
consolidado_tipo_20d<-consolidado_tot[consolidado_tot$YEAR == 2020,c("MONTH","Total_dep")]
consolidado_tipo_19i<-consolidado_tot[consolidado_tot$YEAR == 2019,c("MONTH","Total_ind")]
consolidado_tipo_20i<-consolidado_tot[consolidado_tot$YEAR == 2020,c("MONTH","Total_ind")]

consolidado_tipo<-consolidado_tot[consolidado_tot$YEAR == year_ref,c("MONTH","TOTAL_COT")]
consolidado_tipo$Esperado_tradicional<-consolidado_tipo[consolidado_tipo$MONTH == 1,"TOTAL_COT"]*tipo_general
consolidado_tipo$Esperado_enero<-11993732*tipo_general
consolidado_tipo$Diferencia = (consolidado_tipo$TOTAL_COT - consolidado_tipo$Esperado_enero)
consolidado_tipo$Var_por = (consolidado_tipo$TOTAL_COT - consolidado_tipo$Esperado_enero)/consolidado_tipo$Esperado_enero

consolidado_tipod<-consolidado_tot[consolidado_tot$YEAR == year_ref,c("MONTH","Total_dep")]
consolidado_tipod$Esperado_tradicional<-consolidado_tipod[consolidado_tipod$MONTH == 1,"Total_dep"]*tipo_dependientes
consolidado_tipod$Esperado_enero<-9906736*tipo_dependientes
consolidado_tipod$Diferencia = (consolidado_tipod$Total_dep - consolidado_tipod$Esperado_enero)
consolidado_tipod$Var_por = (consolidado_tipod$Total_dep - consolidado_tipod$Esperado_enero)/consolidado_tipod$Esperado_enero

consolidado_tipoi<-consolidado_tot[consolidado_tot$YEAR == year_ref,c("MONTH","Total_ind")]
consolidado_tipoi$Esperado_tradicional<-consolidado_tipoi[consolidado_tipoi$MONTH == 1,"Total_ind"]*tipo_independientes
consolidado_tipoi$Esperado_enero<-2129681 *tipo_independientes
consolidado_tipoi$Diferencia = (consolidado_tipoi$Total_ind- consolidado_tipoi$Esperado_enero)
consolidado_tipoi$Var_por = (consolidado_tipoi$Total_ind - consolidado_tipoi$Esperado_enero)/consolidado_tipoi$Esperado_enero

consolidado_tipo_tab<-data.frame(
  Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
  Cotizantes = formatC(consolidado_tipo$TOTAL_COT/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Cotizantes_esperado_tradicional = formatC(consolidado_tipo$Esperado_tradicional/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Cotizantes_esperado = formatC(consolidado_tipo$Esperado_enero/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Diferencia = formatC(consolidado_tipo$Diferencia/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Var_por = paste0(round(consolidado_tipo$Var_por * 100,2), "%")
)
consolidado_tipod_tabd<-data.frame(
  Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
  Cotizantes = formatC(consolidado_tipod$Total_dep/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Cotizantes_esperado_tradicional = formatC(consolidado_tipod$Esperado_tradicional/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Cotizantes_esperado = formatC(consolidado_tipod$Esperado_enero/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Diferencia = formatC(consolidado_tipod$Diferencia/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Var_por = paste0(round(consolidado_tipod$Var_por * 100,2), "%")
)
consolidado_tipoi_tabd<-data.frame(
  Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
  Cotizantes = formatC(consolidado_tipoi$Total_ind/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Cotizantes_esperado_tradicional = formatC(consolidado_tipoi$Esperado_tradicional/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Cotizantes_esperado = formatC(consolidado_tipoi$Esperado_enero/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Diferencia = formatC(consolidado_tipoi$Diferencia/1000,format = "f", big.mark = ".",decimal.mark = ",",digits = 1),
  Var_por = paste0(round(consolidado_tipoi$Var_por * 100,2), "%")
)

#tabla1_1_1
#tabla_tot_1_1<-consolidado_tipo_tab%>%
#  select(Mes,Cotizantes,Cotizantes_esperado_tradicional ,Cotizantes_esperado ,Diferencia ,Var_por)%>%
#  kbl(caption = "Total cotizantes frente a año tipo", align = "lccccc", 
#      col.names = c("Año","Total cotizantes (A)","Esperado Enero (B)","Esperado Enero Est (C)","Diferencia (C-A)","Var (C A)"))%>%
#  kable_classic(full_width = F, html_font = "Calibri")%>%
#  #row_spec(row = 1,bold = TRUE)%>%
#  row_spec(row = 0, bold = TRUE,# color = "white", background = "#00BFFF",
#           font_size = "large")
tabla_tot_1_1<-ggtexttable(consolidado_tipo_tab,rows = NULL,
                           cols = c("Año","Total cotizantes (A)","Esperado Enero (B)","Esperado Enero Est (C)","Diferencia (C-A)","Var (C A)"),
                           theme = ttheme("light", base_size = 8))
tabla_tot_1_1d<-ggtexttable(consolidado_tipod_tabd,rows = NULL,
                           cols = c("Año","Total cotizantes (A)","Esperado Enero (B)","Esperado Enero Est (C)","Diferencia (C-A)","Var (C A)"),
                           theme = ttheme("light", base_size = 8))
tabla_tot_1_1i<-ggtexttable(consolidado_tipoi_tabd,rows = NULL,
                           cols = c("Año","Total cotizantes (A)","Esperado Enero (B)","Esperado Enero Est (C)","Diferencia (C-A)","Var (C A)"),
                           theme = ttheme("light", base_size = 8))

Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

grafico_tot_1_1<-rbind(
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipo$TOTAL_COT/consolidado_tipo$TOTAL_COT[1], Categoria = rep("Tipo_calculado_21",nrow(consolidado_tipo))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipo$TOTAL_COT/consolidado_tipo$Esperado_enero[1], Categoria = rep("Tipo_enero_estimado_21",nrow(consolidado_tipo))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = tipo_general, Categoria = rep("IEM",nrow(consolidado_tipo))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipo_19$TOTAL_COT/consolidado_tipo_19$TOTAL_COT[1], Categoria = rep("Tipo_calculado_19",nrow(consolidado_tipo_19))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipo_20$TOTAL_COT/consolidado_tipo_20$TOTAL_COT[1], Categoria = rep("Tipo_calculado_20",nrow(consolidado_tipo_20))))
grafico_tot_1_1d<-rbind(
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipod$Total_dep/consolidado_tipod$Total_dep[1], Categoria = rep("Tipo_calculado_21",nrow(consolidado_tipod))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipod$Total_dep/consolidado_tipod$Esperado_enero[1], Categoria = rep("Tipo_enero_estimado_21",nrow(consolidado_tipod))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = tipo_dependientes, Categoria = rep("IEM",nrow(consolidado_tipod))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipo_19d$Total_dep/consolidado_tipo_19d$Total_dep[1], Categoria = rep("Tipo_calculado_19",nrow(consolidado_tipo_19d))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipo_20d$Total_dep/consolidado_tipo_20d$Total_dep[1], Categoria = rep("Tipo_calculado_20",nrow(consolidado_tipo_20d))))
grafico_tot_1_1i<-rbind(
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipoi$Total_ind/consolidado_tipoi$Total_ind[1], Categoria = rep("Tipo_calculado_21",nrow(consolidado_tipoi))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipoi$Total_ind/consolidado_tipoi$Esperado_enero[1], Categoria = rep("Tipo_enero_estimado_21",nrow(consolidado_tipoi))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = tipo_independientes, Categoria = rep("IEM",nrow(consolidado_tipoi))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipo_19i$Total_ind/consolidado_tipo_19i$Total_ind[1], Categoria = rep("Tipo_calculado_19",nrow(consolidado_tipo_19i))),
  data.frame(Mes_num = 1:12,Mes = Mes,Tipo_tot = consolidado_tipo_20i$Total_ind/consolidado_tipo_20i$Total_ind[1], Categoria = rep("Tipo_calculado_20",nrow(consolidado_tipo_20i))))

grafico_tot_1_1_1<-ggplot(data = grafico_tot_1_1, aes(x = fct_relevel(Mes, "Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
  y = Tipo_tot, group = Categoria, color = Categoria))+
  geom_line(aes(linetype = Categoria, color = Categoria))+
  ggtitle("Comportamiento tipo inter-anual",subtitle = "Todos los cotizantes")+
  xlab("Mes")+ylab("IEM")+theme_minimal()+
  scale_color_manual(values = c("darkorange","grey50","grey50","grey50","black"))+
  scale_size_manual(values = c(1,1,10,10,1))+
  scale_linetype_manual(values = c("solid","dotted","dashed","solid","solid"))+
  theme(legend.position = "top")
grafico_tot_1_1_1d<-ggplot(data = grafico_tot_1_1d, aes(x = fct_relevel(Mes, "Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
                                                      y = Tipo_tot, group = Categoria, color = Categoria))+
  geom_line(aes(linetype = Categoria, color = Categoria))+
  ggtitle("Comportamiento tipo inter-anual",subtitle = "Cotizantes dependientes")+
  xlab("Mes")+ylab("IEM")+theme_minimal()+
  scale_color_manual(values = c("darkorange","grey50","grey50","grey50","blue"))+
  scale_size_manual(values = c(1,1,10,10,1))+
  scale_linetype_manual(values = c("solid","dotted","dashed","solid","solid"))+
  theme(legend.position = "top")
grafico_tot_1_1_1i<-ggplot(data = grafico_tot_1_1i, aes(x = fct_relevel(Mes, "Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
                                                      y = Tipo_tot, group = Categoria, color = Categoria))+
  geom_line(aes(linetype = Categoria, color = Categoria))+
  ggtitle("Comportamiento tipo inter-anual",subtitle = "Cotizantes independientes")+
  xlab("Mes")+ylab("IEM")+theme_minimal()+
  scale_color_manual(values = c("darkorange","grey50","grey50","grey50","red"))+
  scale_size_manual(values = c(1,1,10,10,1))+
  scale_linetype_manual(values = c("solid","dotted","dashed","solid","solid"))+
  theme(legend.position = "top")

ggarrange(tabla_tot_1_1d,grafico_tot_1_1_1d,nrow = 1, ncol = 2)
ggarrange(tabla_tot_1_1i,grafico_tot_1_1_1i,nrow = 1, ncol = 2)
ggarrange(tabla_tot_1_1,grafico_tot_1_1_1,nrow = 1, ncol = 2)

consolidado_tot<-consolidado_tot[order(consolidado_tot$YEAR,consolidado_tot$MONTH),]
consolidado_tot$x<-seq(1,nrow(consolidado_tot))
consolidado_tot$Año<-paste0(consolidado_tot$YEAR,".")
var_anual<-consolidado_tot[,c("YEAR","MONTH","mes2","var_anual_tot","var_anual_dep","var_anual_ind","var_anual_dep_sec_priv")]
var_anual$var_anual_totp<-var_anual$var_anual_tot*100;var_anual$var_anual_depp<-var_anual$var_anual_dep*100;var_anual$var_anual_indp<-var_anual$var_anual_ind*100
var_anual$Año<-paste0(var_anual$YEAR,".")
var_mensual<-consolidado_tot[,c("YEAR","MONTH","mes2","var_mensual_tot","var_mensual_dep","var_mensual_ind","var_mensual_dep_sec_priv")]
var_mensual$var_mensual_totp<-var_mensual$var_mensual_tot*100;var_mensual$var_mensual_depp<-var_mensual$var_mensual_dep*100;var_mensual$var_mensual_indp<-var_mensual$var_mensual_ind*100
var_mensual$Año<-paste0(var_mensual$YEAR,".")

graficio_tot_1_1<-data.frame(
  Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
  IEM = tipo_general,Tipo_observado = consolidado_tipo$Cotizantes/consolidado_tipo$Cotizantes[1],
)

library(reshape)
library(ggplot2)
library(gridExtra)

#actualizar cuando se tenga el nuevo 
Eneros_cot_est<-c(7994277,8604614,8961134,9530890,10096115,10564720,10904789,11208722,11360092,11676912,11759095)
Eneros_cot_obs<-c(7994277,8604614,8961134,9530890,10096115,10564720,10904789,11208722,11360092,11676912,11354371)

tipo<-c(1,1.040,1.040,1.059,1.071,1.071,1.056,1.071,1.096,1.087,1.079,1.057)

tipo_c<-data.frame(IEM = tipo,Periodo = rep("Tipo",12),MONTH = 1:12)
tipo_c21<-data.frame(IEM = c(11759095,consolidado_tot[consolidado_tot$YEAR == 2021,"TOTAL_COT"][-1],NA)/Eneros_cot_est[11],Periodo = rep("2021.",12),MONTH = 1:12)
tipo_c20<-data.frame(IEM = consolidado_tot[consolidado_tot$YEAR == 2020,"TOTAL_COT"]/Eneros_cot_est[10],Periodo = rep("2020.",12),MONTH = 1:12)
tipo_c19<-data.frame(IEM = consolidado_tot[consolidado_tot$YEAR == 2019,"TOTAL_COT"]/Eneros_cot_est[9],Periodo = rep("2019.",12),MONTH = 1:12)
tipo_c18<-data.frame(IEM = consolidado_tot[consolidado_tot$YEAR == 2018,"TOTAL_COT"]/Eneros_cot_est[8],Periodo = rep("2018.",12),MONTH = 1:12)
insumo_grafico1_2<-rbind(tipo_c,tipo_c18,tipo_c19,tipo_c20,tipo_c21)

IEM<-ggplot(insumo_grafico1_2, aes(x = MONTH,y = IEM, group = Periodo))+
  geom_line(aes(linetype=Periodo, color=Periodo, size=Periodo))+theme(legend.position = "top", legend.text = element_text(size = 5), text = element_text(size = 9))+ 
  scale_size_manual(values = c(0.7,0.7,0.7,1,1))+
  scale_linetype_manual(values = c("dotted","dotdash","dashed","solid","solid"))+
  scale_color_manual(values = c("grey20","grey20","grey20","black","blue"))+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  labs(title = "Comportamiento de las cotizaciones frente al año tipo", y = "Indice Estacional Medio")+
  #cambiar
  geom_vline(xintercept = 11, color = "darkorange", size = 0.7)

#cambiar
tabla1_2<-data.frame(Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
                    Observado = formatC(c(consolidado_tot[consolidado_tot$YEAR == 2021,"TOTAL_COT"],NA)/1000,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),
                    Esperado = formatC(c(Eneros_cot_est[11] * tipo)/1000, big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),
                    Diferencia = formatC((c(consolidado_tot[consolidado_tot$YEAR == 2021,"TOTAL_COT"],NA) - Eneros_cot_est[11] * tipo)/1000, big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),
                    Variacion = paste0(formatC((c(consolidado_tot[consolidado_tot$YEAR == 2021,"TOTAL_COT"],NA) - Eneros_cot_est[11] * tipo)/Eneros_cot_est[11] * tipo*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%"))
tabla1_2<-ggtexttable(tabla1_2,rows = NULL,
                      cols  = c("Mes","Observado", "Esperado","Diferencia","Variacion"),theme = ttheme("light", base_size = 7))#%>%
serie_abs <- ggplot(consolidado_tot,aes(x = MONTH, y = TOTAL_COT, color = Año))+
  geom_line(aes(linetype=Año))+theme(legend.position = "top", legend.text = element_text(size = 5), text = element_text(size = 9))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion Anual (%)")+
  labs(title ="Total cotizantes ", subtitle = "Series 2018, 2019, 2020 y 2021")+
  scale_color_manual(values = c("grey20","grey20", "grey20", "blue"))

ggarrange(IEM,tabla1_2,serie_abs,nrow = 1, ncol =3,heights = c(0.5, 0.5,0.5))

#tabla1_2

#######Cambiar

min_var_anual = floor(min(var_anual[,c("var_anual_totp","var_anual_depp","var_anual_indp")]));max_var_anual = ceiling(max(var_anual[,c("var_anual_totp","var_anual_depp","var_anual_indp")]))

var_anualt<-ggplot(var_anual[var_anual$YEAR>=2019,],aes(x = MONTH, y = var_anual_totp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 5), text = element_text(size = 9))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion Anual (%)",limits = c(min_var_anual,max_var_anual))+
  labs(title ="Variaciones anuales", subtitle = "Todos los cotizantes")+
  scale_color_manual(values = c("grey60","black", "blue"))
var_anuald<-ggplot(var_anual[var_anual$YEAR>=2019,],aes(x = MONTH, y = var_anual_depp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 5), text = element_text(size = 9))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion Anual (%)",limits = c(min_var_anual,max_var_anual))+
  labs(title ="Variaciones anuales", subtitle = "Dependientes")+
  scale_color_manual(values = c("grey60","black", "blue"))
var_anuali<-ggplot(var_anual[var_anual$YEAR>=2019,],aes(x = MONTH, y = var_anual_indp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 5), text = element_text(size = 9))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion Anual (%)",limits = c(min_var_anual,max_var_anual))+
  labs(title ="Variaciones anuales", subtitle = "Independientes")+
  scale_color_manual(values = c("grey60","black", "blue"))

min_var_mensual = floor(min(var_mensual[,c("var_mensual_totp","var_mensual_depp","var_mensual_indp")]));max_var_mensual = ceiling(max(var_mensual[,c("var_mensual_totp","var_mensual_depp","var_mensual_indp")]))
var_mensualt<-ggplot(var_mensual,aes(x = MONTH, y = var_mensual_totp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 5),text = element_text(size = 9))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion mensual (%)",limits = c(min_var_mensual,max_var_mensual))+
  labs(title ="Variaciones mensuales", subtitle = "Todos los cotizantes")+
  scale_color_manual(values = c("grey70","grey40","black", "red"))
var_mensuald<-ggplot(var_mensual,aes(x = MONTH, y = var_mensual_depp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 5),text = element_text(size = 9))+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion mensual (%)",limits = c(min_var_mensual,max_var_mensual))+
  labs(title ="Variaciones mensuales", subtitle = "Dependientes")+
  scale_color_manual(values = c("grey70","grey40","black", "red"))
var_mensuali<-ggplot(var_mensual,aes(x = MONTH, y = var_mensual_indp, color = Año))+
  geom_line()+theme(legend.position = "top", legend.text = element_text(size = 5), text = element_text(size = 9))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion mensual (%)",limits = c(min_var_mensual,max_var_mensual))+
  labs(title ="Variaciones mensuales", subtitle = "Independientes")+
  scale_color_manual(values = c("grey70","grey40","black", "red"))

grid.arrange(var_anualt,var_anuald,var_anuali,var_mensualt,var_mensuald,var_mensuali,ncol = 3)

library(car)
library(xlsx)
library(dplyr)
library(tidyverse)
library(DT)
library(kableExtra)
library(knitr)
library(devtools)
library(ggpubr)
library(grid)
library(forecast)
library(tseries)
library(ggfortify)

Serie_completa<-read.csv2("D:/Resultados_IMC/Presentaciones/Serie_cotizantes.csv")
Serie_total_ts<-ts(c(Serie_completa[Serie_completa$Year %in% c(2015,2016,2017,2018,2019),"Tot_cotizantes"],Serie_completa[Serie_completa$Year == 2020 & Serie_completa$Mes == 1,"Tot_cotizantes"]),start = c(2015,1), frequency = 12)
modelo_ser_tot<-auto.arima(Serie_total_ts,stepwise = FALSE,approximation = FALSE)
pronostico_total<-forecast(modelo_ser_tot,level = c(95),h = 15)
serie_total_pro_tot<-autoplot(pronostico_total, ts.colout = "black",predict.colour = 'grey50',predict.linetype = 'dashed',conf.int.colour = "#7FFF00" )+
  labs(title = "Serie de cotizantes", x = "Años ", y = "Total cotizantes")

Serie_total_ts_dep<-ts(c(Serie_completa[Serie_completa$Year %in% c(2015,2016,2017,2018,2019),"Cot_dependientes"],Serie_completa[Serie_completa$Year == 2020 & Serie_completa$Mes == 1,"Cot_dependientes"]),start = c(2015,1), frequency = 12)
modelo_ser_dep<-auto.arima(Serie_total_ts_dep,stepwise = FALSE,approximation = FALSE)
pronostico_dep<-forecast(modelo_ser_dep,level = c(95),h = 15)
serie_total_pro_dep<-autoplot(pronostico_dep, predict.colour = 'blue', ts.colour = "blue",predict.linetype = 'dashed',conf.int.colour = "#7FFF00" )+
  labs(title = "Serie de cotizantes",subtitle = "Cotizantes dependientes", x = "Años ", y = "Total cotizantes")

Serie_total_ts_ind<-ts(c(Serie_completa[Serie_completa$Year %in% c(2014,2015,2016,2017,2018,2019),"Cot_independientes"],Serie_completa[Serie_completa$Year == 2020 & Serie_completa$Mes == 1,"Cot_independientes"]),start = c(2014,1), frequency = 12)
modelo_ser_ind<-auto.arima(Serie_total_ts_ind,stepwise = FALSE,approximation = FALSE)
pronostico_ind<-forecast(modelo_ser_ind,level = c(95),h = 15)
serie_total_pro_ind<-autoplot(pronostico_ind, predict.colour = 'darkred', ts.colour = "red",predict.linetype = 'dashed',conf.int.colour = "#7FFF00" )+
  labs(title = "Serie de cotizantes",subtitle = "Cotizantes independientes", x = "Años ", y = "Total cotizantes")

tabla0_2<- data.frame(c("2.018","2.019","2.020","2.021"),
                      c(formatC(9171320/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(9361354/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(9634045/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),
                        paste0(formatC(9980334/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1)," (",formatC(9313716/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),")")),
                      c(paste0(2.42,"%"),paste0(2.07,"%"),paste0(2.91,"%"),paste0(2.83,"%")),
                      c(formatC(2037402/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(1998738/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(2042867/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),
                        paste0(formatC(2129681/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1)," (",formatC(2044477/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),")")),
                      c(paste0(8.22,"%"),paste0(-1.9,"%"),paste0(2.21,"%"),paste0(4.25,"%")),
                      c(formatC(11208722/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(11360092/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),formatC(11676912/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),
                        paste0(formatC(11993732/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1)," (",formatC(11358193/1000,format = "f",big.mark = ".", decimal.mark = ",",digits = 1),")")),
                      c(paste0(3.43,"%"),paste0(1.35,"%"),paste0(2.79,"%"),paste0(2.71,"%"))); colnames(tabla0_2)<- c("Año","Cot.Dependientes","Var anual","Cot.Independientes","Var anual","Total_Cotizantes","Var anual")
tabla0_2_1<-ggtexttable(tabla0_2,rows = NULL,cols  = c("Año","Cot. Dependientes", "Var anual","Cot. Independientes","Var anual","Total cotizantes","Var anual"),theme = ttheme("light"))

grid.arrange(tabla0_2_1,serie_total_pro_dep,serie_total_pro_ind,serie_total_pro_tot, layout_matrix = rbind(c(1,1,1),c(2,3,4)),ncol = 3,widths = c(2,2,2),heights = c(0.8,1.8))

library(car)
library(forecast)
library(tseries)
library(ggfortify)
library(xlsx)
library(dplyr)
library(tidyverse)
library(DT)
library(kableExtra)
library(knitr)
library(devtools)
library(ggpubr)
library(grid)

#Eneros_cot<-c(8169867,8793610,9157961,9740231,10317871,10796769,11144307,11454916,11638228,12020628)
#Eneros_cot<-c(7994277,8604614,8961134,9530890,10096115,10564720,10904789,11208722,11360092,11676912)

#Tstotal<-ts(Eneros_cot,start = 2011, end = 2020)
#autoplot(Tstotal, ts.colour = "blue", ts.linetype ="dashed")
#ajuste<-auto.arima(y = Tstotal)
#prediccion<-forecast(ajuste, level = c(95), h = 2)

#Eneros<-data.frame(Year = c(2011:2021),Total_cotizantes=c(7994277,8604614,8961134,9530890,10096115,10564720,1090479,11208722,11360092,11676912,NA),
#      Total_cotizantes_p = c(7994277,8604614,8961134,9530890,10096115,10564720,10904789,11208722,11360092,11676912,11758977))
#Eneros$Var_anual_O<-rep(0,nrow(Eneros))
#Eneros$var_anual_P<-rep(0,nrow(Eneros))
#for(i in 2:nrow(Eneros)){
#  Eneros$Var_anual_O[i]<-paste0(formatC((Eneros$Total_cotizantes[i]-Eneros$Total_cotizantes[i-1])/Eneros$Total_cotizantes[i-1]*100,big.mark = ".",decimal.mark = ",",digits = 2),"%")
#  Eneros$var_anual_P[i]<-paste0(formatC((Eneros$Total_cotizantes_p[i]-Eneros$Total_cotizantes_p[i-1])/Eneros$Total_cotizantes_p[i-1]*100,big.mark = ".",decimal.mark = ",",digits = #2),"%")}
#Eneros$Total_cotizantes1<-formatC(Eneros$Total_cotizantes/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
#Eneros$Total_cotizantes_p1<-formatC(Eneros$Total_cotizantes_p/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")

#grafico1<-autoplot(prediccion,main = "Predicción de los cotizantes en Enero 2021 y 2022", xlab = "Años", ylab = "total cotizantes")
#tabla1<-ggtexttable(data.frame(Eneros%>%select(Year,Total_cotizantes1,Var_anual_O,Total_cotizantes_p1,var_anual_P)),rows = NULL,cols  = c("Año","Total Cot", "Var anual","Total Cot #Est","Var anual"),theme = ttheme("light"))#%>%
#ggarrange(grafico1,tabla1,nrow = 1, ncol =2,heights = c(0.5, 0.6))


tabla_pronostico<-read.xlsx("C:/Users/aemendoza/Documents/2022/Febrero/cotizaciones/ajuste del modelo/estimacion_enero.xlsx",sheetName = "estimacion")
tabla_pronostico$Var_anual_O<-rep(0,nrow(tabla_pronostico))
tabla_pronostico$var_anual_P<-rep(0,nrow(tabla_pronostico))
for(i in 2:nrow(tabla_pronostico)){
  tabla_pronostico$Var_anual_O[i]<-paste0(formatC((tabla_pronostico$Total_cotizantes[i]-tabla_pronostico$Total_cotizantes[i-1])/tabla_pronostico$Total_cotizantes[i-1]*100,big.mark = ".",decimal.mark = ",",digits = 2),"%")
  tabla_pronostico$var_anual_P[i]<-paste0(formatC((tabla_pronostico$lambda     [i]-tabla_pronostico$lambda     [i-1])/tabla_pronostico$lambda     [i-1]*100,big.mark = ".",decimal.mark = ",",digits = 2),"%")}
tabla_pronostico$Total_cotizantes1<-formatC(tabla_pronostico$Total_cotizantes/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
tabla_pronostico$Total_cotizantes_p1<-formatC(tabla_pronostico$lambda/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f")
tabla1_1<-ggtexttable(data.frame(tabla_pronostico%>%select(Year,Total_cotizantes1,Var_anual_O,Total_cotizantes_p1,var_anual_P)),rows = NULL,
                      cols  = c("Año","Total Cot", "Var anual","Total Cot #Est","Var anual"),theme = ttheme("light"))#%>%

graficos1_1<-ggplot(tabla_pronostico)+
  geom_ribbon(aes(x = Year, ymin = q2.5, ymax = q97.5),fill = "grey90")+
  geom_path(aes(x = Year, y = lambda), color = "red")+
  geom_point(aes(x = Year, y = Total_cotizantes))+
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "darkorange", size = 1)+
  geom_text(aes(x = Year, y = lambda, label = formatC(tabla_pronostico$lambda/1000,digits = 2,
                                                      format = "f", decimal.mark = ",", big.mark = ".")),size = 2,nudge_x = 0.5)+
  labs(title = "Estimación de los cotizantes en Enero",subtitle = "Años de 2011 a 2021", x = "Año", y = "Total Cotizantes")
ggarrange(graficos1_1,tabla1_1,nrow = 1, ncol =2,heights = c(0.5, 0.6))

consolidado_tot<-consolidado_tot[order(consolidado_tot$YEAR,consolidado_tot$MONTH),]
consolidado_tot$x<-seq(1,nrow(consolidado_tot))
consolidado_tot$Año<-paste0(consolidado_tot$YEAR,".")
var_anual<-consolidado_tot[,c("YEAR","MONTH","mes2","var_anual_tot","var_anual_dep","var_anual_ind","var_anual_dep_sec_priv")]
var_anual$var_anual_totp<-var_anual$var_anual_tot*100;var_anual$var_anual_depp<-var_anual$var_anual_dep*100;var_anual$var_anual_indp<-var_anual$var_anual_ind*100
var_anual$Año<-paste0(var_anual$YEAR,".")
var_mensual<-consolidado_tot[,c("YEAR","MONTH","mes2","var_mensual_tot","var_mensual_dep","var_mensual_ind","var_mensual_dep_sec_priv")]
var_mensual$var_mensual_totp<-var_mensual$var_mensual_tot*100;var_mensual$var_mensual_depp<-var_mensual$var_mensual_dep*100;var_mensual$var_mensual_indp<-var_mensual$var_mensual_ind*100
var_mensual$Año<-paste0(var_mensual$YEAR,".")

library(reshape)
library(ggplot2)
library(gridExtra)

#actualizar cuando se tenga el nuevo 
Eneros_cot_est<-c(7994277,8604614,8961134,9530890,10096115,10564720,10904789,11208722,11360092,11676912,11759095)
Eneros_cot_obs<-c(7994277,8604614,8961134,9530890,10096115,10564720,10904789,11208722,11360092,11676912,11354371)

tipo<-c(1,1.040,1.040,1.059,1.071,1.071,1.056,1.071,1.096,1.087,1.079,1.057)

tipo_c<-data.frame(IEM = tipo,Periodo = rep("Tipo",12),MONTH = 1:12)
tipo_c21<-data.frame(IEM = c(11759095,consolidado_tot[consolidado_tot$YEAR == 2021,"TOTAL_COT"][-1],NA)/Eneros_cot_est[11],Periodo = rep("2021.",12),MONTH = 1:12)
tipo_c20<-data.frame(IEM = consolidado_tot[consolidado_tot$YEAR == 2020,"TOTAL_COT"]/Eneros_cot_est[10],Periodo = rep("2020.",12),MONTH = 1:12)
tipo_c19<-data.frame(IEM = consolidado_tot[consolidado_tot$YEAR == 2019,"TOTAL_COT"]/Eneros_cot_est[9],Periodo = rep("2019.",12),MONTH = 1:12)
tipo_c18<-data.frame(IEM = consolidado_tot[consolidado_tot$YEAR == 2018,"TOTAL_COT"]/Eneros_cot_est[8],Periodo = rep("2018.",12),MONTH = 1:12)
insumo_grafico1_2<-rbind(tipo_c,tipo_c18,tipo_c19,tipo_c20,tipo_c21)

IEM<-ggplot(insumo_grafico1_2, aes(x = MONTH,y = IEM, group = Periodo))+
  geom_line(aes(linetype=Periodo, color=Periodo, size=Periodo))+theme(legend.position = "top", legend.text = element_text(size = 5), text = element_text(size = 9))+ 
  scale_size_manual(values = c(0.7,0.7,0.7,0.8,0.8))+
  scale_linetype_manual(values = c("dotted","dotdash","dashed","solid","solid"))+
  scale_color_manual(values = c("grey20","grey20","grey20","black","blue"))+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  labs(title = "Cotizaciones frente al año tipo",subtitle = "Noviembre 2021", y = "Indice Estacional Medio")+ #cambiar
  geom_vline(xintercept = 11, color = "darkorange", size = 0.6)

#cambiar
tabla1_2<-data.frame(Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
                     Observado = formatC(c(consolidado_tot[consolidado_tot$YEAR == 2021,"TOTAL_COT"],NA)/1000,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),
                     Esperado = formatC(c(Eneros_cot_est[11] * tipo)/1000, big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),
                     Diferencia = formatC((c(consolidado_tot[consolidado_tot$YEAR == 2021,"TOTAL_COT"],NA) - Eneros_cot_est[11] * tipo)/1000, big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),
                     Variacion = paste0(formatC((c(consolidado_tot[consolidado_tot$YEAR == 2021,"TOTAL_COT"],NA) - Eneros_cot_est[11] * tipo)/Eneros_cot_est[11] * tipo*100,big.mark = ".",decimal.mark = ",",digits = 2,format = "f"),"%"))
tabla1_2<-ggtexttable(tabla1_2,rows = NULL,
                      cols  = c("Mes","Observado", "Esperado","Diferencia","Variacion"),theme = ttheme("light", base_size = 7))#%>%
serie_abs <- ggplot(consolidado_tot,aes(x = MONTH, y = TOTAL_COT, color = Año))+
  geom_line(aes(linetype=Año))+theme(legend.position = "top", legend.text = element_text(size = 5), text = element_text(size = 9))+ 
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))+
  scale_y_continuous("Variacion Anual (%)")+
  labs(title ="Total cotizantes ", subtitle = "Series 2018, 2019, 2020 y 2021")+
  scale_color_manual(values = c("grey20","grey20", "grey20", "blue"))

ggarrange(IEM,tabla1_2,serie_abs,nrow = 1, ncol =3,heights = c(0.5, 0.7,0.5))

#####diapositiva por rango IBC 

library(matrixcalc)
library(dplyr)
library(kableExtra)
library(knitr)

setwd("D:/Nuevos_Insumos_IMC/Informacion_general/")

#tabla<-"Salidas_generales_rangoIBC9_12_2021.csv"

salida_IBC<-function(tabla){
  base<-read.csv2(tabla,dec = ".")
  base<-base[,c("YEAR","MONTH","Tipologia","Rango_IBC","Total_relaciones_laborales")]
  base$Rango_IBC_salida<-NULL
  base[base$Rango_IBC == 0.5, "Rango_IBC_salida"]<-rep("<1_SMMLV",nrow(base[base$Rango_IBC == 0.5,]))
  base[base$Rango_IBC == 1, "Rango_IBC_salida"]<-rep("1_SMMLV",nrow(base[base$Rango_IBC == 1,]))
  base[base$Rango_IBC > 1 & base$Rango_IBC <= 2, "Rango_IBC_salida"]<-rep("1-2_SMMLV",nrow(base[base$Rango_IBC > 1 & base$Rango_IBC <= 2,]))
  base[base$Rango_IBC > 2 & base$Rango_IBC <= 5, "Rango_IBC_salida"]<-rep("2-5_SMMLV",nrow(base[base$Rango_IBC > 2 & base$Rango_IBC <= 5,]))
  base[base$Rango_IBC > 5, "Rango_IBC_salida"]<-rep(">5_SMMLV",nrow(base[base$Rango_IBC > 5,]))
  base$Tipologia2<-NULL
  base[base$Tipologia %in% c("Dep_No_priv","Dep_sec_priv"),"Tipologia2"]<-rep("Dependientes", nrow(base[base$Tipologia %in% c("Dep_No_priv","Dep_sec_priv"),]))
  base[base$Tipologia =="Independiente","Tipologia2"]<-rep("Independientes", nrow(base[base$Tipologia =="Independiente",]))

  Resumen_rango<-data.frame(base%>%group_by(Tipologia2,Rango_IBC_salida)%>%summarise(Total = n()))

  for(m in unique(base$MONTH)){
  base_sub_res<-data.frame(base%>%filter(MONTH == m)%>%group_by(Tipologia2,Rango_IBC_salida)%>%summarise(Total_cot = sum(Total_relaciones_laborales, na.rm = TRUE)))
  base_sub_res_tot<-data.frame(base%>%filter(MONTH == m)%>%group_by(Rango_IBC_salida)%>%summarise(Total_cot = sum(Total_relaciones_laborales, na.rm = TRUE)))
  base_sub_res_tot$Tipologia2<-rep("Total",nrow(base_sub_res_tot))
  base_sub_res<-rbind(base_sub_res,base_sub_res_tot)
  colnames(base_sub_res)<-c("Tipologia2","Rango_IBC_salida",paste0("Total_cot_",base$YEAR[1],"_",m))
  Resumen_rango<-merge(Resumen_rango,base_sub_res, by = c("Tipologia2","Rango_IBC_salida"), all = TRUE)
  }
  Resumen_rango<-Resumen_rango[,-3]
  Resumen_rango
} 

salida_IBC_9_12_21<-salida_IBC("Salidas_generales_rangoIBC9_12_2021.csv")
salida_IBC_9_12_20<-salida_IBC("Salidas_generales_rangoIBC9_12_2020.csv")
salida_IBC_9_12_19<-salida_IBC("Salidas_generales_rangoIBC9_12_2019.csv")
salida_IBC_9_12_18<-salida_IBC("Salidas_generales_rangoIBC9_12_2018.csv")
salida_IBC_9_12_17<-salida_IBC("Salidas_generales_rangoIBC9_12_2017.csv")

salida_IBC_res<-merge(salida_IBC_9_12_17,salida_IBC_9_12_18, by = c("Tipologia2","Rango_IBC_salida"), all = TRUE)
salida_IBC_res<-merge(salida_IBC_res,salida_IBC_9_12_19, by = c("Tipologia2","Rango_IBC_salida"), all = TRUE)
salida_IBC_res<-merge(salida_IBC_res,salida_IBC_9_12_20, by = c("Tipologia2","Rango_IBC_salida"), all = TRUE)
salida_IBC_res<-merge(salida_IBC_res,salida_IBC_9_12_21, by = c("Tipologia2","Rango_IBC_salida"), all = TRUE)

salida_IBC_res$var_anual_21<-(salida_IBC_res$Total_cot_2021_12-salida_IBC_res$Total_cot_2020_12)/salida_IBC_res$Total_cot_2020_12
salida_IBC_res$var_anual_20<-(salida_IBC_res$Total_cot_2020_12-salida_IBC_res$Total_cot_2019_12)/salida_IBC_res$Total_cot_2019_12
salida_IBC_res$var_anual_19<-(salida_IBC_res$Total_cot_2019_12-salida_IBC_res$Total_cot_2018_12)/salida_IBC_res$Total_cot_2018_12
salida_IBC_res$var_anual_18<-(salida_IBC_res$Total_cot_2018_12-salida_IBC_res$Total_cot_2017_12)/salida_IBC_res$Total_cot_2017_12

salida_IBC_res$var_mes_21<-(salida_IBC_res$Total_cot_2021_12-salida_IBC_res$Total_cot_2021_11)/salida_IBC_res$Total_cot_2021_11
salida_IBC_res$var_mes_20<-(salida_IBC_res$Total_cot_2020_12-salida_IBC_res$Total_cot_2020_11)/salida_IBC_res$Total_cot_2020_11
salida_IBC_res$var_mes_19<-(salida_IBC_res$Total_cot_2019_12-salida_IBC_res$Total_cot_2019_11)/salida_IBC_res$Total_cot_2019_11
salida_IBC_res$var_mes_18<-(salida_IBC_res$Total_cot_2018_12-salida_IBC_res$Total_cot_2018_11)/salida_IBC_res$Total_cot_2018_11

#variacion mensual 
IBc_dep<-salida_IBC_res[salida_IBC_res$Tipologia2 == "Dependientes",]
grafico_2_1d<-data.frame(Rango = rep(IBc_dep$Rango_IBC_salida,4), Variacion_mensual = vec(as.matrix(IBc_dep[,c("var_mes_21","var_mes_20","var_mes_19","var_mes_18")])),Año = rep(c("2021","2020","2019","2018"),c(5,5,5,5)))
grafico_2_1d[grafico_2_1d$Rango == "<1_SMMLV", "rangonum"]<-rep(1,nrow(grafico_2_1d[grafico_2_1d$Rango == "<1_SMMLV",]));grafico_2_1d[grafico_2_1d$Rango == "1_SMMLV", "rangonum"]<-rep(2,nrow(grafico_2_1d[grafico_2_1d$Rango == "1_SMMLV",]));
grafico_2_1d[grafico_2_1d$Rango == "1-2_SMMLV", "rangonum"]<-rep(3,nrow(grafico_2_1d[grafico_2_1d$Rango == "1-2_SMMLV",]));grafico_2_1d[grafico_2_1d$Rango == "2-5_SMMLV", "rangonum"]<-rep(4,nrow(grafico_2_1d[grafico_2_1d$Rango == "2-5_SMMLV",]));
grafico_2_1d[grafico_2_1d$Rango == ">5_SMMLV", "rangonum"]<-rep(5,nrow(grafico_2_1d[grafico_2_1d$Rango == ">5_SMMLV",]));
IBC_mes_dep_gra<-ggplot(data = grafico_2_1d,
  aes(x = reorder(Rango,rangonum), y = Variacion_mensual*100, fill = Año ))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
  theme_minimal()+scale_fill_brewer(palette = "YlOrBr")+
  geom_text(aes(label = round(Variacion_mensual*100,1)),vjust = 1.5, position = position_dodge(0.9),size = 2)+
  ggtitle("Variación mensual en Diciembre de cada año", subtitle = "Cotizantes Dependientes")+xlab("Rango IBC")+ylab("%")

IBc_ind<-salida_IBC_res[salida_IBC_res$Tipologia2 == "Independientes",]
grafico_2_1i<-data.frame(Rango = rep(IBc_ind$Rango_IBC_salida,4), Variacion_mensual = vec(as.matrix(IBc_dep[,c("var_mes_21","var_mes_20","var_mes_19","var_mes_18")])),Año = rep(c("2021","2020","2019","2018"),c(5,5,5,5)))
grafico_2_1i[grafico_2_1i$Rango == "<1_SMMLV", "rangonum"]<-rep(1,nrow(grafico_2_1i[grafico_2_1i$Rango == "<1_SMMLV",]));grafico_2_1i[grafico_2_1i$Rango == "1_SMMLV", "rangonum"]<-rep(2,nrow(grafico_2_1i[grafico_2_1i$Rango == "1_SMMLV",]));
grafico_2_1i[grafico_2_1i$Rango == "1-2_SMMLV", "rangonum"]<-rep(3,nrow(grafico_2_1i[grafico_2_1i$Rango == "1-2_SMMLV",]));grafico_2_1i[grafico_2_1i$Rango == "2-5_SMMLV", "rangonum"]<-rep(4,nrow(grafico_2_1i[grafico_2_1i$Rango == "2-5_SMMLV",]));
grafico_2_1i[grafico_2_1i$Rango == ">5_SMMLV", "rangonum"]<-rep(5,nrow(grafico_2_1i[grafico_2_1i$Rango == ">5_SMMLV",]));
IBC_mes_ind_gra<-ggplot(data = grafico_2_1i,
                         aes(x = reorder(Rango,rangonum), y = Variacion_mensual*100, fill = Año ))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
  theme_minimal()+scale_fill_brewer(palette = "YlOrBr")+
  geom_text(aes(label = round(Variacion_mensual*100,1)),vjust = 1.5, position = position_dodge(0.9),size = 2)+
  ggtitle("Variación mensual en Diciembre de cada año.", subtitle = "Cotizantes Independientes")+xlab("Rango IBC")+ylab("%")

ggarrange(IBC_mes_dep_gra,IBC_mes_ind_gra,nrow = 1, ncol =2)

salida_IBC_res$diferencia2021<-salida_IBC_res$Total_cot_2021_12-salida_IBC_res$Total_cot_2021_11
salida_IBC_res$diferencia2020<-salida_IBC_res$Total_cot_2020_12-salida_IBC_res$Total_cot_2020_11
salida_IBC_res$diferencia2019<-salida_IBC_res$Total_cot_2019_12-salida_IBC_res$Total_cot_2019_11
salida_IBC_res$diferencia2018<-salida_IBC_res$Total_cot_2018_12-salida_IBC_res$Total_cot_2018_11

salida_IBC_res$diferencia2021a<-salida_IBC_res$Total_cot_2021_12-salida_IBC_res$Total_cot_2020_12
salida_IBC_res$diferencia2020a<-salida_IBC_res$Total_cot_2020_12-salida_IBC_res$Total_cot_2019_12
salida_IBC_res$diferencia2019a<-salida_IBC_res$Total_cot_2019_12-salida_IBC_res$Total_cot_2018_12

tabla2_1_1<-salida_IBC_res[,c("Tipologia2","Rango_IBC_salida","Total_cot_2018_12","Total_cot_2019_12","Total_cot_2020_12","Total_cot_2021_12","diferencia2021","diferencia2020","diferencia2019","diferencia2018","diferencia2021a","diferencia2020a","diferencia2019a")]
tabla2_1_1_subtotales<-data.frame(tabla2_1_1%>%group_by(Tipologia2)%>%
    summarise(Total_cot_2018_12 = sum(Total_cot_2018_12,na.rm = TRUE),Total_cot_2019_12 = sum(Total_cot_2021_12,na.rm = TRUE), Total_cot_2020_12 = sum(Total_cot_2020_12,na.rm = TRUE), Total_cot_2021_12 = sum(Total_cot_2021_12,na.rm = TRUE),
      diferencia2021 = sum(diferencia2021,na.rm = TRUE),    diferencia2020  = sum(diferencia2020,na.rm = TRUE),   diferencia2019 = sum(diferencia2019,na.rm = TRUE),    diferencia2018 = sum(diferencia2018,na.rm = TRUE),    diferencia2021a = sum(diferencia2021a,na.rm = TRUE),  
     diferencia2020a = sum(diferencia2020a,na.rm = TRUE),   diferencia2019a = sum(diferencia2019a,na.rm = TRUE),))
tabla2_1_1_subtotales$Rango_IBC_salida<-rep("Total", nrow(tabla2_1_1_subtotales))
tabla2_1_2<-rbind(tabla2_1_1,tabla2_1_1_subtotales)
tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "<1_SMMLV", "Rango_IBC_salidanum"]<-rep(1,nrow(tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "<1_SMMLV",]));tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "1_SMMLV", "Rango_IBC_salidanum"]<-rep(2,nrow(tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "1_SMMLV",]));
tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "1-2_SMMLV", "Rango_IBC_salidanum"]<-rep(3,nrow(tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "1-2_SMMLV",]));tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "2-5_SMMLV", "Rango_IBC_salidanum"]<-rep(4,nrow(tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "2-5_SMMLV",]));
tabla2_1_2[tabla2_1_2$Rango_IBC_salida == ">5_SMMLV", "Rango_IBC_salidanum"]<-rep(5,nrow(tabla2_1_2[tabla2_1_2$Rango_IBC_salida == ">5_SMMLV",]));tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "Total", "Rango_IBC_salidanum"]<-rep(6,nrow(tabla2_1_2[tabla2_1_2$Rango_IBC_salida == "Total",]));
tabla2_1_2<-data.frame(tabla2_1_2%>%arrange(Tipologia2,Rango_IBC_salidanum))
for(i in 3:ncol(tabla2_1_2)){
  tabla2_1_2[,i]<-formatC(tabla2_1_2[,i]/1000,big.mark = ".",digits = 1,decimal.mark = ",",format = "f")
}

tabla2_1_3<-tabla2_1_2%>%
  select(Tipologia2,Rango_IBC_salida,Total_cot_2019_12,Total_cot_2020_12,Total_cot_2021_12,
         diferencia2018,diferencia2019, diferencia2020, diferencia2021,diferencia2019a,diferencia2020a,diferencia2021a)%>%
  kbl(caption = "Total, diferencia mensuales y anuales de los cotizantes en diciembre 2021", align = "llcccccccccc", 
      col.names = c("Tipo cot.","Rango IBC","Cot. 2019","Cot. 2020","Cot. 2021","2018","2019","2020","2021","2019","2020","2021"))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 6,bold = TRUE)%>%
  row_spec(row = 12,bold = TRUE)%>%
  row_spec(row = 18,bold = TRUE)%>%
  row_spec(row = 0, bold = TRUE)%>%#, color = "white", background = "#00BFFF",font_size = "large")
  add_header_above(c(" " = 2,"Total cot. Diciembre" = 3,"Diferencia mes" = 4,"Diferencia anual" = 3))

#variacion anual
library(ggplot2)
IBc_dep<-salida_IBC_res[salida_IBC_res$Tipologia2 == "Dependientes",]
grafico_2_1d<-data.frame(Rango = rep(IBc_dep$Rango_IBC_salida,3), Variacion_anual = vec(as.matrix(IBc_dep[,c("var_anual_21","var_anual_20","var_anual_19")])),Año = rep(c("2021","2020","2019"),c(5,5,5)))
grafico_2_1d[grafico_2_1d$Rango == "<1_SMMLV", "rangonum"]<-rep(1,nrow(grafico_2_1d[grafico_2_1d$Rango == "<1_SMMLV",]));grafico_2_1d[grafico_2_1d$Rango == "1_SMMLV", "rangonum"]<-rep(2,nrow(grafico_2_1d[grafico_2_1d$Rango == "1_SMMLV",]));
grafico_2_1d[grafico_2_1d$Rango == "1-2_SMMLV", "rangonum"]<-rep(3,nrow(grafico_2_1d[grafico_2_1d$Rango == "1-2_SMMLV",]));grafico_2_1d[grafico_2_1d$Rango == "2-5_SMMLV", "rangonum"]<-rep(4,nrow(grafico_2_1d[grafico_2_1d$Rango == "2-5_SMMLV",]));
grafico_2_1d[grafico_2_1d$Rango == ">5_SMMLV", "rangonum"]<-rep(5,nrow(grafico_2_1d[grafico_2_1d$Rango == ">5_SMMLV",]));
IBC_mes_dep_gra<-ggplot(data = grafico_2_1d,
                        aes(x = reorder(Rango,rangonum), y = Variacion_anual*100, fill = Año ))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
  theme_minimal()+scale_fill_brewer(palette = "YlGnBu")+
  geom_text(aes(label = round(Variacion_anual*100,1)),vjust = 1.5, position = position_dodge(0.9),size = 2)+
  ggtitle("Variación anual en Diciembre", subtitle = "Cotizantes Dependientes")+xlab("Rango IBC")+ylab("%")

IBc_ind<-salida_IBC_res[salida_IBC_res$Tipologia2 == "Independientes",]
grafico_2_1i<-data.frame(Rango = rep(IBc_ind$Rango_IBC_salida,3), Variacion_anual = vec(as.matrix(IBc_dep[,c("var_anual_21","var_anual_20","var_anual_19")])),Año = rep(c("2021","2020","2019"),c(5,5,5)))
grafico_2_1i[grafico_2_1i$Rango == "<1_SMMLV", "rangonum"]<-rep(1,nrow(grafico_2_1i[grafico_2_1i$Rango == "<1_SMMLV",]));grafico_2_1i[grafico_2_1i$Rango == "1_SMMLV", "rangonum"]<-rep(2,nrow(grafico_2_1i[grafico_2_1i$Rango == "1_SMMLV",]));
grafico_2_1i[grafico_2_1i$Rango == "1-2_SMMLV", "rangonum"]<-rep(3,nrow(grafico_2_1i[grafico_2_1i$Rango == "1-2_SMMLV",]));grafico_2_1i[grafico_2_1i$Rango == "2-5_SMMLV", "rangonum"]<-rep(4,nrow(grafico_2_1i[grafico_2_1i$Rango == "2-5_SMMLV",]));
grafico_2_1i[grafico_2_1i$Rango == ">5_SMMLV", "rangonum"]<-rep(5,nrow(grafico_2_1i[grafico_2_1i$Rango == ">5_SMMLV",]));
IBC_mes_ind_gra<-ggplot(data = grafico_2_1i,
                        aes(x = reorder(Rango,rangonum), y = Variacion_anual*100, fill = Año ))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
  theme_minimal()+scale_fill_brewer(palette = "YlGnBu")+
  geom_text(aes(label = round(Variacion_anual*100,1)),vjust = 1.5, position = position_dodge(0.9),size = 2)+
  ggtitle("Variación anual en Diciembre", subtitle = "Cotizantes Independientes")+xlab("Rango IBC")+ylab("%")

ggarrange(IBC_mes_ind_gra,IBC_mes_ind_gra,nrow = 1, ncol =2)

#comportamiento de las novedades 

setwd("D:/Nuevos_Insumos_IMC/Informacion_general/")

#tabla<-"Salidas_generales_rangoIBC9_12_2021.csv"

novedades<-function(tabla){
  base<-read.csv2(tabla,dec = ".")
  Indepedientes<-data.frame(base%>%filter(Tipologia == "Independiente")%>%group_by(YEAR,MONTH)%>%summarise(
    Ingreso = sum(ingreso_Max_Sum,na.rm = TRUE),Por_ingreso = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Retiro = sum(retiro_Max_Sum,na.rm = TRUE),Por_retiro = sum(retiro_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Suspension = sum(suspension_temporal_Max_Sum,na.rm = TRUE),Por_suspension = sum(suspension_temporal_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Var_transitoria = sum(variacion_transitoria_salario_Max_Sum, na.rm = TRUE),Por_var_transitoria = sum(variacion_transitoria_salario_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Var_permanente = sum(variacion_permanente_salario_Max_Sum,na.rm = TRUE),Por_var_permanentes = sum(variacion_permanente_salario_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Incapacidades = sum(incapacidas_por_trabajo_Max_Sum,na.rm = TRUE), Por_incapacidades = sum(incapacidas_por_trabajo_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Vacaciones = sum(vacaciones_Max_Sum, na.rm = TRUE),Por_vacaciones = sum(vacaciones_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Licencia = sum(licencia_maternidad_Max_Sum,na.rm = TRUE),Por_licencia = sum(licencia_maternidad_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Total = sum(Total_relaciones_laborales,na.rm = TRUE)))
  Dependientes<-data.frame(base%>%filter(Tipologia %in% c("Dep_sec_priv","Dep_No_priv"))%>%group_by(YEAR,MONTH)%>%summarise(
    Ingreso = sum(ingreso_Max_Sum,na.rm = TRUE),Por_ingreso = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Retiro = sum(retiro_Max_Sum,na.rm = TRUE),Por_retiro = sum(retiro_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Suspension = sum(suspension_temporal_Max_Sum,na.rm = TRUE),Por_suspension = sum(suspension_temporal_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Var_transitoria = sum(variacion_transitoria_salario_Max_Sum, na.rm = TRUE),Por_var_transitoria = sum(variacion_transitoria_salario_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Var_permanente = sum(variacion_permanente_salario_Max_Sum,na.rm = TRUE),Por_var_permanentes = sum(variacion_permanente_salario_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Incapacidades = sum(incapacidas_por_trabajo_Max_Sum,na.rm = TRUE), Por_incapacidades = sum(incapacidas_por_trabajo_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Vacaciones = sum(vacaciones_Max_Sum, na.rm = TRUE),Por_vacaciones = sum(vacaciones_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Licencia = sum(licencia_maternidad_Max_Sum,na.rm = TRUE),Por_licencia = sum(licencia_maternidad_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Total = sum(Total_relaciones_laborales,na.rm = TRUE)))
  Privados<-data.frame(base%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH)%>%summarise(
    Ingreso = sum(ingreso_Max_Sum,na.rm = TRUE),Por_ingreso = sum(ingreso_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Retiro = sum(retiro_Max_Sum,na.rm = TRUE),Por_retiro = sum(retiro_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Suspension = sum(suspension_temporal_Max_Sum,na.rm = TRUE),Por_suspension = sum(suspension_temporal_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Var_transitoria = sum(variacion_transitoria_salario_Max_Sum, na.rm = TRUE),Por_var_transitoria = sum(variacion_transitoria_salario_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Var_permanente = sum(variacion_permanente_salario_Max_Sum,na.rm = TRUE),Por_var_permanentes = sum(variacion_permanente_salario_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Incapacidades = sum(incapacidas_por_trabajo_Max_Sum,na.rm = TRUE), Por_incapacidades = sum(incapacidas_por_trabajo_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Vacaciones = sum(vacaciones_Max_Sum, na.rm = TRUE),Por_vacaciones = sum(vacaciones_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Licencia = sum(licencia_maternidad_Max_Sum,na.rm = TRUE),Por_licencia = sum(licencia_maternidad_Max_Sum,na.rm = TRUE)/sum(Total_relaciones_laborales,na.rm = TRUE),
    Total = sum(Total_relaciones_laborales,na.rm = TRUE)))
  
  salida = list(Indepedientes = Indepedientes,Privados =Privados,Dependientes = Dependientes)
  salida
}

novedades_9_12_21<-novedades("Salidas_generales_rangoIBC9_12_2021.csv")
novedades_5_8_21<-novedades("Salidas_generales_rangoIBC5_8_2021.csv")
novedades_1_4_21<-novedades("Salidas_generales_rangoIBC1_4_2021.csv")
novedades_9_12_20<-novedades("Salidas_generales_rangoIBC9_12_2020.csv")
novedades_5_8_20<-novedades("Salidas_generales_rangoIBC5_8_2020.csv")
novedades_1_4_20<-novedades("Salidas_generales_rangoIBC1_4_2020.csv")
novedades_9_12_19<-novedades("Salidas_generales_rangoIBC9_12_2019.csv")
novedades_5_8_19<-novedades("Salidas_generales_rangoIBC5_8_2019.csv")
novedades_1_4_19<-novedades("Salidas_generales_rangoIBC1_4_2019.csv")
novedades_9_12_18<-novedades("Salidas_generales_rangoIBC9_12_2018.csv")
novedades_5_8_18<-novedades("Salidas_generales_rangoIBC5_8_2018.csv")
novedades_1_4_18<-novedades("Salidas_generales_rangoIBC1_4_2018.csv")
Independientes <- rbind(novedades_1_4_18$Indepedientes,novedades_5_8_18$Indepedientes,novedades_9_12_18$Indepedientes,
                        novedades_1_4_19$Indepedientes,novedades_5_8_19$Indepedientes,novedades_9_12_19$Indepedientes,
                        novedades_1_4_20$Indepedientes,novedades_5_8_20$Indepedientes,novedades_9_12_20$Indepedientes,
                        novedades_1_4_21$Indepedientes,novedades_5_8_21$Indepedientes,novedades_9_12_21$Indepedientes)
Dependientes <- rbind(novedades_1_4_18$Dependientes,novedades_5_8_18$Dependientes,novedades_9_12_18$Dependientes,
                        novedades_1_4_19$Dependientes,novedades_5_8_19$Dependientes,novedades_9_12_19$Dependientes,
                        novedades_1_4_20$Dependientes,novedades_5_8_20$Dependientes,novedades_9_12_20$Dependientes,
                        novedades_1_4_21$Dependientes,novedades_5_8_21$Dependientes,novedades_9_12_21$Dependientes)
Privados <- rbind(novedades_1_4_18$Privados,novedades_5_8_18$Privados,novedades_9_12_18$Privados,
                      novedades_1_4_19$Privados,novedades_5_8_19$Privados,novedades_9_12_19$Privados,
                      novedades_1_4_20$Privados,novedades_5_8_20$Privados,novedades_9_12_20$Privados,
                      novedades_1_4_21$Privados,novedades_5_8_21$Privados,novedades_9_12_21$Privados)
Independientes<-Independientes[order(Independientes$YEAR,Independientes$MONTH),];
Dependientes<-Dependientes[order(Dependientes$YEAR,Dependientes$MONTH),]
Privados<-Privados[order(Privados$YEAR,Privados$MONTH),]

#ajuste por vacaciones

Dependientes[Dependientes$YEAR<2020,"Vacaciones"]<-Dependientes[Dependientes$YEAR<2020,"Licencia"]
Dependientes[Dependientes$YEAR<2020,"Por_vacaciones"]<-Dependientes[Dependientes$YEAR<2020,"Por_licencia"]
Privados[Privados$YEAR<2020,"Vacaciones"]<-Privados[Privados$YEAR<2020,"Licencia"]
Privados[Privados$YEAR<2020,"Por_vacaciones"]<-Privados[Privados$YEAR<2020,"Por_licencia"]

Independientes<-data.frame(Independientes,
  var_anual_ingreso = rep(0,nrow(Independientes)),var_mensual_ingreso = rep(0,nrow(Independientes)),var_anual_retiro = rep(0,nrow(Independientes)),var_mensual_retiro = rep(0,nrow(Independientes)),
  var_anual_suspension = rep(0,nrow(Independientes)),var_mensual_suspension = rep(0,nrow(Independientes)),var_anual_transitoria = rep(0,nrow(Independientes)),var_mensual_transitoria = rep(0,nrow(Independientes)),
  var_anual_permanente = rep(0,nrow(Independientes)),var_mensual_permanente = rep(0,nrow(Independientes)),var_anual_incapacidades = rep(0,nrow(Independientes)),var_mensual_incapacidades = rep(0,nrow(Independientes)),
  var_anual_vacaciones = rep(0,nrow(Independientes)),var_mensual_vacaciones = rep(0,nrow(Independientes)),var_anual_licencia = rep(0,nrow(Independientes)),var_mensual_licencia = rep(0,nrow(Independientes)))
for(m in c(3,5,7,9,11,13,15,17)){
  for(i in 2:nrow(Independientes)){
    Independientes[i,(m+18)]<-(Independientes[i,m]-Independientes[(i-1),m])/Independientes[(i-1),m]}
  for(j in 13:nrow(Independientes)){
    Independientes[j,(m+17)]<-(Independientes[j,m]-Independientes[(j-12),m])/Independientes[(j-12),m]}
}

Dependientes<-data.frame(Dependientes,
                           var_anual_ingreso = rep(0,nrow(Dependientes)),var_mensual_ingreso = rep(0,nrow(Dependientes)),var_anual_retiro = rep(0,nrow(Dependientes)),var_mensual_retiro = rep(0,nrow(Dependientes)),
                           var_anual_suspension = rep(0,nrow(Dependientes)),var_mensual_suspension = rep(0,nrow(Dependientes)),var_anual_transitoria = rep(0,nrow(Dependientes)),var_mensual_transitoria = rep(0,nrow(Dependientes)),
                           var_anual_permanente = rep(0,nrow(Dependientes)),var_mensual_permanente = rep(0,nrow(Dependientes)),var_anual_incapacidades = rep(0,nrow(Dependientes)),var_mensual_incapacidades = rep(0,nrow(Dependientes)),
                           var_anual_vacaciones = rep(0,nrow(Dependientes)),var_mensual_vacaciones = rep(0,nrow(Dependientes)),var_anual_licencia = rep(0,nrow(Dependientes)),var_mensual_licencia = rep(0,nrow(Dependientes)))
for(m in c(3,5,7,9,11,13,15,17)){
  for(i in 2:nrow(Dependientes)){
    Dependientes[i,(m+18)]<-(Dependientes[i,m]-Dependientes[(i-1),m])/Dependientes[(i-1),m]}
  for(j in 13:nrow(Dependientes)){
    Dependientes[j,(m+17)]<-(Dependientes[j,m]-Dependientes[(j-12),m])/Dependientes[(j-12),m]}
}
Privados<-data.frame(Privados,
                         var_anual_ingreso = rep(0,nrow(Privados)),var_mensual_ingreso = rep(0,nrow(Privados)),var_anual_retiro = rep(0,nrow(Privados)),var_mensual_retiro = rep(0,nrow(Privados)),
                         var_anual_suspension = rep(0,nrow(Privados)),var_mensual_suspension = rep(0,nrow(Privados)),var_anual_transitoria = rep(0,nrow(Privados)),var_mensual_transitoria = rep(0,nrow(Privados)),
                         var_anual_permanente = rep(0,nrow(Privados)),var_mensual_permanente = rep(0,nrow(Privados)),var_anual_incapacidades = rep(0,nrow(Privados)),var_mensual_incapacidades = rep(0,nrow(Privados)),
                         var_anual_vacaciones = rep(0,nrow(Privados)),var_mensual_vacaciones = rep(0,nrow(Privados)),var_anual_licencia = rep(0,nrow(Privados)),var_mensual_licencia = rep(0,nrow(Privados)))
for(m in c(3,5,7,9,11,13,15,17)){
  for(i in 2:nrow(Privados)){
    Privados[i,(m+18)]<-(Privados[i,m]-Privados[(i-1),m])/Privados[(i-1),m]}
  for(j in 13:nrow(Privados)){
    Privados[j,(m+17)]<-(Privados[j,m]-Privados[(j-12),m])/Privados[(j-12),m]}
}

Dependientes_total<-data.frame(Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
          var_anual_ingresos19 = paste0(formatC(Dependientes[Dependientes$YEAR == 2019,"var_anual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_retiro19 = paste0(formatC(Dependientes[Dependientes$YEAR == 2019,"var_anual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_suspencion19 = paste0(formatC(Dependientes[Dependientes$YEAR == 2019,"var_anual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_vacaciones19 = paste0(formatC(Dependientes[Dependientes$YEAR == 2019,"var_anual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_ingresos19 = paste0(formatC(Dependientes[Dependientes$YEAR == 2019,"var_mensual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_retiro19 = paste0(formatC(Dependientes[Dependientes$YEAR == 2019,"var_mensual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_suspencion19 = paste0(formatC(Dependientes[Dependientes$YEAR == 2019,"var_mensual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_vacaciones19 = paste0(formatC(Dependientes[Dependientes$YEAR == 2019,"var_mensual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),
          var_anual_ingresos20 = paste0(formatC(Dependientes[Dependientes$YEAR == 2020,"var_anual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_retiro20 = paste0(formatC(Dependientes[Dependientes$YEAR == 2020,"var_anual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_suspencion20 = paste0(formatC(Dependientes[Dependientes$YEAR == 2020,"var_anual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_vacaciones20 = paste0(formatC(Dependientes[Dependientes$YEAR == 2020,"var_anual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_ingresos20 = paste0(formatC(Dependientes[Dependientes$YEAR == 2020,"var_mensual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_retiro20 = paste0(formatC(Dependientes[Dependientes$YEAR == 2020,"var_mensual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_suspencion20 = paste0(formatC(Dependientes[Dependientes$YEAR == 2020,"var_mensual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_vacaciones20 = paste0(formatC(Dependientes[Dependientes$YEAR == 2020,"var_mensual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),
          var_anual_ingresos21 = paste0(formatC(Dependientes[Dependientes$YEAR == 2021,"var_anual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_retiro21 = paste0(formatC(Dependientes[Dependientes$YEAR == 2021,"var_anual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_suspencion21 = paste0(formatC(Dependientes[Dependientes$YEAR == 2021,"var_anual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_anual_vacaciones21 = paste0(formatC(Dependientes[Dependientes$YEAR == 2021,"var_anual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_ingresos21 = paste0(formatC(Dependientes[Dependientes$YEAR == 2021,"var_mensual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_retiro21 = paste0(formatC(Dependientes[Dependientes$YEAR == 2021,"var_mensual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_suspencion21 = paste0(formatC(Dependientes[Dependientes$YEAR == 2021,"var_mensual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
          var_mensual_vacaciones21 = paste0(formatC(Dependientes[Dependientes$YEAR == 2021,"var_mensual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"))

Grap_3_1<-ggplot(data = Dependientes[Dependientes$YEAR>=2019,], 
  mapping = aes(x = MONTH, y = Por_ingreso*100, fill = paste0(YEAR,".")))+coord_flip()+scale_fill_manual(values=topo.colors(3))+
  geom_bar(stat = "identity" , position = "dodge")+labs(fill = "Años")+ylab("%")+theme_minimal()+theme(legend.position = "top")+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

formatC(Dependientes[Dependientes$YEAR == 2021 & Dependientes$MONTH == 12,"Ingreso"], format = "f", big.mark = ".", decimal.mark = ",", digits = 0)
Grap_3_2<-ggplot(data = Dependientes[Dependientes$YEAR>=2019,], 
  mapping = aes(x = MONTH, y = Por_retiro*100, fill = paste0(YEAR,".")))+coord_flip()+scale_fill_manual(values=topo.colors(3))+
  geom_bar(stat = "identity" , position = "dodge")+labs(fill = "Años")+ylab("%")+theme_minimal()+theme(legend.position = "top")+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

Grap_3_3<-ggplot(data = Dependientes[Dependientes$YEAR>=2019,], 
  mapping = aes(x = MONTH, y = Por_suspension*100, fill = paste0(YEAR,".")))+coord_flip()+scale_fill_manual(values=topo.colors(3))+
  geom_bar(stat = "identity" , position = "dodge")+labs(fill = "Años")+ylab("%")+theme_minimal()+theme(legend.position = "top")+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

Grap_3_4<-ggplot(data = Dependientes[Dependientes$YEAR>=2019,], 
  mapping = aes(x = MONTH, y = Por_vacaciones*100, fill = paste0(YEAR,".")))+coord_flip()+scale_fill_manual(values=topo.colors(3))+
  geom_bar(stat = "identity" , position = "dodge")+labs(fill = "Años")+ylab("%")+theme_minimal()+theme(legend.position = "top")+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

ggarrange(Grap_3_1,Grap_3_2,Grap_3_3,Grap_3_4,nrow = 1, ncol =4, labels = c("Ingresos","Retiros","Suspención T.", "Vacacioes"))

Dependientes_total%>%
  select(Mes,var_anual_ingresos19,var_anual_ingresos20,var_anual_ingresos21,
         var_anual_retiro19,var_anual_retiro20,var_anual_retiro21,
         var_anual_suspencion19,var_anual_suspencion20,var_anual_suspencion21,
         var_anual_vacaciones19,var_anual_vacaciones20,var_anual_vacaciones21)%>%
  kbl(caption = "Variaciones anuales de novedades de ingreso, retiro, suspension temporales y vacaciones", align = "lcccccccccccc", 
      col.names = c("Mes","2019","2020","2021","2019","2020","2021","2019","2020","2021","2019","2020","2021"))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  column_spec(column = 1,border_right  = TRUE)%>%
  column_spec(column = 4,border_right  = TRUE)%>%
  column_spec(column = 7,border_right  = TRUE)%>%
  column_spec(column = 10,border_right  = TRUE)%>%
  row_spec(row = 0, bold = TRUE)%>%#, color = "white", background = "#00BFFF",font_size = "large")
  add_header_above(c(" " = 1,"Ingresos" = 3,"Retiros" = 3,"Suspensiones temporales" = 3,"Vacaciones" = 3))

#serie de ingresos 

nom_seccion <- read.csv2("D:/Aplicacion/tablero/Secciones_economicas_desc2.csv")
seccion_ingresos21<-read.csv2("Salidas_generales_Completo9_12_2021_ingreso_.csv")
seccion_ingresos20<-read.csv2("Salidas_generales_Completo12_12_2020_ingreso_.csv")
seccion_ingresos19<-read.csv2("Salidas_generales_Completo12_12_2019_ingreso_.csv")
seccion_retiros<-read.csv2("Salidas_generales_Completo9_12_2021_retiro_.csv")

ingreso_seccion21 <-  data.frame(seccion_ingresos21%>%filter(Tipologia == "Dep_sec_priv", MONTH >=10)%>%group_by(YEAR,MONTH,Seccion_fn)%>%summarise(Total_ = sum(ingreso_Max_Sum, na.rm = TRUE)))
ingreso_seccion20 <-  data.frame(seccion_ingresos20%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH,Seccion_fn)%>%summarise(Total_ = sum(ingreso_Max_Sum, na.rm = TRUE)))
ingreso_seccion19 <-  data.frame(seccion_ingresos19%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH,Seccion_fn)%>%summarise(Total_ = sum(ingreso_Max_Sum, na.rm = TRUE)))
Ingresos_tot<-rbind(ingreso_seccion19,ingreso_seccion20,ingreso_seccion21)

grafico_in<-ggplot(Ingresos_tot, aes(x = Seccion_fn, y = Total_,fill = as.factor(MONTH)))+labs(title = "Total de relaciones laborales con novedad de ingreso", x = "Sección", y = "Total relaciones laborales", fill = "Mes")+
  geom_bar(stat = "identity", position = 'dodge')+facet_wrap(.~as.factor(YEAR), ncol = 1)+theme_minimal()

nom_secciones <- ggtexttable(nom_seccion, rows = NULL, cols = c("Sección", "Descripción"))

ggarrange(grafico_in,nom_secciones,nrow = 1, ncol = 2)

ingreso_seccion <-  data.frame(seccion_ingresos21%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH,Seccion_fn)%>%summarise(Total_ = sum(ingreso_Max_Sum, na.rm = TRUE)))
retiro_seccion <-  data.frame(seccion_retiros%>%filter(Tipologia == "Dep_sec_priv")%>%group_by(YEAR,MONTH,Seccion_fn)%>%summarise(Total_ = sum(retiro_Max_Sum, na.rm = TRUE)))
entradas_salidas<-rbind(ingreso_seccion,retiro_seccion)
entradas_salidas$Novedad<-c(rep("ingresos",nrow(ingreso_seccion)),rep("retiro",nrow(retiro_seccion)))

tot_secc_economica<- read.csv2("Salidas_generales_Aeconomica9_12_2021.csv")
tot_seccion<-data.frame(tot_secc_economica%>%filter(Tipologia %in% c("Dep_No_priv","Dep_sec_priv"))%>%
            group_by(YEAR,MONTH,Seccion_fn)%>%summarise(Total_cotizantes_dep = sum(Total_relaciones_laborales,na.rm = TRUE)))
entradas_salidas<-merge(entradas_salidas,tot_seccion, by  = c("YEAR","MONTH", "Seccion_fn"), all = TRUE)
entradas_salidas$Participacion<-entradas_salidas$Total_/entradas_salidas$Total_cotizantes_dep
entradas_salidas$mes_nom<-NULL
entradas_salidas[entradas_salidas$MONTH == 9,"mes_nom"]<-rep("Septiembre",nrow(entradas_salidas[entradas_salidas$MONTH ==9,]))
entradas_salidas[entradas_salidas$MONTH == 10,"mes_nom"]<-rep("Octubre",nrow(entradas_salidas[entradas_salidas$MONTH ==10,]))
entradas_salidas[entradas_salidas$MONTH == 11,"mes_nom"]<-rep("Noviembre",nrow(entradas_salidas[entradas_salidas$MONTH ==11,]))
entradas_salidas[entradas_salidas$MONTH == 12,"mes_nom"]<-rep("Diciembre",nrow(entradas_salidas[entradas_salidas$MONTH ==12,]))
entradas_salidas<-entradas_salidas[order(entradas_salidas$MONTH),]

ggplot(data = entradas_salidas,  aes(x = Seccion_fn, y = Participacion*100, group = Novedad))+
  geom_line(aes(color = Novedad))+facet_wrap(.~mes_nom)+theme_minimal()+
  labs(title = "Porcentaje de ingesos y de retiros, 2021", subtitle = "Resultados en los últimos cuatro meses del año", x = "Sección econmica", y = "%")
  
#independientes
  
Independientes_total<-data.frame(Mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
                               var_anual_ingresos19 = paste0(formatC(Independientes[Independientes$YEAR == 2019,"var_anual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_retiro19 = paste0(formatC(Independientes[Independientes$YEAR == 2019,"var_anual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_suspencion19 = paste0(formatC(Independientes[Independientes$YEAR == 2019,"var_anual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_vacaciones19 = paste0(formatC(Independientes[Independientes$YEAR == 2019,"var_anual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_ingresos19 = paste0(formatC(Independientes[Independientes$YEAR == 2019,"var_mensual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_retiro19 = paste0(formatC(Independientes[Independientes$YEAR == 2019,"var_mensual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_suspencion19 = paste0(formatC(Independientes[Independientes$YEAR == 2019,"var_mensual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_vacaciones19 = paste0(formatC(Independientes[Independientes$YEAR == 2019,"var_mensual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),
                               var_anual_ingresos20 = paste0(formatC(Independientes[Independientes$YEAR == 2020,"var_anual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_retiro20 = paste0(formatC(Independientes[Independientes$YEAR == 2020,"var_anual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_suspencion20 = paste0(formatC(Independientes[Independientes$YEAR == 2020,"var_anual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_vacaciones20 = paste0(formatC(Independientes[Independientes$YEAR == 2020,"var_anual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_ingresos20 = paste0(formatC(Independientes[Independientes$YEAR == 2020,"var_mensual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_retiro20 = paste0(formatC(Independientes[Independientes$YEAR == 2020,"var_mensual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_suspencion20 = paste0(formatC(Independientes[Independientes$YEAR == 2020,"var_mensual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_vacaciones20 = paste0(formatC(Independientes[Independientes$YEAR == 2020,"var_mensual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),
                               var_anual_ingresos21 = paste0(formatC(Independientes[Independientes$YEAR == 2021,"var_anual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_retiro21 = paste0(formatC(Independientes[Independientes$YEAR == 2021,"var_anual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_suspencion21 = paste0(formatC(Independientes[Independientes$YEAR == 2021,"var_anual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_anual_vacaciones21 = paste0(formatC(Independientes[Independientes$YEAR == 2021,"var_anual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_ingresos21 = paste0(formatC(Independientes[Independientes$YEAR == 2021,"var_mensual_ingreso"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_retiro21 = paste0(formatC(Independientes[Independientes$YEAR == 2021,"var_mensual_retiro"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_suspencion21 = paste0(formatC(Independientes[Independientes$YEAR == 2021,"var_mensual_suspension"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"),                    
                               var_mensual_vacaciones21 = paste0(formatC(Independientes[Independientes$YEAR == 2021,"var_mensual_vacaciones"]*100, format = "f",digits = 1,big.mark = ".", decimal.mark = ","),"%"))

Grap_3_1i<-ggplot(data = Independientes[Independientes$YEAR>=2019,], 
                 mapping = aes(x = MONTH, y = Por_ingreso*100, fill = paste0(YEAR,".")))+coord_flip()+scale_fill_manual(values=topo.colors(3))+
  geom_bar(stat = "identity" , position = "dodge")+labs(fill = "Años")+ylab("%")+theme_minimal()+theme(legend.position = "top")+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

formatC(Independientes[Independientes$YEAR == 2021 & Independientes$MONTH == 12,"Ingreso"], format = "f", big.mark = ".", decimal.mark = ",", digits = 0)
Grap_3_2i<-ggplot(data = Independientes[Independientes$YEAR>=2019,], 
                 mapping = aes(x = MONTH, y = Por_retiro*100, fill = paste0(YEAR,".")))+coord_flip()+scale_fill_manual(values=topo.colors(3))+
  geom_bar(stat = "identity" , position = "dodge")+labs(fill = "Años")+ylab("%")+theme_minimal()+theme(legend.position = "top")+
  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

#Grap_3_3i<-ggplot(data = Independientes[Independientes$YEAR>=2019,], 
#                 mapping = aes(x = MONTH, y = Por_suspension*100, fill = paste0(YEAR,".")))+coord_flip()+scale_fill_manual(values=topo.colors(3))+
#  geom_bar(stat = "identity" , position = "dodge")+labs(fill = "Años")+ylab("%")+theme_minimal()+theme(legend.position = "top")+
#  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

#Grap_3_4i<-ggplot(data = Independientes[Independientes$YEAR>=2019,], 
#                mapping = aes(x = MONTH, y = Por_vacaciones*100, fill = paste0(YEAR,".")))+coord_flip()+scale_fill_manual(values=topo.colors(3))+
#  geom_bar(stat = "identity" , position = "dodge")+labs(fill = "Años")+ylab("%")+theme_minimal()+theme(legend.position = "top")+
#  scale_x_continuous("Meses",breaks = c(1:12),labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

ggarrange(Grap_3_1i,Grap_3_2i,#Grap_3_3i,Grap_3_4i,
          nrow = 1, ncol =2, labels = c("Ingresos","Retiros"))

Independientes_total%>%
  select(Mes,var_anual_ingresos19,var_anual_ingresos20,var_anual_ingresos21,
         var_anual_retiro19,var_anual_retiro20,var_anual_retiro21,
         var_anual_suspencion19,var_anual_suspencion20,var_anual_suspencion21,
         var_anual_vacaciones19,var_anual_vacaciones20,var_anual_vacaciones21)%>%
  kbl(caption = "Variaciones anuales de novedades de ingreso, retiro, suspension temporales y vacaciones", align = "lcccccccccccc", 
      col.names = c("Mes","2019","2020","2021","2019","2020","2021","2019","2020","2021","2019","2020","2021"))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  column_spec(column = 1,border_right  = TRUE)%>%
  column_spec(column = 4,border_right  = TRUE)%>%
  column_spec(column = 7,border_right  = TRUE)%>%
  column_spec(column = 10,border_right  = TRUE)%>%
  row_spec(row = 0, bold = TRUE)%>%#, color = "white", background = "#00BFFF",font_size = "large")
  add_header_above(c(" " = 1,"Ingresos" = 3,"Retiros" = 3,"Suspensiones temporales" = 3,"Vacaciones" = 3))

##analisis longitudinal 

##tabla iniciual 

setwd("D:/Nuevos_Insumos_IMC/Matrices de transicion/")

panel<-function(tabla){
  Matriz<-read.csv2(tabla,dec = ".")
  Entran_privados<-Matriz[is.na(Matriz$Rango_IBC_pre) & (!is.na(Matriz$Rango_IBC_pos) & Matriz$Tipologia_pos == "Dep_sec_priv"), ]
  Permanecen_privados<-Matriz[(!is.na(Matriz$Rango_IBC_pre) & Matriz$Tipologia_pre == "Dep_sec_priv" ) & (!is.na(Matriz$Rango_IBC_pos) & Matriz$Tipologia_pos == "Dep_sec_priv"), ]
  salen_privados<-Matriz[((!is.na(Matriz$Rango_IBC_pre) & Matriz$Tipologia_pre == "Dep_sec_priv")) & Matriz$Tipologia_pos != "Dep_sec_priv", ]
  
  res_entran<-data.frame(Entran_privados%>%group_by(Rango_IBC_pos)%>%summarise(Privados_entran = sum(Total_relaciones_laborales, na.rm = TRUE)))
  res_permanecen<-data.frame(Permanecen_privados%>%group_by(Rango_IBC_pos)%>%summarise(Privados_permanecen = sum(Total_relaciones_laborales, na.rm = TRUE)))
  res_salen<-data.frame(salen_privados%>%group_by(Rango_IBC_pre)%>%summarise(Privados_salen = sum(Total_relaciones_laborales, na.rm = TRUE), Privados_salen_retiro = sum(retiro_Max_pre_Sum, na.rm = TRUE)))
  matriz_salida <- merge(res_entran,res_permanecen, by = "Rango_IBC_pos", all =TRUE)
  matriz_salida <- merge(matriz_salida,res_salen, by.x = "Rango_IBC_pos", by.y = "Rango_IBC_pre", all =TRUE)
  matriz_salida$Privados_salen_NO_retiro <- matriz_salida$Privados_salen-matriz_salida$Privados_salen_retiro
  
  matriz_salida$Rango_agrupa<-rep(0,nrow(matriz_salida))
  matriz_salida[matriz_salida$Rango_IBC_pos <= 1 ,"Rango_agrupa"] <- rep(1,nrow(matriz_salida[matriz_salida$Rango_IBC_pos <= 1,]))
  matriz_salida[matriz_salida$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa"] <- rep(2,nrow(matriz_salida[matriz_salida$Rango_IBC_pos %in% c(1.5,2),]))
  matriz_salida[matriz_salida$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa"] <- rep(3,nrow(matriz_salida[matriz_salida$Rango_IBC_pos %in% c(2.5,3),]))
  matriz_salida[matriz_salida$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa"] <- rep(4,nrow(matriz_salida[matriz_salida$Rango_IBC_pos %in% c(3.5,4),]))
  matriz_salida[matriz_salida$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa"] <- rep(5,nrow(matriz_salida[matriz_salida$Rango_IBC_pos %in% c(4.5,5),]))
  matriz_salida[matriz_salida$Rango_IBC_pos > 5 ,"Rango_agrupa"] <- rep(6,nrow(matriz_salida[matriz_salida$Rango_IBC_pos> 5,]))
  
  matriz_salida<-data.frame(matriz_salida%>%group_by(Rango_agrupa)%>%summarise(Privados_entran = sum(Privados_entran,na.rm=TRUE),
                                                                               Privados_permanecen = sum(Privados_permanecen, na.rm =TRUE), Privados_salen = sum(Privados_salen, na.rm =TRUE), Privados_salen_retiro = sum(Privados_salen_retiro, na.rm =TRUE),Privados_salen_NO_retiro = sum(Privados_salen_NO_retiro, na.rm =TRUE) ))
  matriz_salida_tot<-data.frame(matriz_salida%>%summarise(Privados_entran = sum(Privados_entran,na.rm=TRUE),
                                                          Privados_permanecen = sum(Privados_permanecen, na.rm =TRUE), Privados_salen = sum(Privados_salen, na.rm =TRUE), Privados_salen_retiro = sum(Privados_salen_retiro, na.rm =TRUE),Privados_salen_NO_retiro = sum(Privados_salen_NO_retiro, na.rm =TRUE) ))
  matriz_salida_tot$Rango_agrupa<-"Total"
  matriz_salida<-rbind(matriz_salida,matriz_salida_tot)
  matriz_salida$Por_entran<-matriz_salida$Privados_entran/(matriz_salida$Privados_entran+matriz_salida$Privados_permanecen)
  matriz_salida$Por_salen<-matriz_salida$Privados_salen/(matriz_salida$Privados_salen +matriz_salida$Privados_permanecen)
  
  matriz_salida<-matriz_salida[order(matriz_salida$Rango_agrupa ),]
  matriz_salida$Rango_agrupa_2<-c("<=1SMMLV","1-2_SMMLV","2-3_SMMLV","3-4_SMMLV","4-5_SMMLV",">5_SMMLV", "Total")
  
  Permanecen_privados$Rango_agrupa_pre<-rep(0,nrow(Permanecen_privados))
  Permanecen_privados$Rango_agrupa_pos<-rep(0,nrow(Permanecen_privados))
  
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre <= 1 ,"Rango_agrupa_pre"] <- rep(1,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre <= 1,]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(1.5,2) ,"Rango_agrupa_pre"] <- rep(2,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(1.5,2),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(2.5,3) ,"Rango_agrupa_pre"] <- rep(3,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(2.5,3),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(3.5,4) ,"Rango_agrupa_pre"] <- rep(4,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(3.5,4),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(4.5,5) ,"Rango_agrupa_pre"] <- rep(5,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(4.5,5),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre > 5 ,"Rango_agrupa_pre"] <- rep(6,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre> 5,]))
  
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos <= 1 ,"Rango_agrupa_pos"] <- rep(1,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos <= 1,]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa_pos"] <- rep(2,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(1.5,2),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa_pos"] <- rep(3,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(2.5,3),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa_pos"] <- rep(4,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(3.5,4),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa_pos"] <- rep(5,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(4.5,5),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos > 5 ,"Rango_agrupa_pos"] <- rep(6,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos> 5,]))
  
  matriz_transision<-matrix(0,nrow = 6, ncol = 8)
  
  for(i in 1:6){
    for(j in 1:6){
      matriz_transision[i,j]<-(sum(Permanecen_privados[Permanecen_privados$Rango_agrupa_pre == i & Permanecen_privados$Rango_agrupa_pos == j, "Total_relaciones_laborales"])/sum(Permanecen_privados[Permanecen_privados$Rango_agrupa_pre == i , "Total_relaciones_laborales"]))
    }
  }
  for(i in 2:6){matriz_transision[i,7]<-sum(matriz_transision[i,1:(i-1)])}
  for(i in 1:5){matriz_transision[i,8]<-sum(matriz_transision[i,(i+1):6])}
  
  Entran_independientes<-Matriz[is.na(Matriz$Rango_IBC_pre) & (!is.na(Matriz$Rango_IBC_pos) & Matriz$Tipologia_pos == "Independiente"), ]
  Permanecen_independientes<-Matriz[(!is.na(Matriz$Rango_IBC_pre) & Matriz$Tipologia_pre == "Independiente" ) & (!is.na(Matriz$Rango_IBC_pos) & Matriz$Tipologia_pos == "Independiente"), ]
  salen_independientes<-Matriz[((!is.na(Matriz$Rango_IBC_pre) & Matriz$Tipologia_pre == "Independiente")) & Matriz$Tipologia_pos != "Independiente", ]
  
  res_entran<-data.frame(Entran_independientes%>%group_by(Rango_IBC_pos)%>%summarise(independientes_entran = sum(Total_relaciones_laborales, na.rm = TRUE)))
  res_permanecen<-data.frame(Permanecen_independientes%>%group_by(Rango_IBC_pos)%>%summarise(independientes_permanecen = sum(Total_relaciones_laborales, na.rm = TRUE)))
  res_salen<-data.frame(salen_independientes%>%group_by(Rango_IBC_pre)%>%summarise(independientes_salen = sum(Total_relaciones_laborales, na.rm = TRUE), independientes_salen_retiro = sum(retiro_Max_pre_Sum, na.rm = TRUE)))
  matriz_salida_ind <- merge(res_entran,res_permanecen, by = "Rango_IBC_pos", all =TRUE)
  matriz_salida_ind <- merge(matriz_salida_ind,res_salen, by.x = "Rango_IBC_pos", by.y = "Rango_IBC_pre", all =TRUE)
  matriz_salida_ind$independientes_salen_NO_retiro <- matriz_salida_ind$independientes_salen-matriz_salida_ind$independientes_salen_retiro
  
  matriz_salida_ind$Rango_agrupa<-rep(0,nrow(matriz_salida_ind))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos <= 1 ,"Rango_agrupa"] <- rep(1,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos <= 1,]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa"] <- rep(2,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(1.5,2),]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa"] <- rep(3,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(2.5,3),]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa"] <- rep(4,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(3.5,4),]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa"] <- rep(5,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(4.5,5),]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos > 5 ,"Rango_agrupa"] <- rep(6,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos> 5,]))
  
  matriz_salida_ind<-data.frame(matriz_salida_ind%>%group_by(Rango_agrupa)%>%summarise(independientes_entran = sum(independientes_entran,na.rm=TRUE),
                                                                                       independientes_permanecen = sum(independientes_permanecen, na.rm =TRUE), independientes_salen = sum(independientes_salen, na.rm =TRUE), independientes_salen_retiro = sum(independientes_salen_retiro, na.rm =TRUE),independientes_salen_NO_retiro = sum(independientes_salen_NO_retiro, na.rm =TRUE) ))
  matriz_salida_ind_tot<-data.frame(matriz_salida_ind%>%summarise(independientes_entran = sum(independientes_entran,na.rm=TRUE),
                                                                  independientes_permanecen = sum(independientes_permanecen, na.rm =TRUE), independientes_salen = sum(independientes_salen, na.rm =TRUE), independientes_salen_retiro = sum(independientes_salen_retiro, na.rm =TRUE),independientes_salen_NO_retiro = sum(independientes_salen_NO_retiro, na.rm =TRUE) ))
  matriz_salida_ind_tot$Rango_agrupa<-"Total"
  matriz_salida_ind<-rbind(matriz_salida_ind,matriz_salida_ind_tot)
  matriz_salida_ind$Por_entran<-matriz_salida_ind$independientes_entran/(matriz_salida_ind$independientes_entran+matriz_salida_ind$independientes_permanecen)
  matriz_salida_ind$Por_salen<-matriz_salida_ind$independientes_salen/(matriz_salida_ind$independientes_salen +matriz_salida_ind$independientes_permanecen)
  
  matriz_salida_ind<-matriz_salida_ind[order(matriz_salida_ind$Rango_agrupa ),]
  matriz_salida_ind$Rango_agrupa_2<-c("<=1SMMLV","1-2_SMMLV","2-3_SMMLV","3-4_SMMLV","4-5_SMMLV",">5_SMMLV", "Total")
  
  Permanecen_independientes$Rango_agrupa_pre<-rep(0,nrow(Permanecen_independientes))
  Permanecen_independientes$Rango_agrupa_pos<-rep(0,nrow(Permanecen_independientes))
  
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre <= 1 ,"Rango_agrupa_pre"] <- rep(1,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre <= 1,]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(1.5,2) ,"Rango_agrupa_pre"] <- rep(2,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(1.5,2),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(2.5,3) ,"Rango_agrupa_pre"] <- rep(3,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(2.5,3),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(3.5,4) ,"Rango_agrupa_pre"] <- rep(4,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(3.5,4),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(4.5,5) ,"Rango_agrupa_pre"] <- rep(5,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(4.5,5),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre > 5 ,"Rango_agrupa_pre"] <- rep(6,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre> 5,]))
  
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos <= 1 ,"Rango_agrupa_pos"] <- rep(1,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos <= 1,]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa_pos"] <- rep(2,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(1.5,2),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa_pos"] <- rep(3,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(2.5,3),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa_pos"] <- rep(4,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(3.5,4),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa_pos"] <- rep(5,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(4.5,5),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos > 5 ,"Rango_agrupa_pos"] <- rep(6,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos> 5,]))
  
  matriz_transision_ind<-matrix(0,nrow = 6, ncol = 8)
  
  for(i in 1:6){
    for(j in 1:6){
      matriz_transision_ind[i,j]<-(sum(Permanecen_independientes[Permanecen_independientes$Rango_agrupa_pre == i & Permanecen_independientes$Rango_agrupa_pos == j, "Total_relaciones_laborales"])/sum(Permanecen_independientes[Permanecen_independientes$Rango_agrupa_pre == i , "Total_relaciones_laborales"]))
    }
  }
  for(i in 2:6){matriz_transision_ind[i,7]<-sum(matriz_transision_ind[i,1:(i-1)])}
  for(i in 1:5){matriz_transision_ind[i,8]<-sum(matriz_transision_ind[i,(i+1):6])}
  
  salida_panel = list(matriz_dep = matriz_salida, transision_dep = matriz_transision, matriz_ind = matriz_salida_ind, transision_ind = matriz_transision_ind)
  salida_panel
} 


matriz_12_21<-panel("Matriz_12_21.csv");matriz_11_21<-panel("Matriz_11_21.csv");matriz_12_20<-panel("Matriz_12_20.csv");matriz_12_19<-panel("Matriz_12_19.csv");matriz_12_18<-panel("Matriz_12_18.csv");matriz_10_21<-panel("Matriz_10_21.csv");matriz_9_21<-panel("Matriz_9_21.csv");matriz_8_21<-panel("Matriz_8_21.csv");matriz_7_21<-panel("Matriz_7_21.csv");matriz_11_20<-panel("Matriz_11_20.csv");matriz_10_20<-panel("Matriz_10_20.csv");matriz_9_20<-panel("Matriz_9_20.csv");matriz_8_20<-panel("Matriz_8_20.csv");matriz_7_20<-panel("Matriz_7_20.csv");matriz_11_19<-panel("Matriz_11_19.csv");matriz_10_19<-panel("Matriz_10_19.csv");matriz_9_19<-panel("Matriz_9_19.csv");matriz_8_19<-panel("Matriz_8_19.csv");matriz_7_19<-panel("Matriz_7_19.csv")

matriz_12_21$matriz_dep$Privados_entran1<-formatC(matriz_12_21$matriz_dep$Privados_entran,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Privados_permanecen1<-formatC(matriz_12_21$matriz_dep$Privados_permanecen,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Privados_salen1<-formatC(matriz_12_21$matriz_dep$Privados_salen,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Privados_salen_retiro1<-formatC(matriz_12_21$matriz_dep$Privados_salen_retiro,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Privados_salen_NO_retiro1<-formatC(matriz_12_21$matriz_dep$Privados_salen_NO_retiro,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Por_entran1<-paste0(round(matriz_12_21$matriz_dep$Por_entran*100,2),"%")
matriz_12_21$matriz_dep$Por_salen1<-paste0(round(matriz_12_21$matriz_dep$Por_salen*100,2),"%")
matriz_12_21$matriz_dep%>%
  select(Rango_agrupa_2,Privados_entran1,Privados_permanecen1,Privados_salen1,
         Privados_salen_retiro1,Privados_salen_NO_retiro1,Por_entran1,Por_salen1)%>%
  kbl(caption = "Matriz resumen de la dinamica pareada entre Diciembre y Noviembre de 2021", align = "lccccccc", 
      col.names = c("Mes","Entran","Permanecen","Salen","Salen con Retiro","Salen sin Retiro","Entran (%)","Salen (%)"))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 0, bold = TRUE)%>%
  row_spec(row = 7, bold = TRUE)%>%
  column_spec(column = 1,border_right  = TRUE)%>%
  column_spec(column = 4,border_right  = TRUE)%>%
  column_spec(column = 7, color = "white", background = "darkgreen")%>%
  column_spec(column = 8, color = "white", background = "darkred")

matriz_11_21$matriz_dep$Privados_entran1<-formatC(matriz_11_21$matriz_dep$Privados_entran,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_11_21$matriz_dep$Privados_permanecen1<-formatC(matriz_11_21$matriz_dep$Privados_permanecen,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_11_21$matriz_dep$Privados_salen1<-formatC(matriz_11_21$matriz_dep$Privados_salen,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_11_21$matriz_dep$Privados_salen_retiro1<-formatC(matriz_11_21$matriz_dep$Privados_salen_retiro,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_11_21$matriz_dep$Privados_salen_NO_retiro1<-formatC(matriz_11_21$matriz_dep$Privados_salen_NO_retiro,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_11_21$matriz_dep$Por_entran1<-paste0(round(matriz_11_21$matriz_dep$Por_entran*100,2),"%")
matriz_11_21$matriz_dep$Por_salen1<-paste0(round(matriz_11_21$matriz_dep$Por_salen*100,2),"%")
matriz_11_21$matriz_dep%>%
  select(Rango_agrupa_2,Privados_entran1,Privados_permanecen1,Privados_salen1,
         Privados_salen_retiro1,Privados_salen_NO_retiro1,Por_entran1,Por_salen1)%>%
  kbl(caption = "Matriz resumen de la dinamica pareada entre Noviembre y Octubre de 2021", align = "lccccccc", 
      col.names = c("Mes","Entran","Permanecen","Salen","Salen con Retiro","Salen sin Retiro","Entran (%)","Salen (%)"))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 0, bold = TRUE)%>%
  row_spec(row = 7,  bold = TRUE)%>%
  column_spec(column = 1,border_right  = TRUE)%>%
  column_spec(column = 4,border_right  = TRUE)%>%
  column_spec(column = 7, color = "white", background = "darkgreen")%>%
  column_spec(column = 8, color = "white", background = "darkred")


matriz_12_21$matriz_dep$Privados_entran1<-formatC(matriz_12_21$matriz_dep$Privados_entran,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Privados_permanecen1<-formatC(matriz_12_21$matriz_dep$Privados_permanecen,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Privados_salen1<-formatC(matriz_12_21$matriz_dep$Privados_salen,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Privados_salen_retiro1<-formatC(matriz_12_21$matriz_dep$Privados_salen_retiro,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Privados_salen_NO_retiro1<-formatC(matriz_12_21$matriz_dep$Privados_salen_NO_retiro,big.mark = ".",decimal.mark = ",",digits = 0,format = "f")
matriz_12_21$matriz_dep$Por_entran1<-paste0(round(matriz_12_21$matriz_dep$Por_entran*100,2),"%")
matriz_12_21$matriz_dep$Por_salen1<-paste0(round(matriz_12_21$matriz_dep$Por_salen*100,2),"%")
matriz_12_21$matriz_dep%>%
  select(Rango_agrupa_2,Privados_entran1,Privados_permanecen1,Privados_salen1,
         Privados_salen_retiro1,Privados_salen_NO_retiro1,Por_entran1,Por_salen1)%>%
  kbl(caption = "Matriz resumen de la dinamica pareada entre Diciembre y Noviembre de 2021", align = "lccccccc", 
      col.names = c("Mes","Entran","Permanecen","Salen","Salen con Retiro","Salen sin Retiro","Entran (%)","Salen (%)"))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 0, bold = TRUE)%>%
  row_spec(row = 6,  underline = TRUE)%>%
  column_spec(column = 1,border_right  = TRUE)%>%
  column_spec(column = 4,border_right  = TRUE)%>%
  column_spec(column = 7, color = "white", background = "darkgreen")%>%
  column_spec(column = 8, color = "white", background = "darkred")

formatC((matriz_12_21$matriz_dep[7,4]-matriz_12_21$matriz_dep[7,2]),big.mark = ".",decimal.mark = ",",digits = 0,format = "d")
formatC((matriz_12_20$matriz_dep[7,4]-matriz_12_20$matriz_dep[7,2]),big.mark = ".",decimal.mark = ",",digits = 0,format = "d")
formatC((matriz_12_19$matriz_dep[7,4]-matriz_12_19$matriz_dep[7,2]),big.mark = ".",decimal.mark = ",",digits = 0,format = "d")
formatC((matriz_12_18$matriz_dep[7,4]-matriz_12_18$matriz_dep[7,2]),big.mark = ".",decimal.mark = ",",digits = 0,format = "d")

#salida 2 

matriz_12_21<-panel("Matriz_12_21.csv")
matriz_11_21<-panel("Matriz_11_21.csv")
matriz_10_21<-panel("Matriz_10_21.csv")
matriz_9_21<-panel("Matriz_9_21.csv")
matriz_8_21<-panel("Matriz_8_21.csv")
matriz_7_21<-panel("Matriz_7_21.csv")

matriz_12_19<-panel("Matriz_12_19.csv")
matriz_11_19<-panel("Matriz_11_19.csv")
matriz_10_19<-panel("Matriz_10_19.csv")
matriz_9_19<-panel("Matriz_9_19.csv")
matriz_8_19<-panel("Matriz_8_19.csv")
matriz_7_19<-panel("Matriz_7_19.csv")

matriz_12_20<-panel("Matriz_12_20.csv")
matriz_11_20<-panel("Matriz_11_20.csv")
matriz_10_20<-panel("Matriz_10_20.csv")
matriz_9_20<-panel("Matriz_9_20.csv")
matriz_8_20<-panel("Matriz_8_20.csv")
matriz_7_20<-panel("Matriz_7_20.csv")

dinamica_serie<-rbind(matriz_12_21$matriz_dep[matriz_12_21$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_11_21$matriz_dep[matriz_11_21$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_10_21$matriz_dep[matriz_10_21$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_9_21$matriz_dep[matriz_9_21$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_8_21$matriz_dep[matriz_8_21$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_7_21$matriz_dep[matriz_7_21$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_12_20$matriz_dep[matriz_12_20$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_11_20$matriz_dep[matriz_11_20$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_10_20$matriz_dep[matriz_10_20$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_9_20$matriz_dep[matriz_9_20$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_8_20$matriz_dep[matriz_8_20$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_7_20$matriz_dep[matriz_7_20$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_12_19$matriz_dep[matriz_12_19$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_11_19$matriz_dep[matriz_11_19$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_10_19$matriz_dep[matriz_10_19$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_9_19$matriz_dep[matriz_9_19$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_8_19$matriz_dep[matriz_8_19$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)],
                    matriz_7_19$matriz_dep[matriz_7_19$matriz_dep$Rango_agrupa_2 == "Total",c(7,8)])

dinamica_serie$year = rep(c("2021","2020","2019"),c(6,6,6))
dinamica_serie$month = rep(c(12,11,10,9,8,7),3)
entradas = ggplot(data = dinamica_serie, aes(x = month, y = Por_entran*100, fill = year))+scale_fill_manual(values=c("cyan1","cyan3","cyan4"))+theme_minimal()+labs(title = "Porcentaje de entradas del sector privado", subtitle = "Ultimo semestre en 2019, 2020 y 2021")+
  geom_bar(stat = "identity", position = "dodge")+scale_x_continuous("Meses",breaks = c(7:12),labels = c("Jul","Ago","Sep","Oct","Nov","Dic"))+ylab("%")
salidas = ggplot(data = dinamica_serie, aes(x = month, y = Por_salen, fill = year))+scale_fill_manual(values=c("khaki2","khaki3","khaki4"))+theme_minimal()+labs(title = "Porcentaje de salidas del sector privado", subtitle = "Ultimo semestre en 2019, 2020 y 2021")+
  geom_bar(stat = "identity", position = "dodge")+scale_x_continuous("Meses",breaks = c(7:12),labels = c("Jul","Ago","Sep","Oct","Nov","Dic"))+ylab("%")
ggarrange(entradas,salidas,nrow = 1, ncol =2)

dinamica_serie_ind<-rbind(matriz_12_21$matriz_ind[matriz_12_21$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_11_21$matriz_ind[matriz_11_21$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_10_21$matriz_ind[matriz_10_21$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_9_21$matriz_ind[matriz_9_21$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_8_21$matriz_ind[matriz_8_21$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_7_21$matriz_ind[matriz_7_21$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_12_20$matriz_ind[matriz_12_20$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_11_20$matriz_ind[matriz_11_20$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_10_20$matriz_ind[matriz_10_20$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_9_20$matriz_ind[matriz_9_20$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_8_20$matriz_ind[matriz_8_20$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_7_20$matriz_ind[matriz_7_20$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_12_19$matriz_ind[matriz_12_19$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_11_19$matriz_ind[matriz_11_19$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_10_19$matriz_ind[matriz_10_19$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_9_19$matriz_ind[matriz_9_19$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_8_19$matriz_ind[matriz_8_19$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)],
                      matriz_7_19$matriz_ind[matriz_7_19$matriz_ind$Rango_agrupa_2 == "Total",c(7,8)])

dinamica_serie_ind$year = rep(c("2021","2020","2019"),c(6,6,6))
dinamica_serie_ind$month = rep(c(12,11,10,9,8,7),3)
entradas = ggplot(data = dinamica_serie_ind, aes(x = month, y = Por_entran*100, fill = year))+scale_fill_manual(values=c("cyan1","cyan3","cyan4"))+theme_minimal()+labs(title = "Porcentaje de entradas del sector privado", subtitle = "Ultimo semestre en 2019, 2020 y 2021")+
  geom_bar(stat = "identity", position = "dodge")+scale_x_continuous("Meses",breaks = c(7:12),labels = c("Jul","Ago","Sep","Oct","Nov","Dic"))+ylab("%")
salidas = ggplot(data = dinamica_serie_ind, aes(x = month, y = Por_salen, fill = year))+scale_fill_manual(values=c("khaki2","khaki3","khaki4"))+theme_minimal()+labs(title = "Porcentaje de salidas del sector privado", subtitle = "Ultimo semestre en 2019, 2020 y 2021")+
  geom_bar(stat = "identity", position = "dodge")+scale_x_continuous("Meses",breaks = c(7:12),labels = c("Jul","Ago","Sep","Oct","Nov","Dic"))+ylab("%")
ggarrange(entradas,salidas,nrow = 1, ncol =2)

##matriz de transision 

MT12_21_dep<-data.frame(paste0(round(matriz_12_21$transision_dep[,1]*100,2),"%"),paste0(round(matriz_12_21$transision_dep[,2]*100,2),"%"),paste0(round(matriz_12_21$transision_dep[,3]*100,2),"%"),paste0(round(matriz_12_21$transision_dep[,4]*100,2),"%"),paste0(round(matriz_12_21$transision_dep[,5]*100,2),"%"),paste0(round(matriz_12_21$transision_dep[,6]*100,2),"%"),paste0(round(matriz_12_21$transision_dep[,7]*100,2),"%"),paste0(round(matriz_12_21$transision_dep[,8]*100,2),"%"));colnames(MT12_21_dep)<-c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV","Incrementan","Disminuyen")
MT12_21_dep$Rango_pre<-c('<=1SMMLV','1-2SMMLV','2-3SMMLV','3-4SMMLV','4-5SMMLV','>5SMMLV')
MT12_21_dep%>%
  select(Rango_pre,'<=1SMMLV','1-2SMMLV','2-3SMMLV','3-4SMMLV','4-5SMMLV','>5SMMLV',Incrementan,Disminuyen)%>%
  kbl(caption = "Matriz de trancisión entre Noviembre y Diciembre de 2021", align = "lcccccccc", 
      col.names = c('Rango IBC','<=1SMMLV','1-2SMMLV','2-3SMMLV','3-4SMMLV','4-5SMMLV','>5SMMLV','Disminuyen %','Incrementan %'))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 0, bold = TRUE)%>%
  column_spec(column = 1, bold = TRUE)%>%
  column_spec(column = 6,border_right  = TRUE)

MT12_20_dep<-data.frame(paste0(round(matriz_12_20$transision_dep[,1]*100,2),"%"),paste0(round(matriz_12_20$transision_dep[,2]*100,2),"%"),paste0(round(matriz_12_20$transision_dep[,3]*100,2),"%"),paste0(round(matriz_12_20$transision_dep[,4]*100,2),"%"),paste0(round(matriz_12_20$transision_dep[,5]*100,2),"%"),paste0(round(matriz_12_20$transision_dep[,6]*100,2),"%"),paste0(round(matriz_12_20$transision_dep[,7]*100,2),"%"),paste0(round(matriz_12_20$transision_dep[,8]*100,2),"%"));colnames(MT12_20_dep)<-c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV","Incrementan","Disminuyen")
MT12_20_dep$Rango_pre<-c('<=1SMMLV','1-2SMMLV','2-3SMMLV','3-4SMMLV','4-5SMMLV','>5SMMLV')
MT12_20_dep%>%
  select(Rango_pre,'<=1SMMLV','1-2SMMLV','2-3SMMLV','3-4SMMLV','4-5SMMLV','>5SMMLV',Incrementan,Disminuyen)%>%
  kbl(caption = "Matriz de trancisión entre Noviembre y Diciembre de 2020", align = "lcccccccc", 
      col.names = c('Rango IBC','<=1SMMLV','1-2SMMLV','2-3SMMLV','3-4SMMLV','4-5SMMLV','>5SMMLV','Disminuyen %','Incrementan %'))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 0, bold = TRUE)%>%
  column_spec(column = 1, bold = TRUE)%>%
  column_spec(column = 6,border_right  = TRUE)

MT12_19_dep<-data.frame(paste0(round(matriz_12_19$transision_dep[,1]*100,2),"%"),paste0(round(matriz_12_19$transision_dep[,2]*100,2),"%"),paste0(round(matriz_12_19$transision_dep[,3]*100,2),"%"),paste0(round(matriz_12_19$transision_dep[,4]*100,2),"%"),paste0(round(matriz_12_19$transision_dep[,5]*100,2),"%"),paste0(round(matriz_12_19$transision_dep[,6]*100,2),"%"),paste0(round(matriz_12_19$transision_dep[,7]*100,2),"%"),paste0(round(matriz_12_19$transision_dep[,8]*100,2),"%"));colnames(MT12_19_dep)<-c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV","Incrementan","Disminuyen")
MT12_19_dep$Rango_pre<-c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV")
MT12_19_dep%>%
  select(Rango_pre,'<=1SMMLV','1-2SMMLV','2-3SMMLV','3-4SMMLV','4-5SMMLV','>5SMMLV',Incrementan,Disminuyen)%>%
  kbl(caption = "Matriz de trancisión entre Noviembre y Diciembre de 2020", align = "lcccccccc", 
      col.names = c('Rango IBC','<=1SMMLV','1-2SMMLV','2-3SMMLV','3-4SMMLV','4-5SMMLV','>5SMMLV','Disminuyen %','Incrementan %'))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 0, bold = TRUE)%>%
  column_spec(column = 1, bold = TRUE)%>%
  column_spec(column = 6,border_right  = TRUE)

paste0(round(mean(matriz_12_19$transision_dep[,7])*100,2),"%")
diag(matriz_12_19$transision_dep[,1:6])

##cruce ultimo trimestre

setwd("D:/Nuevos_Insumos_IMC/Matrices de transicion/")

salen <- "Matriz_salen_9_12_21_new_dep1.csv"
entran <- "Matriz_entran_9_12_21_new_dep1.csv"
permanecen <- "Matriz_permanentes_9_12_21_new1.csv"

trimestre<-function(salen,entran,permanecen){
  salen_tab<-read.csv(salen,sep = ",",dec = ".",na.strings = "$null$")
  entran_tab<-read.csv(entran,sep = ",",dec = ".",na.strings = "$null$")
  permanecen_tab<-read.csv(permanecen,sep = ",",dec = ".",na.strings = "$null$")

  res_salen_tab<-data.frame(salen_tab%>%group_by(Rango_IBC_pre)%>%summarise(Total_salen = sum(Total_personas_salen_dep, na.rm =TRUE)))
  res_entran_tab<-data.frame(entran_tab%>%group_by(Rango_IBC_pos)%>%summarise(Total_entran = sum(Total_personas_entran_dep, na.rm =TRUE)))
  res_permanecen_tab<-data.frame(permanecen_tab%>%
    filter(Tipologia_pos == "Dep_sec_priv", Tipologia_pre == "Dep_sec_priv")%>%group_by(Rango_IBC_pos)%>%summarise(Tota_permanecen = sum(Total_personas_permanecen, na.rm =TRUE)))
  flujo_Tdep<-merge(res_entran_tab,res_permanecen_tab, by = "Rango_IBC_pos", all = TRUE)
  flujo_Tdep<-merge(flujo_Tdep,res_salen_tab, by.x = "Rango_IBC_pos", by.y = "Rango_IBC_pre", all = TRUE)

  flujo_Tdep$Rango_agrupa<-rep(0,nrow(flujo_Tdep))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos <= 1 ,"Rango_agrupa"] <- rep(1,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos <= 1,]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa"] <- rep(2,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(1.5,2),]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa"] <- rep(3,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(2.5,3),]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa"] <- rep(4,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(3.5,4),]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa"] <- rep(5,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(4.5,5),]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos > 5 ,"Rango_agrupa"] <- rep(6,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos> 5,]))

  flujo_Tdep_res <-data.frame(flujo_Tdep%>%group_by(Rango_agrupa)%>%summarise(Total_entran  = sum(Total_entran,na.rm = TRUE),Tota_permanecen   = sum(Tota_permanecen ,na.rm = TRUE),Total_salen  = sum(Total_salen ,na.rm = TRUE)))
  flujo_Tdep_resT <-data.frame(flujo_Tdep%>%summarise(Total_entran  = sum(Total_entran,na.rm = TRUE),Tota_permanecen   = sum(Tota_permanecen ,na.rm = TRUE),Total_salen  = sum(Total_salen ,na.rm = TRUE)))
  flujo_Tdep_resT$Rango_agrupa<-"Total"
  flujo_Tdep_res<-rbind(flujo_Tdep_res,flujo_Tdep_resT)
  flujo_Tdep_res$Rango<-c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV","Total")
  flujo_Tdep_res$por_entrada<-flujo_Tdep_res$Total_entran/(flujo_Tdep_res$Tota_permanecen+flujo_Tdep_res$Total_entran)
  flujo_Tdep_res$por_salen<-flujo_Tdep_res$Total_salen/(flujo_Tdep_res$Tota_permanecen+flujo_Tdep_res$Total_salen)

  res_sale_demo<-data.frame(salen_tab%>%group_by(genero_Max_pre,edad_Max_pre)%>%summarise(total_cot= sum(Total_personas_salen_dep,na.rm = TRUE)))
  res_entra_demo<-data.frame(entran_tab%>%group_by(genero_Max_pos,edad_Max_pos)%>%summarise(total_cot = sum(Total_personas_entran_dep,na.rm = TRUE)))
  
  salen_tab1<-data.frame(salen_tab%>%group_by(Tipologia_pos_Max)%>%summarise(Total_salen = sum(Total_personas_salen_dep,na.rm =TRUE)))
  salen_tab1$por_tipo<-salen_tab1$Total_salen/sum(salen_tab1$Total_salen)
  salidas = list( matriz = flujo_Tdep_res, salida = res_sale_demo, entran =res_entra_demo,tabla_salida = salen_tab1)
  salidas
}

salida_trim_2021<-trimestre("Matriz_salen_9_12_21_new_dep1.csv","Matriz_entran_9_12_21_new_dep1.csv","Matriz_permanentes_9_12_21_new1.csv")
salida_trim_2020<-trimestre("Matriz_salen_9_12_20_new_dep1.csv","Matriz_entran_9_12_20_new_dep1.csv","Matriz_permanentes_9_12_20_new1.csv")
salida_trim_2019<-trimestre("Matriz_salen_9_12_19_new_dep1.csv","Matriz_entran_9_12_19_new_dep1.csv","Matriz_permanentes_9_12_19_new1.csv")

salida_trim_2021$matriz$Total_entran1<-formatC(salida_trim_2021$matriz$Total_entran,format = "f", big.mark = ".", decimal.mark = ",",digits = 0)
salida_trim_2021$matriz$Tota_permanecen1<-formatC(salida_trim_2021$matriz$Tota_permanecen,format = "f", big.mark = ".", decimal.mark = ",",digits = 0)
salida_trim_2021$matriz$Total_salen1<-formatC(salida_trim_2021$matriz$Total_salen,format = "f", big.mark = ".", decimal.mark = ",",digits = 0)
salida_trim_2021$matriz$por_entrada1<-paste0(round(salida_trim_2021$matriz$por_entrada*100,2),"%")
salida_trim_2021$matriz$por_salen1<-paste0(round(salida_trim_2021$matriz$por_salen*100,2),"%")

tabla_trim21<-salida_trim_2021$matriz%>%select(Rango,Total_entran1,Tota_permanecen1,Total_salen1,por_entrada1, por_salen1)
tabla_trim21_1<-ggtexttable(tabla_trim21,rows = NULL,
    cols = c('Rango IBC','Entran','Permanecen','Salen','Entran %','Salen %'),
    theme = ttheme("light", base_size = 10))

#salida_trim_2021$matriz$Tota_permanecen[7]+salida_trim_2021$matriz$Total_entran[7]
#salida_trim_2021$matriz$Tota_permanecen[7]+salida_trim_2021$matriz$Total_salen[7]

salida_tipo21<-salida_trim_2021$tabla_salida 
salida_tipo21$por_salen<-salida_tipo21$Total_salen/sum(salida_tipo21$Total_salen)
grafico_salida<-ggplot(data = salida_tipo21, aes(x = Tipologia_pos_Max , y = Total_salen))+geom_bar(stat = "identity")+coord_flip()+
  labs(title = "Total personas Dependientes privadas que salen de aportar", x = "Tipologia de cambio de las personas que salen", y = "Total personas salen")+theme_minimal()+
  geom_text(aes(label = formatC(Total_salen, big.mark = ".",digits = 0, decimal.mark = ",", format = "f")), hjust = 0, position = position_fill(), size = 5, color = "orange")

ggarrange(tabla_trim20_1,grafico_salida,nrow=1)

### analisis por seccion economica 

setwd("D:/Nuevos_Insumos_IMC/Informacion_general/")

tabla4 <- "Salidas_generales_Aeconomica9_12_2021_.csv"
tabla3 <- "Salidas_generales_Aeconomica9_12_2020.csv"
tabla2 <- "Salidas_generales_Aeconomica9_12_2019.csv"
tabla1 <- "Salidas_generales_Aeconomica9_12_2018.csv"
year = 2021
month = 12

base4<-read.csv2(tabla4,dec = ".")
base3<-read.csv2(tabla3,dec = ".")
base2<-read.csv2(tabla2,dec = ".")
base1<-read.csv2(tabla1,dec = ".")

base4_res4<-data.frame(base4%>%filter(Tipologia == "Dep_sec_priv", YEAR == year, MONTH == month)%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base4_resT4<-data.frame(base4%>%filter(Tipologia == "Dep_sec_priv", YEAR == year, MONTH == month)%>%summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base4_resT4$Seccion_fn = "Total"
base4_res4<-rbind(base4_res4,base4_resT4)
base4_res3<-data.frame(base4%>%filter(Tipologia == "Dep_sec_priv", YEAR == year, MONTH == (month-1))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes3 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base4_resT3<-data.frame(base4%>%filter(Tipologia == "Dep_sec_priv", YEAR == year, MONTH == (month-1))%>%summarise(Total_cotizantes3 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base4_resT3$Seccion_fn = "Total"
base4_res3<-rbind(base4_res3,base4_resT3)
base4_res2<-data.frame(base4%>%filter(Tipologia == "Dep_sec_priv", YEAR == year, MONTH == (month-2))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base4_resT2<-data.frame(base4%>%filter(Tipologia == "Dep_sec_priv", YEAR == year, MONTH == (month-2))%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base4_resT2$Seccion_fn = "Total"
base4_res2<-rbind(base4_res2,base4_resT2)
base4_res1<-data.frame(base4%>%filter(Tipologia == "Dep_sec_priv", YEAR == year, MONTH == (month-3))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base4_resT1<-data.frame(base4%>%filter(Tipologia == "Dep_sec_priv", YEAR == year, MONTH == (month-3))%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base4_resT1$Seccion_fn = "Total"
base4_res1<-rbind(base4_res1,base4_resT1)
base4_res_general<-merge(base4_res1,base4_res2, by = "Seccion_fn", all = TRUE)
base4_res_general<-merge(base4_res_general,base4_res3, by = "Seccion_fn", all = TRUE)
base4_res_general<-merge(base4_res_general,base4_res4, by = "Seccion_fn", all = TRUE)
colnames(base4_res_general)<-c( "Seccion_fn","Total_cotizantes1_21","Total_cotizantes2_21","Total_cotizantes3_21","Total_cotizantes4_21")

base3_res4<-data.frame(base3%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-1), MONTH == month)%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base3_resT4<-data.frame(base3%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-1), MONTH == month)%>%summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base3_resT4$Seccion_fn = "Total"
base3_res4<-rbind(base3_res4,base3_resT4)
base3_res3<-data.frame(base3%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-1), MONTH == (month-1))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes3 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base3_resT3<-data.frame(base3%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-1), MONTH == (month-1))%>%summarise(Total_cotizantes3 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base3_resT3$Seccion_fn = "Total"
base3_res3<-rbind(base3_res3,base3_resT3)
base3_res2<-data.frame(base3%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-1), MONTH == (month-2))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base3_resT2<-data.frame(base3%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-1), MONTH == (month-2))%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base3_resT2$Seccion_fn = "Total"
base3_res2<-rbind(base3_res2,base3_resT2)
base3_res1<-data.frame(base3%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-1), MONTH == (month-3))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base3_resT1<-data.frame(base3%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-1), MONTH == (month-3))%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base3_resT1$Seccion_fn = "Total"
base3_res1<-rbind(base3_res1,base3_resT1)
base3_res_general<-merge(base3_res1,base3_res2, by = "Seccion_fn", all = TRUE)
base3_res_general<-merge(base3_res_general,base3_res3, by = "Seccion_fn", all = TRUE)
base3_res_general<-merge(base3_res_general,base3_res4, by = "Seccion_fn", all = TRUE)
colnames(base3_res_general)<-c( "Seccion_fn","Total_cotizantes1_20","Total_cotizantes2_20","Total_cotizantes3_20","Total_cotizantes4_20")

base2_res4<-data.frame(base2%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-2), MONTH == month)%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base2_resT4<-data.frame(base2%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-2), MONTH == month)%>%summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base2_resT4$Seccion_fn = "Total"
base2_res4<-rbind(base2_res4,base2_resT4)
base2_res3<-data.frame(base2%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-2), MONTH == (month-1))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes3 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base2_resT3<-data.frame(base2%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-2), MONTH == (month-1))%>%summarise(Total_cotizantes3 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base2_resT3$Seccion_fn = "Total"
base2_res3<-rbind(base2_res3,base2_resT3)
base2_res2<-data.frame(base2%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-2), MONTH == (month-2))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base2_resT2<-data.frame(base2%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-2), MONTH == (month-2))%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base2_resT2$Seccion_fn = "Total"
base2_res2<-rbind(base2_res2,base2_resT2)
base2_res1<-data.frame(base2%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-2), MONTH == (month-3))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base2_resT1<-data.frame(base2%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-2), MONTH == (month-3))%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base2_resT1$Seccion_fn = "Total"
base2_res1<-rbind(base2_res1,base2_resT1)
base2_res_general<-merge(base2_res1,base2_res2, by = "Seccion_fn", all = TRUE)
base2_res_general<-merge(base2_res_general,base2_res3, by = "Seccion_fn", all = TRUE)
base2_res_general<-merge(base2_res_general,base2_res4, by = "Seccion_fn", all = TRUE)
colnames(base2_res_general)<-c( "Seccion_fn","Total_cotizantes1_19","Total_cotizantes2_19","Total_cotizantes3_19","Total_cotizantes4_19")

base1_res4<-data.frame(base1%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-3), MONTH == month)%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base1_resT4<-data.frame(base1%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-3), MONTH == month)%>%summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base1_resT4$Seccion_fn = "Total"
base1_res4<-rbind(base1_res4,base1_resT4)
base1_res3<-data.frame(base1%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-3), MONTH == (month-1))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes3 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base1_resT3<-data.frame(base1%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-3), MONTH == (month-1))%>%summarise(Total_cotizantes3 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base1_resT3$Seccion_fn = "Total"
base1_res3<-rbind(base1_res3,base1_resT3)
base1_res2<-data.frame(base1%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-3), MONTH == (month-2))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base1_resT2<-data.frame(base1%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-3), MONTH == (month-2))%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base1_resT2$Seccion_fn = "Total"
base1_res2<-rbind(base1_res2,base1_resT2)
base1_res1<-data.frame(base1%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-3), MONTH == (month-3))%>%group_by(Seccion_fn)%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base1_resT1<-data.frame(base1%>%filter(Tipologia == "Dep_sec_priv", YEAR == (year-3), MONTH == (month-3))%>%summarise(Total_cotizantes2 = sum(Total_relaciones_laborales, na.rm =TRUE)))
base1_resT1$Seccion_fn = "Total"
base1_res1<-rbind(base1_res1,base1_resT1)
base1_res_general<-merge(base1_res1,base1_res2, by = "Seccion_fn", all = TRUE)
base1_res_general<-merge(base1_res_general,base1_res3, by = "Seccion_fn", all = TRUE)
base1_res_general<-merge(base1_res_general,base1_res4, by = "Seccion_fn", all = TRUE)
colnames(base1_res_general)<-c( "Seccion_fn","Total_cotizantes1_18","Total_cotizantes2_18","Total_cotizantes3_18","Total_cotizantes4_18")

base_res_general<-merge(base1_res_general,base2_res_general, by = "Seccion_fn", all = TRUE)
base_res_general<-merge(base_res_general,base3_res_general, by = "Seccion_fn", all = TRUE)
base_res_general<-merge(base_res_general,base4_res_general, by = "Seccion_fn", all = TRUE)

nom_seccion <- read.csv2("D:/Aplicacion/tablero/Secciones_economicas_desc2.csv")

base_res_general<-merge(base_res_general,nom_seccion, by.x = "Seccion_fn", by.y = "Seccion_PILA", all.x =TRUE)
for(i in 6:(ncol(base_res_general)-1)){base_res_general$var_anual<-(base_res_general[,i]-base_res_general[,(i-4)])/base_res_general[,(i-4)];colnames(base_res_general)<-c(colnames(base_res_general[,1:(ncol(base_res_general)-1)]),paste0("Var_anual_",substr(colnames(base_res_general)[i],17,20)))}
for(i in 3:17){base_res_general$var_anual<-(base_res_general[,i]-base_res_general[,(i-1)])/base_res_general[,(i-1)];colnames(base_res_general)<-c(colnames(base_res_general[,1:(ncol(base_res_general)-1)]),paste0("Var_mensual_",substr(colnames(base_res_general)[i],17,20)))}
base_res_general$Distribucion_porcentual<-base_res_general[,17]/base_res_general[base_res_general$Seccion_fn == "Total",17]
base_res_general$contribucion_var_mes<-(base_res_general[,17]-base_res_general[,16])/(base_res_general[base_res_general$Seccion_fn == "Total",16])
base_res_general$contribucion_year_mes<-(base_res_general[,17]-base_res_general[,13])/(base_res_general[base_res_general$Seccion_fn == "Total",13])

base_general_publica<-data.frame(
  Secc = base_res_general$Seccion_fn,
  Seccion_nom = base_res_general$Seccion_PILA_nom,
  Total_cot_pre_mes = formatC(base_res_general$Total_cotizantes3_21/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f"),
  Total_cot_pre_año = formatC(base_res_general$Total_cotizantes4_20/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f"),
  Total_cot = formatC(base_res_general$Total_cotizantes4_21/1000,big.mark = ".",decimal.mark = ",",digits = 1,format = "f"),
  Total_Distriucion = paste0(round(base_res_general$Distribucion_porcentual*100,2),"%"),
  Var_mensual = paste0(round(base_res_general$Var_mensual_4_21*100,2),"%"),
  Var_mensual_aporte = paste0(round(base_res_general$contribucion_var_mes*100,2),"%"),
  Var_anual = paste0(round(base_res_general$Var_anual_4_21*100,2),"%"),
  Var_anual_aporte = paste0(round(base_res_general$contribucion_year_mes*100,2),"%"),
  Var_anual_aporte_num = base_res_general$contribucion_year_mes
)

base_general_publica_T<-base_general_publica[base_general_publica$Secc == "Total",]
base_general_publica_NoT<-base_general_publica[base_general_publica$Secc != "Total",]
base_general_publica_NoT<-base_general_publica_NoT[order( desc(base_general_publica_NoT$Var_anual_aporte_num)),]
base_general_publica<-rbind(base_general_publica_NoT,base_general_publica_T)


base_general_publica%>%
  select(Seccion_nom,Total_cot_pre_mes,Total_cot_pre_año,Total_cot,Total_Distriucion,Var_mensual,Var_mensual_aporte,Var_anual,Var_anual_aporte)%>%
  kbl(caption = "Distribución de los cotizantes dependientes de sec. privado por Sección Economica",align = "lcccccccc", 
      col.names = c("Actividad(sección CIIU Rev.4)","Dic-20","Nov-21","Dic-21","Distribución (%)","var Dic - Nov 21","Contri.","var Dic 21-20","Contri."))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 0, bold = TRUE)%>%
  row_spec(row = 23, bold = TRUE)%>%
  column_spec(column = 2,border_right  = TRUE)%>%
  column_spec(column = 6,border_right  = TRUE)%>%
  column_spec(column = 7,border_right  = TRUE)%>%
  column_spec(column = 9,border_right  = TRUE)
  
base_res_general$Var_anual_4_20_cont<-(base_res_general[,13]-base_res_general[,9])/(base_res_general[base_res_general$Seccion_fn == "Total",9])
base_res_general$Var_anual_4_19_cont<-(base_res_general[,9]-base_res_general[,5])/(base_res_general[base_res_general$Seccion_fn == "Total",5])
base_res_general$Var_mes_4_20_cont<-(base_res_general[,13]-base_res_general[,12])/(base_res_general[base_res_general$Seccion_fn == "Total",12])
base_res_general$Var_mes_4_19_cont<-(base_res_general[,9]-base_res_general[,8])/(base_res_general[base_res_general$Seccion_fn == "Total",8])

base_res_general<-base_res_general[order(base_res_general$Total_cotizantes4_21),]

AE_aporte_anual_graf<-data.frame(seccion = rep(base_res_general$Seccion_fn,3),Periodo = rep(c("dic_21","dic_20","dic_19"),c(nrow(base_res_general),nrow(base_res_general),nrow(base_res_general))),Contribucion = c(base_res_general$Var_anual_4_21,base_res_general$Var_anual_4_20,base_res_general$Var_anual_4_19))
AE_aporte_mes_graf<-data.frame(seccion = rep(base_res_general$Seccion_fn,3),Periodo = rep(c("dic_21","dic_20","dic_19"),c(nrow(base_res_general),nrow(base_res_general),nrow(base_res_general))),Contribucion = c(base_res_general$Var_mensual_4_21,base_res_general$Var_mensual_4_20,base_res_general$Var_mensual_4_19))

AE_aporte_anual_graf<-AE_aporte_anual_graf[AE_aporte_anual_graf$seccion != "Total",]
AE_aporte_mes_graf<-AE_aporte_mes_graf[AE_aporte_mes_graf$seccion != "Total",]

Var_anuales<-ggplot(data = AE_aporte_anual_graf,aes(x = seccion , y = Contribucion*100, group = Periodo))+
  geom_line(aes(linetype = Periodo, color = Periodo), size = 0.8)+theme_minimal()+
  labs(title = "Variación anual en 2021, 2020 y 2019",x = "Sección económica", y = "Contribución %")+
  scale_color_manual(values = c("cyan3","cyan4","darkblue"))+
  theme(legend.position = "bottom")

Var_mensuales<-ggplot(data = AE_aporte_mes_graf,aes(x = seccion , y = Contribucion*100, group = Periodo))+
  geom_line(aes(linetype = Periodo, color = Periodo), size = 0.8)+theme_minimal()+
  labs(title = "Variación mensual en 2021, 2020 y 2019",x = "Sección económica", y = "Contribución %")+
  scale_color_manual(values = c("cyan3","cyan4","darkblue"))+
  theme(legend.position = "bottom")

ggarrange(Var_anuales,Var_mensuales,nrow = 1)

matriz_12_20$matriz_ind%>%
  select(Rango_agrupa_2,independientes_entran1,independientes_permanecen1,independientes_salen1,
         independientes_salen_retiro1,independientes_salen_NO_retiro1,Por_entran1,Por_salen1)%>%
  kbl(caption = "Matriz resumen de la dinamica pareada entre Diciembre y Noviembre de 2020", align = "lccccccc", 
      col.names = c("Mes","Entran","Permanecen","Salen","Salen con Retiro","Salen sin Retiro","Entran (%)","Salen (%)"))%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  row_spec(row = 0, bold = TRUE)%>%
  row_spec(row = 6,  underline = TRUE)%>%
  column_spec(column = 1,border_right  = TRUE)%>%
  column_spec(column = 4,border_right  = TRUE)%>%
  column_spec(column = 7, color = "white", background = "darkgreen")%>%
  column_spec(column = 8, color = "white", background = "darkred")

#####trimestre
setwd("D:/Nuevos_Insumos_IMC/Matrices de transicion/")
salen <- "Matriz_salen_9_12_21_new_dep1.csv"
entran <- "Matriz_entran_9_12_19_new_dep1.csv"
permanecen <- "Matriz_permanentes_9_12_21_new1.csv"

trimestre<-function(salen,entran,permanecen){
  salen_tab<-read.csv(salen,sep = ",",dec = ".",na.strings = "$null$")
  entran_tab<-read.csv(entran,sep = ",",dec = ".",na.strings = "$null$")
  permanecen_tab<-read.csv(permanecen,sep = ",",dec = ".",na.strings = "$null$")
  
  res_salen_tab<-data.frame(salen_tab%>%group_by(Rango_IBC_pre)%>%summarise(Total_salen = sum(Total_personas_salen_dep, na.rm =TRUE)))
  res_entran_tab<-data.frame(entran_tab%>%group_by(Rango_IBC_pos)%>%summarise(Total_entran = sum(Total_personas_entran_dep, na.rm =TRUE)))
  res_permanecen_tab<-data.frame(permanecen_tab%>%
                                   filter(Tipologia_pos == "Dep_sec_priv", Tipologia_pre == "Dep_sec_priv")%>%group_by(Rango_IBC_pos)%>%summarise(Tota_permanecen = sum(Total_personas_permanecen, na.rm =TRUE)))
  flujo_Tdep<-merge(res_entran_tab,res_permanecen_tab, by = "Rango_IBC_pos", all = TRUE)
  flujo_Tdep<-merge(flujo_Tdep,res_salen_tab, by.x = "Rango_IBC_pos", by.y = "Rango_IBC_pre", all = TRUE)
  
  flujo_Tdep$Rango_agrupa<-rep(0,nrow(flujo_Tdep))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos <= 1 ,"Rango_agrupa"] <- rep(1,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos <= 1,]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa"] <- rep(2,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(1.5,2),]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa"] <- rep(3,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(2.5,3),]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa"] <- rep(4,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(3.5,4),]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa"] <- rep(5,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos %in% c(4.5,5),]))
  flujo_Tdep[flujo_Tdep$Rango_IBC_pos > 5 ,"Rango_agrupa"] <- rep(6,nrow(flujo_Tdep[flujo_Tdep$Rango_IBC_pos> 5,]))
  
  flujo_Tdep_res <-data.frame(flujo_Tdep%>%group_by(Rango_agrupa)%>%summarise(Total_entran  = sum(Total_entran,na.rm = TRUE),Tota_permanecen   = sum(Tota_permanecen ,na.rm = TRUE),Total_salen  = sum(Total_salen ,na.rm = TRUE)))
  flujo_Tdep_resT <-data.frame(flujo_Tdep%>%summarise(Total_entran  = sum(Total_entran,na.rm = TRUE),Tota_permanecen   = sum(Tota_permanecen ,na.rm = TRUE),Total_salen  = sum(Total_salen ,na.rm = TRUE)))
  flujo_Tdep_resT$Rango_agrupa<-"Total"
  flujo_Tdep_res<-rbind(flujo_Tdep_res,flujo_Tdep_resT)
  flujo_Tdep_res$Rango<-c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV","Total")
  flujo_Tdep_res$por_entrada<-flujo_Tdep_res$Total_entran/(flujo_Tdep_res$Tota_permanecen+flujo_Tdep_res$Total_entran)
  flujo_Tdep_res$por_salen<-flujo_Tdep_res$Total_salen/(flujo_Tdep_res$Tota_permanecen+flujo_Tdep_res$Total_salen)
  
  res_sale_demo<-data.frame(salen_tab%>%group_by(genero_Max_pre,edad_Max_pre)%>%summarise(total_cot= sum(Total_personas_salen_dep,na.rm = TRUE)))
  res_entra_demo<-data.frame(entran_tab%>%group_by(genero_Max_pos,edad_Max_pos)%>%summarise(total_cot = sum(Total_personas_entran_dep,na.rm = TRUE)))
  
  salen_tab1<-data.frame(salen_tab%>%group_by(Tipologia_pos_Max)%>%summarise(Total_salen = sum(Total_personas_salen_dep,na.rm =TRUE)))
  salidas = list( matriz = flujo_Tdep_res, salida = res_sale_demo, entran =res_entra_demo,tabla_salida = salen_tab1)
  salidas
}

salida_trim_2021$matriz$Tota_permanecen[7]+salida_trim_2021$matriz$Total_entran[7]
salida_trim_2021$matriz$Tota_permanecen[7]+salida_trim_2021$matriz$Total_salen[7]

salida_trim_2021<-trimestre("Matriz_salen_9_12_21_new_dep1.csv","Matriz_entran_9_12_21_new_dep1.csv","Matriz_permanentes_9_12_21_new1.csv")
salida_trim_2020<-trimestre("Matriz_salen_9_12_20_new_dep1.csv","Matriz_entran_9_12_20_new_dep1.csv","Matriz_permanentes_9_12_20_new1.csv")
salida_trim_2019<-trimestre("Matriz_salen_9_12_19_new_dep1.csv","Matriz_entran_9_12_19_new_dep1.csv","Matriz_permanentes_9_12_19_new1.csv")

salida_trim_2021$matriz$Total_entran1<-formatC(salida_trim_2021$matriz$Total_entran,format = "f", big.mark = ".", decimal.mark = ",",digits = 0)
salida_trim_2021$matriz$Tota_permanecen1<-formatC(salida_trim_2021$matriz$Tota_permanecen,format = "f", big.mark = ".", decimal.mark = ",",digits = 0)
salida_trim_2021$matriz$Total_salen1<-formatC(salida_trim_2021$matriz$Total_salen,format = "f", big.mark = ".", decimal.mark = ",",digits = 0)
salida_trim_2021$matriz$por_entrada1<-paste0(round(salida_trim_2021$matriz$por_entrada*100,2),"%")
salida_trim_2021$matriz$por_salen1<-paste0(round(salida_trim_2021$matriz$por_salen*100,2),"%")

tabla_trim21<-salida_trim_2021$matriz%>%select(Rango,Total_entran1,Tota_permanecen1,Total_salen1,por_entrada1, por_salen1)
tabla_trim21_1<-ggtexttable(tabla_trim21,rows = NULL,
                            cols = c('Rango IBC','Entran','Permanecen','Salen','Entran %','Salen %'),
                            theme = ttheme("light", base_size = 10))

salida_tipo21<-salida_trim_2021$tabla_salida 
salida_tipo21$por_salen<-salida_tipo21$Total_salen/sum(salida_tipo21$Total_salen)
grafico_salida_21<-ggplot(data = salida_tipo21, aes(x = Tipologia_pos_Max , y = Total_salen))+geom_bar(stat = "identity", fill = "grey80")+coord_flip()+
  labs(title = "Total personas Dependientes privadas que salen de aportar 2020", x = "Tipologia de cambio de las personas que salen", y = "Total personas salen")+theme_minimal()+
  geom_text(aes(label = formatC(Total_salen, big.mark = ".",digits = 0, decimal.mark = ",", format = "f")), hjust = 0, position = position_fill(), size = 6, color = "gold4")

arrange(tabla_trim20_1,grafico_salida_20,nrow=1)

##demografico 

setwd("D:/Nuevos_Insumos_IMC/Informacion_general/")

#tabla <-"Salidas_generales_edad_sexo9_12_2021_.csv"
#mes <- 12
#año <- 2021

Demografico<-function(tabla,mes,año){
  base<-read.csv2(tabla)
  base_NA<-base[!is.na(base$Edad_fin),]
  base_res_m_4_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "M", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m4 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m4 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_4_21_priv)<-c("Edad",paste0("Tot_priv_M_",año,"_",mes),paste0("Aporte_priv_M_",año,"_",mes))
  base_res_f_4_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "F", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f4 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f4 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_4_21_priv)<-c("Edad",paste0("Tot_priv_f_",año,"_",mes),paste0("Aporte_priv_f_",año,"_",mes))
  base_res_m_3_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "M", MONTH == (mes-1),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m3 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m3 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_3_21_priv)<-c("Edad",paste0("Tot_priv_M_",año,"_",(mes-1)),paste0("Aporte_priv_M_",año,"_",(mes-1)))
  base_res_f_3_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "F", MONTH == (mes-1),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f3 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f3 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_3_21_priv)<-c("Edad",paste0("Tot_priv_f_",año,"_",(mes-1)),paste0("Aporte_priv_f_",año,"_",(mes-1)))
  base_res_m_2_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "M", MONTH == (mes-2),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m2 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m2 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_2_21_priv)<-c("Edad",paste0("Tot_priv_M_",año,"_",(mes-2)),paste0("Aporte_priv_M_",año,"_",(mes-2)))
  base_res_f_2_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "F", MONTH == (mes-2),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f2 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f2 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_2_21_priv)<-c("Edad",paste0("Tot_priv_f_",año,"_",(mes-2)),paste0("Aporte_priv_f_",año,"_",(mes-2)))
  base_res_m_1_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "M", MONTH == (mes-3),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m1 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m1 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_1_21_priv)<-c("Edad",paste0("Tot_priv_M_",año,"_",(mes-3)),paste0("Aporte_priv_M_",año,"_",(mes-3)))
  base_res_f_1_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "F", MONTH == (mes-3),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f1 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f1 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_1_21_priv)<-c("Edad",paste0("Tot_priv_f_",año,"_",(mes-3)),paste0("Aporte_priv_f_",año,"_",(mes-3)))
  
  base_res_m_4_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "M", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m4i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m4i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_4_21_ind)<-c("Edad",paste0("Tot_ind_M_",año,"_",mes),paste0("Aporte_ind_M_",año,"_",mes))
  base_res_f_4_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "F", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f4i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f4i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_4_21_ind)<-c("Edad",paste0("Tot_ind_f_",año,"_",mes),paste0("Aporte_ind_f_",año,"_",mes))
  base_res_m_3_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "M", MONTH == (mes-1),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m3i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m3i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_3_21_ind)<-c("Edad",paste0("Tot_ind_M_",año,"_",(mes-1)),paste0("Aporte_ind_M_",año,"_",(mes-1)))
  base_res_f_3_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "F", MONTH == (mes-1),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f3i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f3i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_3_21_ind)<-c("Edad",paste0("Tot_ind_f_",año,"_",(mes-1)),paste0("Aporte_ind_f_",año,"_",(mes-1)))
  base_res_m_2_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "M", MONTH == (mes-2),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m2i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m2i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_2_21_ind)<-c("Edad",paste0("Tot_ind_M_",año,"_",(mes-2)),paste0("Aporte_ind_M_",año,"_",(mes-2)))
  base_res_f_2_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "F", MONTH == (mes-2),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f2i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f2i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_2_21_ind)<-c("Edad",paste0("Tot_ind_f_",año,"_",(mes-2)),paste0("Aporte_ind_f_",año,"_",(mes-2)))
  base_res_m_1_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "M", MONTH == (mes-3),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m1i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m1i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_1_21_ind)<-c("Edad",paste0("Tot_ind_M_",año,"_",(mes-3)),paste0("Aporte_ind_M_",año,"_",(mes-3)))
  base_res_f_1_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "F", MONTH == (mes-3),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f1i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f1i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_1_21_ind)<-c("Edad",paste0("Tot_ind_f_",año,"_",(mes-3)),paste0("Aporte_ind_f_",año,"_",(mes-3)))
  
  base_res_m_4_21_tot<-data.frame(base_NA%>%filter(Sexo_fin == "M", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m4tot = sum(Total_relaciones_laborales, na.rm = TRUE)));#colnames(base_res_m_4_21_tot)<-c("Edad",paste0("Tot_tot_M_",año,"_",mes),paste0("Aporte_tot_M_",año,"_",mes))
  base_res_f_4_21_tot<-data.frame(base_NA%>%filter(Sexo_fin == "F", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f4tot = sum(Total_relaciones_laborales, na.rm = TRUE)));#colnames(base_res_f_4_21_tot)<-c("Edad",paste0("Tot_tot_f_",año,"_",mes),paste0("Aporte_tot_f_",año,"_",mes))
  
  tabla_demo_tot<-merge(base_res_f_4_21_tot,base_res_m_4_21_tot,by = "Edad_fin", all = TRUE)
  
  tabla_demo<-merge(base_res_m_4_21_priv,base_res_m_3_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_2_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_1_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_4_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_3_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_2_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_1_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_4_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_3_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_2_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_1_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_4_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_3_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_2_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_1_21_ind, by = "Edad_fin", all = TRUE)

  tabla_demo$Edad_cat<-rep(0,nrow(tabla_demo))
  tabla_demo[tabla_demo$Edad_fin>=0 & tabla_demo$Edad_fin<=4, "Edad_cat"]<-rep(1,nrow(tabla_demo[tabla_demo$Edad_fin>=0 & tabla_demo$Edad_fin<=4, ]))
  tabla_demo[tabla_demo$Edad_fin>=5 & tabla_demo$Edad_fin<=9, "Edad_cat"]<-rep(2,nrow(tabla_demo[tabla_demo$Edad_fin>=5 & tabla_demo$Edad_fin<=9, ]))
  tabla_demo[tabla_demo$Edad_fin>=10 & tabla_demo$Edad_fin<=14, "Edad_cat"]<-rep(3,nrow(tabla_demo[tabla_demo$Edad_fin>=10 & tabla_demo$Edad_fin<=14, ]))
  tabla_demo[tabla_demo$Edad_fin>=15 & tabla_demo$Edad_fin<=19, "Edad_cat"]<-rep(4,nrow(tabla_demo[tabla_demo$Edad_fin>=15 & tabla_demo$Edad_fin<=19, ]))
  tabla_demo[tabla_demo$Edad_fin>=20 & tabla_demo$Edad_fin<=24, "Edad_cat"]<-rep(5,nrow(tabla_demo[tabla_demo$Edad_fin>=20 & tabla_demo$Edad_fin<=24, ]))
  tabla_demo[tabla_demo$Edad_fin>=25 & tabla_demo$Edad_fin<=29, "Edad_cat"]<-rep(6,nrow(tabla_demo[tabla_demo$Edad_fin>=25 & tabla_demo$Edad_fin<=29, ]))
  tabla_demo[tabla_demo$Edad_fin>=30 & tabla_demo$Edad_fin<=34, "Edad_cat"]<-rep(7,nrow(tabla_demo[tabla_demo$Edad_fin>=30 & tabla_demo$Edad_fin<=34, ]))
  tabla_demo[tabla_demo$Edad_fin>=35 & tabla_demo$Edad_fin<=39, "Edad_cat"]<-rep(8,nrow(tabla_demo[tabla_demo$Edad_fin>=35 & tabla_demo$Edad_fin<=39, ]))
  tabla_demo[tabla_demo$Edad_fin>=40 & tabla_demo$Edad_fin<=44, "Edad_cat"]<-rep(9,nrow(tabla_demo[tabla_demo$Edad_fin>=40 & tabla_demo$Edad_fin<=44, ]))
  tabla_demo[tabla_demo$Edad_fin>=45 & tabla_demo$Edad_fin<=49, "Edad_cat"]<-rep(10,nrow(tabla_demo[tabla_demo$Edad_fin>=45 & tabla_demo$Edad_fin<=49, ]))
  tabla_demo[tabla_demo$Edad_fin>=50 & tabla_demo$Edad_fin<=54, "Edad_cat"]<-rep(11,nrow(tabla_demo[tabla_demo$Edad_fin>=50 & tabla_demo$Edad_fin<=54, ]))
  tabla_demo[tabla_demo$Edad_fin>=55 & tabla_demo$Edad_fin<=59, "Edad_cat"]<-rep(12,nrow(tabla_demo[tabla_demo$Edad_fin>=55 & tabla_demo$Edad_fin<=59, ]))
  tabla_demo[tabla_demo$Edad_fin>=60 & tabla_demo$Edad_fin<=64, "Edad_cat"]<-rep(13,nrow(tabla_demo[tabla_demo$Edad_fin>=60 & tabla_demo$Edad_fin<=64, ]))
  tabla_demo[tabla_demo$Edad_fin>=65 & tabla_demo$Edad_fin<=69, "Edad_cat"]<-rep(14,nrow(tabla_demo[tabla_demo$Edad_fin>=65 & tabla_demo$Edad_fin<=69, ]))
  tabla_demo[tabla_demo$Edad_fin>=70 & tabla_demo$Edad_fin<=74, "Edad_cat"]<-rep(15,nrow(tabla_demo[tabla_demo$Edad_fin>=70 & tabla_demo$Edad_fin<=74, ]))
  tabla_demo[tabla_demo$Edad_fin>=75 & tabla_demo$Edad_fin<=79, "Edad_cat"]<-rep(16,nrow(tabla_demo[tabla_demo$Edad_fin>=75 & tabla_demo$Edad_fin<=79, ]))
  tabla_demo[tabla_demo$Edad_fin>=80, "Edad_cat"]<-rep(17,nrow(tabla_demo[tabla_demo$Edad_fin>=80, ]))
  
  tabla_demo2<-data.frame(tabla_demo%>%group_by(Edad_cat)%>%summarise(
    Total_cot_m4 = sum(Total_cot_m4,na.rm = TRUE),
    Total_cot_f4 = sum(Total_cot_f4,na.rm = TRUE),
    Total_cot_m3 = sum(Total_cot_m3,na.rm = TRUE),
    Total_cot_f3 = sum(Total_cot_f3,na.rm = TRUE),
    Total_cot_m2 = sum(Total_cot_m2,na.rm = TRUE),
    Total_cot_f2 = sum(Total_cot_f2,na.rm = TRUE),
    Total_cot_m1 = sum(Total_cot_m1,na.rm = TRUE),
    Total_cot_f1 = sum(Total_cot_f1,na.rm = TRUE),
    Total_cot_m4i = sum(Total_cot_m4i,na.rm = TRUE),
    Total_cot_f4i = sum(Total_cot_f4i,na.rm = TRUE),
    Total_cot_m3i = sum(Total_cot_m3i,na.rm = TRUE),
    Total_cot_f3i = sum(Total_cot_f3i,na.rm = TRUE),
    Total_cot_m2i = sum(Total_cot_m2i,na.rm = TRUE),
    Total_cot_f2i = sum(Total_cot_f2i,na.rm = TRUE),
    Total_cot_m1i = sum(Total_cot_m1i,na.rm = TRUE),
    Total_cot_f1i = sum(Total_cot_f1i,na.rm = TRUE),
    Pago_Medio_m4 = sum(Pago_Medio_m4,na.rm = TRUE),
    Pago_Medio_f4 = sum(Pago_Medio_f4,na.rm = TRUE),
    Pago_Medio_m3 = sum(Pago_Medio_m3,na.rm = TRUE),
    Pago_Medio_f3 = sum(Pago_Medio_f3,na.rm = TRUE),
    Pago_Medio_m2 = sum(Pago_Medio_m2,na.rm = TRUE),
    Pago_Medio_f2 = sum(Pago_Medio_f2,na.rm = TRUE),
    Pago_Medio_m1 = sum(Pago_Medio_m1,na.rm = TRUE),
    Pago_Medio_f1 = sum(Pago_Medio_f1,na.rm = TRUE),
    Pago_Medio_m4i = sum(Pago_Medio_m4i,na.rm = TRUE),
    Pago_Medio_f4i = sum(Pago_Medio_f4i,na.rm = TRUE),
    Pago_Medio_m3i = sum(Pago_Medio_m3i,na.rm = TRUE),
    Pago_Medio_f3i = sum(Pago_Medio_f3i,na.rm = TRUE),
    Pago_Medio_m2i = sum(Pago_Medio_m2i,na.rm = TRUE),
    Pago_Medio_f2i = sum(Pago_Medio_f2i,na.rm = TRUE),
    Pago_Medio_m1i = sum(Pago_Medio_m1i,na.rm = TRUE),
    Pago_Medio_f1i = sum(Pago_Medio_f1i,na.rm = TRUE)))

  tabla_demo2_tot<-data.frame(tabla_demo%>%summarise(
    Total_cot_m4 = sum(Total_cot_m4,na.rm = TRUE),
    Total_cot_f4 = sum(Total_cot_f4,na.rm = TRUE),
    Total_cot_m3 = sum(Total_cot_m3,na.rm = TRUE),
    Total_cot_f3 = sum(Total_cot_f3,na.rm = TRUE),
    Total_cot_m2 = sum(Total_cot_m2,na.rm = TRUE),
    Total_cot_f2 = sum(Total_cot_f2,na.rm = TRUE),
    Total_cot_m1 = sum(Total_cot_m1,na.rm = TRUE),
    Total_cot_f1 = sum(Total_cot_f1,na.rm = TRUE),
    Total_cot_m4i = sum(Total_cot_m4i,na.rm = TRUE),
    Total_cot_f4i = sum(Total_cot_f4i,na.rm = TRUE),
    Total_cot_m3i = sum(Total_cot_m3i,na.rm = TRUE),
    Total_cot_f3i = sum(Total_cot_f3i,na.rm = TRUE),
    Total_cot_m2i = sum(Total_cot_m2i,na.rm = TRUE),
    Total_cot_f2i = sum(Total_cot_f2i,na.rm = TRUE),
    Total_cot_m1i = sum(Total_cot_m1i,na.rm = TRUE),
    Total_cot_f1i = sum(Total_cot_f1i,na.rm = TRUE),
    Pago_Medio_m4 = sum(Pago_Medio_m4,na.rm = TRUE),
    Pago_Medio_f4 = sum(Pago_Medio_f4,na.rm = TRUE),
    Pago_Medio_m3 = sum(Pago_Medio_m3,na.rm = TRUE),
    Pago_Medio_f3 = sum(Pago_Medio_f3,na.rm = TRUE),
    Pago_Medio_m2 = sum(Pago_Medio_m2,na.rm = TRUE),
    Pago_Medio_f2 = sum(Pago_Medio_f2,na.rm = TRUE),
    Pago_Medio_m1 = sum(Pago_Medio_m1,na.rm = TRUE),
    Pago_Medio_f1 = sum(Pago_Medio_f1,na.rm = TRUE),
    Pago_Medio_m4i = sum(Pago_Medio_m4i,na.rm = TRUE),
    Pago_Medio_f4i = sum(Pago_Medio_f4i,na.rm = TRUE),
    Pago_Medio_m3i = sum(Pago_Medio_m3i,na.rm = TRUE),
    Pago_Medio_f3i = sum(Pago_Medio_f3i,na.rm = TRUE),
    Pago_Medio_m2i = sum(Pago_Medio_m2i,na.rm = TRUE),
    Pago_Medio_f2i = sum(Pago_Medio_f2i,na.rm = TRUE),
    Pago_Medio_m1i = sum(Pago_Medio_m1i,na.rm = TRUE),
    Pago_Medio_f1i = sum(Pago_Medio_f1i,na.rm = TRUE)))
    
  tabla_demo2_tot$Edad_cat<-"Total"
  tabla_demo2<-rbind(tabla_demo2,tabla_demo2_tot)
  tabla_demo2$edad_rango<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+","Total")
  colnames(tabla_demo)<-c("Edad",paste0(colnames(tabla_demo[2:ncol(tabla_demo)]),"_",mes,"_",año))
  colnames(tabla_demo2)<-c("Edad",paste0(colnames(tabla_demo2[2:ncol(tabla_demo2)]),"_",mes,"_",año))
  salida = list(tabla_demo = tabla_demo, tabla_demo_quiquenales = tabla_demo2, tabla_demo_tot = tabla_demo_tot)
  salida
}

salida_9_12_21<-Demografico("Salidas_generales_edad_sexo9_12_2021.csv",12,2021)
salida_9_12_20<-Demografico("Salidas_generales_edad_sexo9_12_2020.csv",12,2020)
salida_9_12_19<-Demografico("Salidas_generales_edad_sexo9_12_2019.csv",12,2019)

salida_demo_general<-merge(salida_9_12_19$tabla_demo,salida_9_12_20$tabla_demo,by  = "Edad", all = TRUE)
salida_demo_general<-merge(salida_demo_general,salida_9_12_21$tabla_demo,by  = "Edad", all = TRUE)
salida_demo_general$rango_interes<-rep(0,nrow(salida_demo_general))
salida_demo_general[salida_demo_general$Edad>=18 & salida_demo_general$Edad<=28,"rango_interes"]<-rep(1,nrow(salida_demo_general[salida_demo_general$Edad>=18 & salida_demo_general$Edad<=28,]))
tabla_demo_quiquenales<-merge(salida_9_12_19$tabla_demo_quiquenales,salida_9_12_20$tabla_demo_quiquenales,by  = "Edad", all = TRUE)
tabla_demo_quiquenales<-merge(tabla_demo_quiquenales,salida_9_12_21$tabla_demo_quiquenales,by  = "Edad", all = TRUE)

Total_hombres<-c(sum(salida_demo_general[,8], na.rm = TRUE),sum(salida_demo_general[,6], na.rm = TRUE),sum(salida_demo_general[,4], na.rm = TRUE),sum(salida_demo_general[,2], na.rm = TRUE),
                   sum(salida_demo_general[,41], na.rm = TRUE),sum(salida_demo_general[,39], na.rm = TRUE),sum(salida_demo_general[,37], na.rm = TRUE),sum(salida_demo_general[,35], na.rm = TRUE),
                   sum(salida_demo_general[,74], na.rm = TRUE),sum(salida_demo_general[,72], na.rm = TRUE),sum(salida_demo_general[,70], na.rm = TRUE),sum(salida_demo_general[,68], na.rm = TRUE),
                   sum(salida_demo_general[,24], na.rm = TRUE),sum(salida_demo_general[,22], na.rm = TRUE),sum(salida_demo_general[,20], na.rm = TRUE),sum(salida_demo_general[,18], na.rm = TRUE),
                   sum(salida_demo_general[,57], na.rm = TRUE),sum(salida_demo_general[,55], na.rm = TRUE),sum(salida_demo_general[,53], na.rm = TRUE),sum(salida_demo_general[,51], na.rm = TRUE),
                   sum(salida_demo_general[,90], na.rm = TRUE),sum(salida_demo_general[,88], na.rm = TRUE),sum(salida_demo_general[,86], na.rm = TRUE),sum(salida_demo_general[,84], na.rm = TRUE))
Total_mujeres<-c(sum(salida_demo_general[,16], na.rm = TRUE),sum(salida_demo_general[,14], na.rm = TRUE),sum(salida_demo_general[,12], na.rm = TRUE),sum(salida_demo_general[,10], na.rm = TRUE),
                   sum(salida_demo_general[,49], na.rm = TRUE),sum(salida_demo_general[,47], na.rm = TRUE),sum(salida_demo_general[,45], na.rm = TRUE),sum(salida_demo_general[,43], na.rm = TRUE),
                   sum(salida_demo_general[,82], na.rm = TRUE),sum(salida_demo_general[,80], na.rm = TRUE),sum(salida_demo_general[,78], na.rm = TRUE),sum(salida_demo_general[,76], na.rm = TRUE),
                   sum(salida_demo_general[,32], na.rm = TRUE),sum(salida_demo_general[,30], na.rm = TRUE),sum(salida_demo_general[,28], na.rm = TRUE),sum(salida_demo_general[,26], na.rm = TRUE),
                   sum(salida_demo_general[,65], na.rm = TRUE),sum(salida_demo_general[,63], na.rm = TRUE),sum(salida_demo_general[,61], na.rm = TRUE),sum(salida_demo_general[,59], na.rm = TRUE),
                   sum(salida_demo_general[,98], na.rm = TRUE),sum(salida_demo_general[,96], na.rm = TRUE),sum(salida_demo_general[,94], na.rm = TRUE),sum(salida_demo_general[,92], na.rm = TRUE))
Total_jovenes_M<-c(sum(salida_demo_general[salida_demo_general$rango_interes == 1,8], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,6], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,4], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,2], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,41], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,39], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,37], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,35], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,74], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,72], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,70], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,68], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,24], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,22], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,20], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,18], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,57], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,55], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,53], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,51], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,90], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,88], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,86], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,84], na.rm = TRUE))
Total_jovenes_F<-c(sum(salida_demo_general[salida_demo_general$rango_interes == 1,16], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,14], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,12], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,10], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,49], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,47], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,45], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,43], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,82], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,80], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,78], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,76], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,32], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,30], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,28], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,26], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,65], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,63], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,61], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,59], na.rm = TRUE),
                 sum(salida_demo_general[salida_demo_general$rango_interes == 1,98], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,96], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,94], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,92], na.rm = TRUE))
Total_jovenes<-Total_jovenes_M+Total_jovenes_F
Por_jovenes<-(Total_jovenes_M+Total_jovenes_F)/(Total_hombres+Total_mujeres)

salida_sexo<-data.frame(año =rep(rep(c(2019,2020,2021),c(4,4,4)),2),mes = rep(c("Sep","Oct","Nov","Dic"),6),Tipologia = rep(c("Dep.Privado","Independiente"),c(12,12)),
                        Hombres = formatC(Total_hombres,format = "f",digits = 0,big.mark = ".",decimal.mark = ","), 
                        Mujeres = formatC(Total_mujeres,format = "f",digits = 0,big.mark = ".",decimal.mark = ","), 
                        Razon_sexo = round(Total_mujeres/Total_hombres*100,2),
                        Por_hombres = paste0(round(Total_hombres/(Total_hombres+Total_mujeres)*100,2),"%"),
                        Por_mujeres = paste0(round(Total_mujeres/(Total_hombres+Total_mujeres)*100,2),"%"), 
                        Jovenes = formatC(Total_jovenes,format = "f",digits = 0,big.mark = ".",decimal.mark = ","),
                        por_jovenes = paste0(round(Por_jovenes*100,2),"%"))

salida_sexo%>%filter(Tipologia == "Dep.Privado")%>%select(año, mes,Hombres, Mujeres, Razon_sexo, Por_hombres, Por_mujeres, Jovenes, por_jovenes)%>%
  kbl(caption = "Resumen por sexo de los cotizantes del sector privado en 2021", align = "llccccccc", 
      col.names = c("Año","Mes","Total Hombres","Total Mujeres","Razon sexo F/H","Hombres (%)","Mujeres (%)", "Total jovenes", "Jovenes (%)"))%>%
      kable_classic(full_width = F, html_font = "Calibri")%>%
      row_spec(row = 0, bold = TRUE)%>%
      row_spec(row = 4, hline_after = TRUE, background = "#e5e5e5")%>%
      row_spec(row = 8, hline_after = TRUE, background = "#e5e5e5")%>%
      row_spec(row = 12, hline_after = TRUE, background = "#e5e5e5")%>%
      column_spec(column = 7, border_right =TRUE)

tabla_demo_quiquenales_g<-tabla_demo_quiquenales[tabla_demo_quiquenales$Edad %in% c("4","5","6","7","8","9","10","11","12","13","14","15","16"),]
datos_estrutura<-data.frame(Rango_edad = rep(tabla_demo_quiquenales_g$Edad,8), Sexo = rep(c("M","F"),c(4*nrow(tabla_demo_quiquenales_g),4*nrow(tabla_demo_quiquenales_g))),
#cambiar por los meses de referecia                  
      Total_cotizantes_dep = c(tabla_demo_quiquenales_g$Total_cot_m4_12_2019,
                               tabla_demo_quiquenales_g$Total_cot_m4_12_2020,
                               tabla_demo_quiquenales_g$Total_cot_m3_12_2021,
                               tabla_demo_quiquenales_g$Total_cot_m4_12_2021,
                               -1*tabla_demo_quiquenales_g$Total_cot_f4_12_2019,
                               -1*tabla_demo_quiquenales_g$Total_cot_f4_12_2020,
                               -1*tabla_demo_quiquenales_g$Total_cot_f3_12_2021,
                               -1*tabla_demo_quiquenales_g$Total_cot_f4_12_2021),
      Total_cotizantes_ind = c(tabla_demo_quiquenales_g$Total_cot_m4i_12_2019,
                         tabla_demo_quiquenales_g$Total_cot_m4i_12_2020,
                         tabla_demo_quiquenales_g$Total_cot_m3i_12_2021,
                         tabla_demo_quiquenales_g$Total_cot_m4i_12_2021,
                         -1*tabla_demo_quiquenales_g$Total_cot_f4i_12_2019,
                         -1*tabla_demo_quiquenales_g$Total_cot_f4i_12_2020,
                         -1*tabla_demo_quiquenales_g$Total_cot_f3i_12_2021,
                         -1*tabla_demo_quiquenales_g$Total_cot_f4i_12_2021),
      periodo = c(rep("M_12_2019",nrow(tabla_demo_quiquenales_g)),rep("M_12_2020",nrow(tabla_demo_quiquenales_g)),
                  rep("M_11_2021",nrow(tabla_demo_quiquenales_g)),rep("M_12_2021",nrow(tabla_demo_quiquenales_g)),
                  rep("F_12_2019",nrow(tabla_demo_quiquenales_g)),rep("F_12_2020",nrow(tabla_demo_quiquenales_g)),
                  rep("F_11_2021",nrow(tabla_demo_quiquenales_g)),rep("F_12_2021",nrow(tabla_demo_quiquenales_g))))
valores_nombre<-data.frame(Rango_edad = c("4","5","6","7","8","9","10","11","12","13","14","15","16"),Rangos = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-75","76-79"))
datos_estrutura<-merge(datos_estrutura,valores_nombre, by = "Rango_edad", all = TRUE)
datos_estrutura$Rango_edad<-as.numeric(datos_estrutura$Rango_edad)
datos_estrutura<-datos_estrutura[order(datos_estrutura$periodo,datos_estrutura$Rango_edad),]

estructura <-ggplot(data = datos_estrutura, aes(x = Rangos, y = Total_cotizantes_dep, fill = periodo, width = 5))+
  geom_bar(stat = "identity",position = position_dodge(0.6), alpha = 0.6)+
  coord_flip()+theme_minimal()+
  labs(title = "Total cotizantes por grupos de edad",subtitle = "Dependientes de sector privado",x = "Rangos de edad", y = "Total cotizantes")+ylim(-max(abs(datos_estrutura$Total_cotizantes_dep))-5000,max(abs(datos_estrutura$Total_cotizantes_dep))+5000)+
  scale_fill_manual(values = c("orchid1","orchid1","orchid1","orchid4","slateblue1","slateblue1","slateblue1","slateblue4"))

#data_para_estructura<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Salidas_generales_Completo9_12_2021.csv")
#data_para_estructuradesc<-data.frame(data_para_estructura%>%group_by(Tipologia,YEAR,MONTH,Rango_IBC,Sexo_fin,Edad_fin)%>%summarise(Total_relaciones_laborales = sum(Total_relaciones_laborales, na.rm = TRUE)))
#write.csv2(data_para_estructuradesc,"D:/Nuevos_Insumos_IMC/Informacion_general/Estructura_9_12_21.csv")

estructura_insumos<-read.csv2("D:/Nuevos_Insumos_IMC/Informacion_general/Estructura_9_12_21.csv")
estructura_insumosM1<-data.frame(estructura_insumos%>%filter(Rango_IBC == "0.500000",Tipologia == "Dep_sec_priv",Edad_fin >=15 ,Edad_fin <=80, Sexo_fin  %in% c("F","M"))%>%group_by(YEAR,MONTH,Sexo_fin,Edad_fin)%>%summarise(Total_RELAB = sum(Total_relaciones_laborales, na.rm = TRUE)));estructura_insumosM1$Periodo<-rep("<1SMMLV",nrow(estructura_insumosM1))
estructura_insumos1<-data.frame(estructura_insumos%>%filter(Rango_IBC == "1.000000",Tipologia == "Dep_sec_priv",Edad_fin >=15 ,Edad_fin <=80, Sexo_fin  %in% c("F","M"))%>%group_by(YEAR,MONTH,Sexo_fin,Edad_fin)%>%summarise(Total_RELAB = sum(Total_relaciones_laborales, na.rm = TRUE)));estructura_insumos1$Periodo<-rep("1SMMLV",nrow(estructura_insumos1))
estructura_insumos12<-data.frame(estructura_insumos%>%filter(Rango_IBC %in% c("1.500000","2.000000"),Tipologia == "Dep_sec_priv",Edad_fin >=15 ,Edad_fin <=80, Sexo_fin  %in% c("F","M"))%>%group_by(YEAR,MONTH,Sexo_fin,Edad_fin)%>%summarise(Total_RELAB = sum(Total_relaciones_laborales, na.rm = TRUE)));estructura_insumos12$Periodo<-rep("1_2SMMLV",nrow(estructura_insumos12))
estructura_insumos23<-data.frame(estructura_insumos%>%filter(Rango_IBC %in% c("2.500000","3.000000"),Tipologia == "Dep_sec_priv",Edad_fin >=15 ,Edad_fin <=80, Sexo_fin  %in% c("F","M"))%>%group_by(YEAR,MONTH,Sexo_fin,Edad_fin)%>%summarise(Total_RELAB = sum(Total_relaciones_laborales, na.rm = TRUE)));estructura_insumos23$Periodo<-rep("2_3SMMLV",nrow(estructura_insumos23))
estructura_insumos35<-data.frame(estructura_insumos%>%filter(Rango_IBC %in% c("3.500000","4.000000","4.500000","5.000000"),Tipologia == "Dep_sec_priv",Edad_fin >=15 ,Edad_fin <=80, Sexo_fin  %in% c("F","M"))%>%group_by(YEAR,MONTH,Sexo_fin,Edad_fin)%>%summarise(Total_RELAB = sum(Total_relaciones_laborales, na.rm = TRUE)));estructura_insumos35$Periodo<-rep("3_5SMMLV",nrow(estructura_insumos35))
estructura_insumosM5<-data.frame(estructura_insumos%>%filter(Rango_IBC == "5.500000",Tipologia == "Dep_sec_priv",Edad_fin >=15 ,Edad_fin <=80, Sexo_fin  %in% c("F","M"))%>%group_by(YEAR,MONTH,Sexo_fin,Edad_fin)%>%summarise(Total_RELAB = sum(Total_relaciones_laborales, na.rm = TRUE)));estructura_insumosM5$Periodo<-rep(">5SMMLV",nrow(estructura_insumosM5))

estructura_insumos_tot<-rbind(estructura_insumosM1,estructura_insumos1,estructura_insumos12,estructura_insumos23,estructura_insumos35,estructura_insumosM5)
estructura_insumos_tot$Periodo_sexo<-paste0(estructura_insumos_tot$Sexo_fin,"_",estructura_insumos_tot$MONTH)

ggplot(data = estructura_insumos_tot[estructura_insumos_tot$YEAR == 2021 ,],aes(x = Edad_fin, y = Total_RELAB, group = Periodo_sexo, color =  Periodo_sexo))+
  geom_line(aes(linetype=Periodo_sexo, color=Periodo_sexo, size=Periodo_sexo))+
  scale_color_manual(values = c("darkgray","darkgray","darkred","darkgrey","darkgray","darkgray","darkblue","darkgray"))+
  scale_linetype_manual(values = c("twodash", "longdash","solid","dotted","twodash", "longdash","solid","dotted"))+
  scale_size_manual(values = c(0.5,0.5,1,0.5,0.5,0.5,1,0.5))+
  facet_wrap(~Periodo,scales = "free")+theme_bw()+labs(title = "Distribución de cotizantes por edad, sexo y rango salarial en los ultimos 4 mese", x = "Edad", y = "Total cotizantes")


### ubicacion cotizantes

library(glue)
library(stringr)
library(colorspace)
library(sf)
library(classInt)

setwd("D:/Nuevos_Insumos_IMC/Informacion_general/")
municipio<-st_read("C:/Users/aemendoza/Documents/Marcos/MGN_MPIO_POLITICO.shp")
departamento<-st_read("C:/Users/aemendoza/Documents/Marcos/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp")
dpto_sin_SA<-departamento[departamento$DPTO_CCDGO!="88",]

base_ubicacio<-"Salidas_generales_DPTO_MPIO9_12_2021_.csv"
MES <- 12
AÑO <- 2021

datos_ubicacion_cot<-read.csv2(base_ubicacio)
res_departamento4<-data.frame(datos_ubicacion_cot%>%filter(Tipologia == "Dep_sec_priv",YEAR == AÑO, MONTH == MES )%>%group_by(departamento_COT_Max)%>%summarise(Total_relaciones_laborales4 = sum(Total_relaciones_laborales, na.rm = TRUE)));res_departamento4<-res_departamento4[!is.na(res_departamento4$departamento_COT_Max),]
res_departamento3<-data.frame(datos_ubicacion_cot%>%filter(Tipologia == "Dep_sec_priv",YEAR == AÑO, MONTH == (MES-1) )%>%group_by(departamento_COT_Max)%>%summarise(Total_relaciones_laborales3 = sum(Total_relaciones_laborales, na.rm = TRUE)));res_departamento3<-res_departamento3[!is.na(res_departamento3$departamento_COT_Max),]
res_departamento2<-data.frame(datos_ubicacion_cot%>%filter(Tipologia == "Dep_sec_priv",YEAR == AÑO, MONTH == (MES-2) )%>%group_by(departamento_COT_Max)%>%summarise(Total_relaciones_laborales2 = sum(Total_relaciones_laborales, na.rm = TRUE)));res_departamento2<-res_departamento2[!is.na(res_departamento2$departamento_COT_Max),]
res_departamento1<-data.frame(datos_ubicacion_cot%>%filter(Tipologia == "Dep_sec_priv",YEAR == AÑO, MONTH == (MES-3) )%>%group_by(departamento_COT_Max)%>%summarise(Total_relaciones_laborales1 = sum(Total_relaciones_laborales, na.rm = TRUE)));res_departamento1<-res_departamento1[!is.na(res_departamento1$departamento_COT_Max),]
res_municipio<-merge(res_departamento1,res_departamento2, by = "departamento_COT_Max", all = TRUE)
res_municipio<-merge(res_municipio,res_departamento3, by = "departamento_COT_Max", all = TRUE)
res_municipio<-merge(res_municipio,res_departamento4, by = "departamento_COT_Max", all = TRUE)
dpto2<-rep(0,nrow(res_municipio))
for(i in 1:length(dpto2)){
  if(str_length(res_municipio$departamento_COT_Max[i])==1)dpto2[i]<-paste0("0",res_municipio$departamento_COT_Max[i])
  if(str_length(res_municipio$departamento_COT_Max[i])==2)dpto2[i]<-paste0("",res_municipio$departamento_COT_Max[i])}
res_municipio$dpto2<-dpto2
res_municipio$VAR4_3<-(res_municipio$Total_relaciones_laborales4-res_municipio$Total_relaciones_laborales3)/res_municipio$Total_relaciones_laborales3
res_municipio$VAR3_2<-(res_municipio$Total_relaciones_laborales3-res_municipio$Total_relaciones_laborales2)/res_municipio$Total_relaciones_laborales2
res_municipio$VAR2_1<-(res_municipio$Total_relaciones_laborales2-res_municipio$Total_relaciones_laborales1)/res_municipio$Total_relaciones_laborales1

res_municipio1<-merge(dpto_sin_SA[,c("DPTO_CCDGO","DPTO_NANO_")],res_municipio,by.x="DPTO_CCDGO",by.y="dpto2",all.x=TRUE)
res_municipio1_cor<-data.frame(DPTO_CCDGO = rep(res_municipio1$DPTO_CCDGO,3),
                               Variaciones = c(res_municipio1$VAR2_1,res_municipio1$VAR3_2,res_municipio1$VAR4_3), 
                               Periodo = c(rep("1.var. Sep - Oct",nrow(res_municipio1)),rep("2.var. Oct - Nov",nrow(res_municipio1)),rep("2.var. Nov - Dic",nrow(res_municipio1))))
res_municipio1_cor_mapa<-merge(dpto_sin_SA,res_municipio1_cor,by="DPTO_CCDGO",all.x=TRUE)
variable<-res_municipio1_cor_mapa$Variaciones;valmax<-max(variable,na.rm =TRUE);valmin<-min(variable,na.rm = TRUE);
variable[is.na(variable)]<-rep(0,length(variable[is.na(variable)]))
tamd = 8
segmento<-classIntervals(round(c(valmin-0.001,variable[variable!=valmin & variable!=valmax],valmax+0.001),4),n=tamd,style = "quantile")
mapa_dpto<- mutate(res_municipio1_cor_mapa, Var = cut(Variaciones, segmento$brks)); 
nuevos_nombress<-round(quantile(c(valmin-0.001,variable[variable!=valmin & variable!=valmax],valmax+0.001),1/tamd*seq(1,tamd)),4)
levels(mapa_dpto$Var)<-nuevos_nombress
ggplot(data=mapa_dpto)+
  facet_wrap("Periodo", nrow = 1, as.table = TRUE) +
  geom_sf(aes(fill=Var),color="white")+
  scale_fill_brewer(palette = "YlGnBu")+
  ggtitle("Variación mensual de los cotizantes en el trimeste IV de 2021",subtitle="Aportantes Sector Privado ")+
  theme_minimal()+coord_sf(datum=NA)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 6),legend.key.size = unit(0.4,"cm"))+
        guides(fill=guide_legend(title="Var (%)"))

#####################################################################
#La plata
####slide del total y el año tipo

setwd("D:/Nuevos_Insumos_IMC/Informacion_general/")
tabla_plata<-"Salidas_generales_rangoIBC9_12_2021_.csv"

total_monto<-function(tabla_plata){
  base_plata<-read.csv2(tabla_plata)
  base_plata$Nuevos_rangos_IBC<-rep(0,nrow(base_plata))
  base_plata[base_plata$Rango_IBC == "0.500000","Nuevos_rangos_IBC"]<-rep("<1SMMLV",nrow(base_plata[base_plata$Rango_IBC == "0.500000",]))
  base_plata[base_plata$Rango_IBC == "1.000000","Nuevos_rangos_IBC"]<-rep("1SMMLV",nrow(base_plata[base_plata$Rango_IBC == "1.000000",]))
  base_plata[base_plata$Rango_IBC %in% c("1.500000","2.000000"),"Nuevos_rangos_IBC"]<-rep("1_2SMMLV",nrow(base_plata[base_plata$Rango_IBC  %in% c("1.500000","2.000000"),]))
  base_plata[base_plata$Rango_IBC %in% c("2.500000","3.000000","3.500000","4.000000","4.500000","5.000000"),"Nuevos_rangos_IBC"]<-rep("2_5SMMLV",nrow(base_plata[base_plata$Rango_IBC  %in% c("2.500000","3.000000","3.500000","4.000000","4.500000","5.000000"),]))
  base_plata[base_plata$Rango_IBC == "5.500000","Nuevos_rangos_IBC"]<-rep(">5SMMLV",nrow(base_plata[base_plata$Rango_IBC == "5.500000",]))
  base_plata$Tipologia1<-NULL
  base_plata[base_plata$Tipologia %in% c("Dep_No_priv","Dep_sec_priv"),"Tipologia1"]<-rep("Dependientes",nrow(base_plata[base_plata$Tipologia %in% c("Dep_No_priv","Dep_sec_priv"),]))
  base_plata[base_plata$Tipologia == "Independiente","Tipologia1"]<-rep("Independientes",nrow(base_plata[base_plata$Tipologia == "Independiente",]))
  resumen_plata<-data.frame(base_plata%>%group_by(YEAR,MONTH,Tipologia1,Nuevos_rangos_IBC)%>%
  summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), Pago_salud = sum(cot_obligatoria_salud_Max_Sum, na.rm =TRUE),Pago_pension = sum(aporte_cot_obligatoria_pension_Max_Sum, na.rm =TRUE),Pago_ARL = sum(cot_obligatoria_arl_Max_Sum,na.rm = TRUE),
            Pago_CCF = sum(valor_aportes_ccf_Max_Sum, na.rm =TRUE),Pago_SENA = sum(valor_aportes_parafiscales_sena_Max_Sum, na.rm =TRUE),Pago_ICBF = sum(valor_aportes_parafiscales_icbf_Max_Sum,na.rm = TRUE)))
  resumen_plata_ymt<-data.frame(base_plata%>%group_by(YEAR,MONTH,Tipologia1)%>%
                            summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), Pago_salud = sum(cot_obligatoria_salud_Max_Sum, na.rm =TRUE),Pago_pension = sum(aporte_cot_obligatoria_pension_Max_Sum, na.rm =TRUE),Pago_ARL = sum(cot_obligatoria_arl_Max_Sum,na.rm = TRUE),
                                      Pago_CCF = sum(valor_aportes_ccf_Max_Sum, na.rm =TRUE),Pago_SENA = sum(valor_aportes_parafiscales_sena_Max_Sum, na.rm =TRUE),Pago_ICBF = sum(valor_aportes_parafiscales_icbf_Max_Sum,na.rm = TRUE)))
  resumen_plata_ymt$Nuevos_rangos_IBC<-rep("Total_IBC",nrow(resumen_plata_ymt))
  resumen_plata_ym<-data.frame(base_plata%>%group_by(YEAR,MONTH)%>%
                                summarise(Total_cotizantes = sum(Total_relaciones_laborales, na.rm = TRUE), Pago_salud = sum(cot_obligatoria_salud_Max_Sum, na.rm =TRUE),Pago_pension = sum(aporte_cot_obligatoria_pension_Max_Sum, na.rm =TRUE),Pago_ARL = sum(cot_obligatoria_arl_Max_Sum,na.rm = TRUE),
                                          Pago_CCF = sum(valor_aportes_ccf_Max_Sum, na.rm =TRUE),Pago_SENA = sum(valor_aportes_parafiscales_sena_Max_Sum, na.rm =TRUE),Pago_ICBF = sum(valor_aportes_parafiscales_icbf_Max_Sum,na.rm = TRUE)))
  resumen_plata_ym$Nuevos_rangos_IBC<-rep("Total",nrow(resumen_plata_ym))
  resumen_plata_ym$Tipologia1<-rep("Total",nrow(resumen_plata_ym))
  resumen_plata_total<-rbind(resumen_plata,resumen_plata_ymt,resumen_plata_ym)
  resumen_plata_total$Total_Monto<-c(resumen_plata_total$Pago_salud+resumen_plata_total$Pago_pension+resumen_plata_total$Pago_ARL+resumen_plata_total$Pago_CCF+resumen_plata_total$Pago_SENA+resumen_plata_total$Pago_ICBF)
  resumen_plata_total}

monto_9_12_21<-total_monto("Salidas_generales_rangoIBC9_12_2021_.csv")
monto_5_8_21<-total_monto("Salidas_generales_rangoIBC5_8_2021.csv")
monto_1_4_21<-total_monto("Salidas_generales_rangoIBC1_4_2021.csv")
monto_9_12_20<-total_monto("Salidas_generales_rangoIBC9_12_2020.csv")
monto_5_8_20<-total_monto("Salidas_generales_rangoIBC5_8_2020.csv")
monto_1_4_20<-total_monto("Salidas_generales_rangoIBC1_4_2020.csv")
monto_9_12_19<-total_monto("Salidas_generales_rangoIBC9_12_2019.csv")
monto_5_8_19<-total_monto("Salidas_generales_rangoIBC5_8_2019.csv")
monto_1_4_19<-total_monto("Salidas_generales_rangoIBC1_4_2019.csv")
monto_9_12_18<-total_monto("Salidas_generales_rangoIBC9_12_2018.csv")
monto_5_8_18<-total_monto("Salidas_generales_rangoIBC5_8_2018.csv")
monto_1_4_18<-total_monto("Salidas_generales_rangoIBC1_4_2018.csv")

consolidado_monto<-rbind(monto_1_4_18,monto_5_8_18,monto_9_12_18,
                         monto_1_4_19,monto_5_8_19,monto_9_12_19,
                         monto_1_4_20,monto_5_8_20,monto_9_12_20,
                         monto_1_4_21,monto_5_8_21,monto_9_12_21)

valores<-c(0.9753,0.9822,0.9845,0.9891,0.9916,0.9931,0.9918,0.993,0.9947,0.9959,0.997,1,
           1.006,1.0118,1.0162,1.0212,1.0244,1.0271,1.0294,1.0303,1.0326,1.0343,1.0354,1.038,
           1.0424,1.0494,1.0553,1.057,1.0536,1.0497,1.0497,1.0496,1.0529,1.0523,1.0508,1.0548,
           1.0591,1.0658,1.0712,1.0776,1.0884,1.0878,1.0914,1.0962,1.1004,1.1006,1.106,1.1141)

deflactados<-data.frame(YEAR = c(rep(2018,12),rep(2019,12),rep(2020,12),rep(2021,12)),MONTH = c(rep(1:12,4)),valores)

consolidado_monto<-merge(consolidado_monto,deflactados, by = c("YEAR","MONTH"), all = TRUE)
mes<-12;year <- 2021

  consolidado_monto$Pago_salud1<-consolidado_monto$Pago_salud*(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH == mes & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "valores"]/consolidado_monto$valores)
  consolidado_monto$Pago_pension1<-consolidado_monto$Pago_pension*(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH == mes & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "valores"]/consolidado_monto$valores)
  consolidado_monto$Pago_ARL1<-consolidado_monto$Pago_ARL*(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH == mes & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "valores"]/consolidado_monto$valores)
  consolidado_monto$Pago_CCF1<-consolidado_monto$Pago_CCF*(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH == mes & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "valores"]/consolidado_monto$valores)
  consolidado_monto$Pago_SENA1<-consolidado_monto$Pago_SENA*(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH == mes & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "valores"]/consolidado_monto$valores)
  consolidado_monto$Pago_ICBF1<-consolidado_monto$Pago_ICBF*(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH == mes & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "valores"]/consolidado_monto$valores)
  consolidado_monto$Total_Monto1<-consolidado_monto$Total_Monto*(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH == mes & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "valores"]/consolidado_monto$valores)

paste0("$",formatC(sum(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH %in% c(mes-2,mes-1,mes) & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "Total_Monto1"])/1000000,format = "f",big.mark = ".",decimal.mark = "," ,digits = 1))
paste0(round((sum(consolidado_monto[consolidado_monto$YEAR == (year-1) & consolidado_monto$MONTH %in% c(mes-2,mes-1,mes) & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "Total_Monto1"])-sum(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH %in% c(mes-2,mes-1,mes) & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "Total_Monto1"]))/sum(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH %in% c(mes-2,mes-1,mes) & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "Total_Monto1"])*100,2),"%")
paste0(round((sum(consolidado_monto[consolidado_monto$YEAR == (year-1) & consolidado_monto$MONTH %in% c(mes-2,mes-1,mes) & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "Total_Monto1"])-sum(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH %in% c(mes-2,mes-1,mes) & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "Total_Monto1"]))/sum(consolidado_monto[consolidado_monto$YEAR == year & consolidado_monto$MONTH %in% c(mes-2,mes-1,mes) & consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total", "Total_Monto1"])*100,2),"%")

Consolidado_totales<-consolidado_monto[consolidado_monto$Tipologia1 == "Total" & consolidado_monto$Nuevos_rangos_IBC == "Total",c("YEAR","MONTH","Total_Monto1")]
Consolidado_totales<-Consolidado_totales[order(Consolidado_totales$YEAR,Consolidado_totales$MONTH),]
graf_consolidadi_tot<-ggplot(data = Consolidado_totales, aes(x = MONTH, y = Total_Monto1/1000000, color = as.factor(YEAR)))+guides(color = guide_legend(title = "Año"))+theme_minimal()+theme(legend.position = "top")+
  geom_line()+scale_color_manual(values = c("grey80","grey50","grey20","black"))+labs(title = "Aporte total por mes y año", subtitle = "Todos los cotizantes", x = "mes", y = "Monto total en millones")

Consolidado_totales_dep<-consolidado_monto[consolidado_monto$Tipologia1 == "Dependientes" & consolidado_monto$Nuevos_rangos_IBC == "Total_IBC",c("YEAR","MONTH","Total_Monto1")]
Consolidado_totales_dep<-Consolidado_totales_dep[order(Consolidado_totales_dep$YEAR,Consolidado_totales_dep$MONTH),]
graf_consolidadi_tot_dep<-ggplot(data = Consolidado_totales_dep, aes(x = MONTH, y = Total_Monto1/1000000, color = as.factor(YEAR)))+guides(color = guide_legend(title = "Año"))+theme_minimal()+theme(legend.position = "top")+
  geom_line()+scale_color_manual(values = c("grey80","grey50","grey20","blue"))+labs(title = "Aporte total por mes y año", subtitle = "cotizantes dependientes", x = "mes", y = "Monto total en millones")

Consolidado_totales_ind<-consolidado_monto[consolidado_monto$Tipologia1 == "Independientes" & consolidado_monto$Nuevos_rangos_IBC == "Total_IBC",c("YEAR","MONTH","Total_Monto1")]
Consolidado_totales_ind<-Consolidado_totales_ind[order(Consolidado_totales_ind$YEAR,Consolidado_totales_ind$MONTH),]
graf_consolidadi_tot_ind<-ggplot(data = Consolidado_totales_ind, aes(x = MONTH, y = Total_Monto1/1000000, color = as.factor(YEAR)))+guides(color = guide_legend(title = "Año"))+theme_minimal()+theme(legend.position = "top")+
  geom_line()+scale_color_manual(values = c("grey80","grey50","grey20","Orange"))+labs(title = "Aporte total por mes y año", subtitle = "Cotizantes independientes", x = "mes", y = "Monto total en millones")

#ggarrange(graf_consolidadi_tot,graf_consolidadi_tot_dep,graf_consolidadi_tot_ind,nrow=1)

consolidado_monto<-consolidado_monto[order(consolidado_monto$YEAR, consolidado_monto$MONTH, consolidado_monto$Tipologia1, consolidado_monto$Nuevos_rangos_IBC),]
consolidado_monto$promedio_monto <- consolidado_monto$Total_Monto1/consolidado_monto$Total_cotizantes

subbase_2018_pre<-consolidado_monto[consolidado_monto$YEAR == 2018 & consolidado_monto$MONTH == (mes-1),c("Tipologia1","Nuevos_rangos_IBC","Total_Monto1","promedio_monto")];colnames(subbase_2018_pre)<-c("Tipo_cot", "Rango_IBC", "Monto_total18_pre","Promedio_18pre")
subbase_2018<-consolidado_monto[consolidado_monto$YEAR == 2018 & consolidado_monto$MONTH == (mes-0),c("Tipologia1","Nuevos_rangos_IBC","Total_Monto1","promedio_monto")];colnames(subbase_2018)<-c("Tipo_cot", "Rango_IBC", "Monto_total18","Promedio_18")
subbase_2019_pre<-consolidado_monto[consolidado_monto$YEAR == 2019 & consolidado_monto$MONTH == (mes-1),c("Tipologia1","Nuevos_rangos_IBC","Total_Monto1","promedio_monto")];colnames(subbase_2019_pre)<-c("Tipo_cot", "Rango_IBC", "Monto_total19_pre","Promedio_19pre")
subbase_2019<-consolidado_monto[consolidado_monto$YEAR == 2019 & consolidado_monto$MONTH == (mes-0),c("Tipologia1","Nuevos_rangos_IBC","Total_Monto1","promedio_monto")];colnames(subbase_2019)<-c("Tipo_cot", "Rango_IBC", "Monto_total19","Promedio_19")
subbase_2020_pre<-consolidado_monto[consolidado_monto$YEAR == 2020 & consolidado_monto$MONTH == (mes-1),c("Tipologia1","Nuevos_rangos_IBC","Total_Monto1","promedio_monto")];colnames(subbase_2020_pre)<-c("Tipo_cot", "Rango_IBC", "Monto_total20_pre","Promedio_20pre")
subbase_2020<-consolidado_monto[consolidado_monto$YEAR == 2020 & consolidado_monto$MONTH == (mes-0),c("Tipologia1","Nuevos_rangos_IBC","Total_Monto1","promedio_monto")];colnames(subbase_2020)<-c("Tipo_cot", "Rango_IBC", "Monto_total20","Promedio_20")
subbase_2021_pre<-consolidado_monto[consolidado_monto$YEAR == 2021 & consolidado_monto$MONTH == (mes-1),c("Tipologia1","Nuevos_rangos_IBC","Total_Monto1","promedio_monto")];colnames(subbase_2021_pre)<-c("Tipo_cot", "Rango_IBC", "Monto_total21_pre","Promedio_21pre")
subbase_2021<-consolidado_monto[consolidado_monto$YEAR == 2021 & consolidado_monto$MONTH == (mes-0),c("Tipologia1","Nuevos_rangos_IBC","Total_Monto1","promedio_monto")];colnames(subbase_2021)<-c("Tipo_cot", "Rango_IBC", "Monto_total21","Promedio_21")

consolidado_general<-merge(subbase_2018_pre,subbase_2018, by = c("Tipo_cot","Rango_IBC"), all = TRUE)
consolidado_general<-merge(consolidado_general,subbase_2019_pre, by = c("Tipo_cot","Rango_IBC"), all = TRUE)
consolidado_general<-merge(consolidado_general,subbase_2019, by = c("Tipo_cot","Rango_IBC"), all = TRUE)
consolidado_general<-merge(consolidado_general,subbase_2020_pre, by = c("Tipo_cot","Rango_IBC"), all = TRUE)
consolidado_general<-merge(consolidado_general,subbase_2020, by = c("Tipo_cot","Rango_IBC"), all = TRUE)
consolidado_general<-merge(consolidado_general,subbase_2021_pre, by = c("Tipo_cot","Rango_IBC"), all = TRUE)
consolidado_general<-merge(consolidado_general,subbase_2021, by = c("Tipo_cot","Rango_IBC"), all = TRUE)

consolidado_general$var_mes18<-(consolidado_general$Monto_total18-consolidado_general$Monto_total18_pre)/consolidado_general$Monto_total18_pre
consolidado_general$var_mes19<-(consolidado_general$Monto_total19-consolidado_general$Monto_total19_pre)/consolidado_general$Monto_total19_pre
consolidado_general$var_mes20<-(consolidado_general$Monto_total20-consolidado_general$Monto_total20_pre)/consolidado_general$Monto_total20_pre
consolidado_general$var_mes21<-(consolidado_general$Monto_total21-consolidado_general$Monto_total21_pre)/consolidado_general$Monto_total21_pre
consolidado_general$var_anual19<-(consolidado_general$Monto_total19-consolidado_general$Monto_total18)/consolidado_general$Monto_total18
consolidado_general$var_anual20<-(consolidado_general$Monto_total20-consolidado_general$Monto_total19)/consolidado_general$Monto_total19
consolidado_general$var_anual21<-(consolidado_general$Monto_total21-consolidado_general$Monto_total20)/consolidado_general$Monto_total20

Orden_aes<-data.frame(Rango_IBC = c("<1SMMLV","1SMMLV","1_2SMMLV","2_5SMMLV",">5SMMLV","Total_IBC","Total"),orden_1 = 1:7)
consolidado_general<-merge(consolidado_general,Orden_aes, by = "Rango_IBC", all.x = TRUE)
consolidado_general<-consolidado_general[order(consolidado_general$Tipo_cot,consolidado_general$orden_1),]

consolidado_general1<-data.frame(
  Tipo_cotizanes = consolidado_general$Tipo_cot,
  Rango_salario = consolidado_general$Rango_IBC,
  Total_monto19 = paste0("$",formatC(consolidado_general$Monto_total19/1000000,format = "f", big.mark = ".", decimal.mark = ",", digits = 0)),
  Total_monto20 = paste0("$",formatC(consolidado_general$Monto_total20/1000000,format = "f", big.mark = ".", decimal.mark = ",", digits = 0)),
  Total_monto21 = paste0("$",formatC(consolidado_general$Monto_total21/1000000,format = "f", big.mark = ".", decimal.mark = ",", digits = 0)),
  Var_mes_18 = paste0(round(consolidado_general$var_mes18*100,2),"%"),
  Var_mes_19 = paste0(round(consolidado_general$var_mes19*100,2),"%"),
  Var_mes_20 = paste0(round(consolidado_general$var_mes20*100,2),"%"),
  Var_mes_21 = paste0(round(consolidado_general$var_mes21*100,2),"%"),
  Var_anual_19 = paste0(round(consolidado_general$var_anual19*100,2),"%"),
  Var_anual_20 = paste0(round(consolidado_general$var_anual20*100,2),"%"),
  Var_anual_21 = paste0(round(consolidado_general$var_anual21*100,2),"%"))

consolidado_general1<-data.frame(
  Tipo_cotizanes = consolidado_general$Tipo_cot,
  Rango_salario = consolidado_general$Rango_IBC,
  Total_monto19 = paste0("$",formatC(consolidado_general$Monto_total19/1000000,format = "f", big.mark = ".", decimal.mark = ",", digits = 0)),
  Total_monto20 = paste0("$",formatC(consolidado_general$Monto_total20/1000000,format = "f", big.mark = ".", decimal.mark = ",", digits = 0)),
  Total_monto21 = paste0("$",formatC(consolidado_general$Monto_total21/1000000,format = "f", big.mark = ".", decimal.mark = ",", digits = 0)),
  Var_mes_18 = paste0(round(consolidado_general$var_mes18*100,2),"%"),
  Var_mes_19 = paste0(round(consolidado_general$var_mes19*100,2),"%"),
  Var_mes_20 = paste0(round(consolidado_general$var_mes20*100,2),"%"),
  Var_mes_21 = paste0(round(consolidado_general$var_mes21*100,2),"%"),
  Var_anual_19 = paste0(round(consolidado_general$var_anual19*100,2),"%"),
  Var_anual_20 = paste0(round(consolidado_general$var_anual20*100,2),"%"),
  Var_anual_21 = paste0(round(consolidado_general$var_anual21*100,2),"%"))

consolidado_general1%>%
  select(Tipo_cotizanes,Rango_salario,Total_monto19,Total_monto20,Total_monto21,Var_mes_18,Var_mes_19,Var_mes_20,Var_mes_21,Var_anual_19,Var_anual_20,Var_anual_21)%>%
  kbl(caption = "Matriz resumen del monto de cotizacion realizado en diciembre 2019, 2020 y 2021 asi como las variaciones mensuales y anuales por rango y tipo de cotizantes", align = "llcccccccccc", 
      col.names = c("Tipo","Rango IBC","2019","2020","2021","2018","2019","2020","2021","2019","2020","2021"))%>%
  footnote(general = "Se toman en cuenta montos deflactados a partir del valor del IPC con referencia diciembre 2021")%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  kable_styling(full_width = F, font_size = 11)%>%
  column_spec(column = 5,border_right  = TRUE)%>%
  column_spec(column = 9,border_right  = TRUE)%>%
  row_spec(row = 6, bold = TRUE)%>%
  row_spec(row = 12, bold = TRUE)%>%
  row_spec(row = 13, bold = TRUE)%>%
  row_spec(row = 0, bold = TRUE)%>%
  add_header_above(c(" "," ","Monto en muillones" = 3,"Variación mensual" = 4,"Variacón anual" = 3))

rango_orden = data.frame(Nuevos_rangos_IBC  = c('<1SMMLV','1SMMLV','1_2SMMLV','2_5SMMLV','>5SMMLV','Total_IBC'), orden =  c(1,2,3,4,5,6))
consolidado_monto<-merge(consolidado_monto,rango_orden, by = "Nuevos_rangos_IBC", all.x = TRUE)
consolidado_monto<-consolidado_monto[order(consolidado_monto$YEAR,consolidado_monto$MONTH,consolidado_monto$Tipologia1,consolidado_monto$orden),]
consolidado_monto_promedio_4<-consolidado_monto[consolidado_monto$Tipologia1 != "Total" & consolidado_monto$YEAR == 2021 & consolidado_monto$MONTH >=9,c("YEAR","MONTH","Tipologia1","Nuevos_rangos_IBC","promedio_monto","orden")]
consolidado_monto_promedio_3<-consolidado_monto[consolidado_monto$Tipologia1 != "Total" & consolidado_monto$YEAR == 2020 & consolidado_monto$MONTH >=9,c("YEAR","MONTH","Tipologia1","Nuevos_rangos_IBC","promedio_monto","orden")]

ggplot(data = consolidado_monto_promedio_4, aes(reorder(Nuevos_rangos_IBC,orden) , promedio_monto, fill = as.factor(MONTH)))+theme_minimal()+
  scale_fill_viridis_d()+ geom_bar(stat="identity", position="dodge")+facet_wrap(~Tipologia1)+labs(title = "Promedio del monto general de cotización por rango salarial en 2021", x = "Rango salario", y = "Monto promedio" )
ggplot(data = consolidado_monto_promedio_3, aes(reorder(Nuevos_rangos_IBC,orden) , promedio_monto, fill = as.factor(MONTH)))+theme_minimal()+
  scale_fill_brewer(palette = "Greens") + geom_bar(stat="identity", position="dodge")+facet_wrap(~Tipologia1)+labs(title = "Promedio del monto general de cotización por rango salarial en 2020", x = "Rango salario", y = "Monto promedio" )

consolidado_monto_subsiste<-consolidado_monto[consolidado_monto$Nuevos_rangos_IBC %in% c("Total","Total_IBC") & consolidado_monto$YEAR == 2021 & consolidado_monto$MONTH >= 7,c("YEAR","MONTH","Tipologia1","Pago_salud1","Pago_pension1","Pago_ARL1","Pago_CCF1","Pago_SENA1","Pago_ICBF1","Total_Monto1")]
consolidado_monto_subsiste<-consolidado_monto_subsiste[order(consolidado_monto_subsiste$Tipologia1,consolidado_monto_subsiste$MONTH),]
consolidado_monto_subsiste$Pago_salud1<-paste0("$",formatC(consolidado_monto_subsiste$Pago_salud1/1000000,format = "f", big.mark = ".", decimal.mark = ",",digits = 0))
consolidado_monto_subsiste$Pago_pension1<-paste0("$",formatC(consolidado_monto_subsiste$Pago_pension1/1000000,format = "f", big.mark = ".", decimal.mark = ",",digits = 0))
consolidado_monto_subsiste$Pago_ARL1<-paste0("$",formatC(consolidado_monto_subsiste$Pago_ARL1/1000000,format = "f", big.mark = ".", decimal.mark = ",",digits = 0))
consolidado_monto_subsiste$Pago_CCF1<-paste0("$",formatC(consolidado_monto_subsiste$Pago_CCF1/1000000,format = "f", big.mark = ".", decimal.mark = ",",digits = 0))
consolidado_monto_subsiste$Pago_SENA1<-paste0("$",formatC(consolidado_monto_subsiste$Pago_SENA1/1000000,format = "f", big.mark = ".", decimal.mark = ",",digits = 0))
consolidado_monto_subsiste$Pago_ICBF1<-paste0("$",formatC(consolidado_monto_subsiste$Pago_ICBF1/1000000,format = "f", big.mark = ".", decimal.mark = ",",digits = 0))
consolidado_monto_subsiste$Total_Monto1<-paste0("$",formatC(consolidado_monto_subsiste$Total_Monto1/1000000,format = "f", big.mark = ".", decimal.mark = ",",digits = 0))
consolidado_monto_subsiste$mes<-rep(c("Jul","Ago","Sep","Oct","Nov","Dic"),3)

consolidado_monto_subsiste%>%
  select(Tipologia1,mes,Pago_salud1,Pago_pension1,Pago_ARL1,Pago_CCF1,Pago_SENA1,Pago_ICBF1,Total_Monto1)%>%
  kbl(caption = "Monto total por subsistema en millones para el ultimo semestre de 2021 según el tipo de cotizantes",align ="llccccccc",
      col.names = c("Tipo", "Mes", "Pago en salud", "Pago en pensión", "Pago en ARL", "Pago en CCF", "Pago en SENA","Pago en ICBF","Pago total"))%>%
  footnote(general = "Se toman en cuenta montos deflactados a partir del valor del IPC con referencia diciembre 2021")%>%
  kable_classic(full_width = F, html_font = "Calibri")%>%
  kable_styling(full_width = F, font_size = 11)%>%
  row_spec(row = 0, bold = TRUE)%>%
  row_spec(row = 6, bold = TRUE)%>%
  row_spec(row = 12, bold = TRUE)%>%
  row_spec(row = 18, bold = TRUE)%>%
  column_spec(column = 3, border_right  = TRUE)%>%
  column_spec(column = 9, border_right  = TRUE)

  
  ####################################################
##ANEXO 

###Demografico

Proyecciones<-read.csv2("D:/Resultados_IMC/Presentaciones/Proyecciones de población Total-PET.csv")
colnames(salida_9_12_19$tabla_demo_tot)<-c("Edad_fn","Total_cot_f_19","Total_cot_m_19")
colnames(salida_9_12_20$tabla_demo_tot)<-c("Edad_fn","Total_cot_f_20","Total_cot_m_20")
colnames(salida_9_12_21$tabla_demo_tot)<-c("Edad_fn","Total_cot_f_21","Total_cot_m_21")

consolidado_cotizantes<-merge(salida_9_12_19$tabla_demo_tot,salida_9_12_20$tabla_demo_tot, by = "Edad_fn", all = TRUE)
consolidado_cotizantes<-merge(consolidado_cotizantes,salida_9_12_21$tabla_demo_tot, by = "Edad_fn", all = TRUE)

Proyecciones_COT<-merge(Proyecciones,consolidado_cotizantes, by.x = "Edad", by.y = "Edad_fn", all.x = TRUE)
Proyecciones_COT$tasa_f_19<-Proyecciones_COT$Total_cot_f_19/Proyecciones_COT$Mujeres_2019*1000
Proyecciones_COT$tasa_m_19<-Proyecciones_COT$Total_cot_m_19/Proyecciones_COT$Hombres_2019*1000
Proyecciones_COT$tasa_f_20<-Proyecciones_COT$Total_cot_f_20/Proyecciones_COT$Mujeres_2020*1000
Proyecciones_COT$tasa_m_20<-Proyecciones_COT$Total_cot_m_20/Proyecciones_COT$Hombres_2020*1000
Proyecciones_COT$tasa_f_21<-Proyecciones_COT$Total_cot_f_21/Proyecciones_COT$Mujeres_2021*1000
Proyecciones_COT$tasa_m_21<-Proyecciones_COT$Total_cot_m_21/Proyecciones_COT$Hombres_2021*1000

grafico_anexo_2<-data.frame(Year = rep(c("2019","2019","2020","2020","2021","2021"),c(54,54,54,54,54,54)),Edad = rep(Proyecciones_COT$Edad,6),Sexo = rep(rep(c("F","M"),c(54,54)),3),tasa = c(Proyecciones_COT$tasa_f_19,Proyecciones_COT$tasa_m_19,Proyecciones_COT$tasa_f_20,Proyecciones_COT$tasa_m_20,Proyecciones_COT$tasa_f_21,Proyecciones_COT$tasa_m_21))
ggplot(data = grafico_anexo_2,aes(x = Edad, y = tasa, color =  Year))+theme_minimal()+
  geom_line()+facet_wrap(~Sexo)+labs(Titel = "Total de personas cotizando por cada 1000 personas proyectadas", subtitle = "Proyecciones de población DANE")

sum(Proyecciones[,"Total_2021"])
sum(Proyecciones[,"Total_2020"])
sum(Proyecciones[,"Total_2019"])

tinytex::install_tinytex()

tinytex::install_tinytex()
