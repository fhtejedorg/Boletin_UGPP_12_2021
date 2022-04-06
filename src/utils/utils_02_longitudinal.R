
fun_panel<-function(tabla, path = '../data/interim/Matriz/'){
  Matriz<-read.csv2(file.path(path, tabla), dec = ".")
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



fun_output_matriz_transicion <- function(x, title){
  salida_matriz_transicion <- x %>% 
    gt(rowname_col = 'rownames_1') %>% 
    fmt_percent(columns = 1:8, scale_values = TRUE, decimals = 1)  %>% 
    tab_style(
      style = list(
        cell_borders(
          side = c("left", "right"), 
          color = "black",
          weight = px(1)
        )
      ),
      locations = cells_body(
        columns = 7:8
      )
    ) %>%
    cols_align(
      align = "center"
    ) %>%
    cols_width(
      everything() ~ px(100)
    )
  
  salida_matriz_transicion %>%
    gtsave(title, path = '../results/02_longitudinal/', )
}


fun_output_matriz_transicion <- function(x, title){
  tabla_matriz_resumen <- x %>% select(-Rango_agrupa) %>% 
    gt(rowname_col = 'Rango_agrupa_2') %>% 
    fmt_percent(columns = 6:7, scale_values = TRUE, decimals = 1)  %>% 
    fmt_number(columns = 1:5, sep_mark =  '.', dec_mark = ',', decimals = 0)  %>% 
    tab_style(
      style = list(
        cell_borders(
          side = c("left", "right"), 
          color = "black",
          weight = px(1)
        )
      ),
      locations = cells_body(
        columns = 6:7
      )
    ) %>% 
    tab_style(
      style = list(
        cell_borders(
          side = c("top"), 
          color = "black",
          weight = px(1)
        )
      ),
      locations = cells_body(
        rows = 7
      )
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = cells_body(rows = Rango_agrupa_2=='Total')) %>%
    cols_align(
      align = "center"
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#228B22", style = "italic", weight = 'bold')
      ),
      locations = list(cells_body(columns = 6), cells_column_labels(columns = 6))
    ) %>%  
    tab_style(
      style = list(
        cell_text(color = "#B22222", style = "italic", weight = 'bold')
      ),
      locations = list(cells_body(columns = 7), cells_column_labels(columns = 7))
    ) %>%  
    tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      locations = list(cells_column_labels(columns = 1:5))
    ) %>%  
    tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      locations = cells_stub()
    ) %>%  
    cols_label(
      Privados_entran = 'Entran',
      Privados_permanecen = 'Permanecen',
      Privados_salen = 'Salen',
      Privados_salen_retiro = 'Salen con Retiro', 
      Privados_salen_NO_retiro = 'Salen sin Retiro', 
      Por_entran = 'Entran (%)',
      Por_salen = 'Salen (%)'
    ) %>%
    cols_width(
      everything() ~ px(100)
    ) 
  
  tabla_matriz_resumen %>%
    gtsave(title, path = '../results/02_longitudinal/')
}



fun_salidas_entradas_anual <- function(dinamica_serie){
  gg_entran <- ggplot(dinamica_serie, aes(x=mes_format, y= Por_entran*100, group = Año, color=Año))+ 
    geom_line(aes(linetype=Año), size = 1) + 
    geom_point(aes(shape=Año), size = 3)+
    labs(x="Meses") + 
    theme_classic() +
    scale_y_continuous(name = '%', breaks = breaks_pretty(n = 5),limits = c(4.5, 9),
                       labels = scales:::number_format(big.mark = '.', decimal.mark = ',')) +
    scale_color_manual(values=c('#999999','#999999', '#228B22')) +
    scale_fill_manual(name = "none") + 
    scale_linetype_manual(values=c("twodash", "dashed", 'solid'))+
    theme(legend.position="bottom") + 
    facet_wrap(~Tipo,scales = 'free') 
  
  gg_salen <- ggplot(dinamica_serie, aes(x=mes_format, y= Por_salen*100, group = Año, color=Año))+ 
    geom_line(aes(linetype=Año), size = 1) + 
    geom_point(aes(shape=Año), size = 3)+
    labs(x="Meses") + 
    theme_classic() +
    scale_y_continuous(name = '%', breaks = breaks_pretty(n = 6),limits = c(4.5, 11),
                       labels = scales:::number_format(big.mark = '.', decimal.mark = ',')) +
    scale_color_manual(values=c('#999999','#999999', '#B22222')) +
    scale_fill_manual(name = "none") + 
    scale_linetype_manual(values=c("twodash", "dashed", 'solid'))+
    theme(legend.position="bottom") + 
    facet_wrap(~Tipo,scales = 'free')
  
  ggsave(filename = '../figures/02_longitudinal/entradas_anual_dependiente_independiente.png',
         width = 10, height = 6,
         plot = gg_entran)
  
  ggsave(filename = '../figures/02_longitudinal/salidas_anual_dependiente_independiente.png',
         width = 10, height = 6,
         plot = gg_salen)
}