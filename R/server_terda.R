
server <-function(input, output){
  
  ####################################################Introduction##############################################
  
  output$intro1 <- renderHighchart({
    ITLP_mother_base1<-ITLP_mother_base%>%
      filter(year>=input$slider1[1] & year<=input$slider1[2])
    if(input$radio1==0){
      h2<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral en la CDMX</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base1$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Rural",
          data = ITLP_mother_base1$HT_TLPr, tittle=list(text="Rural"),
          color = "#5cbf39 "
        ) %>%
        hc_add_series(
          name="Urbana",
          data = ITLP_mother_base1$HT_TLPu, tittle=list(text="Urbana"),
          color = "#c45c55"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio1==1){
      h2<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral en la CDMX</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base1$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))) %>%
        hc_add_series(
          name="Rural",
          data = ITLP_mother_base1$SM_TLPr, tittle=list(text="Rural"),
          color = "#5cbf39 "
        ) %>%
        hc_add_series(
          name="Urbana",
          data = ITLP_mother_base1$SM_TLPu, tittle=list(text="Urbana"),
          color = "#c45c55"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio1==2){
      h2<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral en la CDMX</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base1$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))) %>%
        hc_add_series(
          name="Rural",
          data = ITLP_mother_base1$MM_TLPr, tittle=list(text="Rural"),
          color = "#5cbf39 "
        ) %>%
        hc_add_series(
          name="Urbana",
          data = ITLP_mother_base1$MM_TLPu, tittle=list(text="Urbana"),
          color = "#c45c55"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    h2
    
  })
  
  ##################################################ITLP gender####################################################
  
  output$gen <- renderHighchart({
    ITLP_mother_base2<-ITLP_mother_base%>%
      filter(year>=input$slider2[1] & year<=input$slider2[2])
    if(input$radio2==0){
      h3<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Género</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base2$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))) %>%
        hc_add_series(
          name="Mujeres",
          data = ITLP_mother_base2$HT_TLPmuj, tittle=list(text="Mujeres"),
          color = "#d52f56 "
        ) %>%
        hc_add_series(
          name="Hombres",
          data = ITLP_mother_base2$HT_TLPhom, tittle=list(text="Hombres"),
          color = "#c1d93e"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio2==1){
      h3<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Género</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base2$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Mujeres",
          data = ITLP_mother_base2$SM_TLPmuj, tittle=list(text="Mujeres"),
          color = "#d52f56 "
        ) %>%
        hc_add_series(
          name="Hombres",
          data = ITLP_mother_base2$SM_TLPhom, tittle=list(text="Hombres"),
          color = "#c1d93e"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio2==2){
      h3<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Género</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base2$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Mujeres",
          data = ITLP_mother_base2$MM_TLPmuj, tittle=list(text="Mujeres"),
          color = "#d52f56 "
        ) %>%
        hc_add_series(
          name="Hombres",
          data = ITLP_mother_base2$MM_TLPhom, tittle=list(text="Hombres"),
          color = "#c1d93e"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    h3
  })
  
  ##################################################ITLP edu####################################################
  
  output$edu <- renderHighchart({
    ITLP_mother_base3<-ITLP_mother_base%>%
      filter(year>=input$slider3[1] & year<=input$slider3[2])
    if(input$radio3==0){
      h4<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Educación</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base3$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))) %>%
        hc_add_series(
          name="< Primaria",
          data = ITLP_mother_base3$HT_TLPedu1, tittle=list(text="< Primaria"),
          color = "#FFFFFF"
        ) %>%
        hc_add_series(
          name="Primaria",
          data = ITLP_mother_base3$HT_TLPedu2, tittle=list(text="Primaria"),
          color = "#da4735"
        ) %>%
        hc_add_series(
          name="Secundaria",
          data = ITLP_mother_base3$HT_TLPedu3, tittle=list(text="Secundaria"),
          color = "#806ca5"
        ) %>%
        hc_add_series(
          name="Medio superior",
          data = ITLP_mother_base3$HT_TLPedu4, tittle=list(text="Medio superior"),
          color = "#85af51"
        ) %>%
        hc_add_series(
          name="Universidad y posgrado",
          data = ITLP_mother_base3$HT_TLPedu5, tittle=list(text="Universidad y posgrado"),
          color = "#3e6370"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio3==1){
      h4<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Educación</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base3$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))) %>%
        hc_add_series(
          name="< Primaria",
          data = ITLP_mother_base3$SM_TLPedu1, tittle=list(text="< Primaria"),
          color = "#FFFFFF"
        ) %>%
        hc_add_series(
          name="Primaria",
          data = ITLP_mother_base3$SM_TLPedu2, tittle=list(text="Primaria"),
          color = "#da4735"
        ) %>%
        hc_add_series(
          name="Secundaria",
          data = ITLP_mother_base3$SM_TLPedu3, tittle=list(text="Secundaria"),
          color = "#806ca5"
        ) %>%
        hc_add_series(
          name="Medio superior",
          data = ITLP_mother_base3$SM_TLPedu4, tittle=list(text="Medio superior"),
          color = "#85af51"
        ) %>%
        hc_add_series(
          name="Universidad y posgrado",
          data = ITLP_mother_base3$SM_TLPedu5, tittle=list(text="Universidad y posgrado"),
          color = "#3e6370"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio3==2){
      h4<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Educación</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base3$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="< Primaria",
          data = ITLP_mother_base3$MM_TLPedu1, tittle=list(text="< Primaria"),
          color = "#FFFFFF"
        ) %>%
        hc_add_series(
          name="Primaria",
          data = ITLP_mother_base3$MM_TLPedu2, tittle=list(text="Primaria"),
          color = "#da4735"
        ) %>%
        hc_add_series(
          name="Secundaria",
          data = ITLP_mother_base3$MM_TLPedu3, tittle=list(text="Secundaria"),
          color = "#806ca5"
        ) %>%
        hc_add_series(
          name="Medio superior",
          data = ITLP_mother_base3$MM_TLPedu4, tittle=list(text="Medio superior"),
          color = "#85af51"
        ) %>%
        hc_add_series(
          name="Universidad y posgrado",
          data = ITLP_mother_base3$MM_TLPedu5, tittle=list(text="Universidad y posgrado"),
          color = "#3e6370"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    h4
  })
  
  ##################################################ITLP pos####################################################
  
  output$pos <- renderHighchart({
    ITLP_mother_base4<-ITLP_mother_base%>%
      filter(year>=input$slider4[1] & year<=input$slider4[2])
    if(input$radio4==0){
      h5<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Posición</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base4$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Subordinado Remunerado",
          data = ITLP_mother_base4$HT_TLPpos_ocu1, tittle=list(text="Subordinado Remunerado"),
          color = "#8be137"
        ) %>%
        hc_add_series(
          name="Empleador",
          data = ITLP_mother_base4$HT_TLPpos_ocu2, tittle=list(text="Empleador"),
          color = "#54a9b3"
        ) %>%
        hc_add_series(
          name="Cuenta Propia",
          data = ITLP_mother_base4$HT_TLPpos_ocu3, tittle=list(text="Cuenta Propia"),
          color = "#da7b3e"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio4==1){
      h5<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Posición</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base4$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Subordinado Remunerado",
          data = ITLP_mother_base4$SM_TLPpos_ocu1, tittle=list(text="Subordinado Remunerado"),
          color = "#8be137"
        ) %>%
        hc_add_series(
          name="Empleador",
          data = ITLP_mother_base4$SM_TLPpos_ocu2, tittle=list(text="Empleador"),
          color = "#54a9b3"
        ) %>%
        hc_add_series(
          name="Cuenta Propia",
          data = ITLP_mother_base4$SM_TLPpos_ocu3, tittle=list(text="Cuenta Propia"),
          color = "#da7b3e"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio4==2){
      h5<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por Posición</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base4$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Subordinado Remunerado",
          data = ITLP_mother_base4$MM_TLPpos_ocu1, tittle=list(text="Subordinado Remunerado"),
          color = "#8be137"
        ) %>%
        hc_add_series(
          name="Empleador",
          data = ITLP_mother_base4$MM_TLPpos_ocu2, tittle=list(text="Empleador"),
          color = "#54a9b3"
        ) %>%
        hc_add_series(
          name="Cuenta Propia",
          data = ITLP_mother_base4$MM_TLPpos_ocu3, tittle=list(text="Cuenta Propia"),
          color = "#da7b3e"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    h5
  })
  
  ##################################################ITLP seg####################################################
  
  output$seg <- renderHighchart({
    ITLP_mother_base5<-ITLP_mother_base%>%
      filter(year>=input$slider5[1] & year<=input$slider5[2])
    if(input$radio5==0){
      h5<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por acceso a seguridad social</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base5$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Con Acceso",
          data = ITLP_mother_base5$HT_TLPseg_soc1, tittle=list(text="Con Acceso"),
          color = "#3c7480"
        ) %>%
        hc_add_series(
          name="Sin Acesso",
          data = ITLP_mother_base5$HT_TLPseg_soc2, tittle=list(text="Sin Acesso"),
          color = "#f4554f"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio5==1){
      h5<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por acceso a seguridad social</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base5$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Con Acceso",
          data = ITLP_mother_base5$SM_TLPseg_soc1, tittle=list(text="Con Acceso"),
          color = "#3c7480"
        ) %>%
        hc_add_series(
          name="Sin Acesso",
          data = ITLP_mother_base5$SM_TLPseg_soc2, tittle=list(text="Sin Acesso"),
          color = "#f4554f"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    
    if(input$radio5==2){
      h5<-highchart()%>%
        hc_title(
          text = "<b>Pobreza laboral por acceso a seguridad social</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_xAxis(
          categories = ITLP_mother_base5$fecha, title=list(text="Trimestre"),
          title = list(style = list(color = "FFFFFF")),
          labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_yAxis_multiples(
          list(title=list(text="ITLP"),
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF")))
        ) %>%
        hc_add_series(
          name="Con Acceso",
          data = ITLP_mother_base5$MM_TLPseg_soc1, tittle=list(text="Con Acceso"),
          color = "#3c7480"
        ) %>%
        hc_add_series(
          name="Sin Acesso",
          data = ITLP_mother_base5$MM_TLPseg_soc2, tittle=list(text="Sin Acesso"),
          color = "#f4554f"
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    h5
  })
  
  ##################################################ITLP trans####################################################
  
  output$transfer <- renderHighchart({
    
    if(input$radio_t==0){
      transferencias<- highchart() %>% 
        hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                   labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_chart(type ="column") %>%
        hc_xAxis(categories = aprobado$Alcaldia,
                 title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))) %>% 
        hc_add_series(data = aprobado$Monto_2018, name = "Aprobado",
                      color = "#749ca4"
        )%>%
        hc_add_series(data = ejercido$Monto_2018, name = "Ejercido",
                      color = "#c84e41"     
        )%>%
        hc_title(
          text = "<b>Transferencias a alcaldías para desarrollo social</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    if(input$radio_t==1){
      transferencias<-highchart() %>% 
        hc_chart(type ="column") %>%
        hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                   labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_xAxis(categories = aprobado$Alcaldia,
                 title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))) %>% 
        hc_add_series(data = aprobado$Monto_2019, name = "Aprobado",
                      color = "#749ca4"
        )%>%
        hc_add_series(data = ejercido$Monto_2019, name = "Ejercido",
                      color = "#c84e41"
        ) %>%
        hc_title(
          text = "<b>Transferencias a alcaldías para desarrollo social</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    if(input$radio_t==2){
      transferencias<-highchart() %>% 
        hc_chart(type ="column") %>%
        hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                   labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_xAxis(categories = aprobado$Alcaldia,
                 title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))) %>% 
        hc_add_series(data = aprobado$Monto_2020, name = "Aprobado",
                      color = "#749ca4"
        )%>%
        hc_add_series(data = ejercido$Monto_2020, name = "Ejercido",
                      color = "#c84e41"
        ) %>%
        hc_title(
          text = "<b>Transferencias a alcaldías para desarrollo social</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    transferencias
  })
  
  ##################################################spend des####################################################
  
  output$gdes <- renderHighchart({
    if(input$radio_des==0){
      hd<- highchart() %>% 
        hc_chart(type ="column") %>%
        hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                   labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_xAxis(categories = desempleo_aprobado$Unidad,
                 title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))) %>% 
        hc_add_series(data = desempleo_aprobado$Monto_2018, name = "Aprobado",
                      color = "#749ca4"
        )%>%
        hc_add_series(data = desempleo_ejercido$Monto_2018, name = "Ejercido",
                      color = "#c84e41" 
        ) %>%
        hc_title(
          text = "<b>Egresos de la CDMX, Sub-función: Desempleo</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    if(input$radio_des==1){
      hd<- highchart() %>% 
        hc_chart(type ="column") %>%
        hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                   labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_xAxis(categories = desempleo_aprobado$Unidad,
                 title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))) %>% 
        hc_add_series(data = desempleo_aprobado$Monto_2019, name = "Aprobado", color = "#749ca4")%>%
        hc_add_series(data = desempleo_ejercido$Monto_2019, name = "Ejercido", color = "#c84e41") %>%
        hc_title(
          text = "<b>Egresos de la CDMX, Sub-función: Desempleo</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    if(input$radio_des==2){
      hd<- highchart() %>% 
        hc_chart(type ="column") %>%
        hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                   labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_xAxis(categories = desempleo_aprobado$Unidad,
                 title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))) %>% 
        hc_add_series(data = desempleo_aprobado$Monto_2020, name = "Aprobado", color = "#749ca4")%>%
        hc_add_series(data = desempleo_ejercido$Monto_2020, name = "Ejercido", color = "#c84e41") %>%
        hc_title(
          text = "<b>Egresos de la CDMX, Sub-función: Desempleo</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    if(input$radio_des==3){
      hd<- highchart() %>% 
        hc_chart(type ="column") %>%
        hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                   labels = list(style = list(color = "FFFFFF"))
        ) %>%
        hc_xAxis(categories = desempleo_aprobado$Unidad,
                 title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))) %>% 
        hc_add_series(data = desempleo_aprobado$Monto_2021, name = "Aprobado", color = "#749ca4")%>%
        hc_add_series(data = desempleo_ejercido$Monto_2021, name = "Ejercido", color = "#c84e41") %>%
        hc_title(
          text = "<b>Egresos de la CDMX, Sub-función: Desempleo</b>",
          style = list(color = "#FFFFFF", useHTML = TRUE)
        ) %>%
        hc_exporting(
          enabled=TRUE
        )%>%
        hc_add_theme(
          hc_theme_db()
        )
    }
    hd
  })
  
  
  output$gdes2 <- renderHighchart({
    highchart() %>% 
      hc_chart(type ="column") %>%
      hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))
      ) %>%
      hc_xAxis(categories = alcaldias_desmpleo$Unidad,
               title = list(style = list(color = "FFFFFF")),
               labels = list(style = list(color = "FFFFFF"))) %>% 
      hc_add_series(data = alcaldias_desmpleo$Aprobado, name = "Aprobado", color = "#749ca4")%>%
      hc_add_series(data = alcaldias_desmpleo$Ejercido, name = "Ejercido", color = "#c84e41") %>%
      hc_title(
        text = "<b>Egresos de la CDMX para Desempleo por Alcaldía</b>",
        style = list(color = "#FFFFFF", useHTML = TRUE)
      ) %>%
      hc_exporting(
        enabled=TRUE
      )%>%
      hc_add_theme(
        hc_theme_db()
      )
  })
  
  
  ##################################################ss####################################################
  cols <- carto_pal(10, "Prism")
  output$ss1 <- renderHighchart({
    programas_cdmx %>%
      hchart(type = 'bar', hcaes(x = Programa, y = Poblacion, group = type)) %>%
      hc_xAxis(
        title = list(style = list(color = "FFFFFF")),
        labels = list(style = list(color = "FFFFFF"))
      )%>%
      hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))
      ) %>%
      hc_colors(cols)%>%
      hc_title(
        text = "<b>Alcances de programas sociales</b>",
        style = list(color = "#FFFFFF", useHTML = TRUE)
      ) %>%
      hc_exporting(
        enabled=TRUE
      )%>%
      hc_add_theme(
        hc_theme_db()
      )
  })
  
  output$ss2 <- renderHighchart({
    Programas_cdmx_presupuesto %>%
      hchart(
        "pie", hcaes(x=Programa, y=presupuesto)
      )%>%
      hc_colors(cols)%>%
      hc_title(
        text = "<b>Presupuesto por programa</b>",
        style = list(color = "#FFFFFF", useHTML = TRUE)
      ) %>%
      hc_exporting(
        enabled=TRUE
      )%>%
      hc_add_theme(
        hc_theme_db()
      )
  })
  
  output$ss3 <- renderHighchart({
    nbi%>%
      hchart(type = 'column', hcaes(x = Estrato, y = count, group = gen))%>%
      hc_xAxis(
        title = list(style = list(color = "FFFFFF")),
        labels = list(style = list(color = "FFFFFF"))
      )%>%
      hc_yAxis(  title = list(style = list(color = "FFFFFF")),
                 labels = list(style = list(color = "FFFFFF"))
      ) %>%
      hc_colors(cols)%>%
      hc_title(
        text = "<b>Población por estratos socioeconómicos NBI</b>",
        style = list(color = "#FFFFFF", useHTML = TRUE)
      ) %>%
      hc_exporting(
        enabled=TRUE
      )%>%
      hc_add_theme(
        hc_theme_db()
      )
  })
  
  ##################################################MAPA####################################################
  
  output$maps <- renderLeaflet({
    
    shape40 <- merge(x = shape_mun, y = sol_ciudad, by = c("CVE_MUN","CVE_ENT"))
    
    
    shape2 <- shape40 
    
    
    popup <- paste0(
      "<b>","Nombre estado: ",   "</b>",   as.character(shape2$NOM_MUN) ,        "<br>",                     
      "<b>", "# de Solicitudes aprobadas"  ,   "</b>",   shape2$approved              , 
      "<b>", "# de Solicitudes negadas"  ,   "</b>",   shape2$not_approved              , "<br>" )
    
    palnumeric <- colorNumeric("viridis", domain = shape2$approved)
    
    palnumeric2<- colorNumeric("magma", domain = shape2$not_approved)
    mytext <- paste(
      "Alcaldia: ",shape2$NOM_MUN,"<br/>", 
      "# de Solicitudes aprobadas: ", round(shape2$approved,2), "<br/>",
      "# de Solicitudes negadas: ", round(shape2$not_approved,2), "<br/>",
      # "Year: ", shape2$Year, "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    m1<- leaflet(shape2,options = leafletOptions(
      zoomControl = TRUE, minZoom = 2, maxZoom = 20, attributionControl = FALSE)) %>%
      # Opcion para anadir imagenes o mapas de fondo (tiles)
      addProviderTiles("CartoDB.Positron") %>% #"Esri.WorldTerrain" "CartoDB.DarkMatter"
      # Funcion para agregar poligonos
      addPolygons(color = "white" ,
                  weight = .001, #linea de division 0.2 el valor perfecto
                  smoothFactor = 0.3, #suavizado entre mapas 0.3 el valor perfecto
                  opacity = 2, # color de la linea
                  fillOpacity = .9,
                  fillColor = ~palnumeric(shape2$approved),# Color de llenado
                  group = "Numero de Solicitudes Aprobadas",
                  layerId = ~shape2$CVE_EDO,                  
                  highlightOptions = highlightOptions(color = "#efb810", weight = ,
                                                      bringToFront = TRUE), #highlight cuando pasas el cursor
                  label = mytext ,                                  # etiqueta cuando pasas el cursor
                  labelOptions = labelOptions(direction = "auto",style = list("font-weight" = "normal", padding = "3px 8px"), 
                                              textsize = "13px",),
                  popup = popup)  %>%  
      addLegend(pal = palnumeric,values = ~shape2$approved, title = "Numero de Solicitudes Aprobadas",position = "topright",opacity=.9) %>% 
      addPolygons(color = "white" ,
                  weight = .001, #linea de division 0.2 el valor perfecto
                  smoothFactor = 0.3, #suavizado entre mapas 0.3 el valor perfecto
                  opacity = 2, # color de la linea
                  fillOpacity = .9,
                  fillColor = ~palnumeric2(shape2$not_approved),
                  group = "Numero de Solicitudes Negadas",
                  layerId = ~shape2$CVE_EDO,                  
                  highlightOptions = highlightOptions(color = "#efb810", weight = ,
                                                      bringToFront = TRUE), #highlight cuando pasas el cursor
                  label = mytext ,                                  # etiqueta cuando pasas el cursor
                  labelOptions = labelOptions(direction = "auto",style = list("font-weight" = "normal", padding = "3px 8px"), 
                                              textsize = "13px",),
                  popup = popup)  %>%  
      addLegend(pal = palnumeric2,values = ~shape2$not_approved, title = "Numero de Solicitudes Negadas",position = "bottomleft",opacity=.9) %>%
      addLayersControl( 
        baseGroups = c("Numero de Solicitudes Aprobadas", "Numero de Solicitudes Negadas"))
    
    m1
    
  })
  
  
}