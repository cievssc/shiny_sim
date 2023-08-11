  #server para a aba forecasting (08-out-21, 16:58h)
  
 #dados base
  dado_forecasting <- reactive({
                      if(input$regiao_forecasting != "Estado"){
                        dadoi <- df_forecasting_casos[df_forecasting_casos[,2] == input$regiao_forecasting,]
                        }
                      if(input$regiao_forecasting == "Estado"){
                      dadoi <- aggregate(cbind(Freq, forecasting_smedia, forecasting_cmedia) ~dat_inicio_sintomas, data = df_forecasting_casos, FUN = sum, na.rm = T)
                      })
 
 
   #values boxes
  output$total_vacina <- renderValueBox({
      dadoi <- dado_vacina()[['Estado']]
      
      valueBox(
      sum(dadoi[,'total'], na.rm = T),
      paste("Total vacinas aplicadas"), icon = icon("syringe"),
      color = "green"
    )
  })
  
   output$total_pessoas <- renderValueBox({
      dadoi <- dado_vacina()[['Estado']]
      
      valueBox(
      sum(dadoi[,2], na.rm = T),
      paste("1a Dose"), icon = icon("smile"),
      color = "green"
    )
  })
  
  output$total_imunizacao <- renderValueBox({
      dadoi <- dado_vacina()[['Estado']]
      
      valueBox(
      sum(dadoi[,3], na.rm = T),
      paste("2a Dose"), icon = icon("smile-beam"),
      color = "green"
    )
  })
  
  output$perc_doses <- renderValueBox({
      dadoi <- dado_vacina()[['Estado']]
      valor <- round(sum(dadoi[,2], na.rm = T)*100/sum(idade_faixa_saude[!(idade_faixa_saude[,2] %in% c("0-9","10-19")),3]),2)
      valueBox(
      paste0(valor,"%"),
      paste("Percentual estadual da população vacinável com 1ª Dose"), icon = icon("sort-numeric-up-alt"),
      color = "green"
    )
  })
  
  output$perc_imuniczacao <- renderValueBox({
      dadoi <- dado_vacina()[['Estado']]
      valor <- round(sum(dadoi[,3], na.rm = T)*100/sum(idade_faixa_saude[!(idade_faixa_saude[,2] %in% c("0-9","10-19")),3]),2)
      valueBox(
      paste0(valor,"%"),
      paste("Percentual estadual da população vacinável com 2ª Dose"), icon = icon("sort-numeric-up-alt"),
      color = "green"
    )
  })
  
  
  
  tabela_vacina <- reactive({dadoi <- dado_vacina()[-1]
                            dadoi <- purrr::map_df(dadoi, function(x){apply(x[,-1],2,sum)}, .id = 'reg_saude') %>% as.data.frame
                            dadoi <- left_join(dadoi, reg_pop, by = 'reg_saude')
                            tabela <- data.frame('Região' = dadoi[,'reg_saude'],
                                       'População' = dadoi[,'populacao'],
                                       'perc.População' = percent((dadoi[,'prop_pop'])),#paste0(round(dadoi[[1]][,'prop_pop']*100,2),'%'),
                                       '1a_Dose' = dadoi[,2],
                                       '2a_Dose' = dadoi[,3],
                                       'Total_doses' = dadoi[,4],
                                        'Cobertura_1a_Dose' = percent((dadoi[,2]/(dadoi[,'populacao']))),#paste0(round(dadoi[[1]][,2]*100/sum(dadoi[[1]][,2]),2),'%'),
                                       'Cobertura_Imunização' = percent((dadoi[,3]/(dadoi[,'populacao'])))#,
                                      #  'prop.Dose_1.prop.População' = round((dadoi[,2]/(dadoi[,'populacao']))/(dadoi[,'prop_pop']),2),#paste0(round(dadoi[[2]][,2]*100/sum(dadoi[[2]][,2]),2),'%'),
                                       #'prop.Dose_2.prop.População' = round((dadoi[,3]/(dadoi[,'populacao']))/(dadoi[,'prop_pop']),2) #paste0(round(dadoi[[2]][,2]*100/sum(dadoi[[2]][,2]),2),'%') 
                                       )
                           tabela})
                           
  
  
  #tabelas
  output$tabela_sintese_vacina <- renderFormattable({  #DT::renderDataTable({#
                           dadoi <- tabela_vacina() %>% arrange(., desc(Cobertura_Imunização))
                           #names(dadoi)[8] <- 'Letalidade aparente'
                           formattable(dadoi, 
                           align = c('l','c','c','c','c','c','c','l'),
                           list(`Cobertura_Imunização` = formattable::color_bar('lightgreen'),
                                `Região` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))))# %>% as.datatable()
                            })
 
  
 
                                 
  #-----------gráficos--------------- 

   
   output$imunizacao_reg <- renderHigh({
                        dadoi <- tabela_vacina()
                        if(input$radio_perc_vacina == 1){dadoi <- dadoi[,c(1,4,5)]
                                                         }
                        if(input$radio_perc_vacina == 2){dadoi <- dadoi[,c(1,7,8)]
                                                        dadoi[,2] <- round(as.numeric(dadoi[,2])*100)
                                                        dadoi[,3] <- round(as.numeric(dadoi[,3])*100)
                                                        }
                        tipo <- 'bar'
                        
                        dado <- list(list('name' = 'Dose 1', 'data' = dadoi[,2]), list('name' = 'Dose 2', 'data' = dadoi[,3])) %>% unname
                        
                        list(tipo, unique(dadoi[,1]), dado) %>% unname
                         
                         })
                         
   output$imunizacao_estado <- renderHigh({
                        dadoi <- tabela_vacina()[,c(4:6)]
                        dadoi <- apply(dadoi, 2, sum) %>% unname
                        pop <- sum(idade_faixa_saude[!(idade_faixa_saude[,2] %in% c("0-9","10-19")),3])
                        if(input$radio_perc_vacina == 2){dadoi <- round(dadoi*100/pop,2)
                                                        }
                        tipo <- 'column'
                        
                        dado <- list(list('name' = 'Dose 1', 'data' = list(dadoi[1]), 'pointPadding' = 0.3, 'pointPlacement' = -0.2), 
                                list('name' = 'Dose 2', 'data' = list(dadoi[2]), 'pointPadding' = 0.3, 'pointPlacement' = -0.2)) %>% unname
                        
                        list(tipo, 'Estado', dado) %>% unname
                         
                         })
                         
   output$fabricante_estado <- renderHigh({
                        texto <- 'Totais doses'
                        dadoi <- dado_fabricante()
                        dadoi <- aggregate(Freq ~ fabricante, data = dadoi, FUN = sum, na.rm = T)
                        if(input$radio_perc_vacina == 2){dadoi$Freq <- round(dadoi$Freq*100/sum(dadoi$Freq),2)
                                                        texto <- 'Composição das vacinas (%)'}
                        tipo <- 'bar'
                        dado <- list(list('name' = texto, 'data' = dadoi[,2])) %>% unname
                        
                        list(tipo, dadoi[,1], dado) %>% unname
                         
                         })
                         
 output$serie_vac_estado <- renderHigh({
                        dadoi <- dado_vacina()[['Estado']]
                        dadoi <- dadoi[,c(1,4)]
                        
                         dadoi$media <- media_movel(dadoi[,2]) %>% as.numeric
                        
                        tipo <- 'column'
                        
                        dado <- list(list('name' = 'Doses', 'data' = dadoi[,2]), list('name' = 'Média móvel', 'data' = round(dadoi[,3]), 'type' = 'line')) %>% unname
                        
                        list(tipo, unique(dadoi[,1]), dado) %>% unname
                         
                         })
                         
                             
 output$idade_estado <- renderHigh({
                        dadoi <- dado_idade()[[1]]
                        #dadoi <- dadoi[dadoi[,3] == input$reg_graficos_vacina & dadoi[,2] >= as.Date('2021-01-15'),-3]
                        #dadoi <- dadoi[,c(1,2,4)]
                        # pop <- sum(idade_faixa_saude[!(idade_faixa_saude[,2] %in% c("0-9","10-19")) & idade_faixa_saude$reg_saude == input$reg_graficos_vacina,3])
                        if(input$radio_perc_vacina == 2){dadoi[,2:3] <- round(dadoi[,2:3]*100/dadoi[,4],2)}
                        
                        dado <- list(list('name' = 'Dose 1', 'data' = dadoi[,2]), list('name' = 'Dose 2', 'data' = dadoi[,3]))
                        tipo <- 'bar'
                                                
                        list(tipo, dadoi[,1], dado) %>% unname
                           })  
  

 #regioes
  output$serie_vac_regiao <- renderHigh({
                        dadoi <- dado_vacina()[[input$reg_graficos_vacina]]
                        dadoi <- dadoi[,c(1,4)]
                        
                        dadoi$media <- media_movel(dadoi[,2]) %>% as.numeric
                        
                        tipo <- 'column'
                        
                        dado <- list(list('name' = 'Doses', 'data' = dadoi[,2]), list('name' = 'Média móvel', 'data' = round(dadoi[,3]), 'type' = 'line')) %>% unname
                        
                        list(tipo, unique(dadoi[,1]), dado) %>% unname
                         
                         })
                         
  output$fabricante_reg <- renderHigh({
                        texto <- 'Totais doses'
                        dadoi <- dado_fabricante()
                        dadoi <- dadoi[dadoi[,3] == input$reg_graficos_vacina,]
                        dadoi <- aggregate(Freq ~ fabricante, data = dadoi, FUN = sum, na.rm = T)
                        #pop <- sum(idade_faixa_saude[!(idade_faixa_saude[,2] %in% c("0-9","10-19")) & 'reg_saude' == input$reg_graficos_vacina,3])
                        
                        if(input$radio_perc_vacina == 2){dadoi$Freq <- round(dadoi$Freq*100/sum(dadoi$Freq),2)
                                                        texto <- 'Composição das vacinas (%)'}
                        tipo <- 'bar'
                        dado <- list(list('name' = texto, 'data' = dadoi[,2])) %>% unname
                        
                        list(tipo, dadoi[,1], dado) %>% unname
                         
                         })
                         
 output$serie_fab_regiao <- renderHigh({
                        dadoi <- dado_fabricante()
                        dadoi <- dadoi[dadoi[,3] == input$reg_graficos_vacina & dadoi[,2] >= as.Date('2021-01-15'),-3]
                        #dadoi <- dadoi[,c(1,2,4)]
                         pop <- sum(idade_faixa_saude[!(idade_faixa_saude[,2] %in% c("0-9","10-19")) & idade_faixa_saude$reg_saude == input$reg_graficos_vacina,3])
                         if(input$radio_perc_vacina == 2){dadoi[,3] <- round(dadoi[,3]*100/pop,2)}
                        
                        dado <- lapply(split(dadoi, dadoi$fabricante), function(x){
                                 list('name' = x[1,1], 'data' = x[,3])
                                 }) %>% unname
                        tipo <- 'line'
                                                
                        list(tipo, unique(dadoi[,2]), dado) %>% unname
                           })                      
                           
 output$idade_regiao <- renderHigh({
                        dadoi <- dado_idade()[[input$reg_graficos_vacina]]
                        #dadoi <- dadoi[dadoi[,3] == input$reg_graficos_vacina & dadoi[,2] >= as.Date('2021-01-15'),-3]
                        #dadoi <- dadoi[,c(1,2,4)]
                        # pop <- sum(idade_faixa_saude[!(idade_faixa_saude[,2] %in% c("0-9","10-19")) & idade_faixa_saude$reg_saude == input$reg_graficos_vacina,3])
                        if(input$radio_perc_vacina == 2){dadoi[,2:3] <- round(dadoi[,2:3]*100/dadoi[,4],2)}
                        
                        dado <- list(list('name' = 'Dose 1', 'data' = dadoi[,2]), list('name' = 'Dose 2', 'data' = dadoi[,3]))
                        tipo <- 'bar'
                                                
                        list(tipo, dadoi[,1], dado) %>% unname
                           })                           
  
  
  
  #-----------mapa---------------
   
   output$mapa_vacina <-  renderLeaflet({leaflet() %>%       setView(lat = -27.5, lng = -51, zoom = 7)  %>%
    addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZGltaXRyaWJlc3NhIiwiYSI6ImNqOW82ZngxaTVhOW0zMm1xZGE2M2hidHoifQ.v16TlYEyqeTRXVsX-9AijQ')   %>%
       addLayersControl(
            baseGroups = c( "Total Doses","1a Dose", "2a Dose", "Cobertura 1a Dose", 'Imunização completa'),
            options = layersControlOptions(collapsed = F), position = 'bottomleft' )  
        })  
        
   observe({
     #req(tabela_vacina())
     dadoi <- tabela_vacina()
     
     mapa <- sp::merge(mapa_regionais, dadoi, by.x = 'reg_saude', by.y = 'Região')

      labells <- sprintf(
  "<strong>%s</strong> %s<br/> <strong>%s</strong> %s<br/> 
  <strong>%s</strong> %s<br/> <strong>%s</strong> %s<br/>
  
  ", #  people / mi<sup>2</sup>",
 'Região: ', mapa$reg_saude, '1a Dose: ', mapa$X1a_Dose,
 '2a Dose: ', mapa$X2a_Dose,'Total Doses: ', mapa$Total_doses) %>% lapply(htmltools::HTML)
   
  mapa$incidencia <- mapa$Total_doses
     paleta <- c('yellow', 'orange')
     cores <- "YlOrRd"
     titulo <- 'Total Doses'
   if(any(input$mapa_vacina_groups %in% 'Total Doses')){
     mapa$incidencia <- mapa$Total_doses
     paleta <- c('yellow', 'orange')
     cores <- "YlOrRd"
     titulo <- 'Total Doses'}

   if(any(input$mapa_vacina_groups %in% '1a Dose')){
     mapa$incidencia <- mapa$X1a_Dose
     paleta <- c('yellow', 'orange')
     cores <- "YlOrRd"
     titulo <- '1a Dose'}
     
     if(any(input$mapa_vacina_groups %in% '2a Dose')){
     mapa$incidencia <- mapa$X2a_Dose
     paleta <- c('yellow', 'orange')
     cores <- "YlOrRd"
     titulo <- '2a Dose'}
     
    if(any(input$mapa_vacina_groups %in% 'Cobertura 1a Dose')){
     mapa$incidencia <-as.numeric(mapa$Cobertura_1a_Dose)*100
     paleta <- c('yellow', 'orange')
     cores <- "YlOrRd"
     titulo <- 'Cobertura 1a Dose'}


   if(any(input$mapa_vacina_groups %in% 'Imunização completa')){
     mapa$incidencia <- as.numeric(mapa$Cobertura_Imunização)*100
     paleta <- c('white', 'blue')
     cores <- 'Blues'
     titulo <- 'Imunização completa'}

   binsi <- unique(as.vector((quantile(mapa$incidencia, probs = c(0,0.30,0.50,0.65,0.8,0.95,1), na.rm = T))))
    if(length(binsi) <= 2){pali <-  colorNumeric(palette = paleta,domain = 1:2,
                na.color = "transparent")}else{
   pali <- colorBin(cores, domain =mapa$incidencia, bins = binsi,
                na.color = "transparent")}
   colorDatai <- pali(mapa$incidencia)


   leafletProxy('mapa_vacina') %>%
             clearShapes() %>%
              setView(lat = -27.5, lng = -51, zoom = 7)  %>%
               addPolygons(data = mapa,  color = "#444444",
    fillColor =  colorDatai,
     stroke = T, smoothFactor = 0.5, fillOpacity = 0.7,
    weight = 1.5,highlight = highlightOptions(
    weight = 5,
    color = "#666",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labells,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "12px",
    maxWidth = '200px',
    direction = "auto"))  %>%
    addLegend(pal = pali,  values = if(length(binsi) <= 2){unique(mapa$incidencia)}else{colorDatai},
   title = titulo, position = "bottomright",
    layerId="colorLegend2")
                    }) 
 
        
      
  