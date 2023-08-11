  #server para a aba geral (18-out-21, 00:10h)
  
  dado_mapa <- reactive({
               if(as.numeric(input$variavelI) == 4){dado <- list(media_precacum1975, media_precacum45_all, media_precacum85_all)}
               if(as.numeric(input$variavelI) == 3){dado <- list(temp_media1975, temp_media45, temp_media85)}
               if(as.numeric(input$variavelI) == 5){dado <- list(media_umd_disp1975, media_umd45_all, media_umd85_all)} 
               #if(as.numeric(input$variavelI) == 6){dado <- list(media_zonal1975, media_zonal10m45, media_zonal10m85)}
               if(as.numeric(input$variavelI) == 6){dado <- list(mapa_vento_hist, mapa_vento45, mapa_vento85)}
               if(as.numeric(input$variavelI) == 7){dado <- list(extremo_precacum1975, extremo_precacum45, extremo_precacum85)}
               dado
               })
  
  
   output$cenario_txt <- renderValueBox({
    if(input$modeloI == 2){titulo <- 'RCP 4.5'}else{
    titulo <- 'RCP 8.5'}
    shinydashboard::valueBox(
      titulo, '2011-2040', icon = icon('poll'),
      color = "yellow" 
    )
  })
  
   #-----------mapa---------------
   
 output$map <- renderLeaflet({ leaflet()   %>%
            addMapPane("left", zIndex = 0) %>%
              addMapPane("right", zIndex = 0) %>%
            addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZGltaXRyaWJlc3NhIiwiYSI6ImNqOW82ZngxaTVhOW0zMm1xZGE2M2hidHoifQ.v16TlYEyqeTRXVsX-9AijQ', options = gridOptions(pane = "left"), layerId = 'baseid')   %>% 
            addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZGltaXRyaWJlc3NhIiwiYSI6ImNqOW82ZngxaTVhOW0zMm1xZGE2M2hidHoifQ.v16TlYEyqeTRXVsX-9AijQ', options = gridOptions(pane = "right"), layerId = 'baseidi')   %>% 
        setView(lng =  -48.5512765, lat = -27.5947736, zoom = 7) %>%
          addPolylines(data = municipiopoly, color = "#f5f5f5",
        weight = 2, opacity = 0.2) %>%
  leaflet.extras2::addSidebyside(#layerId = "sidecontrols",
               leftId = "baseid",
               rightId =   "baseidi")
                }) 
 
        
   
 #mapa casos
   observe({
     req(dado_mapa())
     dadoi <- dado_mapa()
     if(input$variavelI == 5){
     dadoi <- lapply(dadoi, function(x){
               lapply(x,function(z){z*100})})
               }
    #valor da transparência        
     transparencia <- as.numeric(input$decimal)
     
      #mapa <- sp::merge(mapa_regionais, dadoi, by.x = 'reg_saude', by.y = 'Região')
      f1 <- dadoi[[1]][[as.numeric(input$periodo)]]
      f2 <- dadoi[[as.numeric(input$modeloI)]][[as.numeric(input$periodo)]]

     
        #valores para paleta de cores
        if(min(values(f1), na.rm = T) < min(values(f2), na.rm = T)){minimo <- floor(min(values(f1), na.rm = T))}else{minimo <- floor(min(values(f2), na.rm = T))}
        if(max(values(f1), na.rm = T) < max(values(f2), na.rm = T)){maximo <- ceiling(max(values(f2), na.rm = T))}else{maximo <- ceiling(max(values(f1), na.rm = T))}

        val <- as.numeric(c(minimo:maximo))
        
        if(input$variavelI %in% 4:5){
            pale <- colorNumeric(palette = colorRamps::matlab.like2(length(val)), val,
                na.color = "transparent")}else{
            pale <- colorNumeric(palette = c('blue', 'green4', 'yellow', 'red'), val,
                na.color = "transparent")}
                
       if(input$variavelI == 4 | input$variavelI == 5){titulo <- "mm "}else{
       if(input$variavelI == 3){titulo <- 'ºC'}else{
       if(input$variavelI == 7){titulo <- 'mm/dia'}else{titulo <- 'm/s'}}}
        if(input$variavelI == 5){titulo <- '%'}
        if(input$variavelI == 6){titulo <- 'Intensidade de\nOcorrência'}
  
           
  leafletProxy('map') %>%
             clearImages() %>%
            # addMapPane("left", zIndex = 0) %>%
             # addMapPane("right", zIndex = 0) %>%
             addRasterImage(f1, colors =pale, opacity = transparencia, options = gridOptions(pane = "left")) %>%
              addRasterImage(f2, colors =pale, opacity = transparencia, options = gridOptions(pane = "right")) %>%
              addLegend(pal = pale, values = val,
              title = titulo, position = "bottomright", layerId="colorLegend")  
                    
                    }) 
 
        
      