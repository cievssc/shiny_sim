  #server para a aba estiagem (05-nov-2021, 15:31h)
  #click update
 
 
  #wrapping data
  dado_mapa_vento <- reactive({
               dado <- lapply(return_vento_season_adj, function(x){
                       dadoi <- x
                       dadoi$ID <- as.numeric(dadoi$ID)
                       dadoi[dadoi$estacao == vento_estacao() & dadoi$rperiods == as.numeric(input$retorno_vento),]
                       })
               #dado[[input$cenario_vento]]
               dado
               })
  
   vento_estacao <- reactiveVal('DJF')
   
   observe({
   value <- input$periodo_vento
   value <- ifelse(value == 1, 'DJF',
            ifelse(value == 2, 'MAM',
            ifelse(value == 3, 'JJA','SON')
            ))
   vento_estacao(value)
   })
   
   vento_municipio <- reactiveVal(c(88))
   observe({
     value <- municipiopoly@data[municipiopoly@data[,4] == input$municipio_vento,1]
   
   vento_municipio(value)
   })
   
    dado_serie_vento <- reactive({
                         dado <- lapply(return_vento_adj, function(x){
                         dadoi <- x
                         dadoi$ID <- as.numeric(dadoi$ID)
                         dadoi[dadoi$ID == vento_municipio(),]
                       })
                         dado
                          })
    dado_serie_vento_estacao <- reactive({
               dado <- lapply(return_vento_season_adj, function(x){
                       dadoi <- x
                       dadoi$ID <- as.numeric(dadoi$ID)
                       dadoi[dadoi$estacao == vento_estacao() & dadoi$ID == vento_municipio(),]
                       })
               #dado[[input$cenario_vento]]
               dado
               })
 
   
   
   #-----------graficos--------------- 
   output$serie_inteira_vento <- renderHigh({
                                    tipo <- 'spline'
                                    cenario <- c('Histórico', 'RCP 4.5', 'RCP 8.5')
                                    dadoi <- dado_serie_vento()
                                    periodo <- paste(dadoi[[1]][,5], 'anos')
                                    
                                    dadoi <- lapply(1:3, function(x){
                                    dadoii <- dadoi[[x]]
                                    list('name' = cenario[x],
                                         'data' = dadoii[,3])
                                        })
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })
    
  
  output$serie_estacao_vento <- renderHigh({
                                    tipo <- 'spline'
                                    cenario <- c('Histórico', 'RCP 4.5', 'RCP 8.5')
                                    dadoi <- dado_serie_vento_estacao()
                                    periodo <- paste(dadoi[[1]][,6], 'anos')
                                    
                                    dadoi <- 
                                    lapply(1:3, function(x){
                                    dadoi <- dadoi[[x]]
                                   list('name' = cenario[x],
                                         'data' = dadoi[,'c2'])})     
                                       
                                       
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })                        
 
   #-----------mapa---------------
   
 output$mapa_vento <- renderLeaflet({ leaflet()   %>%
            addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZGltaXRyaWJlc3NhIiwiYSI6ImNqOW82ZngxaTVhOW0zMm1xZGE2M2hidHoifQ.v16TlYEyqeTRXVsX-9AijQ')   %>% 
        setView(lng =  -48.5512765, lat = -27.5947736, zoom = 7) 
                }) 
 
  
  #plotando so valores no mapa
  observe({
    #3req(dado_mapa_estiagem())
    req(input$menu == 'vento')
    dadoi <- dado_mapa_vento()[[as.numeric(input$cenario_vento)]]
    dadoi$c2 <- round(dadoi$c2)
    
    
    mapa <- sp::merge(municipiopoly, dadoi, by.x = c('OBJECTID'), by.y = 'ID')
    
     labells <- sprintf(
  "<strong>%s</strong> %s<br/> <strong>%s</strong> %s<br/>", #  people / mi<sup>2</sup>",
 'Município: ', mapa$Municipio, 'Precipitação: ', round(mapa$c2,2)) %>% lapply(htmltools::HTML)
    
   cores <-  'RdBu'
   titulo <-  'Velocidade (m/s)'
   
   binsi <- unique(as.vector((quantile(mapa$c2, probs = c(0,0.30,0.50,0.65,0.8,0.95,1), na.rm = T))))
   
   pali <- colorBin(cores, domain =mapa$c2, bins = binsi,
                na.color = "transparent", reverse = T)
   colorDatai <- pali(mapa$c2)


   leafletProxy('mapa_vento') %>%
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
    addLegend(pal = pali,  values = colorDatai,
   title = titulo, position = "bottomleft",
    layerId="colorLegend2") 
    
    
  
  
  })      
  