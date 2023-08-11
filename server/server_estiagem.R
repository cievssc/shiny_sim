  #server para a aba estiagem (05-nov-2021, 15:31h)
  #click update
  output$textoteste <- renderPrint({
                       input$mapa_estiagem_shape_click
                       })
 
 
  #wrapping data
  dado_mapa_estiagem <- reactive({
               if(as.numeric(input$variavel_estiagem) == 1){dado <- list(tmedia45, tmedia85)}
               if(as.numeric(input$variavel_estiagem) == 2){dado <- list(pdsi1975_45,pdsi1975_85)}
               if(as.numeric(input$variavel_estiagem) == 3){dado <- list(all_umidade45, all_umidade85)}
               dado
               })
  
   estiagem_estacao <- reactiveVal(1)
   
   observe({
   estiagem_estacao(as.numeric(input$periodo_estiagem))
   })
   
   estiagem_municipio <- reactiveVal(c(88))
   observe({
     value <- municipiopoly@data[municipiopoly@data[,4] == input$municipio_estiagem,1]
   
   estiagem_municipio(value)
   })
   
   dado_serie_estiagem <- reactive({
                          dadoi <- dado_mapa_estiagem()
                          dadoi <- lapply(dadoi, function(x){x <- x[x$ID == estiagem_municipio() & x$ano > 1980,]
                                   arrange(x, ano, mes)})
                          dadoi
                          })
                          
   dado_dias_secos <- reactive({
                      dadoi <- dia_seco1975[dia_seco1975$ID == estiagem_municipio(),]
                      dadoii <- subset(dia_seco_cenario, ID == estiagem_municipio() & ano > 2010)
                      list(dadoi, dadoii)
                      })
                      
   #-----------value_boxes---------------                                   
   output$estiagem_txt1 <- renderValueBox({
    dadoi <- dado_dias_secos()[[1]]
    dadoi <-round(mean(dadoi[,3]),2)
    
    titulo <- 'Média anual de dias secos (1981-2010)'
    shinydashboard::valueBox(
    dadoi,  titulo,  icon = icon('history'),
      color = "yellow" 
    )
  })
  
  output$estiagem_txt2 <- renderValueBox({
    dadoi <- dado_dias_secos()[[2]]
    dadoi <- round(mean(dadoi[,3]),2)
    
    titulo <- 'Média anual de dias secos (RCP 4.5)'
    shinydashboard::valueBox(
    dadoi,  titulo,  icon = icon('angle-up'),
      color = "yellow" 
    )
  })
  
  output$estiagem_txt3 <- renderValueBox({
    dadoi <- dado_dias_secos()[[2]]
    dadoi <- round(mean(dadoi[,4]),2)
    
    titulo <- 'Média anual de dias secos (RCP 8.5)'
    shinydashboard::valueBox(
    dadoi,  titulo,  icon = icon('angle-double-up'),
      color = "yellow" 
    )
  })
   
   #-----------graficos--------------- 
   output$serie_inteira_estiagem <- renderHigh({
                                    tipo <- 'line'
                                    dadoi <- dado_serie_estiagem()
                                    if(input$variavel_estiagem == 1){span <- .5}else{
                                    span = .3}
                                    periodo <- with(dadoi[[1]], paste0(mes,'-',ano))
                                    
                                    dadoi <- lapply(1:2, function(x){
                                    dadoi <- dadoi[[x]]
                                    if(x == 1){cenario <- 'RCP 4.5'}else{cenario <- 'RCP 8.5'}
                                    names(dadoi)[5] <- 'variavel'
                                    dadoi$index <- 1:nrow(dadoi)
                                    #dadoi$periodo <- with(dadoi, paste0(mes,'-',ano)
                                    ldadoi <- loess(variavel ~index, data = dadoi, span = span)
                                    ldadoi <- predict(ldadoi)
                                    list('name' = cenario,
                                         'data' = ldadoi)
                                        })
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })
    
  
  output$serie_estacao_estiagem <- renderHigh({
                                    tipo <- 'line'
                                    dadoi <- dado_serie_estiagem()
                                    if(input$variavel_estiagem == 1){span <- .3}else{
                                    span = .3}
                                   
                                     estacao <- ifelse(estiagem_estacao() == 1, c(12,1,2),
                                     ifelse(estiagem_estacao() == 2, c(3:5),
                                     ifelse(estiagem_estacao() == 3, c(6:8),c(9:11)
                                     )))
                                     dadoi <- lapply(dadoi, function(x){x[x$mes %in% estacao& x$tipo != 'historico',]})
                                     periodo <- with(dadoi[[1]], paste0(mes,'-',ano))
                                     
                                    dadoi <- lapply(1:2, function(x){
                                    dadoi <- subset(dadoi[[x]])
                                    if(x == 1){cenario <- 'RCP 4.5'}else{cenario <- 'RCP 8.5'}
                                    names(dadoi)[5] <- 'variavel'
                                    dadoi$index <- 1:nrow(dadoi)
                                    #dadoi$periodo <- with(dadoi, paste0(mes,'-',ano)
                                    ldadoi <- loess(variavel ~index, data = dadoi, span = span)
                                    ldadoi <- predict(ldadoi)
                                    list('name' = cenario,
                                         'data' = ldadoi)
                                        })
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })                        
  
  
  output$dias_secos <- renderHigh({
                      tipo <- 'spline'
                      dadoi <- dado_dias_secos()[[1]]
                      dadoii <- dado_dias_secos()[[2]]
                      periodo <- c(dadoi$ano, dadoii$ano)
                      dado <- list(list('name' = 'Histórico', 'data' = dadoi[,3]),
                                   list('name' = 'RCP 4.5', 'data' = c(rep(NA, nrow(dadoi)),dadoii[,3])),
                                   list('name' = 'RCP 8.5', 'data' =c(rep(NA, nrow(dadoi)),dadoii[,4])))
                      opcao <- list('plotOptions' = list('column' = list('pointPadding' = 0.2, 'borderWidth' = 0)))
                      list(tipo, periodo, dado) %>% unname
                      
                      }) 
                      
                      
   output$dias_consecutivos <- renderHigh({
                      dadoi <- list(dias_consec1975, dias_consec45, dias_consec85)
                      cenario <- c('Histórico','RCP 4.5', 'RCP 8.5')
                      dadoi <- lapply(1:3, function(x){
                                dadoi <- dadoi[[x]]
                                dadoi <- subset(dadoi, ID == estiagem_municipio() & ano > 1980)
                                if(any(dadoi$ano == 2040)){dadoi <- dadoi[dadoi$ano > 2010,]}
                                dadoi <- arrange(dadoi, mes)
                                list('x' = dadoi$mes, 'y' = dadoi$dia_seco, 'type' = 'box', 'name' = cenario[x])
                               
                               })
                               
                     list(dadoi, list('autosize' = 'true','boxmode' = 'group','paper_bgcolor' = 'rgba(0,0,0,0)',
                                 'plot_bgcolor' = 'rgba(0,0,0,0)', 'margin' = c("r"=0, 't'= 0),'legend' = c('orientation' = 'h')
                                 )) %>% unname 
                    })
                    
    output$serie_spi <- renderHigh({
                                    tipo <- 'column'
                                    id <- as.character(estiagem_municipio())
                                    if(input$cenario_estiagem == 1){
                                    dadoi <- dados_spi[dados_spi$ID == id,]
                                    cenario <- 'RCP 4.5'}else{
                                    dadoi <- dados_spi85[dados_spi85$ID == id,]
                                    cenario <- 'RCP 8.5'}
                                    periodo <- dadoi[,2]
                                    
                                    dadoi <- 
                                    list(list('name' = cenario,
                                         'data' = dadoi[,3]),
                                         list('name' = 'null',
                                         'data' = dadoi[,3],
                                         'type' = 'spline',
                                          'color' = 'black')
                                         )
                                    
                                    list(tipo, periodo, dadoi) %>% unname
                                    })
                                    
                                    
    output$serie_palmer <- renderHigh({
                                    tipo <- 'column'
                                    id <- estiagem_municipio()
                                    if(input$cenario_estiagem == 1){
                                    dadoi <- spei_mun(y = id)
                                    cenario <- 'RCP 4.5'}else{
                                    dadoi <- spei_mun(x = pdsi1975_85,y = id)
                                    cenario <- 'RCP 8.5'}
                                    periodo <- pdsi1975_45[pdsi1975_45$ano > 1980,c(2,3)]
                                    periodo <- arrange(periodo, ano, mes) 
                                    periodo <- with(periodo, paste0(mes,'-',ano))
                                    
                                    dadoi <- 
                                    list(list('name' = cenario,
                                         'data' = dadoi$X,
                                         'color' = 'orange'),
                                         list('name' = 'null',
                                         'data' = dadoi$X,
                                         'type' = 'spline',
                                          'color' = 'black')
                                         )
                                    
                                    list(tipo, unique(periodo), dadoi) %>% unname
                                    })
   #-----------mapa---------------
   
 output$mapa_estiagem <- renderLeaflet({ leaflet()   %>%
            addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZGltaXRyaWJlc3NhIiwiYSI6ImNqOW82ZngxaTVhOW0zMm1xZGE2M2hidHoifQ.v16TlYEyqeTRXVsX-9AijQ')   %>% 
        setView(lng =  -48.5512765, lat = -27.5947736, zoom = 7) 
                }) 
 
  
  #plotando so valores no mapa
  observe({
    #3req(dado_mapa_estiagem())
    req(input$menu == 'estiagem')
    dadoi <- dado_mapa_estiagem()[[as.numeric(input$cenario_estiagem)]]
    
    estacao <- ifelse(estiagem_estacao() == 1, c(12,1,2),
               ifelse(estiagem_estacao() == 2, c(3:5),
               ifelse(estiagem_estacao() == 3, c(6:8),c(9:11)
               )))
    
    dadoi <- lapply(split(dadoi, dadoi$tipo), function(x){
               if(x$tipo[1] == 'historico'){
               dadoii <- subset(x, ano > 1980 & mes %in% estacao)}else{
               dadoii <- subset(x, ano > 2010 & mes %in% estacao)}
    
              names(dadoii)[5] <- 'variavel'
              aggregate(variavel ~ ID, data = dadoii, FUN = mean)
             })
    dadoi <- data.frame(OBJECTID = dadoi[[1]][,1], diferenca = round(dadoi[[2]][,2] - dadoi[[1]][,2],3))
    if(input$variavel_estiagem == 3){
    dadoi[,2] <- dadoi[,2]*100}
    
    mapa <- sp::merge(municipiopoly, dadoi, by = 'OBJECTID')
    
     labells <- sprintf(
  "<strong>%s</strong> %s<br/> <strong>%s</strong> %s<br/>", #  people / mi<sup>2</sup>",
 'Município: ', mapa$Municipio, 'Diferença entre cenários: ', mapa$diferenca) %>% lapply(htmltools::HTML)
    
   cores <-  'YlOrRd'
   titulo <- ifelse(input$variavel_estiagem == 1, 'Temperatura (ºC)',
             ifelse(input$variavel_estiagem == 2, 'Precipitação (mm)', 'Umidade do solo (%)'))
   
   binsi <- unique(as.vector((quantile(mapa$diferenca, probs = c(0,0.30,0.50,0.65,0.8,0.95,1), na.rm = T))))
   
   pali <- colorBin(cores, domain =mapa$diferenca, bins = binsi,
                na.color = "transparent", reverse = ifelse(input$variavel_estiagem == 1, F, T))
   colorDatai <- pali(mapa$diferenca)


   leafletProxy('mapa_estiagem') %>%
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
  