  #server para a aba estiagem (05-nov-2021, 15:31h)
  #click update
 
 
  #wrapping data
  dado_mapa_chuva <- reactive({
               dado <- lapply(return_prec_season, function(x){
                       dadoi <- x
                       dadoi$ID <- as.numeric(dadoi$ID)
                       dadoi[dadoi$estacao == chuva_estacao() & dadoi$rperiods == as.numeric(input$retorno_chuva),]
                       })
               #dado[[input$cenario_chuva]]
               dado
               })
  
   chuva_estacao <- reactiveVal('DJF')
   
   observe({
   value <- input$periodo_chuva
   value <- ifelse(value == 1, 'DJF',
            ifelse(value == 2, 'MAM',
            ifelse(value == 3, 'JJA','SON')
            ))
   chuva_estacao(value)
   })
   
   chuva_municipio <- reactiveVal(c(88))
   observe({
     value <- municipiopoly@data[municipiopoly@data[,4] == input$municipio_chuva,1]
   
   chuva_municipio(value)
   })
   
    dado_serie_chuva <- reactive({
                         dado <- lapply(return_prec, function(x){
                         dadoi <- x
                         dadoi$ID <- as.numeric(dadoi$ID)
                         dadoi[dadoi$ID == chuva_municipio(),]
                       })
                         dado
                          })
    dado_serie_chuva_estacao <- reactive({
               dado <- lapply(return_prec_season, function(x){
                       dadoi <- x
                       dadoi$ID <- as.numeric(dadoi$ID)
                       dadoi[dadoi$estacao == chuva_estacao() & dadoi$ID == chuva_municipio(),]
                       })
               #dado[[input$cenario_chuva]]
               dado
               })
 
                          
   dado_dias_umidos <- reactive({
                       if(input$chuva_mm == 1){
                      dadoii <- subset(dia_umido_cenario, ID == chuva_municipio() & ano > 1980 &
                                !(ano == 2010 & cenario == '1975'))}else{
                      dadoii <- subset(dia_umido_cenario_20mm, ID == chuva_municipio() & ano > 1980 &
                                !(ano == 2010 & cenario != '1975'))          
                                }
                      
                      dadoii
                      })
                      
   
   dado_dias_umido_sequencia <- reactive({
                          dadoii <- subset(dia_umido_sequencia, ID == chuva_municipio() & ano > 1980 &
                                !(ano == 2010 & cenario == '1975'))
                          dadoii   
                             })
                             
                             
  dado_percentil_chuva <- reactive({
                          if(input$cenario_chuva == 2){
                          dadoi <- list('historico' = anomalia1975[anomalia1975$ano > 1980,], 'rcp45' = anomalia45[anomalia45$ano > 2010,])
                          }else{
                          dadoi <- list('historico' = anomalia1975[anomalia1975$ano > 1980,], 'rcp85' = anomalia85[anomalia85$ano > 2010,])}
                          if(input$percentis_chuva == 1){coluna <- c(1:3,4,7)}
                          if(input$percentis_chuva == 2){coluna <- c(1:3,5,8)}
                          if(input$percentis_chuva == 3){coluna <- c(1:3,6,9)}
                          dadoi <- lapply(dadoi, function(x){dadoi <- subset(x, ID == chuva_municipio())
                                   dadoi[,coluna]
                                   })
                          dadoi
                          
                          })
                      
   #-----------value_boxes---------------                                   
   output$chuva_txt1 <- renderValueBox({
    dadoi <- dado_dias_umidos()
    dadoi <- dadoi[dadoi$cenario == '1975',]
    dadoi <-round(mean(dadoi[,4]),2)
    
    titulo <- 'Média anual de dias umidos (1981-2010)'
    shinydashboard::valueBox(
    dadoi,  titulo,  icon = icon('history'),
      color = "blue" 
    )
  })
  
  output$chuva_txt2 <- renderValueBox({
    dadoi <- dado_dias_umidos()
    dadoi <- dadoi[dadoi$cenario == 'rcp45',]
    dadoi <-round(mean(dadoi[,4]),2)
    
    titulo <- 'Média anual de dias umidos (RCP 4.5)'
    shinydashboard::valueBox(
    dadoi,  titulo,  icon = icon('angle-up'),
      color = "blue" 
    )
  })
  
  output$chuva_txt3 <- renderValueBox({
    dadoi <- dado_dias_umidos()
    dadoi <- dadoi[dadoi$cenario == 'rcp85',]
    dadoi <-round(mean(dadoi[,4]),2)
    
    titulo <- 'Média anual de dias umidos (RCP 8.5)'
    shinydashboard::valueBox(
    dadoi,  titulo,  icon = icon('angle-double-up'),
      color = "blue" 
    )
  })
  
   output$chuva_p1 <- renderValueBox({
     dadoi <- dado_percentil_chuva()
    dadoi_hist <- round( mean(dadoi[[1]][,4], na.rm = T),2)
    dadoi_cenario <- round(mean(dadoi[[2]][,4], na.rm = T),2)
    dadoi <- paste0(dadoi_hist, '/',dadoi_cenario)
    titulo <- 'Média precipitação histórico / cenário'
    shinydashboard::valueBox(
    dadoi,  titulo,  icon = icon('cloud-rain'),
      color = "blue" 
    )
  })
  
  output$chuva_p2 <- renderValueBox({
    dadoi <- dado_percentil_chuva()
    dadoi_hist <- round( mean(dadoi[[1]][,5], na.rm = T),2)*100
    dadoi_cenario <- round(mean(dadoi[[2]][,5], na.rm = T),2)*100
    dadoi <- paste0(dadoi_hist, '%/',dadoi_cenario,'%')
    titulo <- 'Percentual histórico / cenário'
    shinydashboard::valueBox(
    dadoi,  titulo,  icon = icon('umbrella'),
      color = "blue" 
    )
  })
   
   #-----------graficos--------------- 
   output$serie_inteira_chuva <- renderHigh({
                                    tipo <- 'spline'
                                    cenario <- c('Histórico', 'RCP 4.5', 'RCP 8.5')
                                    dadoi <- dado_serie_chuva()
                                    periodo <- paste(dadoi[[1]][,5], 'anos')
                                    
                                    dadoi <- lapply(1:3, function(x){
                                    dadoii <- dadoi[[x]]
                                    list('name' = cenario[x],
                                         'data' = dadoii[,3])
                                        })
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })
    
  
  output$serie_estacao_chuva <- renderHigh({
                                    tipo <- 'spline'
                                    cenario <- c('Histórico', 'RCP 4.5', 'RCP 8.5')
                                    dadoi <- dado_serie_chuva_estacao()
                                    periodo <- paste(dadoi[[1]][,6], 'anos')
                                    
                                    dadoi <- 
                                    lapply(1:3, function(x){
                                    dadoi <- dadoi[[x]]
                                   list('name' = cenario[x],
                                         'data' = dadoi[,'c2'])})     
                                       
                                       
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })                        
  
  output$dias_umidos <- renderHigh({
                      tipo <- 'spline'
                      cenario <- c('Histórico', 'RCP 4.5', 'RCP 8.5')
                      periodo <- 1981:2040
                      dadoi <- dado_dias_umidos()
                      dadoi <- split(dadoi,dadoi$cenario)
                      dado <- lapply(1:3,function(x){
                              dado <- dadoi[[x]]
                              if(x == 1){
                              list('name' = cenario[x], 'data' = dado[,4]) }else{
                              list('name' = cenario[x], 'data' = c(rep(NA,30),dado[,4])) 
                              }
                              })
                      opcao <- list('plotOptions' = list('column' = list('pointPadding' = 0.2, 'borderWidth' = 0)))
                      list(tipo, periodo, dado) %>% unname
                      
                      })
                      
                      
  output$dias_consecutivos_chuva <- renderHigh({
                      dadoi <- dado_dias_umido_sequencia()
                      dadoi <- split(dadoi, dadoi$cenario)
                      cenario <- c('Histórico','RCP 4.5', 'RCP 8.5')
                      dadoi <- lapply(1:3, function(x){
                                dadoi <- dadoi[[x]]
                                list('x' = dadoi$mes, 'y' = dadoi$dia_umido, 'type' = 'box', 'name' = cenario[x])
                               
                               })
                               
                     list(dadoi, list('autosize' = 'true','boxmode' = 'group','paper_bgcolor' = 'rgba(0,0,0,0)',
                                 'plot_bgcolor' = 'rgba(0,0,0,0)', 'margin' = c("r"=0, 't'= 0),'legend' = c('orientation' = 'h')
                                 )) %>% unname 
                    })
  
  output$serie_rai <- renderHigh({
                                    tipo <- 'column'
                                    if(input$cenario_chuva == 2){dadoi <- dados_rai45
                                    cenario <- 'RCP 4.5'}else{
                                    dadoi <- dados_rai85
                                    cenario <- 'RCP 8.5'
                                    }
                                    span = .95
                                    dadoi <- subset(dadoi, ID == chuva_municipio() & year > 1980)
                                    periodo <- with(dadoi, paste0(month,'-',year))
                                    dadoi$index <- 1:nrow(dadoi)
                                    #dadoi$periodo <- with(dadoi, paste0(mes,'-',ano)
                                    ldadoi <- loess(rai ~index, data = dadoi, span = span)
                                    ldadoi <- predict(ldadoi)
                                    dadoi <- list(
                                    list('name' = cenario,
                                          'data' = round(dadoi$rai,2)))
                                        
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })
  
  
  output$serie_pmedia <- renderHigh({
                                    tipo <- 'spline'
                                    dadoi <- dado_percentil_chuva()
                                    if(input$cenario_chuva == 2){
                                    cenario <- c('RCP 4.5')}else{
                                    cenario <- 'RCP 8.5'
                                    } 
                                    periodo <- c(with(dadoi[[1]], paste0(mes,'-',ano)),
                                               with(dadoi[[2]], paste0(mes,'-',ano)))
                                   
                                    dadoi <- list(
                                    list('name' = 'Histórico',
                                          'data' = round(dadoi[[1]][,4],2)),
                                    list('name' = 'cenario',
                                          'data' = c(rep(NA,nrow(dadoi[[1]])),round(dadoi[[2]][,4],2)))      
                                          )
                                        
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })
 
 
 output$serie_ppercentual <- renderHigh({
                                    tipo <- 'spline'
                                    dadoi <- dado_percentil_chuva()
                                    if(input$cenario_chuva == 2){
                                    cenario <- c('RCP 4.5')}else{
                                    cenario <- 'RCP 8.5'
                                    } 
                                    periodo <- c(with(dadoi[[1]], paste0(mes,'-',ano)),
                                               with(dadoi[[2]], paste0(mes,'-',ano)))
                                   
                                    dadoi <- list(
                                    list('name' = 'Histórico',
                                          'data' = round(dadoi[[1]][,5],2)*100),
                                    list('name' = 'cenario',
                                          'data' = c(rep(NA,nrow(dadoi[[1]])),round(dadoi[[2]][,5],2)*100))      
                                          )
                                        
                                    
                                     list(tipo, periodo, dadoi) %>% unname
                                    })  
   #-----------mapa---------------
   
 output$mapa_chuva <- renderLeaflet({ leaflet()   %>%
            addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZGltaXRyaWJlc3NhIiwiYSI6ImNqOW82ZngxaTVhOW0zMm1xZGE2M2hidHoifQ.v16TlYEyqeTRXVsX-9AijQ')   %>% 
        setView(lng =  -48.5512765, lat = -27.5947736, zoom = 7) 
                }) 
 
  
  #plotando so valores no mapa
  observe({
    #3req(dado_mapa_estiagem())
    req(input$menu == 'chuva')
    dadoi <- dado_mapa_chuva()[[as.numeric(input$cenario_chuva)]]
    dadoi$c2 <- round(dadoi$c2)
    
    
    mapa <- sp::merge(municipiopoly, dadoi, by.x = c('OBJECTID'), by.y = 'ID')
    
     labells <- sprintf(
  "<strong>%s</strong> %s<br/> <strong>%s</strong> %s<br/>", #  people / mi<sup>2</sup>",
 'Município: ', mapa$Municipio, 'Precipitação: ', round(mapa$c2,2)) %>% lapply(htmltools::HTML)
    
   cores <-  'RdBu'
   titulo <-  'Precipitação diária (mm)'
   
   binsi <- unique(as.vector((quantile(mapa$c2, probs = c(0,0.30,0.50,0.65,0.8,0.95,1), na.rm = T))))
   
   pali <- colorBin(cores, domain =mapa$c2, bins = binsi,
                na.color = "transparent", reverse = T)
   colorDatai <- pali(mapa$c2)


   leafletProxy('mapa_chuva') %>%
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
  