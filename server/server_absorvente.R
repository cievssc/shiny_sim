  #server para a aba absorvente (14-dez-2022, 10:35h)
  
  dados_absorventes <- eventReactive(input$head_atualizar, {
                              if(any(input$all_municipio == 'Todos')){dadoi <- absorventes}else{
                                 dadoi <- subset(absorventes, municipio %in% input$all_municipio)}
                              dadoi}, ignoreNULL = FALSE)

  
  mod_summary_card_server('absorvente_total_card', 
                            card_large(heading =  'Total de estudantes beneficiadas',
                              uiOutput('absorvente_total')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$absorvente_total <- renderUI({
                    dadoi <- dados_absorventes()
                    dadoi <- sum(dadoi$qtde_estudantes, na.rm = T)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                    )
                    })
  
  
  #-------------------------------------
  #mapa
  mod_summary_card_server('absorvente_mapa', 
                   card_large(heading = tagList(h1('Alunas beneficiadas')),
                      leafletOutput('absorventes_mapa_leaflet') %>% withSpinner(color="#0dc5c1"))
                             )
                             
                             
  output$absorventes_mapa_leaflet <- renderLeaflet({ absorventes_leaflet_data()
        })
        
        
  absorventes_leaflet_data <- reactive({
  
   dadoi <- dados_absorventes()
   mapa_dado <- sp::merge(municipiopoly, dadoi, by.x = 'Municipio', by.y = 'municipio')      
                      
   
    
   fill_color <- function(x){
                   bins <- unique(as.vector(round(quantile(x, probs = c(0,0.30,0.50,0.7,0.85,0.95,0.98,1),na.rm = T))))
                   pal <- colorBin("YlOrRd", domain = x, bins = bins, na.color = NA)
                   colorData <- pal(x)
                   list(pal, colorData)
                          }
   cores <- fill_color(mapa_dado$qtde_estudantes)
   
   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'IDHM: ', mapa_dado$idhm, 'Qtde. beneficiadas ',mapa_dado$qtde_estudantes) %>% lapply(htmltools::HTML)
         }

  mapa <- leaflet() %>%
        addTiles(urlTemplate ='https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}') %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor = fill_color(mapa_dado$qtde_estudantes)[[2]], 
        stroke = T, smoothFactor = 0.5, fillOpacity = 0.8, weight = 1.5,
    highlight = highlightOptions(
    weight = 5,
    color = "#666",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labells(mapa_dado),
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "12px",
    maxWidth = '200px',
    direction = "auto")) %>%
    addLegend(pal = fill_color(mapa_dado$qtde_estudantes)[[1]], values = mapa_dado$qtde_estudantes, 
  position = "bottomright", layerId="colorLegend2",className = 'info legenda', title = 'Qtde. beneficiadas') 
   
    
                  
  }) #reactive 
  
  #-------------------------------------
  #tabela   
  mod_summary_card_server('absorvente_tabela', 
                   card_large(heading = tagList(h1('Participações das regiões')),
                      tableOutput('absorventes_tabela') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$absorventes_tabela <- renderText({
                        dadoi <- dados_absorventes()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        dadoi <- aggregate(qtde_estudantes ~ mesorregiao, data = dadoi, FUN = sum)
                        dadoi$perc_total  <- round(dadoi$qtde_estudantes*100/sum(dadoi$qtde_estudantes),2)
                        adoi <- arrange(dadoi, desc(qtde_estudantes))
                        names(dadoi) <- c('Região', 'Beneficiadas', '% Total')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "380px")
                           
                           })
                           
                           
  #tabela_all   
  mod_summary_card_server('absorvente_tabela_all', 
                   card_large(heading = tagList(h1('Tabela dos dados')),
                     tableOutput('absorventes_tabela_dt') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$absorventes_tabela_dt <-renderText({
                        dadoi <- dados_absorventes()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        dadoi <- dadoi[,c(2,6,3,5)]
                        names(dadoi) <- c('Município', 'Região', 'IDH-M', 'Qtde_Beneficiadas')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "500px")
                           
                           
                           })       