  #server para a aba bolsa (12-dez-2022, 10:35h)
  
  dados_bolsa <- eventReactive(input$head_atualizar, {
                              if(any(input$all_municipio == 'Todos')){dadoi <- programa_bolsa}else{
                                 dadoi <- subset(programa_bolsa, municipio %in% input$all_municipio)}
                              dadoi}, ignoreNULL = FALSE)

  
 
  #-------------------------------------                   
  #card progress bar
  mod_summary_card_server('bolsa_progress',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('Qtde. bolsas em relação ao total de Estudantes'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('bolsa_adesao'))))),
                      uiOutput('bolsa_progress')
                             )
                             ) #end mod_summary_card
  
  output$bolsa_progress <- renderUI({
                                   dadoi <- dados_bolsa()
                                   dadoi <- with(dadoi, round(sum(qtde_bolsas)*100/sum(qtde_estudantes_em)))
                                   func_progress_bar(dadoi, hidden = T)})
                             
  output$bolsa_adesao <- renderText({
                                 dadoi <- dados_bolsa()
                                 
                                 with(dadoi,paste0(sum(qtde_bolsas),'/',sum(qtde_estudantes_em)))  
                                   })
  
  
  #-------------------------------------
  #mapa
  mod_summary_card_server('bolsa_mapa', 
                   card_large(heading = tagList(div(class = 'd-flex align-itens-center',
                                                h1('Mapa de adesões'),
                              tags$div(class = "ms-auto lh-1 text-muted small", 
                      selectInput('bolsa_tipo_mapa', label = NULL, choices = c('Qtde. Bolsistas' = 1, 'Total alunos' = 2, '% Bolsistas' = 3), selected = 1,
                      multiple = F, width = '80%')))),
                      leafletOutput('bolsa_mapa_leaflet') %>% withSpinner(color="#0dc5c1"))
                             )
  
   output$bolsa_mapa_leaflet <- renderLeaflet({ bolsa_leaflet_data()
        })
        
        
  bolsa_leaflet_data <- reactive({
  
   dadoi <- dados_bolsa()
   dadoi$perc <- with(dadoi, (qtde_bolsas*100)/qtde_estudantes_em)
   mapa_dado <- sp::merge(municipiopoly, dadoi, by.x = 'Municipio', by.y = 'municipio')      
                      
   
   fill_color <- function(x){
                   bins <- unique(as.vector(round(quantile(x, probs = c(0,0.30,0.50,0.7,0.85,0.95,0.98,1),na.rm = T))))
                   pal <- colorBin("YlOrRd", domain = x, bins = bins, na.color = NA)
                   colorData <- pal(x)
                   list(pal, colorData,x)
                          }
    if(input$bolsa_tipo_mapa == 1){
       cores <- fill_color(mapa_dado$qtde_bolsas)
       }
    if(input$bolsa_tipo_mapa == 2){
       cores <- fill_color(mapa_dado$qtde_estudantes_em)
       }
    if(input$bolsa_tipo_mapa == 3){
       cores <- fill_color(mapa_dado$perc)
       }


   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s<br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'IDHM: ', mapa_dado$idhm, 'Qtde.Alunos ',mapa_dado$qtde_estudantes_em,
 'Qtde. bolsistas: ',mapa_dado$qtde_bolsas, '% Bosistas: ', paste(round(mapa_dado$perc,1),'%')) %>% lapply(htmltools::HTML)
         }

  mapa <- leaflet() %>%
        addTiles(urlTemplate ='https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}') %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor = cores[[2]], 
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
    addLegend(pal = cores[[1]], values = cores[[3]], 
  position = "bottomright", layerId="colorLegend2",className = 'info legenda')
   
    
                  
  }) #reactive 
  
  #-------------------------------------
  #tabela   
  mod_summary_card_server('bolsa_tabela', 
                   card_large(heading = tagList(h1('Participações das regiões')),
                      tableOutput('bolsa_tabela') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$bolsa_tabela <- renderText({
                        dadoi <- dados_bolsa()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        #regs <- with(dadoi, as.data.frame(table(mesorregiao)))
                        dadoi <- aggregate(cbind(qtde_bolsas, qtde_estudantes_em) ~ mesorregiao, data = dadoi, FUN = sum)
                        dadoi$perc_total  <- round(dadoi$qtde_bolsas*100/sum(dadoi$qtde_estudantes_em),2)
                        dadoi$perc_reg <- with(dadoi, round(qtde_bolsas*100/qtde_estudantes_em,2))
                        dadoi <- arrange(dadoi, desc(qtde_bolsas))
                        names(dadoi) <- c('Região', 'Qtde.bolsistas', 'Qtde.Estudantes','% Total', '% da região')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "380px")
                           
                           })
                           
                           
  #tabela_all   
  mod_summary_card_server('bolsa_tabela_all', 
                   card_large(heading = tagList(h1('Tabela dos dados')),
                     tableOutput('bolsa_tabela_dt') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$bolsa_tabela_dt <-renderText({
                        dadoi <- dados_bolsa()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        dadoi <- dadoi[,c(2,7,3,5,6)]
                        names(dadoi) <- c('Município', 'Região', 'IDH-M', 'Qtde. Estudantes EM','Qtde. Bolsas')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "500px")
                           
                           
                           })       