  #server para a aba cras (12-dez-2022, 10:35h)
  
  dados_cras <- eventReactive(input$head_atualizar, {
                              if(any(input$all_municipio == 'Todos')){dadoi <- cras}else{
                                 dadoi <- subset(cras, municipio %in% input$all_municipio)}
                              dadoi}, ignoreNULL = FALSE)

  
 
  #-------------------------------------                   
  #card progress bar
  mod_summary_card_server('cras_progress',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('Quantidade de adesões'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('cras_adesao'))))),
                      uiOutput('cras_progress')
                             )
                             ) #end mod_summary_card
  
  output$cras_progress <- renderUI({
                                   dadoi <- dados_cras()
                                 dadoi$contemplado <- factor(dadoi$contemplado, levels = c('não','sim'))
                                   func_progress_bar(round(table(dadoi$contemplado)[2]*100/nrow(dadoi)), hidden = T)})
                             
  output$cras_adesao <- renderText({
                                 dadoi <- dados_cras()
                                 dadoi$contemplado <- factor(dadoi$contemplado, levels = c('não','sim'))
                                 dadoi <- table(dadoi$contemplado)
                                 
                                 paste0(dadoi[2],'/',sum(dadoi, na.rm = T))  
                                   })
  
  
  #-------------------------------------
  #mapa
  mod_summary_card_server('cras_mapa', 
                   card_large(heading = tagList(h1('Mapa de adesões')),
                      leafletOutput('cras_mapa_leaflet') %>% withSpinner(color="#0dc5c1"))
                             )
                             
                             
  output$cras_mapa_leaflet <- renderLeaflet({ cras_leaflet_data()
        })
        
        
  cras_leaflet_data <- reactive({
  
   dadoi <- dados_cras()
   
   dadoi$contemplado <- factor(dadoi$contemplado, levels = c('sim','não'))
   mapa_dado <- sp::merge(municipiopoly, dadoi, by.x = 'Municipio', by.y = 'municipio')      
   
   factpal  <-  colorFactor( palette = c('#1B67F5','#ED2F0D'),  levels = levels(mapa_dado$contemplado), na.color = NA)#,
                  #  na.color = "transparent")

   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'IDHM: ', mapa_dado$idhm, 'Contemplado? ',mapa_dado$contemplado,
 'Status: ',mapa_dado$status) %>% lapply(htmltools::HTML)
         }

 leaflet() %>%
        addTiles(urlTemplate ='https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}',
         options = providerTileOptions(minZoom = 7)) %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% #clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor = ~factpal(contemplado), 
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
    addLegend(pal = factpal,className = 'info legenda',  values = mapa_dado$contemplado, title = 'Contemplação')   # className = "legenBox",
   
    
                  
  }) #reactive 
  
  #-------------------------------------
  #tabela   
  mod_summary_card_server('cras_tabela', 
                   card_large(heading = tagList(h1('Participações das regiões')),
                      tableOutput('cras_tabela') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$cras_tabela <- renderText({
                        dadoi <- dados_cras()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        regs <- with(dadoi, as.data.frame(table(mesorregiao)))
                        dadoi$cont <- ifelse(dadoi$contemplado == 'sim', T,F)
                        dadoi <- aggregate(cont ~ mesorregiao, data = dadoi, FUN = sum)
                        dadoi$perc_total  <- round(dadoi$cont*100/sum(dadoi$cont),2)
                        dadoi$perc_reg <- round(dadoi$cont*100/regs[,2],2)
                        dadoi <- arrange(dadoi, desc(cont))
                        names(dadoi) <- c('Região', 'Adesões', '% Total', '% da região')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "380px")
                           
                           })
                           
                           
  #tabela_all   
  mod_summary_card_server('cras_tabela_all', 
                   card_large(heading = tagList(h1('Tabela dos dados')),
                     tableOutput('cras_tabela_dt') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$cras_tabela_dt <-renderText({
                        dadoi <- dados_cras()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        dadoi <- dadoi[,c(2,7,3,5,6)]
                        names(dadoi) <- c('Município', 'Região', 'IDH-M', 'Contemplado?','Status')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "500px")
                           
                           
                           })       