  #server para a aba sine (15-dez-2022, 09?57h)
  
  dados_sine <- eventReactive(input$head_atualizar, {
                              if(any(input$all_municipio == 'Todos')){dadoi <- caravana}else{
                                 dadoi <- subset(caravana, municipio %in% input$all_municipio)}
                              dadoi}, ignoreNULL = FALSE)

  
 
  #-------------------------------------                   
  #card progress bar
  mod_summary_card_server('sine_progress',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('Quantidade de postos instalados'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('sine_adesao'))))),
                      uiOutput('sine_progress')
                             )
                             ) #end mod_summary_card
  
  output$sine_progress <- renderUI({
                                   dadoi <- dados_sine()
                                 dadoi$contemplado <- with(dadoi, ifelse(status_implantacao == 'Sim',T,F))
                                   func_progress_bar(round(sum(dadoi$contemplado)*100/nrow(dadoi)), hidden = T)})
                             
  output$sine_adesao <- renderText({
                                 dadoi <- dados_sine()
                                 dadoi$contemplado <- with(dadoi, ifelse(status_implantacao == 'Sim',T,F))
                                 
                                 
                                 paste0(sum(dadoi$contemplado),'/',nrow(dadoi))  
                                   })
  
  
  #-------------------------------------
  #mapa
  mod_summary_card_server('sine_mapa', 
                   card_large(heading = tagList(h1('Mapa de implementações')),
                      leafletOutput('sine_mapa_leaflet') %>% withSpinner(color="#0dc5c1"))
                             )
                             
                             
  output$sine_mapa_leaflet <- renderLeaflet({ sine_leaflet_data()
        })
        
        
  sine_leaflet_data <- reactive({
  
   dadoi <- dados_sine()
   dadoi$contemplado <- with(dadoi, ifelse(status_implantacao == 'Sim','sim','não'))
   dadoi$contemplado <- factor(dadoi$contemplado, levels = c('sim','não'))
   dadoi$status_implantacao <- with(dadoi, ifelse(status_implantacao == 'Sim','Posto instalado',status_implantacao))
   mapa_dado <- sp::merge(municipiopoly, dadoi, by.x = 'Municipio', by.y = 'municipio')      
                      
   
   factpal  <-  colorFactor( c('#1B67F5','#ED2F0D'),  levels = levels(mapa_dado$contemplado), na.color = NA)#,
                  #  na.color = "transparent")

   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s<br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'IDHM: ', mapa_dado$idhm, 'Situação: ',mapa_dado$status_implantacao,
 'Data da instalação: ',format(mapa_dado$data_implantacao, '%d-%m-%Y'),
 'Teve caravana do emprego?', mapa_dado$teve_caravana) %>% lapply(htmltools::HTML)
         }

  mapa <- leaflet() %>%
        addTiles(urlTemplate ='https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}') %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% clearControls() %>% clearShapes() %>%
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
    addLegend(pal = factpal, values = mapa_dado$contemplado, 
  position = "bottomright", layerId="colorLegend2",className = 'info legenda', title = 'Posto instalado?') 
   
    
                  
  }) #reactive 
  
  #-------------------------------------
  #tabela   
  mod_summary_card_server('sine_tabela', 
                   card_large(heading = tagList(h1('Participações das regiões')),
                      tableOutput('sine_tabela') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$sine_tabela <- renderText({
                        dadoi <- dados_sine()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        regs <- as.data.frame(table(dadoi$mesorregiao))
                        dadoi$cont <- with(dadoi, ifelse(status_implantacao == 'Sim',T,F))
                        dadoi <- aggregate(cont ~ mesorregiao, data = dadoi, FUN = sum)
                        dadoi$perc_total  <- round(dadoi$cont*100/sum(dadoi$cont),2)
                        dadoi$perc_reg <- round(dadoi$cont*100/regs[,2],2)
                        dadoi <- arrange(dadoi, desc(cont))
                        names(dadoi) <- c('Região', 'Instalações', '% postos instalados', '% da região')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "380px")
                           
                           })
                           
                           
  #tabela_all   
  mod_summary_card_server('sine_tabela_all', 
                   card_large(heading = tagList(h1('Tabela dos dados')),
                     tableOutput('sine_tabela_dt') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$sine_tabela_dt <-renderText({
                        dadoi <- dados_sine()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        dadoi <- dadoi[,c(2,8,3,5,6,7)]
                        names(dadoi) <- c('Município', 'Região', 'IDH-M', 'Situação','Data instalação', 'Teve caravana do emprego?')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "500px")
                           
                           
                           })   
 
 #-------------------------------------
 #gráfico
 mod_summary_card_server('sine_graf_status', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h2(class = 'card-title', 'Situação de implementação')),
                            div(class = 'body',
                      apexchartOutput('sine_chart', height = '450px')))
                             ) #end taglist
                             )
                             
                             
  output$sine_chart <- renderApex({
                           dadoi <- dados_sine()
                           dadoi$status_implantacao <- with(dadoi, ifelse(status_implantacao == 'Sim','Posto instalado',status_implantacao))
                           dadoi <- with(dadoi, as.data.frame(table(status_implantacao), stringAsFactors = F))
                           dadoi <- dplyr::arrange(dadoi, desc(Freq))
                           
                       list(series = list(list(name = 'Situação',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%',
                                                       stacked = TRUE),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = c(dadoi$status_implantacao)
                                                      ),
                                            
                                              plotOptions = list(
                                                        bar = list(
                                                          horizontal = TRUE,
                                                          borderRadius = 4,
                                                          dataLabels = list(
                                                             total = list(
                                                                enabled = TRUE,
                                                                style = list(
                                                                    fontSize = '13px',
                                                                    fontWeight = 900)
                                                                )
                                                             )
                                                          )
                                                          ),
                                              legend = c(show = TRUE)
                                              )
                                     })  #end renderapex                          
                          