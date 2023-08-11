  #server para a aba moradia (15-dez-2022, 15:04h)
  
  dados_moradia <- eventReactive(input$head_atualizar, {
                              if(any(input$all_municipio == 'Todos')){dadoi <- moradia}else{
                                 dadoi <- subset(moradia, municipio %in% input$all_municipio)}
                              dadoi}, ignoreNULL = FALSE)

  
 
  #-------------------------------------                   
  #card progress bar
  mod_summary_card_server('moradia_progress',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('Documentação municipal enviada no 1º semestre'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('moradia_adesao'))))),
                      uiOutput('moradia_progress')
                             )
                             ) #end mod_summary_card
  
  output$moradia_progress <- renderUI({
                                   dadoi <- dados_moradia()
                                 dadoi$contemplado <- with(dadoi, ifelse(documentacao == 'sim',T,F))
                                   func_progress_bar(round(sum(dadoi$contemplado)*100/nrow(dadoi)), hidden = T)})
                             
  output$moradia_adesao <- renderText({
                                 dadoi <- dados_moradia()
                                 dadoi$contemplado <- with(dadoi, ifelse(documentacao == 'sim',T,F))
                                 
                                 
                                 paste0(sum(dadoi$contemplado),'/',nrow(dadoi))  
                                   })
  
  
  #-------------------------------------
  #mapa
  mod_summary_card_server('moradia_mapa', 
                   card_large(heading = tagList(h1('Mapa situacional')),
                      leafletOutput('moradia_mapa_leaflet') %>% withSpinner(color="#0dc5c1"))
                             )
                             
                             
  output$moradia_mapa_leaflet <- renderLeaflet({ moradia_leaflet_data()
        })
        
        
  moradia_leaflet_data <- reactive({
  
   dadoi <- dados_moradia()
   #dadoi$contemplado <- with(dadoi, ifelse(status_implantacao == 'Sim','sim','não'))
   dadoi$documentacao <- factor(dadoi$documentacao, levels = c('sim','não'))
   mapa_dado <- sp::merge(municipiopoly, dadoi, by.x = 'Municipio', by.y = 'municipio')      
                      
   
   factpal  <-  colorFactor( c('#1B67F5','#ED2F0D'),  levels = levels(mapa_dado$documentacao), na.color = NA)#,
                  #  na.color = "transparent")

   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s<br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'IDHM: ', mapa_dado$idhm, 'Enviou documentação? ' ,  mapa_dado$documentacao,
 'Data da instalação: ',format(mapa_dado$dt_recebimento, '%d-%m-%Y'),
 'Situação: ',mapa_dado$status_obras) %>% lapply(htmltools::HTML)
         }

  mapa <- leaflet() %>%
        addTiles(urlTemplate ='https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}') %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor = ~factpal(documentacao), 
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
    addLegend(pal = factpal, values = mapa_dado$documentacao, 
  position = "bottomright",className = 'info legenda', title = 'Enviou\nDocumentação?')
   
   
    
                  
  }) #reactive 
  
  #-------------------------------------
  #tabela   
  mod_summary_card_server('moradia_tabela', 
                   card_large(heading = tagList(h1('Participações das regiões')),
                      tableOutput('moradia_tabela') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$moradia_tabela <- renderText({
                        dadoi <- dados_moradia()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        regs <- as.data.frame(table(dadoi$mesorregiao))
                        dadoi$cont <- with(dadoi, ifelse(documentacao == 'sim',T,F))
                        dadoi <- aggregate(cont ~ mesorregiao, data = dadoi, FUN = sum)
                        dadoi$perc_total  <- round(dadoi$cont*100/sum(dadoi$cont),2)
                        dadoi$perc_reg <- round(dadoi$cont*100/regs[,2],2)
                        dadoi <- arrange(dadoi, desc(cont))
                        names(dadoi) <- c('Região', 'Qtde. envios', '% Total', '% da região')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "380px")
                           
                           })
                           
                           
  #tabela_all   
  mod_summary_card_server('moradia_tabela_all', 
                   card_large(heading = tagList(h1('Tabela dos dados')),
                     tableOutput('moradia_tabela_dt') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$moradia_tabela_dt <-renderText({
                        dadoi <- dados_moradia()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        dadoi <- dadoi[,c(2,8,3,5,7,6)]
                        names(dadoi) <- c('Município', 'Região', 'IDH-M', 'Envio documentação','Andamento das obras','Data instalação')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "500px")
                           
                           
                           })   
 
 #-------------------------------------
 #gráfico
 mod_summary_card_server('moradia_graf_status', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h2(class = 'card-title', 'Situação de implementação')),
                            div(class = 'body',
                      apexchartOutput('moradia_chart', height = '450px')))
                             ) #end taglist
                             )
                             
                             
  output$moradia_chart <- renderApex({
                           dadoi <- dados_moradia()
                           #dadoi$status_implantacao <- with(dadoi, ifelse(status_implantacao == 'Sim','Posto instalado',status_implantacao))
                           dadoi <- with(dadoi, as.data.frame(table(status_obras), stringAsFactors = F))
                           dadoi <- dplyr::arrange(dadoi, desc(Freq))
                           
                       list(series = list(list(name = 'Situação',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%',
                                                       stacked = TRUE),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = c(dadoi$status_obras)
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
                          