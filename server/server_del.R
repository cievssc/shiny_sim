  #server para a aba del (05-jan-2023, 15:39h)
  
  dados_del <- eventReactive(input$head_atualizar, {
                              if(any(input$all_municipio == 'Todos')){dadoi <- del}else{
                                 dadoi <- subset(del, municipio %in% input$all_municipio)}
                              
                              dadoi}, ignoreNULL = FALSE)

  dados_del_resumo <- reactive({
                      dadoi <- dados_del()
                      dadoi$inicializacao <- factor(dadoi$inicializacao, levels = c('não','sim'))
                      dadoi$sensibilizacao <- factor(dadoi$sensibilizacao, levels = c('não','sim'))
                      dadoi$documental <- ifelse(dadoi$documental == 'Decreto aprovado e nomeação dos conselheiros',
                                                      'sim','não') %>% factor(., levels = c('não','sim'))
                      dadoi$situacional <- ifelse(dadoi$situacional == 'sim',
                                                      'sim','não') %>% factor(., levels = c('não','sim'))
                      dadoi$inst_governanca <- ifelse(dadoi$inst_governanca == 'Formação do conselho e oficialização das Ct`s',
                                                      'sim','não') %>% factor(., levels = c('não','sim'))
                      dadoi$estruturacao <- ifelse(dadoi$estruturacao == 'Possui projetos estruturados',
                                                      'sim','não') %>% factor(., levels = c('não','sim'))
                                                      dadoi
                      })
 
  #-------------------------------------                   
  #card progress bar
  #etapa inicializacao
  mod_summary_card_server('del_progress_init',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('i. Inicialização'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('del_inicializacao'))))),
                      uiOutput('del_barra_inicializacao')
                             )
                             ) #end mod_summary_card
  
  output$del_barra_inicializacao <- renderUI({
                                   dadoi <- dados_del_resumo()
                                  dadoi$contemplado <- dadoi$inicializacao
                                   func_progress_bar(round(table(dadoi$contemplado)[2]*100/nrow(dadoi)), hidden = T)})
                             
  output$del_inicializacao <- renderText({
                                 dadoi <- dados_del_resumo()
                                 dadoi$contemplado <- dadoi$inicializacao
                                 dadoi <- table(dadoi$contemplado)
                                 
                                 paste0(dadoi[2],'/',dadoi[2])  
                                   })
  
  #etapa sensibilizacao
  mod_summary_card_server('del_progress_sens',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('ii. Sensibilização'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('del_sensibilizacao'))))),
                      uiOutput('del_barra_sensibilizacao')
                             )
                             ) #end mod_summary_card
  
  output$del_barra_sensibilizacao <- renderUI({
                                   dadoi <- dados_del_resumo()
                                 #dadoi$sensibilizacao[is.na(dadoi$sensibilizacao)] <- 'não' 
                                 dadoi$contemplado <- dadoi$sensibilizacao
                                   func_progress_bar(round(table(dadoi$contemplado)[2]*100/nrow(dadoi)), hidden = T)})
                             
  output$del_sensibilizacao <- renderText({
                                 dadoi <- dados_del_resumo()
                                 #dadoi$sensibilizacao[is.na(dadoi$sensibilizacao)] <- 'não'
                                 dadoi$contemplado <- dadoi$sensibilizacao
                                 dadoi <- table(dadoi$contemplado)
                                 
                                 paste0(dadoi[2],'/',sum(dadoi, na.rm = T))  
                                   })
  
  
  #etapa documental
  mod_summary_card_server('del_progress_doc',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('iii. Análise documental'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('del_documental'))))),
                      uiOutput('del_barra_documental')
                             )
                             ) #end mod_summary_card
  
  output$del_barra_documental <- renderUI({
                                   dadoi <- dados_del_resumo()
                                 #dadoi$documental[is.na(dadoi$documental)] <- 'não' 
                                 dadoi$contemplado <- dadoi$documental
                                   func_progress_bar(round(table(dadoi$contemplado)[2]*100/nrow(dadoi)), hidden = T)})
                             
  output$del_documental <- renderText({
                                 dadoi <- dados_del_resumo()
                                 dadoi$contemplado <- dadoi$documental
                                 dadoi <- table(dadoi$contemplado)
                                 
                                 paste0(dadoi[2],'/',sum(dadoi, na.rm = T))  
                                   })
                                   
  
  #etapa situacional
  mod_summary_card_server('del_progress_situac',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('iv. Análise situacional'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('del_situacional'))))),
                      uiOutput('del_barra_situacional')
                             )
                             ) #end mod_summary_card
  
  output$del_barra_situacional <- renderUI({
                                   dadoi <- dados_del_resumo()
                                 dadoi$contemplado <- dadoi$situacional 
                                   func_progress_bar(round(table(dadoi$contemplado)[2]*100/nrow(dadoi)), hidden = T)})
                             
  output$del_situacional <- renderText({
                                 dadoi <- dados_del_resumo()
                                 dadoi$contemplado <- dadoi$situacional 
                                 dadoi <- table(dadoi$contemplado)
                                 
                                 paste0(dadoi[2],'/',sum(dadoi, na.rm = T))  
                                   })
                                   
                                   
  #etapa inst_governanca
  mod_summary_card_server('del_progress_gov',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h2("v. Institucionalização e Governança"),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('del_inst_governanca'))))),
                      uiOutput('del_barra_inst_governanca')
                             )
                             ) #end mod_summary_card
  
  output$del_barra_inst_governanca <- renderUI({
                                   dadoi <- dados_del_resumo()
                                  dadoi$contemplado <- dadoi$inst_governanca
                                   func_progress_bar(round(table(dadoi$contemplado)[2]*100/nrow(dadoi)), hidden = T)})
                             
  output$del_inst_governanca <- renderText({
                                 dadoi <- dados_del_resumo()
                                 dadoi$contemplado <- dadoi$inst_governanca
                                 dadoi <- table(dadoi$contemplado)
                                 
                                 paste0(dadoi[2],'/',sum(dadoi, na.rm = T))  
                                   })                                                      
 
  #etapa estruturação de projetos
  mod_summary_card_server('del_progress_estrut',
                        div(class = 'card-body',
                                 div(class = 'card-title',
                                  div(class = 'd-flex align-itens-center',
                                      h1('vi. Estruturação de projetos'),
                                       div(class = "ms-auto text-mute", 
                      h2(textOutput('del_estruturacao'))))),
                      uiOutput('del_barra_estruturacao')
                             )
                             ) #end mod_summary_card
  
  output$del_barra_estruturacao <- renderUI({
                                   dadoi <- dados_del_resumo()
                                  dadoi$contemplado <- dadoi$estruturacao
                                   func_progress_bar(round(table(dadoi$contemplado)[2]*100/nrow(dadoi)), hidden = T)})
                             
  output$del_estruturacao <- renderText({
                                 dadoi <- dados_del_resumo()
                                 dadoi$contemplado <- dadoi$estruturacao
                                 dadoi <- table(dadoi$contemplado)
                                 
                                 paste0(dadoi[2],'/',sum(dadoi, na.rm = T))  
                                   })
  
  
  #-------------------------------------
 #gráfico
 #gráfico de 
 mod_summary_card_server('del_graf', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h2(class = 'card-title', 'Etapa atual')),
                            div(class = 'body',
                      apexchartOutput('del_chart', height = '450px')))
                             ) #end taglist
                             )
                             
                             
  output$del_chart <- renderApex({
                           dadoi <- dados_del_resumo()
                           #dadoi$status_implantacao <- with(dadoi, ifelse(status_implantacao == 'Sim','Posto instalado',status_implantacao))
                           dadoii <- with(dadoi, 
                                     c('estruturacao' = table(estruturacao)[2],
                                       'inst_govenanca' = table(inst_governanca)[2] - table(estruturacao)[2],
                                       'situacional' = table(situacional)[2] - table(inst_governanca)[2],
                                        'documental' = table(documental)[2],
                                        'sensibilizacao' = table(sensibilizacao)[2] - table(situacional)[2],
                                        'inicializacao'= table(inicializacao)[2] - table(sensibilizacao)[2]
                                                           )
                           )
                           
                       list(series = list(list(name = 'Situação',data = dadoii %>% unname)
                                              ),
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%',
                                                       stacked = TRUE),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = c('Estruturação de projetos','Institucionalização e Governança','Análise situacional',
                                                      'Análise documental','Sensibilização','Inicialização')
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
 
 #gráfico específico (13-jan-2023, 12:53h)                         
 
 mod_summary_card_server('del_graf2', 
                   card_large(heading = tagList(div(class = 'd-flex align-itens-center',
                                                h1('Especificação das etapas'),
                              tags$div(class = "ms-auto lh-1 text-muted small", 
                      selectInput('del_tipo_graf', label = NULL, choices = c('Inicialização' = 5, 'Sensibilização' = 6, 'Análise documental' = 7,
                                                     'Análise situacional' = 8, 'Inst. e Governança' = 9, 'Estruturação de projetos' = 10), selected = 5,
                      multiple = F)))), #, width = '80%'
                      apexchartOutput('del_chartii', height = '450px') %>% withSpinner(color="#0dc5c1"))
                             )

 output$del_chartii <- renderApex({
                           dadoi <- dados_del()
                           
                           if(input$del_tipo_graf == 5){
                           dadoi <- dadoi[,5]
                           dadoi <- ifelse(dadoi == 'sim','100% realizado', 'Não realizado') %>%
                                    factor(., levels = c('100% realizado', 'Não realizado'))
                           dadoi <- as.data.frame(table(dadoi), stringsAsFactors = F)
                           }
                           
                           if(input$del_tipo_graf == 6){
                           dadoi <- dadoi[,6]
                           dadoi <- ifelse(dadoi == 'sim','100% realizado', 'Não realizado') %>%
                                    factor(., levels = c('100% realizado', 'Não realizado'))
                           dadoi <- as.data.frame(table(dadoi), stringsAsFactors = F)
                           }
                           
                           if(input$del_tipo_graf == 7){
                           dadoi <- dadoi[,7]
                           dadoi <- ifelse(dadoi == 'sim','100% realizado',
                                     ifelse(dadoi == 'não','Não realizado',dadoi)) %>%
                                    as.factor(.)
                           dadoi <- as.data.frame(table(dadoi), stringsAsFactors = F)
                           }
                           
                           if(input$del_tipo_graf == 8){
                           dadoi <- dadoi[,8]
                           dadoi <- ifelse(dadoi == 'sim','100% realizado',
                                     ifelse(dadoi == 'não','Não realizado',dadoi)) %>%
                                    as.factor(.)
                           dadoi <- as.data.frame(table(dadoi), stringsAsFactors = F)
                           }
                           
                           if(input$del_tipo_graf == 9){
                           dadoi <- dadoi[,9]
                           dadoi <- ifelse(dadoi == 'sim','Formação do conselho e oficialização das Ct`s',
                                     ifelse(dadoi == 'não','Não realizado',dadoi)) %>%
                                    as.factor(.)
                           dadoi <- as.data.frame(table(dadoi), stringsAsFactors = F)
                           }
                           
                           if(input$del_tipo_graf == 10){
                           dadoi <- dadoi[,10]
                           dadoi <- ifelse(dadoi == 'sim','Possui projetos estruturados',
                                     ifelse(dadoi == 'não','Não realizado',dadoi)) %>%
                                    as.factor(.)
                           dadoi <- as.data.frame(table(dadoi), stringsAsFactors = F)
                           }
                           #dadoi$status_implantacao <- with(dadoi, ifelse(status_implantacao == 'Sim','Posto instalado',status_implantacao))
                           
                           
                       list(series = list(list(name = 'Situação',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%',
                                                       stacked = TRUE),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = dadoi[,1]
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

  
#-------------------------------------
  #mapa
  mod_summary_card_server('del_mapa', 
                   card_large(heading = tagList(div(class = 'd-flex align-itens-center',
                                                h1('Mapa situacional')),
                              
                      leafletOutput('del_mapa_leaflet') %>% withSpinner(color="#0dc5c1")
                             )
                             ))
  
   output$del_mapa_leaflet <- renderLeaflet({ del_leaflet_data()
        })
        
        
  del_leaflet_data <- reactive({
  
   dadoi <- dados_del_resumo()
   dadoi$estruturacao <- with(dadoi, ifelse(estruturacao == 'sim',T,F))
   dadoi$inst_governanca <- with(dadoi, ifelse(inst_governanca == 'sim',T,F) %>%
                                       ifelse(estruturacao == T,F,.))
   dadoi$situacional <- with(dadoi, ifelse(situacional == 'sim',T,F) %>%
                                       ifelse((inst_governanca | estruturacao),F,.))
   dadoi$sensibilizacao <- with(dadoi, ifelse(sensibilizacao == 'sim',T,F) %>%
                                       ifelse((inst_governanca | estruturacao | situacional),F,.))
   dadoi$inicializacao <- with(dadoi, ifelse(inicializacao == 'sim',T,F) %>%
                                       ifelse((inst_governanca | estruturacao | situacional |sensibilizacao),F,.))
   dadoi$documental <- with(dadoi, ifelse(documental == 'sim',T,F))
   
   dadoi$resumo <- with(dadoi, ifelse(estruturacao == T, 'Estruturação de projetos',
                               ifelse(inst_governanca == T, 'Institucionalização e Governança',
                               ifelse(situacional == T, 'Análise situacional',
                               ifelse(sensibilizacao == T, 'Sensibilização','Inicialização')
                               )))) %>% factor(. ,levels = c('Inicialização', 'Sensibilização',
                                                        'Análise situacional', 'Institucionalização e Governança',
                                                        'Estruturação de projetos'))
   
   mapa_dado <- sp::merge(municipiopoly, dadoi, by.x = 'Municipio', by.y = 'municipio')      
                      
   factpal  <-  colorFactor(palette = "YlOrRd",  levels = levels(mapa_dado$resumo), na.color = NA)#,
                  #  na.color = "transparent")

   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s<br/>%s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'IDHM: ', mapa_dado$idhm, 'Etapa atual: ',mapa_dado$resumo) %>% lapply(htmltools::HTML)
         }

  mapa <- leaflet() %>%
        addTiles(urlTemplate ='https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}') %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor = ~factpal(resumo), 
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
    addLegend(pal = factpal, values = mapa_dado$resumo, 
  position = "bottomright", className = 'info legenda', title = 'Etapa atual')
   mapa
    
                  
  }) #reactive 
    
  
 #-------------------------------------
  #tabela   
  mod_summary_card_server('del_tabela_all', 
                   card_large(heading = tagList(h1('Tabela dos dados')),
                     tableOutput('del_tabela_dt') %>% withSpinner(color="#0dc5c1"))
                             )
  
  
  
  output$del_tabela_dt <-renderText({
                        dadoi <- dados_del()
                        dadoi <- dplyr::left_join(dadoi,municipios_br[,c(3,5)], by = 'codigo')
                        dadoi <- dadoi[,c(2,11,3,5:10)]
                        names(dadoi) <- c('Município', 'Região', 'IDH-M', 'Inicialização', 'Sensibilização', 'Análise documental',
                                        'Análise situacional', 'Inst. e Governança', 'Estruturação de projetos')
                           kbl(dadoi) %>%
                           kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           scroll_box(width = "100%", height = "500px") 
                           })   