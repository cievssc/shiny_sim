  #server para a aba mortalidade infantil (27-nov-2022, 17:15h)
  
  #dados iniciais
  dados_inicio_inf <- DBI::dbGetQuery(conn(), "select * FROM inf_dados_empilhados WHERE active = true")
  
  output$inf_dropopcoes <- renderUI(
                               tagList(
                                  selectInput('inf_local_obito', label = h5('Local de ocorrência do óbito:'),
                                      choices = c('Todos','Hospital', 'Via pública', 'Domicílio', 'Outro serviço de saúde','Aldeia indígena'),
                                      selected = 'Todos',
                                      multiple = T),
                                      
                                  selectInput('inf_escola', label = h5('Escolaridade'),
                                      choices = c('Todos' = 0,'Sem escolaridade' = 1,'Fundamental I'= 2,'Fundamental II' =3, 'Ensino médio' =4 ,'Superior Incompleto' = 5, 'Superior Completo' = 6, 'Ignorado' = 7), 
                                       selected = 0 , multiple = T),     
                                  selectInput('inf_corpele', label = h5('Cor da pele'),
                                      choices =  c('Todos' = 0,'Branca' = 1, 'Preta' = 2, 'Amarela' = 3, 'Parda' = 4, 'Indígena' = 5, 'Ignorado' = 6),
                                      selected = 0,
                                      multiple = T),
                                      selectInput('inf_conjugal', label = h5('Situação conjugal'),
                                      choices =  c('Todos' = 0,'Casado' = 1, 'Solteiro' = 2, 'Viúvo' = 3, 'Separado/divorciado' = 4, 'União estável' = 5, 'Ignorado' = 6),
                                      selected = 0,
                                      multiple = T)
                                       )      
                                     )

 
  #organizando os dados
  dados_all_inf <- eventReactive(input$head_atualizar,{
                 req(!is.null(input$head_municipio))
                 dadoi <- dados_inicio_inf
                 dadoi$evitavel <- with(dadoi, ifelse(cid_evitavel != '', 'Evitável', 'Não evitável')) %>%
                                   factor(., levels = c('Evitável', 'Não evitável'))
                 if(!is.null(input$inf_daterange)){
                 dadoi <- subset(dadoi, 
                                 (dat_obito >= as.Date(input$long_daterange[1]) & dat_obito <= as.Date(input$long_daterange[2])))
                }
                     if(!any(input$head_municipio == 'Todos')){
                        dadoi <- dadoi[(dadoi$municipio_obito %in% input$head_municipio) | (dadoi$municipio_residencia %in% input$head_municipio),]
                        }
                 
                 if(nrow(dadoi) == 0){
                  showModal(modalDialog(
                  title = NULL,
                  tagList(
                   p("Não há registros no período.")),
                   easyClose = TRUE,
                   footer = NULL
                   ))
                   NULL
                   }else{
                   dadoi}
                   }, ignoreNULL = F)                            
  
  
   dados_analise_inf <- reactiveVal(0)
   
  
   observeEvent(c(input$head_atualizar),{ #input$inf_atualizar,
                     req(!is.null(dados_all_inf()))
                     dadoi <- dados_all_inf()
                     if(input$inf_dropdown >0){
                     if(!any(input$inf_local_obito == 'Todos')){
                        dadoi <- dadoi[(dadoi$local_obito %in% input$inf_local_obito),]
                        }
                     if(!any(input$inf_escola == 0)){
                        dadoi <- dadoi[(dadoi$escolaridade %in% input$inf_escola),]
                        }
                      if(!any(input$inf_corpele == 0)){
                        dadoi <- dadoi[(dadoi$cor_pele %in% input$inf_corpele),]
                        }
                      if(!any(input$inf_conjugal == 0)){
                        dadoi <- dadoi[(dadoi$sit_conjuntal %in% input$inf_conjugal),]
                        }                  
                                      }
                     if(nrow(dadoi) == 0){
                  showModal(modalDialog(
                  title = NULL,
                  tagList(
                   p("Não há registros no período.")),
                   easyClose = TRUE,
                   footer = NULL
                   ))
                   NULL
                   }else{
                   dados_analise_inf(dadoi)
                   }     
                          })
  
  #-------------------------------------
  #séries temporais dos dados                      
  serie_obito_inf <- reactive({
                 dadoi <- dados_analise_inf()
                 dadoi$mes_ano <- func_semana(dadoi$dat_obito)
                  serie <- with(dadoi,as.data.frame(table(mes_ano, evitavel))  )
                  serie <- tidyr::spread(serie, key = evitavel, value = Freq)
                  serie$Total <- apply(serie[,2:3],1,sum) 
                  serie
                        })   
  
  #contingência de obitos de munici´pios
  inf_municipio_resid <- reactive({
                        dadoi <- dados_analise_inf()
                        dadoi <- with(dadoi, as.data.frame(table(municipio_responsavel, evitavel)))       
                        dadoi <- tidyr::spread(dadoi, key = evitavel, value = Freq)
                        dadoi$Total <- apply(dadoi[,2:3],1,sum)
                        dadoi
                               })
  
  inf_municipio_ocor <- reactive({
                        dadoi <- dados_analise_inf()
                        dadoi <- with(dadoi, as.data.frame(table(municipio, evitavel)))       
                        dadoi <- tidyr::spread(dadoi, key = evitavel, value = Freq)
                        dadoi$Total <- apply(dadoi[,2:3],1,sum)
                        dadoi
                               })
  
  
  #-------------------------------------                   
  #card serie óbitos
  mod_summary_card_server('inf_serie_temporal', 
                            card_large(heading =  'Série Semana Epidemiológica',
                              apexchartOutput('inf_serie_chart', height = '500px') %>% withSpinner(color="#0dc5c1")
                             )
                             )
  

    output$inf_serie_chart <- renderApex({
                           dadoi <- serie_obito_inf()
                           dadoi[dadoi == 0] <-NA 
                           
                       list(series = list(list(name = 'Não evitável',data = dadoi[,3]),
                                          list(name = 'Evitável',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%',
                                                       stacked = TRUE),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = c(dadoi$mes_ano)
                                                      ),
                                            
                                              plotOptions = list(
                                                        bar = list(
                                                          horizontal = FALSE,
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
                        
  
  #card total õbitos
  mod_summary_card_server('inf_total_obito', 
                            card_large(heading =  'Total de óbitos no período',
                              uiOutput('inf_total')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$inf_total <- renderUI({
                    dadoi <- serie_obito_inf()
                    dadoi <- sum(dadoi$Total, na.rm = T)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                    )
                    })
  
  #card total evitável
  mod_summary_card_server('inf_perc_evitavel', 
                            card_large(heading =  '% de óbitos evitáveis',
                              uiOutput('inf_evitavel')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$inf_evitavel <- renderUI({
                    dadoi <- dados_analise_inf()
                    dadoi <- table(dadoi$evitavel)
                    dadoi <- round(dadoi[1]*100/sum(dadoi),2)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',paste0(dadoi,'%'))
                    )
                    })                  
  
  #card var semana
  mod_summary_card_server('inf_var_semana', 
                         summary_card_content(subheader = 'Variação em relação a SE anterior',
                                              heading = textOutput('inf_mes_falta'),
                           in_body =  uiOutput('inf_variacao') %>% withSpinner(color="#0dc5c1"))
                                )
  
   output$inf_mes_falta <- renderText({paste('Semana epidemiológica', serie_obito_inf()[nrow(serie_obito_inf()),1])})
 
   output$inf_variacao <- renderUI({
                          dadoi <- serie_obito_inf() 
                          dadoi <- dadoi[nrow(dadoi),4] - dadoi[nrow(dadoi)-1,4]
                          tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi))
                            })
                            
                            
  #---------------------------------
  #mapa
  mod_summary_card_server('inf_mapa', 
                   card_large(heading = tagList(h1('Mapa'),tags$div(class = "ms-auto lh-1 text-muted small", 
                      selectInput('inf_tipo_mapa', label = NULL, choices = c('Ocorrência' = 1, 'Residência' = 2, 'Georref' = 3), selected = 1,
                      multiple = F))),
                      leafletOutput('inf_mapa_leaflet') %>% withSpinner(color="#0dc5c1"))
                             )
                             
                             
  output$inf_mapa_leaflet <- renderLeaflet({ inf_leaflet_data()
        })
        
  inf_leaflet_data <- reactive({
  
   if(input$inf_tipo_mapa == 1){
   dadoi <- inf_municipio_ocor()
   
   mapa_dado <- sp::merge(municipiopoly, dadoi, by.x = 'Municipio', by.y = 'municipio')      
                      
   
   fill_color <- function(x){
                   bins <- unique(as.vector(quantile(x, probs = c(0,0.30,0.50,0.7,0.85,0.95,0.98,1),na.rm = T)))
                   pal <- colorBin("YlOrRd", domain = x, bins = bins)
                   colorData <- pal(x)
                   list(pal, colorData)
                          }


  
   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'Evitável: ', mapa_dado$Evitável, 'Não evitável: ',mapa_dado$`Não evitável`,
 'Total: ',mapa_dado$Total) %>% lapply(htmltools::HTML)
         }

  mapa <- leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor =  fill_color(mapa_dado$Total)[[2]], 
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
    addLegend(pal = fill_color(mapa_dado$Total)[[1]],className = 'info legenda',  values = fill_color(mapa_dado$Total)[[2]], opacity = 0.7, title = 'Qtde. de Registros',
  position = "bottomright", layerId="colorLegend2")
  
    }
    
    if(input$inf_tipo_mapa == 2){
    dadoi <- inf_municipio_resid()
   
   mapa_dado <- sp::merge(municipiopoly, dadoi, by.x = 'Municipio', by.y = 'municipio_responsavel')      
                      
   
   fill_color <- function(x){
                   bins <- unique(as.vector(quantile(x, probs = c(0,0.30,0.50,0.7,0.85,0.95,0.98,1),na.rm = T)))
                   pal <- colorBin("YlOrRd", domain = x, bins = bins)
                   colorData <- pal(x)
                   list(pal, colorData)
                          }

   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'Evitável: ', mapa_dado$Evitável, 'Não evitável: ',mapa_dado$`Não evitável`,
 'Total: ',mapa_dado$Total) %>% lapply(htmltools::HTML)
         }

   mapa <- leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor =  fill_color(mapa_dado$Total)[[2]], 
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
    addLegend(pal = fill_color(mapa_dado$Total)[[1]], values = fill_color(mapa_dado$Total)[[2]], opacity = 0.7, title = 'Qtde. de Registros',
  position = "bottomright", layerId="colorLegend2") 
    }
   if(input$inf_tipo_mapa == 3){

  dadoi <- dados_analise_inf()
  
  dadoi <- dadoi[dadoi$endereco_obito !='',]
  dadoi <- dplyr::left_join(dadoi, cid_evitavel, by = c('cid_evitavel' = 'CID10'))
  
  endereco <- with(dadoi, paste0(endereco,',',bairro,', ',inf_uf_obito))     
  
  endereco <- purrr::map_df(endereco, func_geo)     
  
  dadoi <- SpatialPointsDataFrame(as.matrix(endereco[,c(2,1)]), data = dadoi)#,
                                 #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
  #dadoi$evitavel <- factor(dadoi$evitavel, levels = c("Evitável", "Não evitável"))
  #dadoi <- dadoi[!is.na(dadoi$Latitude),] 
  
  labells <-
     sprintf(         
  "<strong>%s %s</strong><br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
 'Município: ',dadoi$municipio,  'Local óbito: ',dadoi$local_obito,
 'Causa evitável: ',dadoi$descricao) %>% lapply(htmltools::HTML)
   
   pal <- colorFactor(c("navy", "red"), domain = c("Evitável", "Não evitável"))        
    mapa <- leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lat = -27.5, lng = -51, zoom = 7) %>% clearShapes() %>%
        addCircleMarkers(data = dadoi, label = labells,  color = ~pal(evitavel), stroke = F, fillOpacity = .5,
                         radius = 8,
                         layerId = ~id) 


   }
   
   
   mapa                   
  }) #reactive                       
                          


  #------------------------------


  #sexo ----------------------------
 mod_summary_card_server('inf_sexo', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Registros')),
                            div(class = 'body',
                      apexchartOutput('inf_sexo_chart', height = '300px')))
                             ) #end taglist
                             )
 
 output$inf_sexo_chart <- renderApex({
                           dadoi <- dados_analise_inf()
                           dadoi$sexo <- factor(dadoi$sexo, levels = c(1:2))
                           dadoi <- as.data.frame(table(dadoi$sexo))
                           list(series = c(dadoi$Freq),
                            chart = list(type = 'donut',
                                         height = '100%'),
                            labels = c('Masculino', 'Feminino'),
                            legend = list(position = 'bottom'))
                             
                                     })  #end renderapex 
 
  #cor da pele ----------------------------
 mod_summary_card_server('inf_cor', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Cor da pele')),
                            div(class = 'body',
                      apexchartOutput('inf_cor_chart', height = '300px')))
                             ) #end taglist
                             )
 
 output$inf_cor_chart <- renderApex({
                           dadoi <- dados_analise_inf()
                          dadoi$cor_pele <- factor(dadoi$cor_pele, levels = 1:6)
                          dadoi <- with(dadoi, as.data.frame(table(cor_pele)))
                          
                           list(series = list(list(name = 'Quantidade',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar',
                                                           stacked = TRUE, 
                                                       #toolbar = c(show = FALSE),
                                                       height = '90%'),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '80%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories = c('Branca' , 'Preta' , 'Amarela' , 'Parda' , 'Indígena', 'Ignorado' )
                                                      ),
                                              
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = FALSE)
                                              )
                                     })  #end renderapex
 
 #cor da pele ----------------------------
 mod_summary_card_server('inf_escolaridade', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Escolaridade')),
                            div(class = 'body',
                      apexchartOutput('inf_escola_chart', height = '300px')))
                             ) #end taglist
                             )
 
 output$inf_escola_chart <- renderApex({
                           dadoi <- dados_analise_inf()
                          dadoi$escolaridade <- factor(dadoi$escolaridade, levels = 1:7)
                          dadoi <- with(dadoi, as.data.frame(table(escolaridade)))
                           list(series = list(list(name = 'Quantidade',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar',
                                                        stacked = TRUE,    
                                                       #toolbar = c(show = FALSE),
                                                       height = '90%'),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '80%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories = c('Sem escolaridade','Fundamental I','Fundamental II',
                                                                      'Ensino\nmédio' ,'Superior\nIncompleto' , 'Superior\nCompleto' , 'Ignorado')
                                                      ),
                                              
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = FALSE)
                                              )
                                     })  #end renderapex
 
 
 #idade resp ----------------------------
 mod_summary_card_server('inf_idaderesp', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Idade')),
                            div(class = 'body',
                      apexchartOutput('inf_idade_chart', height = '300px')))
                             ) #end taglist
                             )
 
 output$inf_idade_chart <- renderApex({
                           dadoi <- dados_analise_inf()
                          dadoi$idade_faixa <- with(dadoi, cut(idade, breaks = c(-Inf, 17,29,39,49,Inf),
                                                    labels = c('Até 17 anos','18-29','30-39','40-49','+50')))
                          dadoi <- with(dadoi, as.data.frame(table(idade_faixa)))
                           list(series = list(list(name = 'Quantidade',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar',
                                                        stacked = TRUE,    
                                                       #toolbar = c(show = FALSE),
                                                       height = '90%'),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '80%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories = c('Até\n17 anos','18-29','30-39','40-49','+50')
                                                      ),
                                              
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = FALSE)
                                              )
                                     })  #end renderapex

  #idade resp ----------------------------
 mod_summary_card_server('inf_qtde_filhos', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Qtde. de Filhos')),
                            div(class = 'body',
                      apexchartOutput('inf_qtdefilhos_chart', height = '300px')))
                             ) #end taglist
                             )
 
 output$inf_qtdefilhos_chart <- renderApex({
                           dadoi <- dados_analise_inf()
                          
                          dadoi <- with(dadoi, as.data.frame(table(qtde_filhos)))
                           list(series = list(list(name = 'Quantidade',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar',
                                                        stacked = TRUE,    
                                                       #toolbar = c(show = FALSE),
                                                       height = '90%'),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '80%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories = levels(dadoi[,1])
                                                      ),
                                              
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = FALSE)
                                              )
                                     })  #end renderapex
                                     
  #situacao conjugal ----------------------------
 mod_summary_card_server('inf_sitconjugal', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Qtde. de Filhos')),
                            div(class = 'body',
                      apexchartOutput('inf_sitconjugal_chart', height = '300px')))
                             ) #end taglist
                             )
 
 output$inf_sitconjugal_chart <- renderApex({
                           dadoi <- dados_analise_inf()
                           dadoi$sit_conjungal <- factor(dadoi$sit_conjungal, levels = 1:6)
                          dadoi <- with(dadoi, as.data.frame(table(sit_conjungal)))
                           list(series = list(list(name = 'Quantidade',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar',
                                                        stacked = TRUE,    
                                                       #toolbar = c(show = FALSE),
                                                       height = '90%'),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '80%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories = c('Casado' , 'Solteiro', 'Viúvo', 'Separado/divorciado', 'União estável', 'Ignorado')
                                                      ),
                                              
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = FALSE)
                                              )
                                     })  #end renderapex
           